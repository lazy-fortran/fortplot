module fortplot_color_parsing
    !! Core color parsing functionality for matplotlib-compatible color syntax
    !! 
    !! Supports:
    !! - Hex colors: #FF0000, #F00, #FF000080
    !! - RGB tuples: (1.0, 0.5, 0.0), (255, 128, 0)
    !! - Named colors: red, blue, green, etc.
    !! - Single letters: r, g, b, c, m, y, k, w
    !! - RGBA with alpha channel support
    !! - Performance optimization through caching
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_color_definitions, only: color_t, NUM_NAMED_COLORS, named_color_names, &
        named_color_values, single_letters, letter_to_named, clamp_to_unit, &
        to_lowercase, to_lowercase_char
    use fortplot_logging, only: log_warning
    implicit none
    
    private
    public :: parse_color, parse_color_rgba, is_valid_color
    public :: validate_color_for_backend, clear_color_cache
    public :: parse_colors_bulk, get_cache_hit_rate
    
    ! Color cache for performance optimization
    type :: color_cache_entry_t
        character(len=256) :: color_string = ""
        integer :: string_length = 0
        type(color_t) :: color
        logical :: used = .false.
    end type color_cache_entry_t
    
    ! Cache parameters
    integer, parameter :: MAX_CACHE_SIZE = 1000
    type(color_cache_entry_t) :: color_cache(MAX_CACHE_SIZE)
    integer :: cache_size = 0
    integer :: cache_hits = 0
    integer :: cache_requests = 0

contains

    subroutine parse_color(color_str, rgb, success)
        !! Parse matplotlib-compatible color string to RGB values [0,1]
        character(len=*), intent(in) :: color_str
        real(wp), intent(out) :: rgb(3)
        logical, intent(out) :: success
        
        type(color_t) :: color
        
        call parse_color_internal(color_str, color)
        success = color%valid
        if (success) then
            rgb = [color%r, color%g, color%b]
        else
            rgb = [0.0_wp, 0.0_wp, 0.0_wp]
        end if
    end subroutine parse_color
    
    subroutine parse_color_rgba(color_str, rgba, success)
        !! Parse color string to RGBA values [0,1] including alpha channel
        character(len=*), intent(in) :: color_str
        real(wp), intent(out) :: rgba(4)
        logical, intent(out) :: success
        
        type(color_t) :: color
        
        call parse_color_internal(color_str, color)
        success = color%valid
        if (success) then
            rgba = [color%r, color%g, color%b, color%a]
        else
            rgba = [0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp]
        end if
    end subroutine parse_color_rgba
    
    function is_valid_color(color_str) result(is_valid)
        !! Check if color string is valid without full parsing
        character(len=*), intent(in) :: color_str
        logical :: is_valid
        
        type(color_t) :: color
        
        call parse_color_internal(color_str, color)
        is_valid = color%valid
    end function is_valid_color
    
    subroutine parse_color_internal(color_str, color)
        !! Internal color parsing with caching
        character(len=*), intent(in) :: color_str
        type(color_t), intent(out) :: color
        
        integer :: cache_idx
        character(len=:), allocatable :: trimmed_str
        
        cache_requests = cache_requests + 1
        
        ! Trim and convert to lowercase for consistent lookup
        trimmed_str = trim(adjustl(color_str))
        call to_lowercase(trimmed_str)
        
        ! Check cache first
        cache_idx = find_in_cache(trimmed_str)
        if (cache_idx > 0) then
            cache_hits = cache_hits + 1
            color = color_cache(cache_idx)%color
            return
        end if
        
        ! Parse new color
        call parse_color_string(trimmed_str, color)
        
        ! Add to cache if valid
        if (color%valid) then
            call add_to_cache(trimmed_str, color)
        end if
    end subroutine parse_color_internal
    
    subroutine parse_color_string(color_str, color)
        !! Core color string parsing logic
        character(len=*), intent(in) :: color_str
        type(color_t), intent(out) :: color
        
        character(len=:), allocatable :: str
        
        str = trim(color_str)
        color%valid = .false.
        color%a = 1.0_wp  ! Default alpha
        
        ! Handle empty strings
        if (len_trim(str) == 0) then
            return
        end if
        
        ! Try different parsing methods
        if (str(1:1) == '#') then
            call parse_hex_color(str, color)
        else if (str(1:1) == '(' .and. str(len_trim(str):len_trim(str)) == ')') then
            call parse_rgb_tuple(str, color)
        else if (len_trim(str) == 1) then
            call parse_single_letter(str, color)
        else
            call parse_named_color(str, color)
        end if
    end subroutine parse_color_string
    
    subroutine parse_hex_color(hex_str, color)
        !! Parse hex color: #FF0000, #F00, #FF000080
        character(len=*), intent(in) :: hex_str
        type(color_t), intent(out) :: color
        
        character(len=:), allocatable :: hex_part
        integer :: hex_len, r_val, g_val, b_val, a_val
        
        hex_part = hex_str(2:)  ! Remove #
        hex_len = len_trim(hex_part)
        
        select case (hex_len)
        case (3)  ! #RGB shorthand
            if (parse_hex_digit_pair(hex_part(1:1) // hex_part(1:1), r_val) .and. &
                parse_hex_digit_pair(hex_part(2:2) // hex_part(2:2), g_val) .and. &
                parse_hex_digit_pair(hex_part(3:3) // hex_part(3:3), b_val)) then
                color%r = real(r_val, wp) / 255.0_wp
                color%g = real(g_val, wp) / 255.0_wp
                color%b = real(b_val, wp) / 255.0_wp
                color%a = 1.0_wp
                color%valid = .true.
            end if
        case (6)  ! #RRGGBB
            if (parse_hex_digit_pair(hex_part(1:2), r_val) .and. &
                parse_hex_digit_pair(hex_part(3:4), g_val) .and. &
                parse_hex_digit_pair(hex_part(5:6), b_val)) then
                color%r = real(r_val, wp) / 255.0_wp
                color%g = real(g_val, wp) / 255.0_wp
                color%b = real(b_val, wp) / 255.0_wp
                color%a = 1.0_wp
                color%valid = .true.
            end if
        case (8)  ! #RRGGBBAA
            if (parse_hex_digit_pair(hex_part(1:2), r_val) .and. &
                parse_hex_digit_pair(hex_part(3:4), g_val) .and. &
                parse_hex_digit_pair(hex_part(5:6), b_val) .and. &
                parse_hex_digit_pair(hex_part(7:8), a_val)) then
                color%r = real(r_val, wp) / 255.0_wp
                color%g = real(g_val, wp) / 255.0_wp
                color%b = real(b_val, wp) / 255.0_wp
                color%a = real(a_val, wp) / 255.0_wp
                color%valid = .true.
            end if
        end select
    end subroutine parse_hex_color
    
    function parse_hex_digit_pair(hex_pair, value) result(success)
        !! Parse two hex digits to integer value
        character(len=2), intent(in) :: hex_pair
        integer, intent(out) :: value
        logical :: success
        
        integer :: digit1, digit2
        
        success = hex_digit_to_int(hex_pair(1:1), digit1) .and. &
                  hex_digit_to_int(hex_pair(2:2), digit2)
        
        if (success) then
            value = digit1 * 16 + digit2
        else
            value = 0
        end if
    end function parse_hex_digit_pair
    
    function hex_digit_to_int(hex_char, value) result(success)
        !! Convert single hex character to integer
        character(len=1), intent(in) :: hex_char
        integer, intent(out) :: value
        logical :: success
        
        success = .true.
        select case (hex_char)
        case ('0':'9')
            value = ichar(hex_char) - ichar('0')
        case ('a':'f')
            value = ichar(hex_char) - ichar('a') + 10
        case ('A':'F')
            value = ichar(hex_char) - ichar('A') + 10
        case default
            success = .false.
            value = 0
        end select
    end function hex_digit_to_int
    
    subroutine parse_rgb_tuple(tuple_str, color)
        !! Parse RGB/RGBA tuple: (1.0, 0.5, 0.0), (255, 128, 0), (1.0, 0.5, 0.0, 0.8)
        character(len=*), intent(in) :: tuple_str
        type(color_t), intent(out) :: color
        
        character(len=:), allocatable :: content
        real(wp) :: values(4)
        integer :: n_values, comma_pos, start_pos, end_pos, i
        character(len=50) :: value_str
        integer :: iostat
        
        ! Extract content between parentheses
        content = trim(tuple_str(2:len_trim(tuple_str)-1))
        
        ! Parse comma-separated values
        n_values = 0
        start_pos = 1
        
        do i = 1, 4  ! Maximum 4 values (RGBA)
            comma_pos = index(content(start_pos:), ',')
            if (comma_pos == 0) then
                ! Last value
                end_pos = len_trim(content)
            else
                end_pos = start_pos + comma_pos - 2
            end if
            
            value_str = trim(adjustl(content(start_pos:end_pos)))
            read(value_str, *, iostat=iostat) values(i)
            
            if (iostat /= 0) then
                return  ! Parse error
            end if
            
            n_values = n_values + 1
            
            if (comma_pos == 0) exit
            start_pos = start_pos + comma_pos
        end do
        
        ! Validate number of components
        if (n_values /= 3 .and. n_values /= 4) then
            return
        end if
        
        ! Convert and clamp values
        ! Determine if values are normalized [0,1] or 8-bit [0,255]
        ! Use threshold of 2.0 to distinguish between slightly out-of-range normalized and 8-bit values
        if (any(values(1:3) > 2.0_wp)) then
            ! 8-bit values
            color%r = clamp_to_unit(values(1) / 255.0_wp)
            color%g = clamp_to_unit(values(2) / 255.0_wp)
            color%b = clamp_to_unit(values(3) / 255.0_wp)
            if (n_values == 4) then
                color%a = clamp_to_unit(values(4) / 255.0_wp)
            else
                color%a = 1.0_wp
            end if
        else
            ! Normalized values
            color%r = clamp_to_unit(values(1))
            color%g = clamp_to_unit(values(2))
            color%b = clamp_to_unit(values(3))
            if (n_values == 4) then
                color%a = clamp_to_unit(values(4))
            else
                color%a = 1.0_wp
            end if
        end if
        
        color%valid = .true.
    end subroutine parse_rgb_tuple
    
    subroutine parse_single_letter(letter_str, color)
        !! Parse single letter color: r, g, b, c, m, y, k, w
        character(len=*), intent(in) :: letter_str
        type(color_t), intent(out) :: color
        
        character(len=1) :: letter
        integer :: i
        
        letter = letter_str(1:1)
        call to_lowercase_char(letter)
        
        do i = 1, size(single_letters)
            if (letter == single_letters(i)) then
                color%r = named_color_values(1, letter_to_named(i))
                color%g = named_color_values(2, letter_to_named(i))
                color%b = named_color_values(3, letter_to_named(i))
                color%a = 1.0_wp
                color%valid = .true.
                return
            end if
        end do
    end subroutine parse_single_letter
    
    subroutine parse_named_color(name_str, color)
        !! Parse named color: red, blue, green, etc.
        character(len=*), intent(in) :: name_str
        type(color_t), intent(out) :: color
        
        character(len=:), allocatable :: name
        integer :: i
        
        name = trim(name_str)
        call to_lowercase(name)
        
        do i = 1, NUM_NAMED_COLORS
            if (name == trim(named_color_names(i))) then
                color%r = named_color_values(1, i)
                color%g = named_color_values(2, i)
                color%b = named_color_values(3, i)
                color%a = 1.0_wp
                color%valid = .true.
                return
            end if
        end do
    end subroutine parse_named_color
    
    ! Cache management functions
    function find_in_cache(color_str) result(cache_idx)
        !! Find color in cache, return index or 0 if not found
        character(len=*), intent(in) :: color_str
        integer :: cache_idx
        
        integer :: i
        
        cache_idx = 0
        do i = 1, cache_size
            if (color_cache(i)%used .and. &
                color_cache(i)%string_length > 0 .and. &
                color_cache(i)%color_string(1:color_cache(i)%string_length) == color_str) then
                cache_idx = i
                return
            end if
        end do
    end function find_in_cache
    
    subroutine add_to_cache(color_str, color)
        !! Add color to cache with LRU replacement
        character(len=*), intent(in) :: color_str
        type(color_t), intent(in) :: color
        
        integer :: insert_idx
        
        if (cache_size < MAX_CACHE_SIZE) then
            cache_size = cache_size + 1
            insert_idx = cache_size
        else
            ! Simple replacement - overwrite first entry (could be improved to LRU)
            insert_idx = 1
        end if
        
        color_cache(insert_idx)%string_length = len_trim(color_str)
        color_cache(insert_idx)%color_string = color_str
        color_cache(insert_idx)%color = color
        color_cache(insert_idx)%used = .true.
    end subroutine add_to_cache
    
    subroutine clear_color_cache()
        !! Clear the color cache
        integer :: i
        
        do i = 1, cache_size
            color_cache(i)%color_string = ""
            color_cache(i)%string_length = 0
            color_cache(i)%used = .false.
        end do
        
        cache_size = 0
        cache_hits = 0
        cache_requests = 0
    end subroutine clear_color_cache
    
    function get_cache_hit_rate() result(hit_rate)
        !! Get cache hit rate for performance monitoring
        real(wp) :: hit_rate
        
        if (cache_requests > 0) then
            hit_rate = real(cache_hits, wp) / real(cache_requests, wp)
        else
            hit_rate = 0.0_wp
        end if
    end function get_cache_hit_rate
    
    ! Backend validation
    function validate_color_for_backend(color_str, backend) result(is_valid)
        !! Validate color for specific backend constraints
        character(len=*), intent(in) :: color_str, backend
        logical :: is_valid
        
        type(color_t) :: color
        character(len=:), allocatable :: backend_lower
        
        call parse_color_internal(color_str, color)
        is_valid = color%valid
        
        if (.not. is_valid) return
        
        backend_lower = trim(backend)
        call to_lowercase(backend_lower)
        
        select case (backend_lower)
        case ('ascii')
            ! ASCII backend can handle all valid colors (maps to characters)
            is_valid = .true.
        case ('png')
            ! PNG backend supports full RGBA
            is_valid = .true.
        case ('pdf')
            ! PDF backend supports RGB (alpha may be limited)
            is_valid = .true.
        case default
            is_valid = .true.
        end select
    end function validate_color_for_backend
    
    ! Bulk operations for performance
    subroutine parse_colors_bulk(color_specs, rgb_results, success_flags)
        !! Parse multiple colors efficiently
        character(len=*), intent(in) :: color_specs(:)
        real(wp), intent(out) :: rgb_results(:,:)
        logical, intent(out) :: success_flags(:)
        
        integer :: i, n_colors
        type(color_t) :: color
        
        n_colors = size(color_specs)
        
        do i = 1, n_colors
            call parse_color_internal(color_specs(i), color)
            success_flags(i) = color%valid
            if (color%valid) then
                rgb_results(:, i) = [color%r, color%g, color%b]
            else
                rgb_results(:, i) = [0.0_wp, 0.0_wp, 0.0_wp]
            end if
        end do
    end subroutine parse_colors_bulk

end module fortplot_color_parsing