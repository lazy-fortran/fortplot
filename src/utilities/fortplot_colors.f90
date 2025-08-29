module fortplot_colors
    !! Color parsing and management for matplotlib-compatible color syntax
    !! 
    !! Supports:
    !! - Hex colors: #FF0000, #F00
    !! - RGB tuples: (1.0, 0.5, 0.0), (255, 128, 0)
    !! - Named colors: red, blue, green, etc.
    !! - Single letters: r, g, b, c, m, y, k, w
    !! - RGBA with alpha channel support
    !! - Performance optimization through caching
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_logging, only: log_warning
    implicit none
    
    private
    public :: color_t, parse_color, parse_color_rgba, is_valid_color
    public :: validate_color_for_backend, clear_color_cache
    public :: parse_colors_bulk, get_cache_hit_rate
    public :: rgb_to_hsv, rgb_to_lab
    public :: apply_colormap_to_array
    
    ! Color type for unified representation
    type :: color_t
        real(wp) :: r = 0.0_wp
        real(wp) :: g = 0.0_wp  
        real(wp) :: b = 0.0_wp
        real(wp) :: a = 1.0_wp  ! Alpha channel
        logical :: valid = .false.
    end type color_t
    
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
    
    ! Named color constants - CSS4/matplotlib standard colors
    integer, parameter :: NUM_NAMED_COLORS = 20
    character(len=12), parameter :: named_color_names(NUM_NAMED_COLORS) = [ &
        'red         ', 'green       ', 'blue        ', 'cyan        ', &
        'magenta     ', 'yellow      ', 'black       ', 'white       ', &
        'gray        ', 'orange      ', 'purple      ', 'brown       ', &
        'pink        ', 'olive       ', 'navy        ', 'lime        ', &
        'teal        ', 'silver      ', 'maroon      ', 'indigo      ' &
    ]
    
    real(wp), parameter :: named_color_values(3, NUM_NAMED_COLORS) = reshape([ &
        1.0_wp, 0.0_wp, 0.0_wp,  & ! red
        0.0_wp, 0.5_wp, 0.0_wp,  & ! green (CSS4 green, not lime)
        0.0_wp, 0.0_wp, 1.0_wp,  & ! blue
        0.0_wp, 1.0_wp, 1.0_wp,  & ! cyan
        1.0_wp, 0.0_wp, 1.0_wp,  & ! magenta
        1.0_wp, 1.0_wp, 0.0_wp,  & ! yellow
        0.0_wp, 0.0_wp, 0.0_wp,  & ! black
        1.0_wp, 1.0_wp, 1.0_wp,  & ! white
        0.5_wp, 0.5_wp, 0.5_wp,  & ! gray
        1.0_wp, 0.647_wp, 0.0_wp, & ! orange
        0.5_wp, 0.0_wp, 0.5_wp,  & ! purple
        0.647_wp, 0.165_wp, 0.165_wp, & ! brown
        1.0_wp, 0.753_wp, 0.796_wp, & ! pink
        0.5_wp, 0.5_wp, 0.0_wp,  & ! olive
        0.0_wp, 0.0_wp, 0.5_wp,  & ! navy
        0.0_wp, 1.0_wp, 0.0_wp,  & ! lime
        0.0_wp, 0.5_wp, 0.5_wp,  & ! teal
        0.753_wp, 0.753_wp, 0.753_wp, & ! silver
        0.5_wp, 0.0_wp, 0.0_wp,  & ! maroon
        0.294_wp, 0.0_wp, 0.51_wp  & ! indigo
    ], [3, NUM_NAMED_COLORS])
    
    ! Single letter color mapping (matplotlib compatible)
    character(len=1), parameter :: single_letters(8) = ['r', 'g', 'b', 'c', 'm', 'y', 'k', 'w']
    integer, parameter :: letter_to_named(8) = [1, 2, 3, 4, 5, 6, 7, 8]  ! Map to named_color_values indices
    
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
    
    function clamp_to_unit(value) result(clamped)
        !! Clamp value to [0,1] range
        real(wp), intent(in) :: value
        real(wp) :: clamped
        
        clamped = max(0.0_wp, min(1.0_wp, value))
    end function clamp_to_unit
    
    subroutine to_lowercase(str)
        !! Convert string to lowercase in-place
        character(len=:), allocatable, intent(inout) :: str
        integer :: i
        
        do i = 1, len(str)
            call to_lowercase_char(str(i:i))
        end do
    end subroutine to_lowercase
    
    subroutine to_lowercase_char(char)
        !! Convert single character to lowercase
        character(len=1), intent(inout) :: char
        
        if (char >= 'A' .and. char <= 'Z') then
            char = achar(iachar(char) + 32)
        end if
    end subroutine to_lowercase_char
    
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
    
    ! Color space conversions
    subroutine rgb_to_hsv(rgb, hsv)
        !! Convert RGB to HSV color space
        real(wp), intent(in) :: rgb(3)
        real(wp), intent(out) :: hsv(3)
        
        real(wp) :: r, g, b, max_val, min_val, delta
        
        r = rgb(1)
        g = rgb(2)
        b = rgb(3)
        
        max_val = max(r, max(g, b))
        min_val = min(r, min(g, b))
        delta = max_val - min_val
        
        ! Value
        hsv(3) = max_val
        
        ! Saturation
        if (max_val > 0.0_wp) then
            hsv(2) = delta / max_val
        else
            hsv(2) = 0.0_wp
        end if
        
        ! Hue
        if (delta == 0.0_wp) then
            hsv(1) = 0.0_wp
        else if (max_val == r) then
            hsv(1) = 60.0_wp * modulo((g - b) / delta, 6.0_wp)
        else if (max_val == g) then
            hsv(1) = 60.0_wp * ((b - r) / delta + 2.0_wp)
        else
            hsv(1) = 60.0_wp * ((r - g) / delta + 4.0_wp)
        end if
    end subroutine rgb_to_hsv
    
    subroutine rgb_to_lab(rgb, lab)
        !! Convert RGB to LAB color space (simplified implementation)
        real(wp), intent(in) :: rgb(3)
        real(wp), intent(out) :: lab(3)
        
        real(wp) :: xyz(3)
        
        ! First convert RGB to XYZ, then XYZ to LAB
        call rgb_to_xyz(rgb, xyz)
        call xyz_to_lab(xyz, lab)
    end subroutine rgb_to_lab
    
    subroutine rgb_to_xyz(rgb, xyz)
        !! Convert RGB to XYZ color space
        real(wp), intent(in) :: rgb(3)
        real(wp), intent(out) :: xyz(3)
        
        real(wp) :: r, g, b
        
        ! Gamma correction
        r = gamma_correct(rgb(1))
        g = gamma_correct(rgb(2))
        b = gamma_correct(rgb(3))
        
        ! sRGB to XYZ transformation matrix
        xyz(1) = 0.4124564_wp * r + 0.3575761_wp * g + 0.1804375_wp * b
        xyz(2) = 0.2126729_wp * r + 0.7151522_wp * g + 0.0721750_wp * b
        xyz(3) = 0.0193339_wp * r + 0.1191920_wp * g + 0.9503041_wp * b
    end subroutine rgb_to_xyz
    
    function gamma_correct(channel) result(corrected)
        !! Apply gamma correction for sRGB
        real(wp), intent(in) :: channel
        real(wp) :: corrected
        
        if (channel <= 0.04045_wp) then
            corrected = channel / 12.92_wp
        else
            corrected = ((channel + 0.055_wp) / 1.055_wp)**2.4_wp
        end if
    end function gamma_correct
    
    subroutine xyz_to_lab(xyz, lab)
        !! Convert XYZ to LAB color space
        real(wp), intent(in) :: xyz(3)
        real(wp), intent(out) :: lab(3)
        
        real(wp), parameter :: XN = 0.95047_wp  ! D65 illuminant
        real(wp), parameter :: YN = 1.00000_wp
        real(wp), parameter :: ZN = 1.08883_wp
        
        real(wp) :: fx, fy, fz
        
        fx = lab_f(xyz(1) / XN)
        fy = lab_f(xyz(2) / YN)
        fz = lab_f(xyz(3) / ZN)
        
        lab(1) = 116.0_wp * fy - 16.0_wp  ! L*
        lab(2) = 500.0_wp * (fx - fy)     ! a*
        lab(3) = 200.0_wp * (fy - fz)     ! b*
    end subroutine xyz_to_lab
    
    function lab_f(t) result(f_val)
        !! LAB conversion helper function
        real(wp), intent(in) :: t
        real(wp) :: f_val
        
        real(wp), parameter :: DELTA = 6.0_wp / 29.0_wp
        
        if (t > DELTA**3) then
            f_val = t**(1.0_wp/3.0_wp)
        else
            f_val = t / (3.0_wp * DELTA**2) + 4.0_wp / 29.0_wp
        end if
    end function lab_f
    
    ! Colormap application for large arrays
    subroutine apply_colormap_to_array(values, colormap, rgb_mapped)
        !! Apply colormap to array of values efficiently
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in) :: colormap
        real(wp), intent(out) :: rgb_mapped(:,:)
        
        integer :: i, n_points
        real(wp) :: val_min, val_max, normalized_val
        
        n_points = size(values)
        val_min = minval(values)
        val_max = maxval(values)
        
        ! Avoid division by zero
        if (val_max == val_min) then
            rgb_mapped = 0.5_wp  ! Mid-gray for uniform data
            return
        end if
        
        do i = 1, n_points
            normalized_val = (values(i) - val_min) / (val_max - val_min)
            call apply_colormap_value(normalized_val, colormap, rgb_mapped(:, i))
        end do
    end subroutine apply_colormap_to_array
    
    subroutine apply_colormap_value(normalized_val, colormap, rgb)
        !! Apply colormap to single normalized value [0,1]
        real(wp), intent(in) :: normalized_val
        character(len=*), intent(in) :: colormap
        real(wp), intent(out) :: rgb(3)
        
        character(len=:), allocatable :: cmap_lower
        real(wp) :: t
        
        t = clamp_to_unit(normalized_val)
        cmap_lower = trim(colormap)
        call to_lowercase(cmap_lower)
        
        select case (cmap_lower)
        case ('viridis')
            call viridis_colormap(t, rgb)
        case ('plasma')
            call plasma_colormap(t, rgb)
        case ('coolwarm')
            call coolwarm_colormap(t, rgb)
        case default
            ! Default to simple grayscale
            rgb = [t, t, t]
        end select
    end subroutine apply_colormap_value
    
    subroutine viridis_colormap(t, rgb)
        !! Simplified viridis colormap
        real(wp), intent(in) :: t
        real(wp), intent(out) :: rgb(3)
        
        ! Simplified viridis approximation
        rgb(1) = 0.267004_wp + t * (0.993248_wp - 0.267004_wp)  ! Purple to yellow
        rgb(2) = 0.004874_wp + t * (0.906157_wp - 0.004874_wp)
        rgb(3) = 0.329415_wp + t * (0.143936_wp - 0.329415_wp)
    end subroutine viridis_colormap
    
    subroutine plasma_colormap(t, rgb)
        !! Simplified plasma colormap  
        real(wp), intent(in) :: t
        real(wp), intent(out) :: rgb(3)
        
        rgb(1) = 0.050383_wp + t * (0.940015_wp - 0.050383_wp)  ! Dark to bright
        rgb(2) = 0.029803_wp + t * (0.975158_wp - 0.029803_wp)
        rgb(3) = 0.527975_wp + t * (0.131326_wp - 0.527975_wp)
    end subroutine plasma_colormap
    
    subroutine coolwarm_colormap(t, rgb)
        !! Coolwarm diverging colormap
        real(wp), intent(in) :: t
        real(wp), intent(out) :: rgb(3)
        
        if (t < 0.5_wp) then
            ! Cool side (blue to white)
            rgb(1) = 0.230_wp + 2.0_wp * t * (1.0_wp - 0.230_wp)
            rgb(2) = 0.299_wp + 2.0_wp * t * (1.0_wp - 0.299_wp)
            rgb(3) = 0.754_wp + 2.0_wp * t * (1.0_wp - 0.754_wp)
        else
            ! Warm side (white to red)
            rgb(1) = 1.0_wp + 2.0_wp * (t - 0.5_wp) * (0.706_wp - 1.0_wp)
            rgb(2) = 1.0_wp + 2.0_wp * (t - 0.5_wp) * (0.016_wp - 1.0_wp)
            rgb(3) = 1.0_wp + 2.0_wp * (t - 0.5_wp) * (0.150_wp - 1.0_wp)
        end if
    end subroutine coolwarm_colormap

end module fortplot_colors