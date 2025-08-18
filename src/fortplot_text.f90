module fortplot_text
    use iso_c_binding
    use fortplot_stb_truetype
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    use fortplot_logging, only: log_error
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: init_text_system, cleanup_text_system, render_text_to_image, calculate_text_width, calculate_text_height
    public :: render_rotated_text_to_image, get_font_metrics
    public :: get_font_ascent_ratio, find_font_by_name, find_any_available_font
    
    ! Constants for text rendering
    integer, parameter :: DEFAULT_FONT_SIZE = 16
    real(wp), parameter :: PI = 3.14159265359_wp

    ! Module state
    type(stb_fontinfo_t) :: global_font
    logical :: font_initialized = .false.
    real(wp) :: font_scale = 0.0_wp
    
    
contains

    function init_text_system() result(success)
        !! Initialize STB TrueType font system with robust font discovery
        logical :: success
        
        success = .false.
        
        if (font_initialized) then
            success = .true.
            return
        end if
        
        success = discover_and_init_font()
        
        if (.not. success) then
            call log_error("Could not initialize STB TrueType - no fonts found")
        end if
        
    end function init_text_system

    function discover_and_init_font() result(success)
        !! Discover and initialize font from system locations
        logical :: success
        character(len=256) :: font_path
        
        success = .false.
        
        ! Priority order: Helvetica -> Liberation Sans -> Arial -> DejaVu Sans
        if (find_font_by_name("Helvetica", font_path)) then
            success = try_init_font(font_path)
            if (success) return
        end if
        
        if (find_font_by_name("Liberation Sans", font_path)) then
            success = try_init_font(font_path)
            if (success) return
        end if
        
        if (find_font_by_name("Arial", font_path)) then
            success = try_init_font(font_path)
            if (success) return
        end if
        
        if (find_font_by_name("DejaVu Sans", font_path)) then
            success = try_init_font(font_path)
            if (success) return
        end if
        
    end function discover_and_init_font

    function find_font_by_name(font_name, font_path) result(found)
        !! Find font by name in typical system locations
        character(len=*), intent(in) :: font_name
        character(len=256), intent(out) :: font_path
        logical :: found
        
        found = .false.
        
        select case (trim(font_name))
        case ("Helvetica")
            call check_helvetica_paths(font_path, found)
        case ("Liberation Sans")
            call check_liberation_paths(font_path, found)
        case ("Arial")
            call check_arial_paths(font_path, found)
        case ("DejaVu Sans")
            call check_dejavu_paths(font_path, found)
        end select
        
    end function find_font_by_name

    function find_any_available_font(font_path) result(found)
        !! Find any available font using same priority order as system initialization
        character(len=256), intent(out) :: font_path
        logical :: found
        
        found = .false.
        
        ! Use same priority order as discover_and_init_font
        if (find_font_by_name("Helvetica", font_path)) then
            found = .true.
            return
        end if
        
        if (find_font_by_name("Liberation Sans", font_path)) then
            found = .true.
            return
        end if
        
        if (find_font_by_name("Arial", font_path)) then
            found = .true.
            return
        end if
        
        if (find_font_by_name("DejaVu Sans", font_path)) then
            found = .true.
            return
        end if
        
    end function find_any_available_font

    subroutine check_helvetica_paths(font_path, found)
        character(len=256), intent(out) :: font_path
        logical, intent(out) :: found
        character(len=256) :: candidates(4)
        integer :: i
        
        found = .false.
        
        candidates(1) = "/System/Library/Fonts/Helvetica.ttc"
        candidates(2) = "/System/Library/Fonts/HelveticaNeue.ttc"
        candidates(3) = "/Library/Fonts/Helvetica.ttf"
        candidates(4) = "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
        
        do i = 1, 4
            if (file_exists(candidates(i))) then
                font_path = candidates(i)
                found = .true.
                return
            end if
        end do
    end subroutine check_helvetica_paths

    subroutine check_liberation_paths(font_path, found)
        character(len=256), intent(out) :: font_path
        logical, intent(out) :: found
        character(len=256) :: candidates(4)
        integer :: i
        
        found = .false.
        
        candidates(1) = "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
        candidates(2) = "/usr/share/fonts/liberation-fonts/LiberationSans-Regular.ttf"
        candidates(3) = "/usr/share/fonts/TTF/LiberationSans-Regular.ttf"
        candidates(4) = "/usr/local/share/fonts/LiberationSans-Regular.ttf"
        
        do i = 1, 4
            if (file_exists(candidates(i))) then
                font_path = candidates(i)
                found = .true.
                return
            end if
        end do
    end subroutine check_liberation_paths

    subroutine check_arial_paths(font_path, found)
        character(len=256), intent(out) :: font_path
        logical, intent(out) :: found
        character(len=256) :: candidates(5)
        integer :: i
        
        found = .false.
        
        candidates(1) = "/System/Library/Fonts/Arial.ttf"
        candidates(2) = "/Library/Fonts/Arial.ttf"
        candidates(3) = "/usr/share/fonts/truetype/msttcorefonts/arial.ttf"
        candidates(4) = "/usr/share/fonts/TTF/arial.ttf"
        candidates(5) = "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
        
        do i = 1, 5
            if (file_exists(candidates(i))) then
                font_path = candidates(i)
                found = .true.
                return
            end if
        end do
    end subroutine check_arial_paths

    subroutine check_dejavu_paths(font_path, found)
        character(len=256), intent(out) :: font_path
        logical, intent(out) :: found
        character(len=256) :: candidates(4)
        integer :: i
        
        found = .false.
        
        candidates(1) = "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
        candidates(2) = "/usr/share/fonts/TTF/DejaVuSans.ttf"
        candidates(3) = "/usr/share/fonts/dejavu-fonts/DejaVuSans.ttf"
        candidates(4) = "/usr/local/share/fonts/DejaVuSans.ttf"
        
        do i = 1, 4
            if (file_exists(candidates(i))) then
                font_path = candidates(i)
                found = .true.
                return
            end if
        end do
    end subroutine check_dejavu_paths

    function file_exists(file_path) result(exists)
        !! Check if file exists using inquire
        character(len=*), intent(in) :: file_path
        logical :: exists
        
        inquire(file=trim(file_path), exist=exists)
    end function file_exists

    function try_init_font(font_path) result(success)
        !! Try to initialize font from given path
        character(len=*), intent(in) :: font_path
        logical :: success
        
        success = .false.
        
        if (stb_init_font(global_font, trim(font_path))) then
            font_scale = stb_scale_for_pixel_height(global_font, real(DEFAULT_FONT_SIZE, wp))
            font_initialized = .true.
            success = .true.
        end if
        
    end function try_init_font

    subroutine cleanup_text_system()
        !! Clean up STB TrueType font system
        if (font_initialized) then
            call stb_cleanup_font(global_font)
            font_initialized = .false.
            font_scale = 0.0_wp
        end if
    end subroutine cleanup_text_system

    function calculate_text_width(text) result(width)
        !! Calculate the pixel width of text using STB TrueType with UTF-8 support
        character(len=*), intent(in) :: text
        integer :: width
        integer :: i, char_code, advance_width, left_side_bearing
        integer :: char_len
        
        ! Initialize text system if not already done
        if (.not. font_initialized) then
            if (.not. init_text_system()) then
                call log_error("STB TrueType initialization failed in calculate_text_width")
                width = len_trim(text) * 8  ! Fallback estimate
                return
            end if
        end if
        
        width = 0
        i = 1
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))
            if (char_len == 0) then
                ! Invalid UTF-8, treat as single byte
                char_code = iachar(text(i:i))
                i = i + 1
            else
                char_code = utf8_to_codepoint(text, i)
                i = i + char_len
            end if
            
            call stb_get_codepoint_hmetrics(global_font, char_code, advance_width, left_side_bearing)
            ! Scale to pixel coordinates
            width = width + int(real(advance_width) * font_scale)
        end do
        
    end function calculate_text_width

    function calculate_text_height(text) result(height)
        !! Calculate the pixel height of text using STB TrueType
        character(len=*), intent(in) :: text
        integer :: height
        integer :: ascent, descent, line_gap
        
        ! Suppress unused parameter warnings
        associate(unused_text => text); end associate
        
        if (.not. font_initialized) then
            if (.not. init_text_system()) then
                height = DEFAULT_FONT_SIZE  ! Fallback
                return
            end if
        end if
        
        ! Get font metrics and scale to pixels
        call stb_get_font_vmetrics(global_font, ascent, descent, line_gap)
        height = int(real(ascent - descent) * font_scale)
        
        ! Ensure minimum reasonable height
        if (height <= 0) height = DEFAULT_FONT_SIZE
        
    end function calculate_text_height

    subroutine render_text_to_image(image_data, width, height, x, y, text, r, g, b)
        !! Render text to image using STB TrueType with UTF-8 support
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        integer :: pen_x, pen_y, i, char_code
        integer :: advance_width, left_side_bearing
        type(c_ptr) :: bitmap_ptr
        integer :: bmp_width, bmp_height, xoff, yoff
        integer :: char_len
        
        if (.not. font_initialized) then
            if (.not. init_text_system()) then
                call render_simple_placeholder(image_data, width, height, x, y, r, g, b)
                return
            end if
        end if
        
        pen_x = x
        pen_y = y
        
        i = 1
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))
            if (char_len == 0) then
                ! Invalid UTF-8, treat as single byte
                char_code = iachar(text(i:i))
                i = i + 1
            else
                char_code = utf8_to_codepoint(text, i)
                i = i + char_len
            end if
            
            ! Get character bitmap
            bitmap_ptr = stb_get_codepoint_bitmap(global_font, font_scale, font_scale, char_code, &
                                                 bmp_width, bmp_height, xoff, yoff)
            
            if (c_associated(bitmap_ptr)) then
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                     bitmap_ptr, bmp_width, bmp_height, xoff, yoff, r, g, b)
                call stb_free_bitmap(bitmap_ptr)
            end if
            
            ! Advance pen position
            call stb_get_codepoint_hmetrics(global_font, char_code, advance_width, left_side_bearing)
            pen_x = pen_x + int(real(advance_width) * font_scale)
        end do
    end subroutine render_text_to_image

    subroutine render_stb_glyph(image_data, width, height, pen_x, pen_y, bitmap_ptr, &
                               bmp_width, bmp_height, xoff, yoff, r, g, b)
        !! Render STB TrueType glyph bitmap to image
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, pen_x, pen_y
        type(c_ptr), intent(in) :: bitmap_ptr
        integer, intent(in) :: bmp_width, bmp_height, xoff, yoff
        integer(1), intent(in) :: r, g, b
        integer(c_int8_t), pointer :: bitmap_buffer(:)
        integer :: glyph_x, glyph_y, img_x, img_y, row, col, pixel_idx
        integer :: alpha_int
        real :: alpha_f, bg_r, bg_g, bg_b
        
        if (bmp_width <= 0 .or. bmp_height <= 0) then
            return
        end if
        
        call c_f_pointer(bitmap_ptr, bitmap_buffer, [bmp_width * bmp_height])
        
        glyph_x = pen_x + xoff
        glyph_y = pen_y + yoff  ! STB yoff is negative for characters above baseline
        
        do row = 0, bmp_height - 1
            do col = 0, bmp_width - 1
                img_x = glyph_x + col
                img_y = glyph_y + row
                
                if (img_x >= 0 .and. img_x < width .and. img_y >= 0 .and. img_y < height) then
                    ! Convert signed int8 to unsigned (0-255 range)
                    alpha_int = int(bitmap_buffer(row * bmp_width + col + 1))
                    if (alpha_int < 0) alpha_int = alpha_int + 256
                    
                    if (alpha_int > 0) then  ! Only render non-transparent pixels
                        pixel_idx = (img_y * width + img_x) * 3 + 1
                        
                        ! Safety bounds check  
                        if (pixel_idx < 1 .or. pixel_idx + 2 > width * height * 3) then
                            cycle  ! Skip this pixel if out of bounds
                        end if
                        
                        alpha_f = real(alpha_int) / 255.0
                        bg_r = real(int(image_data(pixel_idx), &
                            kind=selected_int_kind(2)) + merge(256, 0, image_data(pixel_idx) < 0))
                        bg_g = real(int(image_data(pixel_idx + 1), &
                            kind=selected_int_kind(2)) + merge(256, 0, image_data(pixel_idx + 1) < 0))
                        bg_b = real(int(image_data(pixel_idx + 2), &
                            kind=selected_int_kind(2)) + merge(256, 0, image_data(pixel_idx + 2) < 0))
                        
                        ! Alpha blending
                        image_data(pixel_idx) = int(bg_r * (1.0 - alpha_f) + real(int(r) + merge(256, 0, r < 0)) * alpha_f, 1)
                        image_data(pixel_idx + 1) = int(bg_g * (1.0 - alpha_f) + real(int(g) + merge(256, 0, g < 0)) * alpha_f, 1)
                        image_data(pixel_idx + 2) = int(bg_b * (1.0 - alpha_f) + real(int(b) + merge(256, 0, b < 0)) * alpha_f, 1)
                    end if
                end if
            end do
        end do
    end subroutine render_stb_glyph
    


    subroutine render_simple_placeholder(image_data, width, height, x, y, r, g, b)
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        integer(1), intent(in) :: r, g, b
        integer :: pixel_idx, img_x, img_y, max_idx
        
        max_idx = width * height * 3
        
        
        do img_y = y, min(y + 6, height - 1)
            do img_x = x, min(x + 4, width - 1)
                if (img_x >= 0 .and. img_y >= 0) then
                    pixel_idx = (img_y * width + img_x) * 3 + 1
                    if (pixel_idx > 0 .and. pixel_idx <= max_idx - 2) then
                        image_data(pixel_idx) = r
                        image_data(pixel_idx + 1) = g
                        image_data(pixel_idx + 2) = b
                    end if
                end if
            end do
        end do
    end subroutine render_simple_placeholder

    
    
    function get_character_pixel(char_code, x, y) result(pixel_set)
        integer, intent(in) :: char_code, x, y
        logical :: pixel_set
        
        pixel_set = .false.
        
        select case (char_code)
        case (48) ! '0'
            pixel_set = (x == 0 .or. x == 3) .and. (y >= 1 .and. y <= 6) .or. &
                       (y == 0 .or. y == 7) .and. (x >= 1 .and. x <= 2)
        case (49) ! '1'
            pixel_set = x == 2 .and. (y >= 0 .and. y <= 7)
        case (50) ! '2'
            pixel_set = (y == 0 .or. y == 3 .or. y == 7) .and. (x >= 0 .and. x <= 3) .or. &
                       x == 3 .and. (y >= 1 .and. y <= 2) .or. &
                       x == 0 .and. (y >= 4 .and. y <= 6)
        case (51) ! '3'
            pixel_set = (y == 0 .or. y == 3 .or. y == 7) .and. (x >= 0 .and. x <= 3) .or. &
                       x == 3 .and. ((y >= 1 .and. y <= 2) .or. (y >= 4 .and. y <= 6))
        case (53) ! '5'
            pixel_set = (y == 0 .or. y == 3 .or. y == 7) .and. (x >= 0 .and. x <= 3) .or. &
                       x == 0 .and. (y >= 1 .and. y <= 2) .or. &
                       x == 3 .and. (y >= 4 .and. y <= 6)
        case (55) ! '7'
            pixel_set = y == 0 .and. (x >= 0 .and. x <= 3) .or. &
                       x == 3 .and. (y >= 1 .and. y <= 7)
        case (45) ! '-'
            pixel_set = y == 3 .and. (x >= 0 .and. x <= 3)
        case (46) ! '.'
            pixel_set = y == 7 .and. x == 1
        case default
            pixel_set = (x >= 1 .and. x <= 2) .and. (y >= 2 .and. y <= 5)
        end select
    end function get_character_pixel

    subroutine render_rotated_text_to_image(image_data, width, height, x, y, text, r, g, b, angle)
        !! Render rotated text to PNG image using STB TrueType with UTF-8 support
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: angle  ! Rotation angle in degrees
        
        integer :: i, char_code, pen_x, pen_y
        integer :: advance_width, left_side_bearing
        type(c_ptr) :: bitmap_ptr
        integer :: bmp_width, bmp_height, xoff, yoff
        real(wp) :: cos_a, sin_a
        integer :: char_len
        
        if (.not. font_initialized) then
            if (.not. init_text_system()) then
                return
            end if
        end if
        
        pen_x = x
        pen_y = y
        cos_a = cos(angle * PI / 180.0_wp)
        sin_a = sin(angle * PI / 180.0_wp)
        
        ! For now, render text normally (STB doesn't have built-in rotation)
        ! TODO: Implement proper bitmap rotation if needed
        i = 1
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))
            if (char_len == 0) then
                ! Invalid UTF-8, treat as single byte
                char_code = iachar(text(i:i))
                i = i + 1
            else
                char_code = utf8_to_codepoint(text, i)
                i = i + char_len
            end if
            
            bitmap_ptr = stb_get_codepoint_bitmap(global_font, font_scale, font_scale, char_code, &
                                                 bmp_width, bmp_height, xoff, yoff)
            
            if (c_associated(bitmap_ptr)) then
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                     bitmap_ptr, bmp_width, bmp_height, xoff, yoff, r, g, b)
                call stb_free_bitmap(bitmap_ptr)
            end if
            
            ! Advance with rotation
            call stb_get_codepoint_hmetrics(global_font, char_code, advance_width, left_side_bearing)
            pen_x = pen_x + int(real(advance_width) * font_scale * cos_a)
            pen_y = pen_y + int(real(advance_width) * font_scale * sin_a)
        end do
    end subroutine render_rotated_text_to_image

    subroutine get_font_metrics(ascent_pixels, descent_pixels, line_gap_pixels, success)
        !! Get font metrics in pixels for current font
        real(wp), intent(out) :: ascent_pixels, descent_pixels, line_gap_pixels
        logical, intent(out) :: success
        integer :: ascent, descent, line_gap
        
        success = .false.
        ascent_pixels = 0.0_wp
        descent_pixels = 0.0_wp  
        line_gap_pixels = 0.0_wp
        
        if (.not. font_initialized) then
            if (.not. init_text_system()) then
                return
            end if
        end if
        
        if (font_initialized) then
            call stb_get_font_vmetrics(global_font, ascent, descent, line_gap)
            ascent_pixels = real(ascent, wp) * font_scale
            descent_pixels = abs(real(descent, wp)) * font_scale  ! descent is usually negative
            line_gap_pixels = real(line_gap, wp) * font_scale
            success = .true.
        end if
    end subroutine get_font_metrics

    function get_font_ascent_ratio() result(ratio)
        !! Get the ratio of font ascent to total height
        !! This is used to properly center text vertically
        real(wp) :: ratio
        integer :: ascent, descent, line_gap
        
        ratio = 0.7_wp  ! Default fallback value
        
        if (.not. font_initialized) then
            if (.not. init_text_system()) then
                return
            end if
        end if
        
        if (font_initialized) then
            call stb_get_font_vmetrics(global_font, ascent, descent, line_gap)
            ! Calculate ratio of ascent to total height
            ! descent is typically negative, so we use abs()
            if (ascent > 0) then
                ratio = real(ascent, wp) / real(ascent - descent, wp)
            end if
        end if
    end function get_font_ascent_ratio

end module fortplot_text
