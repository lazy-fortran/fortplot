module fortplot_text_rendering
    use iso_c_binding
    use fortplot_stb_truetype
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    use fortplot_logging, only: log_error
    use fortplot_text_fonts, only: init_text_system, get_global_font, get_font_scale, &
                                     is_font_initialized, get_font_scale_for_size
    use fortplot_mathtext, only: parse_mathtext, render_mathtext_elements, &
                                 calculate_mathtext_width, calculate_mathtext_height
    use fortplot_raster_primitives, only: draw_line_distance_aa
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: render_text_to_image, calculate_text_width, calculate_text_height
    public :: render_rotated_text_to_image, calculate_text_descent
    public :: calculate_text_width_with_size, render_text_with_size
    public :: TITLE_FONT_SIZE, LABEL_FONT_SIZE, TICK_FONT_SIZE

    ! Constants for text rendering
    ! Font sizes in pixels (matching matplotlib at 96 DPI)
    ! Matplotlib uses: title=12pt, labels=10pt, ticks=10pt
    ! At 96 DPI: 1pt = 1.333 pixels
    integer, parameter :: DEFAULT_FONT_SIZE = 16  ! ~12pt at 96 DPI
    integer, parameter :: TITLE_FONT_SIZE = 20     ! ~15pt at 96 DPI (larger than matplotlib's 12pt)
    integer, parameter :: LABEL_FONT_SIZE = 16     ! ~12pt at 96 DPI
    integer, parameter :: TICK_FONT_SIZE = 13      ! ~10pt at 96 DPI
    real(wp), parameter :: PI = 3.14159265359_wp


contains

    function has_mathtext(text) result(is_mathtext)
        !! Check if text contains mathematical notation
        character(len=*), intent(in) :: text
        logical :: is_mathtext

        is_mathtext = (index(text, '^') > 0) .or. &
                      (index(text, '_') > 0) .or. &
                      (index(text, '\') > 0)
    end function has_mathtext

    function calculate_text_width(text) result(width)
        !! Calculate the pixel width of text using STB TrueType with UTF-8 support
        !! Supports mathematical notation with superscripts and subscripts
        use fortplot_mathtext, only: mathtext_element_t
        character(len=*), intent(in) :: text
        integer :: width
        integer :: i, char_code, advance_width, left_side_bearing
        integer :: char_len
        type(stb_fontinfo_t) :: font
        real(wp) :: scale
        type(mathtext_element_t), allocatable :: elements(:)

        ! Initialize text system if not already done
        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                call log_error("STB TrueType initialization failed in calculate_text_width")
                width = len_trim(text) * 8  ! Fallback estimate
                return
            end if
        end if

        ! Check if text contains mathematical notation
        if (has_mathtext(text)) then
            elements = parse_mathtext(text)
            width = calculate_mathtext_width_internal(elements, real(DEFAULT_FONT_SIZE, wp))
            return
        end if

        ! Standard text rendering path
        font = get_global_font()
        scale = get_font_scale()

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

            call stb_get_codepoint_hmetrics(font, char_code, advance_width, left_side_bearing)
            ! Scale to pixel coordinates
            width = width + int(real(advance_width) * scale)
        end do

    end function calculate_text_width

    function calculate_text_width_with_size(text, pixel_height) result(width)
        !! Calculate text width using a specific font size
        !! Supports mathematical notation with superscripts and subscripts
        use fortplot_text_fonts, only: get_font_scale_for_size
        use fortplot_mathtext, only: mathtext_element_t
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: pixel_height
        integer :: width
        integer :: i, char_code, advance_width, left_side_bearing
        integer :: char_len
        type(stb_fontinfo_t) :: font
        real(wp) :: scale
        type(mathtext_element_t), allocatable :: elements(:)

        ! Initialize text system if not already done
        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                ! Fallback estimate for the given size
                width = int(len_trim(text) * pixel_height * 0.6_wp)
                return
            end if
        end if

        ! Check if text contains mathematical notation
        if (has_mathtext(text)) then
            elements = parse_mathtext(text)
            width = calculate_mathtext_width_internal(elements, pixel_height)
            return
        end if

        ! Standard text rendering path
        font = get_global_font()
        scale = get_font_scale_for_size(pixel_height)

        width = 0
        i = 1
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))
            if (char_len == 0) then
                char_code = iachar(text(i:i))
                i = i + 1
            else
                char_code = utf8_to_codepoint(text, i)
                i = i + char_len
            end if

            call stb_get_codepoint_hmetrics(font, char_code, advance_width, left_side_bearing)
            width = width + int(real(advance_width) * scale)
        end do

    end function calculate_text_width_with_size

    function calculate_text_height(text) result(height)
        !! Calculate the pixel height of text using STB TrueType
        !! Supports mathematical notation with superscripts and subscripts
        use fortplot_mathtext, only: mathtext_element_t
        character(len=*), intent(in) :: text
        integer :: height
        integer :: ascent, descent, line_gap
        type(stb_fontinfo_t) :: font
        real(wp) :: scale
        type(mathtext_element_t), allocatable :: elements(:)

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                height = DEFAULT_FONT_SIZE  ! Fallback
                return
            end if
        end if

        ! Check if text contains mathematical notation
        if (has_mathtext(text)) then
            elements = parse_mathtext(text)
            height = calculate_mathtext_height_internal(elements, real(DEFAULT_FONT_SIZE, wp))
            return
        end if

        ! Standard text rendering path
        font = get_global_font()
        scale = get_font_scale()

        ! Get font metrics and scale to pixels
        call stb_get_font_vmetrics(font, ascent, descent, line_gap)
        height = int(real(ascent - descent) * scale)

        ! Ensure minimum reasonable height
        if (height <= 0) height = DEFAULT_FONT_SIZE

    end function calculate_text_height

    function calculate_text_descent(text) result(descent_pixels)
        !! Calculate the descent (below baseline) portion of text in pixels
        !! Returns positive pixels for the descent portion that extends below baseline
        character(len=*), intent(in) :: text
        integer :: descent_pixels
        integer :: ascent, descent, line_gap
        type(stb_fontinfo_t) :: font
        real(wp) :: scale

        ! Suppress unused parameter warnings
        associate(unused_text => text); end associate

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                descent_pixels = 4  ! Fallback descent estimate
                return
            end if
        end if

        font = get_global_font()
        scale = get_font_scale()

        ! Get font metrics - descent is typically negative (below baseline)
        call stb_get_font_vmetrics(font, ascent, descent, line_gap)
        descent_pixels = int(abs(real(descent) * scale))

        ! Ensure minimum reasonable descent
        if (descent_pixels <= 0) descent_pixels = 4

    end function calculate_text_descent

    subroutine render_text_to_image(image_data, width, height, x, y, text, r, g, b)
        !! Render text to image using STB TrueType with UTF-8 support
        !! Supports mathematical notation with superscripts and subscripts
        use fortplot_mathtext, only: mathtext_element_t
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        integer :: pen_x, pen_y, i, char_code
        integer :: advance_width, left_side_bearing
        type(c_ptr) :: bitmap_ptr
        integer :: bmp_width, bmp_height, xoff, yoff
        integer :: char_len
        type(stb_fontinfo_t) :: font
        real(wp) :: scale
        type(mathtext_element_t), allocatable :: elements(:)

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                call render_simple_placeholder(image_data, width, height, x, y, r, g, b)
                return
            end if
        end if

        ! Check if text contains mathematical notation
        if (has_mathtext(text)) then
            elements = parse_mathtext(text)
            call render_mathtext_elements_internal(image_data, width, height, x, y, &
                                        elements, r, g, b, real(DEFAULT_FONT_SIZE, wp))
            return
        end if

        ! Standard text rendering path
        font = get_global_font()
        scale = get_font_scale()

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
            bitmap_ptr = stb_get_codepoint_bitmap(font, scale, scale, char_code, &
                                                 bmp_width, bmp_height, xoff, yoff)

            if (c_associated(bitmap_ptr)) then
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                     bitmap_ptr, bmp_width, bmp_height, xoff, yoff, r, g, b)
                call stb_free_bitmap(bitmap_ptr)
            end if

            ! Advance pen position
            call stb_get_codepoint_hmetrics(font, char_code, advance_width, left_side_bearing)
            pen_x = pen_x + int(real(advance_width) * scale)
        end do
    end subroutine render_text_to_image

    subroutine render_text_with_size(image_data, width, height, x, y, text, r, g, b, pixel_height)
        !! Render text with specific font size
        !! Supports mathematical notation with superscripts and subscripts
        use fortplot_text_fonts, only: get_font_scale_for_size
        use fortplot_mathtext, only: mathtext_element_t
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: pixel_height
        integer :: pen_x, pen_y, i, char_code
        integer :: advance_width, left_side_bearing
        type(c_ptr) :: bitmap_ptr
        integer :: bmp_width, bmp_height, xoff, yoff
        integer :: char_len
        type(stb_fontinfo_t) :: font
        real(wp) :: scale
        type(mathtext_element_t), allocatable :: elements(:)

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                return
            end if
        end if

        ! Check if text contains mathematical notation
        if (has_mathtext(text)) then
            elements = parse_mathtext(text)
            call render_mathtext_elements_internal(image_data, width, height, x, y, &
                                        elements, r, g, b, pixel_height)
            return
        end if

        ! Standard text rendering path
        font = get_global_font()
        scale = get_font_scale_for_size(pixel_height)

        pen_x = x
        pen_y = y

        i = 1
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))
            if (char_len == 0) then
                char_code = iachar(text(i:i))
                i = i + 1
            else
                char_code = utf8_to_codepoint(text, i)
                i = i + char_len
            end if

            bitmap_ptr = stb_get_codepoint_bitmap(font, scale, scale, char_code, &
                                                 bmp_width, bmp_height, xoff, yoff)

            if (c_associated(bitmap_ptr)) then
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, bitmap_ptr, &
                                    bmp_width, bmp_height, xoff, yoff, r, g, b)
                call stb_free_bitmap(bitmap_ptr)
            end if

            call stb_get_codepoint_hmetrics(font, char_code, advance_width, left_side_bearing)
            pen_x = pen_x + int(real(advance_width) * scale)
        end do

    end subroutine render_text_with_size

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
        type(stb_fontinfo_t) :: font
        real(wp) :: scale

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                return
            end if
        end if

        font = get_global_font()
        scale = get_font_scale()

        pen_x = x
        pen_y = y
        cos_a = cos(angle * PI / 180.0_wp)
        sin_a = sin(angle * PI / 180.0_wp)

        ! Render glyphs at normal orientation (no per-glyph bitmap rotation)
        ! Individual glyph rotation would require complex bitmap transformation
        ! which is computationally expensive and rarely used in practice.
        ! The pen position advances along the rotated baseline (implemented below),
        ! providing reasonable behavior for most text rendering scenarios.
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

            bitmap_ptr = stb_get_codepoint_bitmap(font, scale, scale, char_code, &
                                                 bmp_width, bmp_height, xoff, yoff)

            if (c_associated(bitmap_ptr)) then
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                     bitmap_ptr, bmp_width, bmp_height, xoff, yoff, r, g, b)
                call stb_free_bitmap(bitmap_ptr)
            end if

            ! Advance with rotation
            call stb_get_codepoint_hmetrics(font, char_code, advance_width, left_side_bearing)
            pen_x = pen_x + int(real(advance_width) * scale * cos_a)
            pen_y = pen_y + int(real(advance_width) * scale * sin_a)
        end do
    end subroutine render_rotated_text_to_image

    function calculate_mathtext_width_internal(elements, base_font_size) result(total_width)
        !! Calculate total width of mathematical text elements
        use fortplot_mathtext, only: mathtext_element_t
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(in) :: base_font_size
        integer :: total_width

        integer :: i, element_width
        real(wp) :: element_font_size

        total_width = 0

        do i = 1, size(elements)
            element_font_size = base_font_size * elements(i)%font_size_ratio
            if (elements(i)%element_type == 3) then
                ! ELEMENT_SQRT: include symbol width
                element_width = calculate_text_width_with_size_internal(elements(i)%text, element_font_size)
                total_width = total_width + int(0.6_wp * element_font_size) + element_width
            else
                element_width = calculate_text_width_with_size_internal(elements(i)%text, element_font_size)
                total_width = total_width + element_width
            end if
        end do

    end function calculate_mathtext_width_internal

    function calculate_text_width_with_size_internal(text, pixel_height) result(width)
        !! Internal text width calculation to avoid circular dependencies
        use fortplot_text_fonts, only: get_font_scale_for_size
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: pixel_height
        integer :: width
        integer :: i, char_code, advance_width, left_side_bearing
        integer :: char_len
        type(stb_fontinfo_t) :: font
        real(wp) :: scale

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                width = int(len_trim(text) * pixel_height * 0.6_wp)
                return
            end if
        end if

        font = get_global_font()
        scale = get_font_scale_for_size(pixel_height)

        width = 0
        i = 1
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))
            if (char_len == 0) then
                char_code = iachar(text(i:i))
                i = i + 1
            else
                char_code = utf8_to_codepoint(text, i)
                i = i + char_len
            end if

            call stb_get_codepoint_hmetrics(font, char_code, advance_width, left_side_bearing)
            width = width + int(real(advance_width) * scale)
        end do

    end function calculate_text_width_with_size_internal

    function calculate_text_height_with_size_internal(pixel_height) result(height)
        !! Internal text height calculation based on font metrics
        use fortplot_text_fonts, only: get_font_metrics
        real(wp), intent(in) :: pixel_height
        integer :: height
        real(wp) :: ascent, descent, line_gap, scale
        logical :: success

        call get_font_metrics(ascent, descent, line_gap, success)
        if (.not. success) then
            height = int(pixel_height)
            return
        end if
        scale = pixel_height / (ascent - descent)
        height = int((ascent - descent) * scale)
    end function calculate_text_height_with_size_internal

    function calculate_mathtext_height_internal(elements, base_font_size) result(total_height)
        !! Calculate total height of mathematical text elements
        use fortplot_mathtext, only: mathtext_element_t
        use fortplot_text_fonts, only: get_font_metrics
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(in) :: base_font_size
        integer :: total_height

        integer :: i
        real(wp) :: ascent, descent, line_gap, element_font_size
        real(wp) :: max_above_baseline, max_below_baseline
        logical :: success

        ! Get base font metrics
        call get_font_metrics(ascent, descent, line_gap, success)
        if (.not. success) then
            total_height = int(base_font_size)
            return
        end if

        max_above_baseline = ascent * base_font_size / (ascent - descent)
        max_below_baseline = abs(descent) * base_font_size / (ascent - descent)

        ! Check each element for maximum extents
        do i = 1, size(elements)
            element_font_size = base_font_size * elements(i)%font_size_ratio

            select case (elements(i)%element_type)
            case (1)  ! ELEMENT_SUPERSCRIPT
                max_above_baseline = max(max_above_baseline, &
                    elements(i)%vertical_offset * base_font_size + &
                    ascent * element_font_size / (ascent - descent))
            case (2)  ! ELEMENT_SUBSCRIPT
                max_below_baseline = max(max_below_baseline, &
                    -elements(i)%vertical_offset * base_font_size + &
                    abs(descent) * element_font_size / (ascent - descent))
            case (3)  ! ELEMENT_SQRT
                max_above_baseline = max(max_above_baseline, &
                    ascent * element_font_size / (ascent - descent))
                max_below_baseline = max(max_below_baseline, &
                    abs(descent) * element_font_size / (ascent - descent))
            end select
        end do

        total_height = int(max_above_baseline + max_below_baseline)

    end function calculate_mathtext_height_internal

    subroutine render_mathtext_elements_internal(image_data, width, height, x, y, elements, &
                                       r, g, b, base_font_size)
        !! Render mathematical text elements to image
        use fortplot_mathtext, only: mathtext_element_t
        use fortplot_text_fonts, only: get_font_metrics
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        type(mathtext_element_t), intent(in) :: elements(:)
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: base_font_size

        integer :: i, pen_x, pen_y
        real(wp) :: element_font_size
        integer :: rad_width, sym_w, rad_height, top_y
        real(wp) :: ascent, descent, line_gap
        logical :: success

        pen_x = x
        pen_y = y

        ! Get font metrics for baseline calculation
        call get_font_metrics(ascent, descent, line_gap, success)
        if (.not. success) return

        do i = 1, size(elements)
            element_font_size = base_font_size * elements(i)%font_size_ratio

            if (elements(i)%element_type == 3) then
                rad_width = calculate_text_width_with_size_internal(elements(i)%text, element_font_size)
                rad_height = calculate_text_height_with_size_internal(element_font_size)
                sym_w = int(0.6_wp * element_font_size)
                top_y = pen_y - rad_height
                call draw_line_distance_aa(image_data, width, height, real(pen_x, wp), real(pen_y, wp), &
                    real(pen_x + sym_w/2, wp), real(pen_y + sym_w/2, wp), r, g, b, 1.0_wp)
                call draw_line_distance_aa(image_data, width, height, real(pen_x + sym_w/2, wp), real(pen_y + sym_w/2, wp), &
                    real(pen_x + sym_w, wp), real(top_y, wp), r, g, b, 1.0_wp)
                call draw_line_distance_aa(image_data, width, height, real(pen_x + sym_w, wp), real(top_y, wp), &
                    real(pen_x + sym_w + rad_width, wp), real(top_y, wp), r, g, b, 1.0_wp)
                call render_text_with_size_internal(image_data, width, height, pen_x + sym_w, pen_y, &
                    elements(i)%text, r, g, b, element_font_size)
                pen_x = pen_x + sym_w + rad_width
            else
                ! Calculate vertical position based on element type and offset
                ! PNG uses inverted Y axis (y=0 at top), so negate vertical offset
                pen_y = y - int(elements(i)%vertical_offset * base_font_size)

                ! Render the element using internal function
                call render_text_with_size_internal(image_data, width, height, pen_x, pen_y, &
                                         elements(i)%text, r, g, b, element_font_size)

                ! Advance horizontal position
                pen_x = pen_x + calculate_text_width_with_size_internal(elements(i)%text, element_font_size)
            end if
        end do

    end subroutine render_mathtext_elements_internal

    subroutine render_text_with_size_internal(image_data, width, height, x, y, text, r, g, b, pixel_height)
        !! Internal text rendering to avoid circular dependencies
        use fortplot_text_fonts, only: get_font_scale_for_size
        integer(1), intent(inout) :: image_data(*)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: pixel_height
        integer :: pen_x, pen_y, i, char_code
        integer :: advance_width, left_side_bearing
        type(c_ptr) :: bitmap_ptr
        integer :: bmp_width, bmp_height, xoff, yoff
        integer :: char_len
        type(stb_fontinfo_t) :: font
        real(wp) :: scale

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                return
            end if
        end if

        font = get_global_font()
        scale = get_font_scale_for_size(pixel_height)

        pen_x = x
        pen_y = y

        i = 1
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))
            if (char_len == 0) then
                char_code = iachar(text(i:i))
                i = i + 1
            else
                char_code = utf8_to_codepoint(text, i)
                i = i + char_len
            end if

            bitmap_ptr = stb_get_codepoint_bitmap(font, scale, scale, char_code, &
                                                 bmp_width, bmp_height, xoff, yoff)

            if (c_associated(bitmap_ptr)) then
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, bitmap_ptr, &
                                    bmp_width, bmp_height, xoff, yoff, r, g, b)
                call stb_free_bitmap(bitmap_ptr)
            end if

            call stb_get_codepoint_hmetrics(font, char_code, advance_width, left_side_bearing)
            pen_x = pen_x + int(real(advance_width) * scale)
        end do

    end subroutine render_text_with_size_internal

end module fortplot_text_rendering
