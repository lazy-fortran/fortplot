module fortplot_raster_text_rendering
    !! Raster-specific text rendering primitives (glyph rasterization, mathtext drawing)
    use iso_c_binding, only: c_ptr, c_f_pointer, c_int8_t, &
                             c_associated
    use fortplot_stb_truetype
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    use fortplot_text_fonts, only: init_text_system, get_global_font, get_font_scale, &
                                   is_font_initialized, get_font_scale_for_size, &
                                   get_font_metrics
    use fortplot_mathtext, only: parse_mathtext, mathtext_element_t
    use fortplot_raster_primitives, only: draw_line_distance_aa
    use fortplot_text_layout, only: has_mathtext, preprocess_math_text, &
                                    calculate_mathtext_width_internal, &
                                    calculate_text_width_with_size_internal, &
                                    calculate_text_height_with_size_internal, &
                                    DEFAULT_FONT_SIZE
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: render_text_to_image, render_text_with_size, render_rotated_text_to_image

    real(wp), parameter :: PI = 3.14159265359_wp

contains

    subroutine render_text_to_image(image_data, width, height, x, y, text, r, g, b)
        !! Render text to image using STB TrueType with UTF-8 support
        !! Supports mathematical notation with superscripts and subscripts
        integer(1), intent(inout) :: image_data(:)
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
        character(len=2048) :: processed
        integer :: plen

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                call render_simple_placeholder(image_data, width, height, x, y, r, g, b)
                return
            end if
        end if

        if (has_mathtext(text)) then
            call preprocess_math_text(text, processed, plen)
            elements = parse_mathtext(processed(1:plen))
            call render_mathtext_elements_internal(image_data, width, height, x, y, &
                                                   elements, r, g, b, &
                                                   real(DEFAULT_FONT_SIZE, wp))
            return
        end if

        font = get_global_font()
        scale = get_font_scale()

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
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                      bitmap_ptr, bmp_width, bmp_height, xoff, &
                                      yoff, r, g, &
                                      b)
                call stb_free_bitmap(bitmap_ptr)
            end if

            call stb_get_codepoint_hmetrics(font, char_code, advance_width, &
                                            left_side_bearing)
            pen_x = pen_x + int(real(advance_width)*scale)
        end do
    end subroutine render_text_to_image

    subroutine render_text_with_size(image_data, width, height, x, y, text, &
                                     r, g, b, pixel_height)
        !! Render text with specific font size
        !! Supports mathematical notation with superscripts and subscripts
        integer(1), intent(inout) :: image_data(:)
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
        character(len=2048) :: processed
        integer :: plen

        if (.not. is_font_initialized()) then
            if (.not. init_text_system()) then
                return
            end if
        end if

        if (has_mathtext(text)) then
            call preprocess_math_text(text, processed, plen)
            elements = parse_mathtext(processed(1:plen))
            call render_mathtext_elements_internal(image_data, width, height, x, y, &
                                                   elements, r, g, b, pixel_height)
            return
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
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                      bitmap_ptr, bmp_width, bmp_height, xoff, &
                                      yoff, r, g, &
                                      b)
                call stb_free_bitmap(bitmap_ptr)
            end if

            call stb_get_codepoint_hmetrics(font, char_code, advance_width, &
                                            left_side_bearing)
            pen_x = pen_x + int(real(advance_width)*scale)
        end do
    end subroutine render_text_with_size

    subroutine render_rotated_text_to_image(image_data, width, height, x, y, text, &
                                            r, g, b, angle, pixel_height)
        !! Render rotated text to PNG image using STB TrueType with UTF-8 support
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: angle  ! Rotation angle in degrees
        real(wp), intent(in), optional :: pixel_height

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
        if (present(pixel_height)) then
            scale = get_font_scale_for_size(pixel_height)
        end if

        pen_x = x
        pen_y = y
        cos_a = cos(angle*PI/180.0_wp)
        sin_a = sin(angle*PI/180.0_wp)

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
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                      bitmap_ptr, bmp_width, bmp_height, xoff, &
                                      yoff, r, g, &
                                      b)
                call stb_free_bitmap(bitmap_ptr)
            end if

            call stb_get_codepoint_hmetrics(font, char_code, advance_width, &
                                            left_side_bearing)
            pen_x = pen_x + int(real(advance_width)*scale*cos_a)
            pen_y = pen_y + int(real(advance_width)*scale*sin_a)
        end do
    end subroutine render_rotated_text_to_image

    recursive subroutine render_mathtext_elements_internal(image_data, width, &
                                                           height, x, y, elements, r, &
                                                           g, b, base_font_size)
        !! Render mathematical text elements to image for the raster backend
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height, x, y
        type(mathtext_element_t), intent(in) :: elements(:)
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: base_font_size

        integer :: i, pen_x, pen_y
        real(wp) :: element_font_size
        integer :: rad_width, sym_w, rad_height, top_y
        real(wp) :: ascent, descent, line_gap
        logical :: success
        type(mathtext_element_t), allocatable :: rad_elements(:)

        pen_x = x
        pen_y = y

        call get_font_metrics(ascent, descent, line_gap, success)
        if (.not. success) return

        do i = 1, size(elements)
            element_font_size = base_font_size*elements(i)%font_size_ratio

            if (elements(i)%element_type == 3) then
                pen_y = y

                rad_elements = parse_mathtext(elements(i)%text)
                rad_width = calculate_mathtext_width_internal(rad_elements, &
                                                              element_font_size)
                rad_height = calculate_text_height_with_size_internal(element_font_size)
                sym_w = int(0.6_wp*element_font_size)
                top_y = pen_y - rad_height
                call draw_line_distance_aa(image_data, width, height, &
                                           real(pen_x, wp), real(pen_y, wp), &
                                           real(pen_x + sym_w/2, wp), &
                                           real(pen_y + sym_w/2, wp), &
                                           real(r, wp)/255.0_wp, &
                                           real(g, wp)/255.0_wp, &
                                           real(b, wp)/255.0_wp, 0.1_wp)
                call draw_line_distance_aa(image_data, width, height, &
                                           real(pen_x + sym_w/2, wp), &
                                           real(pen_y + sym_w/2, wp), &
                                           real(pen_x + sym_w, wp), &
                                           real(top_y, wp), &
                                           real(r, wp)/255.0_wp, &
                                           real(g, wp)/255.0_wp, &
                                           real(b, wp)/255.0_wp, 0.1_wp)
                call draw_line_distance_aa(image_data, width, height, &
                                           real(pen_x + sym_w, wp), &
                                           real(top_y, wp), &
                                           real(pen_x + sym_w + rad_width, wp), &
                                           real(top_y, wp), &
                                           real(r, wp)/255.0_wp, &
                                           real(g, wp)/255.0_wp, &
                                           real(b, wp)/255.0_wp, 0.1_wp)
                call render_mathtext_elements_internal(image_data, width, height, &
                                                       pen_x + sym_w, pen_y, &
                                                       rad_elements, r, g, b, &
                                                       element_font_size)
                pen_x = pen_x + sym_w + rad_width
            else
                pen_y = y - int(elements(i)%vertical_offset*base_font_size)
                call render_text_with_size_internal(image_data, width, height, &
                                                    pen_x, pen_y, elements(i)%text, r, &
                                                    g, b, element_font_size)
                pen_x = pen_x + calculate_text_width_with_size_internal( &
                        elements(i)%text, element_font_size)
            end if
        end do
    end subroutine render_mathtext_elements_internal

    subroutine render_text_with_size_internal(image_data, width, height, x, y, text, &
                                              r, g, b, pixel_height)
        !! Internal text rendering helper to avoid circular dependencies
        !! Note: Uses len(text) not len_trim to preserve trailing spaces in mathtext
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: pixel_height
        integer :: pen_x, pen_y, i, char_code
        integer :: advance_width, left_side_bearing
        type(c_ptr) :: bitmap_ptr
        integer :: bmp_width, bmp_height, xoff, yoff
        integer :: char_len, text_len
        type(stb_fontinfo_t) :: font
        real(wp) :: scale

        text_len = len(text)

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
        do while (i <= text_len)
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
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                      bitmap_ptr, bmp_width, bmp_height, xoff, &
                                      yoff, r, g, &
                                      b)
                call stb_free_bitmap(bitmap_ptr)
            end if

            call stb_get_codepoint_hmetrics(font, char_code, advance_width, &
                                            left_side_bearing)
            pen_x = pen_x + int(real(advance_width)*scale)
        end do
    end subroutine render_text_with_size_internal

    subroutine render_stb_glyph(image_data, width, height, pen_x, pen_y, bitmap_ptr, &
                                bmp_width, bmp_height, xoff, yoff, r, g, b)
        !! Render STB TrueType glyph bitmap to image
        integer(1), intent(inout) :: image_data(:)
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

        call c_f_pointer(bitmap_ptr, bitmap_buffer, [bmp_width*bmp_height])

        glyph_x = pen_x + xoff
        glyph_y = pen_y + yoff

        do row = 0, bmp_height - 1
            do col = 0, bmp_width - 1
                img_x = glyph_x + col
                img_y = glyph_y + row

                if (img_x >= 0 .and. img_x < width .and. img_y >= 0 .and. &
                    img_y < height) then
                    alpha_int = int(bitmap_buffer(row*bmp_width + col + 1))
                    if (alpha_int < 0) alpha_int = alpha_int + 256

                    if (alpha_int > 0) then
                        pixel_idx = (img_y*width + img_x)*3 + 1

                        if (pixel_idx < 1 .or. pixel_idx + 2 > width*height*3) then
                            cycle
                        end if

                        alpha_f = real(alpha_int)/255.0
                        bg_r = real(int(image_data(pixel_idx), &
                                        kind=selected_int_kind(2)) + &
                                    merge(256, 0, image_data(pixel_idx) < 0))
                        bg_g = real(int(image_data(pixel_idx + 1), &
                                        kind=selected_int_kind(2)) + &
                                    merge(256, 0, image_data(pixel_idx + 1) < 0))
                        bg_b = real(int(image_data(pixel_idx + 2), &
                                        kind=selected_int_kind(2)) + &
                                    merge(256, 0, image_data(pixel_idx + 2) < 0))

                        image_data(pixel_idx) = int(bg_r*(1.0 - alpha_f) + &
                                                    real(int(r) + merge(256, 0, r < &
                                                                        0))*alpha_f, 1)
                        image_data(pixel_idx + 1) = int(bg_g*(1.0_wp - alpha_f) + &
                                                        real(int(g) + &
                                                             merge(256, 0, g < 0))* &
                                                        alpha_f, 1)
                        image_data(pixel_idx + 2) = int(bg_b*(1.0_wp - alpha_f) + &
                                                        real(int(b) + &
                                                             merge(256, 0, b < 0))* &
                                                        alpha_f, 1)
                    end if
                end if
            end do
        end do
    end subroutine render_stb_glyph

    subroutine render_simple_placeholder(image_data, width, height, x, y, r, g, b)
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height, x, y
        integer(1), intent(in) :: r, g, b
        integer :: pixel_idx, img_x, img_y, max_idx

        max_idx = width*height*3

        do img_y = y, min(y + 6, height - 1)
            do img_x = x, min(x + 4, width - 1)
                if (img_x >= 0 .and. img_y >= 0) then
                    pixel_idx = (img_y*width + img_x)*3 + 1
                    if (pixel_idx > 0 .and. pixel_idx <= max_idx - 2) then
                        image_data(pixel_idx) = r
                        image_data(pixel_idx + 1) = g
                        image_data(pixel_idx + 2) = b
                    end if
                end if
            end do
        end do
    end subroutine render_simple_placeholder

end module fortplot_raster_text_rendering
