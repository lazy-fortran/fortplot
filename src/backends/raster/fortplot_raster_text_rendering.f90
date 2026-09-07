module fortplot_raster_text_rendering
    !! Raster-specific text rendering primitives (glyph rasterization, mathtext drawing)
    use fortplot_truetype
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    use fortplot_text_fonts, only: init_text_system, get_global_font, get_font_scale, &
                                   is_font_initialized, get_font_scale_for_size, &
                                   get_font_metrics
    use fortplot_mathtext, only: parse_mathtext, mathtext_element_t, &
                                ELEMENT_NORMAL, ELEMENT_SQRT, ELEMENT_FRACTION, &
                                mathtext_scripts_share_anchor
    use fortplot_mathtext_layout, only: mathtext_vertical_bounds, &
                                       fraction_vertical_offsets
    use fortplot_raster_primitives, only: draw_line_distance_aa
    use fortplot_text_layout, only: has_mathtext, preprocess_math_text, &
                                    calculate_mathtext_width_internal, &
                                    calculate_text_width_with_size_internal, &
                                    calculate_text_height_with_size_internal, &
                                    DEFAULT_FONT_SIZE
    use, intrinsic :: iso_fortran_env, only: wp => real64, int8
    implicit none

    private
    public :: render_text_to_image, render_text_with_size, render_rotated_text_to_image

    real(wp), parameter :: PI = 3.14159265359_wp
    type(truetype_font_t) :: raster_italic_font
    logical :: raster_italic_attempted = .false.
    logical :: raster_italic_available = .false.

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
        integer(int8), allocatable :: bitmap(:)
        integer :: bmp_width, bmp_height, xoff, yoff
        integer :: char_len
        type(truetype_font_t) :: font
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

            call font%get_codepoint_bitmap(scale, scale, char_code, bitmap, &
                                           bmp_width, bmp_height, xoff, yoff)

            if (allocated(bitmap)) then
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                      bitmap, bmp_width, bmp_height, xoff, &
                                      yoff, r, g, &
                                      b)
            end if

            call font%get_hmetrics(char_code, advance_width, left_side_bearing)
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
        integer(int8), allocatable :: bitmap(:)
        integer :: bmp_width, bmp_height, xoff, yoff
        integer :: char_len
        type(truetype_font_t) :: font
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

            call font%get_codepoint_bitmap(scale, scale, char_code, bitmap, &
                                           bmp_width, bmp_height, xoff, yoff)

            if (allocated(bitmap)) then
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                      bitmap, bmp_width, bmp_height, xoff, &
                                      yoff, r, g, &
                                      b)
            end if

            call font%get_hmetrics(char_code, advance_width, left_side_bearing)
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
        integer(int8), allocatable :: bitmap(:)
        integer :: bmp_width, bmp_height, xoff, yoff
        real(wp) :: cos_a, sin_a
        integer :: char_len
        type(truetype_font_t) :: font
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

            call font%get_codepoint_bitmap(scale, scale, char_code, bitmap, &
                                           bmp_width, bmp_height, xoff, yoff)

            if (allocated(bitmap)) then
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                      bitmap, bmp_width, bmp_height, xoff, &
                                      yoff, r, g, &
                                      b)
            end if

            call font%get_hmetrics(char_code, advance_width, left_side_bearing)
            pen_x = pen_x + int(real(advance_width)*scale*cos_a)
            pen_y = pen_y + int(real(advance_width)*scale*sin_a)
        end do
    end subroutine render_rotated_text_to_image

    recursive subroutine render_mathtext_elements_internal(image_data, width, &
                                                           height, x, y, elements, r, &
                                                           g, b, base_font_size)
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height, x, y
        type(mathtext_element_t), intent(in) :: elements(:)
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: base_font_size
        integer :: i, pen_x, pen_y, anchor, element_x, element_width, sym_w
        real(wp) :: element_font_size, rad_ascent
        type(mathtext_element_t), allocatable :: children(:)

        pen_x = x
        anchor = x
        do i = 1, size(elements)
            element_x = pen_x
            if (mathtext_scripts_share_anchor(elements, i)) element_x = anchor
            anchor = element_x
            element_font_size = base_font_size * elements(i)%font_size_ratio
            pen_y = y - int(elements(i)%vertical_offset * base_font_size)
            if (elements(i)%element_type == ELEMENT_FRACTION) then
                call render_fraction_raster(image_data, width, height, elements(i), &
                                             element_x, pen_y, element_font_size, &
                                             r, g, b, element_width)
            else if (elements(i)%element_type /= ELEMENT_NORMAL) then
                children = parse_mathtext(elements(i)%text, elements(i)%italic)
                element_width = calculate_mathtext_width_internal(children, &
                                                                    element_font_size)
                if (elements(i)%element_type == ELEMENT_SQRT) then
                    sym_w = int(0.6_wp * element_font_size)
                    rad_ascent = mathtext_raster_ascent(children, element_font_size)
                    call draw_raster_radical(image_data, width, height, element_x, &
                                             pen_y, sym_w, element_width, rad_ascent, &
                                             element_font_size, r, g, b)
                    element_x = element_x + sym_w
                end if
                call render_mathtext_elements_internal(image_data, width, height, &
                                                        element_x, pen_y, children, &
                                                        r, g, b, element_font_size)
            else
                call render_text_with_size_internal(image_data, width, height, &
                                                     element_x, pen_y, &
                                                     elements(i)%text, &
                                                     r, g, b, element_font_size, &
                                                     elements(i)%italic)
                element_width = calculate_text_width_with_size_internal( &
                    elements(i)%text, element_font_size)
            end if
            pen_x = max(pen_x, element_x + element_width)
        end do
    end subroutine render_mathtext_elements_internal

    subroutine draw_raster_radical(image_data, width, height, x, y, sym_w, rad_width, &
                                   rad_ascent, fs, r, g, b)
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height, x, y, sym_w, rad_width
        real(wp), intent(in) :: rad_ascent, fs
        integer(1), intent(in) :: r, g, b
        real(wp) :: px(5), py(5), rgb(3)
        integer :: i

        px = real(x, wp) + [0.0_wp, 0.12_wp * fs, 0.3_wp * fs, &
                            real(sym_w, wp), real(sym_w + rad_width, wp)]
        py = real(y, wp) + [-0.25_wp * fs, -0.32_wp * fs, 0.08_wp * fs, &
                            -rad_ascent - 0.08_wp * fs, -rad_ascent - 0.08_wp * fs]
        rgb = real([iand(int(r), 255), iand(int(g), 255), iand(int(b), 255)], wp) / &
              255.0_wp
        do i = 1, 4
            call draw_line_distance_aa(image_data, width, height, px(i), py(i), &
                                       px(i + 1), py(i + 1), rgb(1), rgb(2), rgb(3), &
                                       0.055_wp * fs)
        end do
    end subroutine draw_raster_radical

    real(wp) function mathtext_raster_ascent(elements, fs) result(top)
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(in) :: fs
        real(wp) :: above, below

        call mathtext_vertical_bounds(elements, above, below)
        top = above * fs
    end function mathtext_raster_ascent

    recursive subroutine render_fraction_raster(image_data, width, height, element, &
                                                 x, y, fs, r, g, b, total_width)
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height, x, y
        type(mathtext_element_t), intent(in) :: element
        real(wp), intent(in) :: fs
        integer(1), intent(in) :: r, g, b
        integer, intent(out) :: total_width
        type(mathtext_element_t), allocatable :: numerator(:), denominator(:)
        integer :: nw, dw, w, child_x, child_y, pad
        real(wp) :: numerator_y, denominator_y, rgb(3)

        numerator = parse_mathtext(element%text, element%italic)
        denominator = parse_mathtext(element%denominator, element%italic)
        nw = calculate_mathtext_width_internal(numerator, 0.7_wp * fs)
        dw = calculate_mathtext_width_internal(denominator, 0.7_wp * fs)
        w = max(nw, dw)
        pad = int(0.125_wp * fs)
        total_width = w + pad
        call fraction_vertical_offsets(element, numerator_y, denominator_y)
        child_x = x + pad / 2 + (w - nw) / 2
        child_y = y - nint(numerator_y * fs)
        call render_mathtext_elements_internal(image_data, width, height, child_x, &
                                                child_y, numerator, r, g, b, &
                                                0.7_wp * fs)
        child_x = x + pad / 2 + (w - dw) / 2
        child_y = y - nint(denominator_y * fs)
        call render_mathtext_elements_internal(image_data, width, height, child_x, &
                                                child_y, denominator, r, g, b, &
                                                0.7_wp * fs)
        rgb = real([iand(int(r), 255), iand(int(g), 255), iand(int(b), 255)], wp) / &
              255.0_wp
        call draw_line_distance_aa(image_data, width, height, real(x + pad / 2, wp), &
                                   real(y, wp) - 0.25_wp * fs, &
                                   real(x + pad / 2 + w, wp), &
                                   real(y, wp) - 0.25_wp * fs, &
                                   rgb(1), rgb(2), rgb(3), 0.0625_wp * fs)
    end subroutine render_fraction_raster

    subroutine render_text_with_size_internal(image_data, width, height, x, y, text, &
                                              r, g, b, pixel_height, italic)
        !! Internal text rendering helper to avoid circular dependencies
        !! Note: Uses len(text) not len_trim to preserve trailing spaces in mathtext
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height, x, y
        character(len=*), intent(in) :: text
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in) :: pixel_height
        logical, intent(in), optional :: italic
        integer :: pen_x, pen_y, i, char_code
        integer :: advance_width, left_side_bearing
        integer(int8), allocatable :: bitmap(:)
        integer :: bmp_width, bmp_height, xoff, yoff
        integer :: char_len, text_len
        type(truetype_font_t) :: font
        real(wp) :: scale
        logical :: glyph_italic

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

            glyph_italic = .false.
            if (present(italic)) then
                glyph_italic = italic .and. is_alpha_codepoint(char_code) .and. &
                               ensure_raster_italic_font()
            end if

            if (glyph_italic) then
                call raster_italic_font%get_codepoint_bitmap(scale, scale, &
                                                             char_code, bitmap, &
                                                             bmp_width, bmp_height, &
                                                             xoff, yoff)
            else
                call font%get_codepoint_bitmap(scale, scale, char_code, bitmap, &
                                               bmp_width, bmp_height, xoff, yoff)
            end if

            if (allocated(bitmap)) then
                call render_stb_glyph(image_data, width, height, pen_x, pen_y, &
                                      bitmap, bmp_width, bmp_height, xoff, &
                                      yoff, r, g, b)
            end if

            if (glyph_italic) then
                call raster_italic_font%get_hmetrics(char_code, advance_width, &
                                                     left_side_bearing)
            else
                call font%get_hmetrics(char_code, advance_width, left_side_bearing)
            end if
            pen_x = pen_x + int(real(advance_width)*scale)
        end do
    end subroutine render_text_with_size_internal

    logical function ensure_raster_italic_font() result(available)
        character(len=256), parameter :: candidates(8) = [ &
            character(len=256) :: &
            "/usr/share/fonts/truetype/dejavu/DejaVuSans-Oblique.ttf", &
            "/usr/share/fonts/truetype/dejavu/DejaVuSansCondensed-Oblique.ttf", &
            "/usr/share/fonts/TTF/DejaVuSans-Oblique.ttf", &
            "/usr/share/fonts/TTF/DejaVuSansCondensed-Oblique.ttf", &
            "/usr/share/fonts/Adwaita/AdwaitaSans-Italic.ttf", &
            "/usr/share/fonts/truetype/liberation/LiberationSans-Italic.ttf", &
            "/usr/share/fonts/Liberation/LiberationSans-Italic.ttf", &
            "/usr/share/fonts/truetype/LiberationSans-Italic.ttf" ]
        integer :: i
        logical :: exists

        if (.not. raster_italic_attempted) then
            raster_italic_attempted = .true.
            do i = 1, size(candidates)
                inquire (file=trim(candidates(i)), exist=exists)
                if (.not. exists) cycle
                raster_italic_available = raster_italic_font%init(trim(candidates(i)))
                if (raster_italic_available) exit
            end do
        end if
        available = raster_italic_available
    end function ensure_raster_italic_font

    pure function is_alpha_codepoint(codepoint) result(is_alpha)
        !! True for ASCII letters (the glyphs matplotlib renders italic in math).
        integer, intent(in) :: codepoint
        logical :: is_alpha
        is_alpha = (codepoint >= iachar('A') .and. codepoint <= iachar('Z')) .or. &
                   (codepoint >= iachar('a') .and. codepoint <= iachar('z'))
    end function is_alpha_codepoint

    subroutine render_stb_glyph(image_data, width, height, pen_x, pen_y, bitmap, &
                                bmp_width, bmp_height, xoff, yoff, r, g, b, slant)
        !! Render STB TrueType glyph bitmap to image. A non-zero slant applies a
        !! horizontal shear (synthetic oblique) proportional to height above the
        !! baseline, leaving the advance width unchanged.
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: width, height, pen_x, pen_y
        integer(int8), intent(in) :: bitmap(:)
        integer, intent(in) :: bmp_width, bmp_height, xoff, yoff
        integer(1), intent(in) :: r, g, b
        real(wp), intent(in), optional :: slant
        integer :: glyph_x, glyph_y, img_x, img_y, row, col, pixel_idx
        integer :: alpha_int, shear_dx
        real(wp) :: slant_factor
        real :: alpha_f, bg_r, bg_g, bg_b

        if (bmp_width <= 0 .or. bmp_height <= 0) then
            return
        end if

        slant_factor = 0.0_wp
        if (present(slant)) slant_factor = slant

        glyph_x = pen_x + xoff
        glyph_y = pen_y + yoff

        do row = 0, bmp_height - 1
            ! Shift each row left/right by an amount proportional to its height
            ! above the baseline. pen_y is the baseline; (glyph_y+row) is the
            ! pixel row, so height above baseline is pen_y-(glyph_y+row).
            shear_dx = 0
            if (slant_factor /= 0.0_wp) then
                shear_dx = nint(slant_factor*real(pen_y - (glyph_y + row), wp))
            end if
            do col = 0, bmp_width - 1
                img_x = glyph_x + col + shear_dx
                img_y = glyph_y + row

                if (img_x >= 0 .and. img_x < width .and. img_y >= 0 .and. &
                    img_y < height) then
                    alpha_int = iand(int(bitmap(row*bmp_width + col + 1)), 255)

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
