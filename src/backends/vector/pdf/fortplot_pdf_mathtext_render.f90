module fortplot_pdf_mathtext_render
    !! PDF mathtext rendering utilities

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_mathtext, only: mathtext_element_t, parse_mathtext, &
                                ELEMENT_NORMAL, ELEMENT_SQRT, ELEMENT_FRACTION, &
                                ELEMENT_SPACE, &
                                mathtext_scripts_share_anchor
    use fortplot_mathtext_layout, only: mathtext_vertical_bounds, &
                                       fraction_vertical_offsets
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_pdf_core, only: pdf_context_core, PDF_LABEL_SIZE
    use fortplot_pdf_text_segments, only: render_mixed_font_at_position, &
                                          switch_to_helvetica_font, &
                                          switch_to_symbol_font
    use fortplot_pdf_text_escape, only: escape_pdf_string, unicode_to_symbol_char, &
                                        unicode_codepoint_to_pdf_escape
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    use fortplot_text_layout, only: preprocess_math_text
    use fortplot_pdf_text_metrics, only: estimate_pdf_text_width, &
                                          measure_mathtext_elements_width
    implicit none
    private

    public :: draw_pdf_mathtext
    public :: render_mathtext_element_pdf

    real(wp), parameter :: ITALIC_SHEAR = 0.2126_wp
        !! Text-matrix shear for synthetic oblique (~12 deg, tan(12 deg)), used to
        !! slant math-mode letters the way matplotlib italicises math variables.

contains

    subroutine draw_pdf_mathtext(this, x, y, text, font_size)
        !! Draw text with mathematical notation
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in), optional :: font_size

        character(len=2048) :: preprocessed_text
        integer :: processed_len
        character(len=4096) :: math_ready
        integer :: mlen
        real(wp) :: fs

        fs = PDF_LABEL_SIZE
        if (present(font_size)) fs = font_size

        call process_latex_in_text(text, preprocessed_text, processed_len)
        call preprocess_math_text(preprocessed_text(1:processed_len), math_ready, mlen)
        call render_mathtext_with_unicode_superscripts(this, x, y, &
                                                       math_ready(1:mlen), fs)
    end subroutine draw_pdf_mathtext

    recursive subroutine render_mathtext_element_pdf(this, element, x_pos, &
                                                     baseline_y, base_font_size)
        class(pdf_context_core), intent(inout) :: this
        type(mathtext_element_t), intent(in) :: element
        real(wp), intent(inout) :: x_pos
        real(wp), intent(in) :: baseline_y, base_font_size
        real(wp) :: elem_font_size, elem_y, rad_width, rad_ascent
        type(mathtext_element_t), allocatable :: children(:)

        elem_font_size = base_font_size * element%font_size_ratio
        elem_y = baseline_y + element%vertical_offset * base_font_size
        if (element%element_type == ELEMENT_SPACE) then
            x_pos = x_pos + 0.2_wp * estimate_pdf_text_width('m', elem_font_size)
            return
        end if
        if (element%element_type == ELEMENT_FRACTION) then
            call render_fraction_pdf(this, element, x_pos, elem_y, elem_font_size)
            return
        end if
        if (element%element_type /= ELEMENT_NORMAL) then
            children = parse_mathtext(element%text, element%italic)
            if (element%element_type == ELEMENT_SQRT) then
                rad_width = measure_mathtext_elements_width(children, elem_font_size)
                rad_ascent = mathtext_pdf_ascent(children, elem_font_size)
                call draw_radical_pdf(this, x_pos, elem_y, rad_width, &
                                      rad_ascent, elem_font_size)
                x_pos = x_pos + 0.6_wp * elem_font_size
            end if
            call render_mathtext_elements_pdf(this, children, x_pos, elem_y, &
                                              elem_font_size)
            return
        end if
        if (element%italic) then
            call render_oblique_text_at_position(this, x_pos, elem_y, element%text, &
                                                 elem_font_size)
        else
            call render_mixed_font_at_position(this, x_pos, elem_y, element%text, &
                                               elem_font_size)
        end if
        x_pos = x_pos + estimate_pdf_text_width(element%text, elem_font_size)
    end subroutine render_mathtext_element_pdf

    subroutine draw_radical_pdf(this, x, y, rad_width, rad_ascent, fs)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y, rad_width, rad_ascent, fs
        real(wp) :: top_y
        character(len=64) :: cmd

        top_y = y + rad_ascent + 0.08_wp * fs
        this%stream_data = this%stream_data // 'ET' // new_line('a') // &
                           'q' // new_line('a') // '[] 0 d' // new_line('a')
        write (cmd, '(F0.3," w")') 0.055_wp * fs
        this%stream_data = this%stream_data // trim(cmd) // new_line('a')
        this%stream_data = this%stream_data // &
            trim(to_move_cmd(x, y + 0.25_wp * fs)) // new_line('a') // &
            trim(to_line_cmd(x + 0.12_wp * fs, y + 0.32_wp * fs)) // new_line('a') // &
            trim(to_line_cmd(x + 0.3_wp * fs, y - 0.08_wp * fs)) // new_line('a') // &
            trim(to_line_cmd(x + 0.6_wp * fs, top_y)) // new_line('a') // &
            trim(to_line_cmd(x + 0.6_wp * fs + rad_width, top_y)) // new_line('a') // &
            'S' // new_line('a') // 'Q' // new_line('a') // 'BT' // new_line('a')
    end subroutine draw_radical_pdf

    real(wp) function mathtext_pdf_ascent(elements, fs) result(ascent)
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(in) :: fs
        real(wp) :: above, below

        call mathtext_vertical_bounds(elements, above, below)
        ascent = above * fs
    end function mathtext_pdf_ascent

    recursive subroutine render_fraction_pdf(this, element, x, y, fs)
        class(pdf_context_core), intent(inout) :: this
        type(mathtext_element_t), intent(in) :: element
        real(wp), intent(inout) :: x
        real(wp), intent(in) :: y, fs
        type(mathtext_element_t), allocatable :: numerator(:), denominator(:)
        real(wp) :: nw, dw, w, child_x, numerator_y, denominator_y
        character(len=64) :: cmd

        numerator = parse_mathtext(element%text, element%italic)
        denominator = parse_mathtext(element%denominator, element%italic)
        nw = measure_mathtext_elements_width(numerator, 0.7_wp * fs)
        dw = measure_mathtext_elements_width(denominator, 0.7_wp * fs)
        w = max(nw, dw)
        call fraction_vertical_offsets(element, numerator_y, denominator_y)
        child_x = x + 0.0625_wp * fs + 0.5_wp * (w - nw)
        call render_mathtext_elements_pdf(this, numerator, child_x, &
                                          y + numerator_y * fs, 0.7_wp * fs)
        child_x = x + 0.0625_wp * fs + 0.5_wp * (w - dw)
        call render_mathtext_elements_pdf(this, denominator, child_x, &
                                          y + denominator_y * fs, 0.7_wp * fs)
        this%stream_data = this%stream_data // 'ET' // new_line('a') // &
                           'q' // new_line('a') // '[] 0 d' // new_line('a')
        write (cmd, '(F0.3," w")') 0.0625_wp * fs
        this%stream_data = this%stream_data // trim(cmd) // new_line('a') // &
            trim(to_move_cmd(x + 0.0625_wp * fs, y + 0.25_wp * fs)) // &
            new_line('a') // &
            trim(to_line_cmd(x + 0.0625_wp * fs + w, y + 0.25_wp * fs)) // &
            new_line('a') // 'S' // new_line('a') // 'Q' // new_line('a') // &
            'BT' // new_line('a')
        x = x + w + 0.125_wp * fs
    end subroutine render_fraction_pdf

    subroutine render_oblique_text_at_position(this, x, y, text, font_size)
        !! Render a math run with synthetic oblique: each ASCII letter gets a
        !! sheared text matrix, digits/operators stay upright. Greek letters and
        !! other math glyphs in the Symbol font keep their Symbol mapping (matching
        !! the upright mixed-font path) so '\Theta' etc. still emit '/F6' + octal
        !! escapes. Advance widths match the upright Helvetica metrics so layout
        !! is unchanged.
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: font_size
        character(len=64) :: font_cmd
        character(len=64) :: escaped
        character(len=16) :: winansi_escape
        character(len=8) :: symbol_char
        integer :: i, text_len, codepoint, esc_len, char_len
        real(wp) :: pen_x, shear
        integer :: current_font
        integer, parameter :: FONT_NONE = 0, FONT_HELVETICA = 1, FONT_SYMBOL = 2

        ! Track which font is currently selected so we only emit a font switch
        ! when the glyph class changes (Symbol vs Helvetica).
        current_font = FONT_NONE

        pen_x = x
        text_len = len(text)
        i = 1
        do while (i <= text_len)
            char_len = max(1, utf8_char_length(text(i:i)))
            if (i + char_len - 1 > text_len) char_len = 1
            if (char_len == 1) then
                codepoint = ichar(text(i:i))
            else
                codepoint = utf8_to_codepoint(text, i)
            end if

            call unicode_to_symbol_char(codepoint, symbol_char)
            if (len_trim(symbol_char) > 0) then
                ! Math glyph available in the Symbol font (e.g. uppercase Greek):
                ! render upright in Symbol, no synthetic shear.
                if (current_font /= FONT_SYMBOL) then
                    call switch_to_symbol_font(this, font_size)
                    current_font = FONT_SYMBOL
                end if
                shear = 0.0_wp
                if (codepoint >= 945 .and. codepoint <= 969) shear = ITALIC_SHEAR
                write (font_cmd, '("1 0 ", F0.4, " 1 ", F0.3, 1X, F0.3, " Tm")') &
                    shear, pen_x, y
                this%stream_data = this%stream_data//trim(adjustl(font_cmd)) &
                    //new_line('a')
                this%stream_data = this%stream_data//'('//trim(symbol_char)// &
                    ') Tj'//new_line('a')
            else
                if (current_font /= FONT_HELVETICA) then
                    call switch_to_helvetica_font(this, font_size)
                    current_font = FONT_HELVETICA
                end if
                shear = 0.0_wp
                ! Only single-byte ASCII letters slant; math variables are ASCII.
                if (char_len == 1 .and. is_ascii_letter(codepoint)) shear = ITALIC_SHEAR

                escaped = ''
                esc_len = 0
                if (char_len == 1) then
                    call escape_pdf_string(text(i:i), escaped, esc_len)
                else
                    ! Non-ASCII glyph without a Symbol mapping (e.g. U+00BC ¼):
                    ! emit the single WinAnsi byte via its octal escape instead of
                    ! the raw UTF-8 bytes, which would render as mojibake.
                    call unicode_codepoint_to_pdf_escape(codepoint, winansi_escape)
                    if (len_trim(winansi_escape) > 0) then
                        esc_len = len_trim(winansi_escape)
                        escaped(1:esc_len) = winansi_escape(1:esc_len)
                    else
                        call escape_pdf_string('?', escaped, esc_len)
                    end if
                end if

                write (font_cmd, '("1 0 ", F0.4, " 1 ", F0.3, 1X, F0.3, " Tm")') &
                    shear, pen_x, y
                this%stream_data = this%stream_data//trim(adjustl(font_cmd)) &
                    //new_line('a')
                this%stream_data = this%stream_data//'('//escaped(1:esc_len)// &
                    ') Tj'//new_line('a')
            end if

            pen_x = pen_x + estimate_pdf_text_width(text(i:i+char_len-1), font_size)
            i = i + char_len
        end do
    end subroutine render_oblique_text_at_position

    pure function is_ascii_letter(codepoint) result(is_letter)
        integer, intent(in) :: codepoint
        logical :: is_letter
        is_letter = (codepoint >= iachar('A') .and. codepoint <= iachar('Z')) .or. &
                    (codepoint >= iachar('a') .and. codepoint <= iachar('z'))
    end function is_ascii_letter

    subroutine render_mathtext_with_unicode_superscripts(this, x, y, text, font_size)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: font_size

        type(mathtext_element_t), allocatable :: elements(:)
        real(wp) :: x_pos

        elements = parse_mathtext(text)
        this%stream_data = this%stream_data//'BT'//new_line('a')
        x_pos = x
        call render_mathtext_elements_pdf(this, elements, x_pos, y, font_size)
        this%stream_data = this%stream_data//'ET'//new_line('a')
    end subroutine render_mathtext_with_unicode_superscripts

    recursive subroutine render_mathtext_elements_pdf(this, elements, x, y, fs)
        class(pdf_context_core), intent(inout) :: this
        type(mathtext_element_t), intent(in) :: elements(:)
        real(wp), intent(inout) :: x
        real(wp), intent(in) :: y, fs
        real(wp) :: anchor, element_x
        integer :: i

        anchor = x
        do i = 1, size(elements)
            element_x = x
            if (mathtext_scripts_share_anchor(elements, i)) element_x = anchor
            anchor = element_x
            call render_mathtext_element_pdf(this, elements(i), element_x, y, fs)
            x = max(x, element_x)
        end do
    end subroutine render_mathtext_elements_pdf

    pure function to_move_cmd(x, y) result(cmd)
        real(wp), intent(in) :: x, y
        character(len=64) :: cmd
        write (cmd, '(F0.3,1X,F0.3,1X,A)') x, y, 'm'
        cmd = trim(adjustl(cmd))
    end function to_move_cmd

    pure function to_line_cmd(x, y) result(cmd)
        real(wp), intent(in) :: x, y
        character(len=64) :: cmd
        write (cmd, '(F0.3,1X,F0.3,1X,A)') x, y, 'l'
        cmd = trim(adjustl(cmd))
    end function to_line_cmd
end module fortplot_pdf_mathtext_render
