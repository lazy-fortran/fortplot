module fortplot_pdf_mathtext_render
    !! PDF mathtext rendering utilities

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_mathtext, only: mathtext_element_t, parse_mathtext, ELEMENT_SQRT
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_pdf_core, only: pdf_context_core, PDF_LABEL_SIZE
    use fortplot_pdf_text_render, only: draw_mixed_font_text
    use fortplot_pdf_text_segments, only: render_mixed_font_at_position, &
                                          switch_to_helvetica_font
    use fortplot_pdf_text_escape, only: escape_pdf_string
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    use fortplot_text_layout, only: preprocess_math_text
    use fortplot_pdf_text_metrics, only: estimate_pdf_text_width
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

    subroutine render_mathtext_element_pdf(this, element, x_pos, baseline_y, &
                                           base_font_size)
        !! Render a single mathematical text element in PDF
        class(pdf_context_core), intent(inout) :: this
        type(mathtext_element_t), intent(in) :: element
        real(wp), intent(inout) :: x_pos
        real(wp), intent(in) :: baseline_y, base_font_size

        real(wp) :: elem_font_size, elem_y
        real(wp) :: char_width
        integer :: i, codepoint, char_len, text_len
        real(wp) :: sym_w, rad_width, top_y

        elem_font_size = base_font_size*element%font_size_ratio
        elem_y = baseline_y + element%vertical_offset*base_font_size

        ! Handle square root specially by drawing the radical (check mark + overbar)
        if (element%element_type == ELEMENT_SQRT) then
            ! Width of radical symbol and radicand
            sym_w = 0.6_wp*elem_font_size
            rad_width = estimate_pdf_text_width(element%text, elem_font_size)

            ! Place overbar slightly above baseline by approximately one font size
            top_y = baseline_y + elem_font_size

            ! Exit text object, draw path for radical, then re-enter text object
            this%stream_data = this%stream_data//'ET'//new_line('a')

            ! Use current stroke settings; draw the two slanted ticks and horizontal bar
            block
                character(len=64) :: cmd
                cmd = to_move_cmd(x_pos, baseline_y)
                this%stream_data = this%stream_data//trim(adjustl(cmd))//new_line('a')

                cmd = to_line_cmd(x_pos + sym_w/2.0_wp, baseline_y + sym_w/2.0_wp)
                this%stream_data = this%stream_data//trim(adjustl(cmd))//new_line('a')

                cmd = to_line_cmd(x_pos + sym_w, top_y)
                this%stream_data = this%stream_data//trim(adjustl(cmd))//new_line('a')

                cmd = to_line_cmd(x_pos + sym_w + rad_width, top_y)
                this%stream_data = this%stream_data//trim(adjustl(cmd))//new_line('a')

                this%stream_data = this%stream_data//'S'//new_line('a')
            end block

            ! Re-enter text mode for the radicand
            this%stream_data = this%stream_data//'BT'//new_line('a')
            call render_mixed_font_at_position(this, x_pos + sym_w, elem_y, &
                                               element%text, elem_font_size)

            x_pos = x_pos + sym_w + rad_width
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

    subroutine render_oblique_text_at_position(this, x, y, text, font_size)
        !! Render a math run with synthetic oblique: each ASCII letter gets a
        !! sheared text matrix, digits/operators stay upright. Advance widths
        !! match the upright Helvetica metrics so layout is unchanged.
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: font_size
        character(len=64) :: font_cmd
        character(len=64) :: escaped
        integer :: i, text_len, codepoint, esc_len, char_len
        real(wp) :: pen_x, shear

        write (font_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), font_size
        this%stream_data = this%stream_data//trim(adjustl(font_cmd))//new_line('a')

        pen_x = x
        text_len = len(text)
        i = 1
        do while (i <= text_len)
            char_len = max(1, utf8_char_length(text(i:i)))
            if (i + char_len - 1 > text_len) char_len = 1
            codepoint = ichar(text(i:i))
            shear = 0.0_wp
            ! Only single-byte ASCII letters slant; math variables are ASCII.
            if (char_len == 1 .and. is_ascii_letter(codepoint)) shear = ITALIC_SHEAR

            write (font_cmd, '("1 0 ", F0.4, " 1 ", F0.3, 1X, F0.3, " Tm")') &
                shear, pen_x, y
            this%stream_data = this%stream_data//trim(adjustl(font_cmd))//new_line('a')

            escaped = ''
            esc_len = 0
            call escape_pdf_string(text(i:i+char_len-1), escaped, esc_len)
            this%stream_data = this%stream_data//'('//escaped(1:esc_len)// &
                ') Tj'//new_line('a')

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
        integer :: i

        elements = parse_mathtext(text)
        this%stream_data = this%stream_data//'BT'//new_line('a')
        x_pos = x
        do i = 1, size(elements)
            call render_mathtext_element_pdf(this, elements(i), x_pos, y, font_size)
        end do
        this%stream_data = this%stream_data//'ET'//new_line('a')
    end subroutine render_mathtext_with_unicode_superscripts

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
