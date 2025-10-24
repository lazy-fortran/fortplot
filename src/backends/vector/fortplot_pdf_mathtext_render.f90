module fortplot_pdf_mathtext_render
    !! PDF mathtext rendering utilities

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_mathtext, only: mathtext_element_t, parse_mathtext, ELEMENT_SQRT
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_pdf_core, only: pdf_context_core, PDF_LABEL_SIZE
    use fortplot_pdf_text_render, only: draw_mixed_font_text
    use fortplot_pdf_text_segments, only: render_mixed_font_at_position
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
    use fortplot_text_layout, only: preprocess_math_text
    use fortplot_pdf_text_metrics, only: estimate_pdf_text_width
    implicit none
    private

    public :: draw_pdf_mathtext
    public :: render_mathtext_element_pdf

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

        elem_font_size = base_font_size * element%font_size_ratio
        elem_y = baseline_y + element%vertical_offset * base_font_size

        ! Handle square root specially by drawing the radical (check mark + overbar)
        if (element%element_type == ELEMENT_SQRT) then
            ! Width of radical symbol and radicand
            sym_w = 0.6_wp * elem_font_size
            rad_width = estimate_pdf_text_width(element%text, elem_font_size)

            ! Place overbar slightly above baseline by approximately one font size
            top_y = baseline_y + elem_font_size

            ! Exit text object, draw path for radical, then re-enter text object
            this%stream_data = this%stream_data // 'ET' // new_line('a')

            ! Use current stroke settings; draw the two slanted ticks and horizontal bar
            block
                character(len=64) :: cmd
                cmd = to_move_cmd(x_pos, baseline_y)
                this%stream_data = this%stream_data // trim(adjustl(cmd)) // new_line('a')

                cmd = to_line_cmd(x_pos + sym_w/2.0_wp, baseline_y + sym_w/2.0_wp)
                this%stream_data = this%stream_data // trim(adjustl(cmd)) // new_line('a')

                cmd = to_line_cmd(x_pos + sym_w, top_y)
                this%stream_data = this%stream_data // trim(adjustl(cmd)) // new_line('a')

                cmd = to_line_cmd(x_pos + sym_w + rad_width, top_y)
                this%stream_data = this%stream_data // trim(adjustl(cmd)) // new_line('a')

                this%stream_data = this%stream_data // 'S' // new_line('a')
            end block

            ! Re-enter text mode for the radicand
            this%stream_data = this%stream_data // 'BT' // new_line('a')
            call render_mixed_font_at_position(this, x_pos + sym_w, elem_y, &
                 element%text, elem_font_size)

            x_pos = x_pos + sym_w + rad_width
            return
        end if

        call render_mixed_font_at_position(this, x_pos, elem_y, element%text, &
            elem_font_size)

        char_width = 0.0_wp
        i = 1
        ! Calculate width including trailing spaces by scanning beyond len_trim
        text_len = len_trim(element%text)
        do while (text_len < len(element%text))
            if (element%text(text_len+1:text_len+1) == ' ') then
                text_len = text_len + 1
            else
                exit
            end if
        end do

        do while (i <= text_len)
            char_len = utf8_char_length(element%text(i:i))
            if (char_len == 0) then
                codepoint = iachar(element%text(i:i))
                char_len = 1
            else
                codepoint = utf8_to_codepoint(element%text, i)
            end if

            if (codepoint >= 48 .and. codepoint <= 57) then
                char_width = char_width + elem_font_size * 0.55_wp
            else if (codepoint >= 65 .and. codepoint <= 90) then
                char_width = char_width + elem_font_size * 0.65_wp
            else if (codepoint >= 97 .and. codepoint <= 122) then
                char_width = char_width + elem_font_size * 0.5_wp
            else if (codepoint == 32) then
                char_width = char_width + elem_font_size * 0.3_wp
            else if (codepoint >= 8304 .and. codepoint <= 8313) then
                char_width = char_width + elem_font_size * 0.4_wp
            else
                char_width = char_width + elem_font_size * 0.5_wp
            end if

            i = i + char_len
        end do

        x_pos = x_pos + char_width
    end subroutine render_mathtext_element_pdf

    subroutine render_mathtext_with_unicode_superscripts(this, x, y, text, font_size)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: font_size

        type(mathtext_element_t), allocatable :: elements(:)
        real(wp) :: x_pos
        integer :: i

        elements = parse_mathtext(text)
        this%stream_data = this%stream_data // 'BT' // new_line('a')
        x_pos = x
        do i = 1, size(elements)
            call render_mathtext_element_pdf(this, elements(i), x_pos, y, font_size)
        end do
        this%stream_data = this%stream_data // 'ET' // new_line('a')
    end subroutine render_mathtext_with_unicode_superscripts

    pure function to_move_cmd(x, y) result(cmd)
        real(wp), intent(in) :: x, y
        character(len=64) :: cmd
        write(cmd, '(F0.3,1X,F0.3,1X,A)') x, y, 'm'
        cmd = trim(adjustl(cmd))
    end function to_move_cmd

    pure function to_line_cmd(x, y) result(cmd)
        real(wp), intent(in) :: x, y
        character(len=64) :: cmd
        write(cmd, '(F0.3,1X,F0.3,1X,A)') x, y, 'l'
        cmd = trim(adjustl(cmd))
    end function to_line_cmd
end module fortplot_pdf_mathtext_render
