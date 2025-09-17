module fortplot_pdf_mathtext_render
    !! PDF mathtext rendering utilities

    use iso_fortran_env, only: wp => real64
    use fortplot_mathtext, only: mathtext_element_t, parse_mathtext
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_pdf_core, only: pdf_context_core, PDF_LABEL_SIZE
    use fortplot_pdf_text_render, only: draw_mixed_font_text
    use fortplot_pdf_text_segments, only: render_mixed_font_at_position
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length
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

        character(len=1024) :: preprocessed_text
        integer :: processed_len
        real(wp) :: fs

        fs = PDF_LABEL_SIZE
        if (present(font_size)) fs = font_size

        call process_latex_in_text(text, preprocessed_text, processed_len)

        if (index(preprocessed_text(1:processed_len), '^') > 0 .or. &
            index(preprocessed_text(1:processed_len), '_') > 0) then
            call render_mathtext_with_unicode_superscripts(this, x, y, &
                preprocessed_text(1:processed_len), fs)
        else
            call render_text_with_unicode_superscripts(this, x, y, &
                preprocessed_text(1:processed_len), fs)
        end if
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
        integer :: i, codepoint, char_len

        elem_font_size = base_font_size * element%font_size_ratio
        elem_y = baseline_y + element%vertical_offset * base_font_size

        call render_mixed_font_at_position(this, x_pos, elem_y, element%text, &
            elem_font_size)

        char_width = 0.0_wp
        i = 1
        do while (i <= len_trim(element%text))
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

    subroutine render_text_with_unicode_superscripts(this, x, y, text, font_size)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: font_size

        call draw_mixed_font_text(this, x, y, text, font_size)
    end subroutine render_text_with_unicode_superscripts

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

end module fortplot_pdf_mathtext_render
