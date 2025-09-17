module fortplot_pdf_text_segments
    !! Mixed-font PDF text segmentation helpers

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core
    use fortplot_pdf_text_escape, only: escape_pdf_string, unicode_to_symbol_char, &
        unicode_codepoint_to_pdf_escape
    use fortplot_unicode, only: utf8_to_codepoint, utf8_char_length, check_utf8_sequence
    implicit none
    private

    public :: process_text_segments
    public :: process_rotated_text_segments
    public :: render_mixed_font_at_position
    public :: switch_to_symbol_font
    public :: switch_to_helvetica_font
    public :: emit_pdf_escape_or_fallback

contains

    subroutine process_text_segments(this, text, in_symbol_font, font_size)
        !! Process text segments for mixed font rendering
        class(pdf_context_core), intent(inout) :: this
        character(len=*), intent(in) :: text
        logical, intent(inout) :: in_symbol_font
        real(wp), intent(in) :: font_size
        integer :: i, codepoint, char_len
        character(len=8) :: symbol_char
        character(len=8) :: escaped_char
        integer :: esc_len
        logical :: is_valid

        i = 1
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))

            if (char_len <= 1) then
                codepoint = ichar(text(i:i))
                call unicode_to_symbol_char(codepoint, symbol_char)
                if (len_trim(symbol_char) > 0) then
                    if (.not. in_symbol_font) then
                        call switch_to_symbol_font(this, font_size)
                        in_symbol_font = .true.
                    end if
                    this%stream_data = this%stream_data // '(' // trim(symbol_char) // &
                        ') Tj' // new_line('a')
                else
                    if (in_symbol_font) then
                        call switch_to_helvetica_font(this, font_size)
                        in_symbol_font = .false.
                    end if
                    escaped_char = ''
                    esc_len = 0
                    call escape_pdf_string(text(i:i), escaped_char, &
                        esc_len)
                    this%stream_data = this%stream_data // '(' // &
                        escaped_char(1:esc_len) // ') Tj' // new_line('a')
                end if
                i = i + 1
            else
                call check_utf8_sequence(text, i, is_valid, char_len)
                if (is_valid .and. i + char_len - 1 <= len_trim(text)) then
                    codepoint = utf8_to_codepoint(text, i)
                else
                    codepoint = 0
                end if

                call unicode_to_symbol_char(codepoint, symbol_char)
                if (len_trim(symbol_char) > 0) then
                    if (.not. in_symbol_font) then
                        call switch_to_symbol_font(this, font_size)
                        in_symbol_font = .true.
                    end if
                    this%stream_data = this%stream_data // '(' // trim(symbol_char) // &
                        ') Tj' // new_line('a')
                else
                    call emit_pdf_escape_or_fallback(this, codepoint, font_size)
                end if
                i = i + max(1, char_len)
            end if
        end do
    end subroutine process_text_segments

    subroutine emit_pdf_escape_or_fallback(this, codepoint, font_size)
        class(pdf_context_core), intent(inout) :: this
        integer, intent(in) :: codepoint
        real(wp), intent(in) :: font_size
        character(len=8) :: escape_seq
        character(len=8) :: escaped_char
        integer :: esc_len

        call unicode_codepoint_to_pdf_escape(codepoint, escape_seq)
        if (len_trim(escape_seq) > 0) then
            call switch_to_helvetica_font(this, font_size)
            this%stream_data = this%stream_data // '(' // trim(escape_seq) // &
                ') Tj' // new_line('a')
        else
            call switch_to_helvetica_font(this, font_size)
            escaped_char = ''
            esc_len = 0
            call escape_pdf_string('?', escaped_char, esc_len)
            this%stream_data = this%stream_data // '(' // escaped_char(1:esc_len) // &
                ') Tj' // new_line('a')
        end if
    end subroutine emit_pdf_escape_or_fallback

    subroutine process_rotated_text_segments(this, text, font_size)
        !! Process text segments for rotated mixed font rendering
        class(pdf_context_core), intent(inout) :: this
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: font_size
        logical :: in_symbol_font

        in_symbol_font = .false.
        call process_text_segments(this, text, in_symbol_font, font_size)
    end subroutine process_rotated_text_segments

    subroutine switch_to_symbol_font(this, font_size)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: font_size
        character(len=64) :: font_cmd

        write(font_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_symbol_obj(), font_size
        this%stream_data = this%stream_data // trim(adjustl(font_cmd)) // new_line('a')
    end subroutine switch_to_symbol_font

    subroutine switch_to_helvetica_font(this, font_size)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: font_size
        character(len=64) :: font_cmd

        write(font_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), font_size
        this%stream_data = this%stream_data // trim(adjustl(font_cmd)) // new_line('a')
    end subroutine switch_to_helvetica_font

    subroutine render_mixed_font_at_position(this, x, y, text, font_size)
        !! Render text with mixed fonts at specific position and size
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: font_size
        character(len=1024) :: text_cmd
        logical :: in_symbol_font

        in_symbol_font = .false.

        write(text_cmd, '("/F", I0, 1X, F0.1, " Tf")') &
            this%fonts%get_helvetica_obj(), font_size
        this%stream_data = this%stream_data // trim(adjustl(text_cmd)) // new_line('a')

        write(text_cmd, '("1 0 0 1 ", F0.3, 1X, F0.3, " Tm")') &
            x, y
        this%stream_data = this%stream_data // trim(adjustl(text_cmd)) // new_line('a')

        call process_text_segments(this, text, in_symbol_font, font_size)
    end subroutine render_mixed_font_at_position

end module fortplot_pdf_text_segments
