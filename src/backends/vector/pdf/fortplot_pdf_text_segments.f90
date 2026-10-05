module fortplot_pdf_text_segments
    !! Mixed-font PDF text segmentation helpers

    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core
    use fortplot_pdf_text_escape, only: escape_pdf_string, unicode_to_symbol_char, &
        unicode_codepoint_to_pdf_escape, lookup_script_fallback, &
        SCRIPT_SCALE, SUPERSCRIPT_RISE, SUBSCRIPT_DROP
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
        !! Groups consecutive glyphs for the same font into a single Tj to
        !! avoid odd intra-word spacing in some PDF viewers.
        class(pdf_context_core), intent(inout) :: this
        character(len=*), intent(in) :: text
        logical, intent(inout) :: in_symbol_font
        real(wp), intent(in) :: font_size
        integer :: i, n, codepoint, char_len
        character(len=8) :: symbol_char
        logical :: is_valid
        character(len=2048) :: buffer
        integer :: buf_len
        logical :: buf_is_symbol

        buffer = ''
        buf_len = 0
        buf_is_symbol = in_symbol_font

        i = 1
        n = len_trim(text)
        ! Scan forward from len_trim to include trailing spaces (but not padding)
        do while (n < len(text))
            if (ichar(text(n+1:n+1)) == 32) then
                n = n + 1  ! Include trailing space
            else
                exit  ! Stop at first non-space padding character
            end if
        end do

        do while (i <= n)
            char_len = utf8_char_length(text(i:i))

            if (char_len <= 1) then
                codepoint = ichar(text(i:i))

                ! Special-case common ASCII characters that require distinct Tj segments
                ! - Space: emit as "( ) Tj" to preserve exact spacing across viewers
                ! - Parentheses: emit as separate escaped glyphs "(\() Tj" and "(\)) Tj"
                if (text(i:i) == ' ') then
                    call flush_buffer()
                    if (in_symbol_font) then
                        call switch_to_helvetica_font(this, font_size)
                        in_symbol_font = .false.
                    end if
                    this%stream_data = this%stream_data // '( ) Tj' // new_line('a')
                    i = i + 1
                    cycle
                else if (text(i:i) == '(' .or. text(i:i) == ')') then
                    call flush_buffer()
                    if (in_symbol_font) then
                        call switch_to_helvetica_font(this, font_size)
                        in_symbol_font = .false.
                    end if
                    if (text(i:i) == '(') then
                        this%stream_data = this%stream_data // '(\() Tj' // new_line('a')
                    else
                        this%stream_data = this%stream_data // '(\)) Tj' // new_line('a')
                    end if
                    i = i + 1
                    cycle
                end if

                call unicode_to_symbol_char(codepoint, symbol_char)
                if (len_trim(symbol_char) > 0) then
                    if (.not. buf_is_symbol .and. buf_len > 0) call flush_buffer()
                    call append_symbol_esc(symbol_char)
                else
                    if (buf_is_symbol .and. buf_len > 0) call flush_buffer()
                    call append_escaped_helvetica(text(i:i))
                end if
                i = i + 1
            else
                call check_utf8_sequence(text, i, is_valid, char_len)
                if (is_valid .and. i + char_len - 1 <= n) then
                    codepoint = utf8_to_codepoint(text, i)
                else
                    codepoint = 0
                end if

                call unicode_to_symbol_char(codepoint, symbol_char)
                if (len_trim(symbol_char) > 0) then
                    if (.not. buf_is_symbol .and. buf_len > 0) call flush_buffer()
                    call append_symbol_esc(symbol_char)
                else
                    call flush_buffer()
                    call emit_pdf_escape_or_fallback(this, codepoint, font_size)
                end if
                i = i + max(1, char_len)
            end if
        end do

        call flush_buffer()

    contains

        subroutine flush_buffer()
            if (buf_len <= 0) return
            if (buf_is_symbol .and. .not. in_symbol_font) then
                call switch_to_symbol_font(this, font_size)
                in_symbol_font = .true.
            else if ((.not. buf_is_symbol) .and. in_symbol_font) then
                call switch_to_helvetica_font(this, font_size)
                in_symbol_font = .false.
            end if
            this%stream_data = this%stream_data // '(' // buffer(1:buf_len) // ') Tj' // &
                new_line('a')
            buf_len = 0
        end subroutine flush_buffer

        subroutine append_escaped_helvetica(ch)
            character(len=*), intent(in) :: ch
            character(len=12) :: escaped
            integer :: elen
            escaped = ''
            elen = 0
            call escape_pdf_string(ch, escaped, elen)
            if (buf_len + elen > len(buffer)) then
                call flush_buffer()
            end if
            if (buf_len == 0) buf_is_symbol = .false.
            buffer(buf_len+1:buf_len+elen) = escaped(1:elen)
            buf_len = buf_len + elen
        end subroutine append_escaped_helvetica

        subroutine append_symbol_esc(seq)
            character(len=*), intent(in) :: seq
            integer :: slen
            slen = len_trim(seq)
            if (slen <= 0) return
            if (buf_len + slen > len(buffer)) then
                call flush_buffer()
            end if
            if (buf_len == 0) buf_is_symbol = .true.
            buffer(buf_len+1:buf_len+slen) = seq(1:slen)
            buf_len = buf_len + slen
        end subroutine append_symbol_esc

        ! Internal helper procedures for buffering glyph emission
    end subroutine process_text_segments

    subroutine emit_pdf_escape_or_fallback(this, codepoint, font_size)
        class(pdf_context_core), intent(inout) :: this
        integer, intent(in) :: codepoint
        real(wp), intent(in) :: font_size
        character(len=8) :: escape_seq
        character(len=8) :: escaped_char
        integer :: esc_len

        character(len=1) :: base
        integer :: rise

        call unicode_codepoint_to_pdf_escape(codepoint, escape_seq)
        call lookup_script_fallback(codepoint, base, rise)
        if (len_trim(escape_seq) > 0) then
            call switch_to_helvetica_font(this, font_size)
            this%stream_data = this%stream_data // '(' // trim(escape_seq) // &
                ') Tj' // new_line('a')
        else if (rise /= 0) then
            call emit_script_glyph(this, base, rise, font_size)
        else if (codepoint == 295 .or. codepoint == 8463) then
            ! U+0127 / U+210F h-bar: Helvetica h overstruck with its macron,
            ! centred on the ascender stem (advance widths h 556, macron 333).
            call switch_to_helvetica_font(this, font_size)
            this%stream_data = this%stream_data // '[(h) 613 (' // achar(92) // &
                '257) -280] TJ' // new_line('a')
        else if (codepoint == 8214) then
            ! U+2016 double vertical line
            call switch_to_helvetica_font(this, font_size)
            this%stream_data = this%stream_data // '(||) Tj' // new_line('a')
        else
            call switch_to_helvetica_font(this, font_size)
            escaped_char = ''
            esc_len = 0
            call escape_pdf_string('?', escaped_char, esc_len)
            this%stream_data = this%stream_data // '(' // escaped_char(1:esc_len) // &
                ') Tj' // new_line('a')
        end if
    end subroutine emit_pdf_escape_or_fallback

    subroutine emit_script_glyph(this, base, rise, font_size)
        !! Superscript/subscript glyph: reduced Helvetica size and text rise,
        !! restoring both afterwards.
        class(pdf_context_core), intent(inout) :: this
        character(len=1), intent(in) :: base
        integer, intent(in) :: rise
        real(wp), intent(in) :: font_size
        character(len=32) :: rise_cmd
        character(len=8) :: escaped
        integer :: esc_len

        call switch_to_helvetica_font(this, SCRIPT_SCALE*font_size)
        if (rise > 0) then
            write (rise_cmd, '(F0.2, " Ts")') SUPERSCRIPT_RISE*font_size
        else
            write (rise_cmd, '(F0.2, " Ts")') -SUBSCRIPT_DROP*font_size
        end if
        call escape_pdf_string(base, escaped, esc_len)
        this%stream_data = this%stream_data // trim(adjustl(rise_cmd)) // &
            new_line('a') // '(' // escaped(1:esc_len) // ') Tj' // new_line('a') // &
            '0 Ts' // new_line('a')
        call switch_to_helvetica_font(this, font_size)
    end subroutine emit_script_glyph

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
