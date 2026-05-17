program test_pdf_text_split_smoke
    !! Smoke test for refactored PDF text modules
    use iso_fortran_env, only: wp => real64
    use fortplot_pdf_core, only: pdf_context_core, create_pdf_canvas_core, finalize_pdf_stream
    use fortplot_pdf_text, only: draw_pdf_text, draw_mixed_font_text, draw_pdf_mathtext, &
        escape_pdf_string, unicode_to_symbol_char, unicode_codepoint_to_pdf_escape, &
        estimate_pdf_text_width
    implicit none

    type(pdf_context_core) :: ctx
    character(len=32) :: escaped
    character(len=8) :: expected
    character(len=8) :: symbol_char
    character(len=8) :: escape_seq
    integer :: escaped_len

    ctx = create_pdf_canvas_core(120.0_wp, 90.0_wp)

    call draw_pdf_text(ctx, 10.0_wp, 20.0_wp, 'Split test')
    call draw_mixed_font_text(ctx, 15.0_wp, 35.0_wp, 'alpha β', 12.0_wp)
    call draw_pdf_mathtext(ctx, 20.0_wp, 50.0_wp, 'x^2 + y^2', 11.0_wp)
    call finalize_pdf_stream(ctx)

    if (index(ctx%stream_data, 'BT') <= 0 .or. index(ctx%stream_data, 'ET') <= 0) then
        print *, 'FAIL: expected BT/ET markers in stream'
        stop 1
    end if

    escaped = ''
    call escape_pdf_string('(test)', escaped, escaped_len)
    expected = char(92) // '(' // 'test' // char(92) // ')'

    if (escaped_len /= len_trim(expected)) then
        print *, 'FAIL: escape_pdf_string produced unexpected length'
        stop 1
    end if

    if (escaped(1:escaped_len) /= expected) then
        print *, 'FAIL: escape_pdf_string did not escape parentheses'
        stop 1
    end if

    call unicode_to_symbol_char(945, symbol_char)
    if (len_trim(symbol_char) == 0) then
        print *, 'FAIL: unicode_to_symbol_char returned empty mapping for alpha'
        stop 1
    end if

    call unicode_codepoint_to_pdf_escape(960, escape_seq)
    if (len_trim(escape_seq) == 0) then
        print *, 'FAIL: unicode_codepoint_to_pdf_escape did not map pi'
        stop 1
    end if

    if (estimate_pdf_text_width('abc', 12.0_wp) <= 0.0_wp) then
        print *, 'FAIL: expected positive width for simple text'
        stop 1
    end if

    print *, 'PASS: split pdf text smoke test'
end program test_pdf_text_split_smoke
