program test_pdf_mathtext_italic_unicode_2057
    !! Regression test for issue #2057: italic ($...$) math annotations must emit
    !! WinAnsi/Symbol glyph escapes, not raw UTF-8 bytes. Before the fix the ¼
    !! (U+00BC) inside a math run was written as its two raw UTF-8 bytes
    !! (0xC2 0xBC), rendering as the mojibake "Â¼" and colliding with the
    !! following "sin(x)".
    use fortplot
    use fortplot_test_pdf_utils, only: extract_pdf_stream_text
    use fortplot_test_output_helpers, only: ensure_test_output_dir
    implicit none

    character(len=:), allocatable :: output_dir, out_pdf, stream_text
    integer :: status

    call ensure_test_output_dir('pdf_mathtext_italic_2057', output_dir)
    out_pdf = trim(output_dir)//'annotation_mathtext.pdf'

    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_plot([0.0_wp, 6.0_wp], [-1.0_wp, 1.0_wp])
    call text(3.0_wp, 0.5_wp, &
              "$∂f/∂x = cos(x)e^{-x/4} - ¼sin(x)e^{-x/4}$", &
              coord_type=COORD_DATA, font_size=9.0_wp, alignment="center")
    call text(3.0_wp, -0.2_wp, "$lim_{x→∞} e^{-x} = 0$", &
              coord_type=COORD_DATA, font_size=10.0_wp, alignment="center")
    call savefig(out_pdf)

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF content stream'
        stop 1
    end if

    ! No raw UTF-8 lead byte for ¼ (0xC2) may leak into the italic run: its
    ! presence is the mojibake ("Â¼") the fix removes.
    if (index(stream_text, achar(194)) > 0) then
        print *, 'FAIL: raw UTF-8 lead byte 0xC2 in stream (mojibake ¼)'
        stop 1
    end if
    if (index(stream_text, achar(188)) > 0) then
        print *, 'FAIL: raw byte 0xBC in stream (¼ not WinAnsi-escaped)'
        stop 1
    end if

    ! ¼ must render via its WinAnsi octal escape \274 in the Helvetica font.
    if (index(stream_text, achar(92)//'274') == 0) then
        print *, 'FAIL: missing WinAnsi escape \\274 for ¼'
        stop 1
    end if

    ! Symbol-font glyphs used by the annotations must still be emitted as
    ! Symbol escapes from the math (italic) path.
    if (index(stream_text, achar(92)//'266') == 0) then
        print *, 'FAIL: missing Symbol escape \\266 for partial derivative'
        stop 1
    end if
    if (index(stream_text, achar(92)//'256') == 0) then
        print *, 'FAIL: missing Symbol escape \\256 for right arrow'
        stop 1
    end if
    if (index(stream_text, achar(92)//'245') == 0) then
        print *, 'FAIL: missing Symbol escape \\245 for infinity'
        stop 1
    end if

    ! No unmapped-glyph fallback should appear.
    if (index(stream_text, '(?) Tj') > 0) then
        print *, 'FAIL: fallback (?) Tj present (glyph not mapped)'
        stop 1
    end if

    ! Raw math markup must be consumed by the parser, never drawn literally.
    if (index(stream_text, '^{') > 0) then
        print *, 'FAIL: literal superscript markup ^{ left in PDF stream'
        stop 1
    end if
    if (index(stream_text, 'lim_{') > 0) then
        print *, 'FAIL: literal subscript markup lim_{ left in PDF stream'
        stop 1
    end if

    print *, 'PASS: issue #2057 italic math annotations render without mojibake'
end program test_pdf_mathtext_italic_unicode_2057
