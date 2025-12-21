program test_pdf_unicode_annotations_1415
    !! Regression test for issue #1415 (annotation_demo PDF glyph fallbacks)
    !!
    !! Ensure common Unicode math symbols used by annotation_demo do not fall
    !! back to '?' in the PDF content stream.
    use fortplot
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: out_pdf = 'test/output/test_pdf_unicode_1415.pdf'
    character(len=:), allocatable :: stream_text
    integer :: status

    call figure()
    call title('Unicode: ∂f/∂x, lim x→∞, ¼')
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
    call savefig(out_pdf)

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF content stream'
        stop 1
    end if

    if (index(stream_text, '?') > 0) then
        print *, 'FAIL: Found fallback ? in PDF stream (unicode not mapped)'
        stop 1
    end if

    if (index(stream_text, achar(92)//'266') == 0) then
        print *, 'FAIL: Missing Symbol escape for partial differential (\\266)'
        stop 1
    end if

    if (index(stream_text, achar(92)//'245') == 0) then
        print *, 'FAIL: Missing Symbol escape for infinity (\\245)'
        stop 1
    end if

    if (index(stream_text, achar(92)//'256') == 0) then
        print *, 'FAIL: Missing Symbol escape for right arrow (\\256)'
        stop 1
    end if

    if (index(stream_text, achar(92)//'274') == 0) then
        print *, 'FAIL: Missing PDF escape for 1/4 fraction (\\274)'
        stop 1
    end if

    print *, 'PASS: Issue #1415 unicode glyphs render in PDF without fallback'
end program test_pdf_unicode_annotations_1415

