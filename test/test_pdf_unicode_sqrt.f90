program test_pdf_unicode_sqrt
    use fortplot
    use test_pdf_utils, only: extract_pdf_stream_text
    use fortplot_validation, only: validation_result_t, validate_file_exists
    implicit none

    character(len=:), allocatable :: stream_text
    integer :: status
    type(validation_result_t) :: val
    character(len=*), parameter :: outfile = 'test/output/test_pdf_sqrt.pdf'

    call figure()
    call plot([0.0d0, 1.0d0], [0.0d0, 1.0d0])
    ! Include the Unicode square root symbol directly
    call title('PDF sqrt: √x')
    call savefig(outfile)

    val = validate_file_exists(outfile)
    if (.not. val%passed) then
        print *, 'FAIL: PDF file not created for sqrt test'
        stop 1
    end if

    call extract_pdf_stream_text(outfile, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: Could not extract PDF stream text'
        stop 1
    end if

    if (index(stream_text, '?') > 0) then
        print *, 'FAIL: Found fallback ? in PDF stream (sqrt not mapped)'
        stop 1
    end if

    ! Heuristic: ensure the Symbol octal escape for radical (\214) is present
    if (index(stream_text, '\214') == 0) then
        print *, 'FAIL: Expected Symbol escape \\214 for sqrt not found'
        stop 1
    end if

    ! Ensure the character following the radical (here 'x') renders as a normal text segment
    if (index(stream_text, '(x) Tj') == 0) then
        print *, 'FAIL: Expected following text segment for x not found'
        stop 1
    end if

    print *, 'PASS: Unicode square root renders in PDF without fallback'
end program test_pdf_unicode_sqrt
