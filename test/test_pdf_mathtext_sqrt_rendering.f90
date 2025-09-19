program test_pdf_mathtext_sqrt_rendering
    !! Verify that LaTeX-style \sqrt{} renders in PDF (radical shape + radicand)
    use fortplot
    use test_pdf_utils, only: extract_pdf_stream_text
    use fortplot_validation, only: validation_result_t, validate_file_exists
    implicit none

    character(len=:), allocatable :: stream_text
    integer :: status
    type(validation_result_t) :: val
    character(len=*), parameter :: outfile = 'test/output/test_pdf_math_sqrt.pdf'

    call figure()
    call plot([0.0d0, 1.0d0], [0.0d0, 1.0d0])
    ! Use LaTeX-style sqrt so it goes through mathtext parser
    call title('PDF math sqrt: \sqrt{x}')
    call legend()
    call savefig(outfile)

    val = validate_file_exists(outfile)
    if (.not. val%passed) then
        print *, 'FAIL: PDF file not created for math sqrt test'
        stop 1
    end if

    call extract_pdf_stream_text(outfile, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: Could not extract PDF stream for math sqrt test'
        stop 1
    end if

    ! Ensure the raw command string "\\sqrt" is not emitted to the PDF text stream
    if (index(stream_text, '\\sqrt') > 0) then
        print *, 'FAIL: Literal \\sqrt found in PDF stream (mathtext not applied)'
        stop 1
    end if

    ! Ensure radicand 'x' is present as a text segment
    if (index(stream_text, '(x) Tj') == 0) then
        print *, 'FAIL: Expected radicand text segment (x) not found'
        stop 1
    end if

    ! Heuristic: stream should contain at least one stroke command associated
    ! with the radical path drawing near the text section.
    if (index(stream_text, ' S') == 0 .and. index(stream_text, '\nS\n') == 0) then
        print *, 'FAIL: Expected stroke command for radical not found'
        stop 1
    end if

    print *, 'PASS: LaTeX-style \\sqrt{} renders in PDF (no literal fallback)'
end program test_pdf_mathtext_sqrt_rendering

