! Test: PDF text escaping for parentheses in mixed-font rendering
program test_pdf_text_escaping
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, ylabel, savefig
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: out_pdf = 'build/test/output/pdf_label_parens.pdf'
    character(len=:), allocatable :: stream_text
    integer :: status
    logical :: found_open, found_close

    call figure(figsize=[6.4_wp, 4.8_wp])
    call ylabel('sin(x)')
    call savefig(out_pdf)

    found_open = .false.
    found_close = .false.

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF stream'
        stop 1
    end if

    found_open = index(stream_text, '(\()') > 0
    found_close = index(stream_text, '(\))') > 0

    if (.not. found_open) then
        print *, 'FAIL: missing escaped open parenthesis sequence (\() Tj in PDF stream'
        stop 1
    end if
    if (.not. found_close) then
        print *, 'FAIL: missing escaped close parenthesis sequence (\)) Tj in PDF stream'
        stop 1
    end if

    print *, 'PASS: PDF parentheses escaped in mixed-font text'
end program test_pdf_text_escaping
