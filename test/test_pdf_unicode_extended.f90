program test_pdf_unicode_extended
    !! Validate PDF backend maps common Unicode math symbols to Type1 escapes
    use fortplot
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: out_pdf = 'test/output/test_pdf_unicode_extended.pdf'
    character(len=:), allocatable :: stream_text
    integer :: status
    logical :: has_superscript_two, has_superscript_three, has_superscript_one
    logical :: has_multiply_sign, has_middle_dot, has_degree

    call figure()
    call title('Superscripts: E = mc², I = r³ + φ¹')
    call xlabel('Products × and dot · with 25° offset')
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
    call savefig(out_pdf)

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF content stream'
        stop 1
    end if

    has_superscript_two   = index(stream_text, '\\262') > 0
    has_superscript_three = index(stream_text, '\\263') > 0
    has_superscript_one   = index(stream_text, '\\271') > 0
    has_multiply_sign     = index(stream_text, '\\327') > 0
    has_middle_dot        = index(stream_text, '\\267') > 0
    has_degree            = index(stream_text, '\\260') > 0

    if (.not. has_superscript_two) then
        print *, 'FAIL: missing PDF escape for superscript two (\\262)'
        stop 1
    end if
    if (.not. has_superscript_three) then
        print *, 'FAIL: missing PDF escape for superscript three (\\263)'
        stop 1
    end if
    if (.not. has_superscript_one) then
        print *, 'FAIL: missing PDF escape for superscript one (\\271)'
        stop 1
    end if
    if (.not. has_multiply_sign) then
        print *, 'FAIL: missing PDF escape for multiply sign (\\327)'
        stop 1
    end if
    if (.not. has_middle_dot) then
        print *, 'FAIL: missing PDF escape for middle dot (\\267)'
        stop 1
    end if
    if (.not. has_degree) then
        print *, 'FAIL: missing PDF escape for degree sign (\\260)'
        stop 1
    end if

    print *, 'PASS: Extended Unicode math glyphs mapped to Type1 escape sequences'
end program test_pdf_unicode_extended
