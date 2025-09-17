program test_pdf_unicode_uppercase
    !! Verify PDF renders uppercase Greek letters (e.g., \Psi) via Symbol font
    use fortplot
    use test_pdf_utils, only: extract_pdf_stream_text
    implicit none

    character(len=*), parameter :: out_pdf = 'test/output/test_pdf_unicode_uppercase.pdf'
    character(len=:), allocatable :: stream_text
    integer :: status
    logical :: has_symbol_font, has_upper_psi, has_upper_theta, has_upper_omega

    call figure()
    call title('Uppercase: \Psi test')
    call xlabel('Theta \Theta and Omega \Omega')
    call ylabel('Check Psi \Psi in label')
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
    call savefig(out_pdf)

    call extract_pdf_stream_text(out_pdf, stream_text, status)
    if (status /= 0) then
        print *, 'FAIL: unable to read PDF stream text'
        stop 1
    end if

    has_symbol_font  = index(stream_text, '/F6') > 0
    has_upper_psi    = index(stream_text, '\131') > 0
    has_upper_theta  = index(stream_text, '\121') > 0
    has_upper_omega  = index(stream_text, '\127') > 0
    if (.not. has_symbol_font) then
        print *, 'FAIL: missing Symbol font switch (/F6)'
        stop 1
    end if
    if (.not. has_upper_psi) then
        print *, 'FAIL: missing uppercase Psi (\\131) escape in PDF stream'
        stop 1
    end if
    if (.not. has_upper_theta) then
        print *, 'FAIL: missing uppercase Theta (\\121) escape in PDF stream'
        stop 1
    end if
    if (.not. has_upper_omega) then
        print *, 'FAIL: missing uppercase Omega (\\127) escape in PDF stream'
        stop 1
    end if

    print *, 'PASS: PDF uppercase Greek (Psi/Theta/Omega) mapped via Symbol'

end program test_pdf_unicode_uppercase
