program test_pdf_unicode_uppercase
    !! Verify PDF renders uppercase Greek letters (e.g., \Psi) via Symbol font
    use fortplot
    implicit none

    character(len=*), parameter :: out_pdf = 'test/output/test_pdf_unicode_uppercase.pdf'
    integer :: unit, ios
    integer(kind=8) :: fsize
    character, allocatable :: data(:)
    logical :: has_symbol_font, has_upper_psi, has_upper_theta, has_upper_omega

    call figure()
    call title('Uppercase: \Psi test')
    call xlabel('Theta \Theta and Omega \Omega')
    call ylabel('Check Psi \Psi in label')
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
    call savefig(out_pdf)

    open(newunit=unit, file=out_pdf, access='stream', form='unformatted', status='old', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ', trim(out_pdf)
        stop 1
    end if
    inquire(unit=unit, size=fsize)
    if (fsize <= 0) then
        print *, 'FAIL: zero-size PDF'
        close(unit)
        stop 1
    end if
    allocate(character(len=1) :: data(fsize))
    read(unit, iostat=ios) data
    close(unit)
    if (ios /= 0) then
        print *, 'FAIL: cannot read PDF data'
        stop 1
    end if

    has_symbol_font  = bytes_contains(data, fsize, '/F6')
    has_upper_psi    = bytes_contains(data, fsize, '\131')
    has_upper_theta  = bytes_contains(data, fsize, '\121')
    has_upper_omega  = bytes_contains(data, fsize, '\127')
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

contains
    logical function bytes_contains(arr, n, pat) result(found)
        character(len=1), intent(in) :: arr(n)
        integer(kind=8), intent(in) :: n
        character(len=*), intent(in) :: pat
        integer :: i, j, m
        found = .false.
        m = len_trim(pat)
        if (m <= 0) return
        do i = 1, int(n) - m + 1
            do j = 1, m
                if (arr(i+j-1) /= pat(j:j)) exit
                if (j == m) then
                    found = .true.
                    return
                end if
            end do
        end do
    end function bytes_contains
end program test_pdf_unicode_uppercase
