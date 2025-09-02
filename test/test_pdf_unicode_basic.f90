program test_pdf_unicode_basic
    !! Verifies PDF backend handles basic Unicode (Greek) via Symbol mapping
    use fortplot
    implicit none

    character(len=*), parameter :: out_pdf = 'test/output/test_pdf_unicode_basic.pdf'
    integer :: unit, ios
    integer(kind=8) :: fsize
    character, allocatable :: data(:)
    logical :: has_symbol_font

    call figure()
    call title('alpha α beta β')
    call xlabel('μ = 1, σ = 2')
    call ylabel('θ and Ω test')
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

    ! Check presence of Symbol font switches indicating Greek handling
    has_symbol_font = bytes_contains(data, fsize, '/F6')
    if (.not. has_symbol_font) then
        print *, 'FAIL: missing Symbol font switches (/F6) in PDF content'
        stop 1
    end if

    print *, 'PASS: PDF Unicode basic Greek mapping present'
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
end program test_pdf_unicode_basic
