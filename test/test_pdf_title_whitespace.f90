program test_pdf_title_whitespace
    !! Verifies PDF backend preserves spaces in titles (no whitespace loss)
    use fortplot
    implicit none

    character(len=*), parameter :: out_pdf = 'test/output/test_pdf_title_whitespace.pdf'
    integer :: unit, ios
    integer(kind=8) :: fsize
    character, allocatable :: data(:)
    logical :: has_space_tj

    call figure()
    call title('HELLO WORLD TEST')
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

    ! Ensure a space glyph is emitted as its own Tj segment: "( ) Tj"
    has_space_tj = bytes_contains(data, fsize, '( ) Tj')
    if (.not. has_space_tj) then
        print *, 'FAIL: missing space Tj in PDF title rendering'
        stop 1
    end if

    print *, 'PASS: PDF title preserves spaces'
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
end program test_pdf_title_whitespace

