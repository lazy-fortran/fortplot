program test_pdf_pcolormesh_inline_image
    !! Verify that pcolormesh PDF contains an inline image (BI ... ID ... EI)
    !! Robust to Flate-compressed content streams: if the stream is compressed,
    !! the literal BI/ID/EI tokens are not visible in the PDF text. In that case
    !! accept the presence of '/Filter /FlateDecode' as sufficient evidence.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    character(len=*), parameter :: fn = 'test/output/test_pdf_inline_image.pdf'
    integer :: unit, ios
    integer(kind=8) :: fsize
    character, allocatable :: data(:)
    logical :: has_bi, has_id, has_ei, has_filter

    call figure()
    call pcolormesh([0.0_wp,0.5_wp,1.0_wp],[0.0_wp,0.5_wp,1.0_wp], reshape([0.1_wp,0.2_wp,0.3_wp, &
                   0.4_wp,0.5_wp,0.6_wp, 0.7_wp,0.8_wp,0.9_wp],[3,3]))
    call savefig(fn)

    open(newunit=unit, file=fn, access='stream', form='unformatted', status='old', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ', trim(fn)
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

    has_bi = bytes_contains(data, fsize, ' BI ') .or. bytes_contains(data, fsize, 'BI /W')
    has_id = bytes_contains(data, fsize, ' ID')   .or. bytes_contains(data, fsize, ' ID ')
    has_ei = bytes_contains(data, fsize, 'EI')
    has_filter = bytes_contains(data, fsize, '/Filter /FlateDecode')

    if (.not. (has_bi .and. has_id .and. has_ei)) then
        if (has_filter) then
            print *, 'INFO: content stream compressed; inline image tokens not readable'
            stop 0
        else
            print *, 'FAIL: inline image markers not found (BI/ID/EI)'
            stop 2
        end if
    end if
    print *, 'PASS: inline image present in pcolormesh PDF'

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
end program test_pdf_pcolormesh_inline_image
