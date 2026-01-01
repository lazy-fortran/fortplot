program test_pdf_info_trailer_reference_1552
    use, intrinsic :: iso_fortran_env, only: int64
    use fortplot, only: figure, plot, savefig, title, wp
    use test_output_helpers, only: assert_pdf_file_valid, ensure_test_output_dir
    implicit none

    character(len=:), allocatable :: out_pdf
    character(len=:), allocatable :: output_dir
    integer(int64) :: fsize
    integer :: ios
    integer :: unit
    character(len=1), allocatable :: data(:)
    logical :: has_info_ref

    call ensure_test_output_dir('pdf_info_trailer_reference_1552', output_dir)
    out_pdf = trim(output_dir)//'info_trailer_reference.pdf'

    call figure()
    call plot([0.0_wp, 1.0_wp], [0.0_wp, 1.0_wp])
    call title('PDF trailer includes /Info reference')
    call savefig(out_pdf)

    call assert_pdf_file_valid(out_pdf)

    open (newunit=unit, file=out_pdf, access='stream', form='unformatted', &
          status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, 'FAIL: cannot open ', trim(out_pdf)
        stop 1
    end if

    inquire (unit=unit, size=fsize)
    if (fsize <= 0_int64) then
        print *, 'FAIL: zero-size PDF'
        close (unit)
        stop 1
    end if

    allocate (character(len=1) :: data(fsize))
    read (unit, iostat=ios) data
    close (unit)
    if (ios /= 0) then
        print *, 'FAIL: cannot read PDF data'
        stop 1
    end if

    has_info_ref = trailer_contains(data, fsize, '/Info 1 0 R')
    if (.not. has_info_ref) then
        print *, 'FAIL: PDF trailer missing /Info 1 0 R'
        stop 1
    end if

    print *, 'PASS: PDF trailer includes /Info reference'
contains
    integer(int64) function bytes_find_last(arr, n, pat) result(pos)
        integer(int64), intent(in) :: n
        character(len=1), intent(in) :: arr(n)
        character(len=*), intent(in) :: pat
        integer(int64) :: i
        integer :: j
        integer(int64) :: m

        pos = 0_int64
        m = int(len_trim(pat), int64)
        if (m <= 0_int64) return
        if (n < m) return

        do i = n - m + 1_int64, 1_int64, -1_int64
            do j = 1, int(m)
                if (arr(i + int(j - 1, int64)) /= pat(j:j)) exit
                if (j == int(m)) then
                    pos = i
                    return
                end if
            end do
        end do
    end function bytes_find_last

    logical function bytes_contains_range(arr, start_pos, end_pos, pat) result(found)
        character(len=1), intent(in) :: arr(:)
        integer(int64), intent(in) :: start_pos
        integer(int64), intent(in) :: end_pos
        character(len=*), intent(in) :: pat
        integer(int64) :: i
        integer :: j
        integer(int64) :: m

        found = .false.
        m = int(len_trim(pat), int64)
        if (m <= 0_int64) return
        if (start_pos < 1_int64) return
        if (end_pos < start_pos) return
        if (end_pos > int(size(arr), int64)) return
        if (end_pos - start_pos + 1_int64 < m) return

        do i = start_pos, end_pos - m + 1_int64
            do j = 1, int(m)
                if (arr(i + int(j - 1, int64)) /= pat(j:j)) exit
                if (j == int(m)) then
                    found = .true.
                    return
                end if
            end do
        end do
    end function bytes_contains_range

    logical function trailer_contains(arr, n, pat) result(found)
        integer(int64), intent(in) :: n
        character(len=1), intent(in) :: arr(n)
        character(len=*), intent(in) :: pat
        integer(int64) :: trailer_pos
        integer(int64) :: startxref_pos

        found = .false.
        trailer_pos = bytes_find_last(arr, n, 'trailer')
        if (trailer_pos <= 0_int64) return

        startxref_pos = bytes_find_last(arr, n, 'startxref')
        if (startxref_pos <= trailer_pos) return

        found = bytes_contains_range(arr, trailer_pos, startxref_pos - 1_int64, pat)
    end function trailer_contains
end program test_pdf_info_trailer_reference_1552
