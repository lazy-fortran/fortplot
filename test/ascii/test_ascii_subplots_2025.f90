program test_ascii_subplots_2025
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, subplot, plot, title, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    real(wp) :: x(8), y(8)
    integer :: i, unit, ios
    integer :: line_tl, line_tr, line_bl, line_br
    logical :: dir_ok, file_exists
    character(len=*), parameter :: outfile = &
        'build/test/output/ascii_subplots_2025.txt'
    character(len=512) :: line

    call create_directory_runtime('build/test/output', dir_ok)

    do i = 1, size(x)
        x(i) = real(i - 1, wp)
    end do

    call figure(figsize=[12.0_wp, 8.0_wp])

    call subplot(2, 2, 1)
    y = x
    call plot(x, y)
    call title('subplot 1')

    call subplot(2, 2, 2)
    y = x**2
    call plot(x, y)
    call title('subplot 2')

    call subplot(2, 2, 3)
    y = sqrt(x + 1.0_wp)
    call plot(x, y)
    call title('subplot 3')

    call subplot(2, 2, 4)
    y = 1.0_wp / (x + 1.0_wp)
    call plot(x, y)
    call title('subplot 4')

    call savefig(outfile)

    inquire(file=outfile, exist=file_exists)
    if (.not. file_exists) then
        print *, 'FAIL: ASCII subplot file missing'
        stop 1
    end if

    call find_title_line(outfile, 'subplot 1', line_tl)
    call find_title_line(outfile, 'subplot 2', line_tr)
    call find_title_line(outfile, 'subplot 3', line_bl)
    call find_title_line(outfile, 'subplot 4', line_br)

    if (line_tl <= 0 .or. line_tr <= 0 .or. line_bl <= 0 .or. line_br <= 0) then
        print *, 'FAIL: missing subplot titles in ASCII output'
        stop 1
    end if

    if (.not. (line_tl < line_bl .and. line_tr < line_br)) then
        print *, 'FAIL: subplot titles do not occupy separate vertical bands'
        print *, 'subplot 1 line = ', line_tl
        print *, 'subplot 2 line = ', line_tr
        print *, 'subplot 3 line = ', line_bl
        print *, 'subplot 4 line = ', line_br
        stop 1
    end if

    print *, 'PASS: ASCII subplot titles render in distinct bands'

contains

    subroutine find_title_line(path, needle, line_no)
        character(len=*), intent(in) :: path, needle
        integer, intent(out) :: line_no
        integer :: unit, ios, current
        character(len=512) :: buf

        line_no = 0
        current = 0
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'FAIL: cannot open ', path
            stop 1
        end if
        do
            read(unit, '(A)', iostat=ios) buf
            if (ios /= 0) exit
            current = current + 1
            if (index(buf, needle) > 0) then
                line_no = current
                exit
            end if
        end do
        close(unit)
    end subroutine find_title_line

end program test_ascii_subplots_2025
