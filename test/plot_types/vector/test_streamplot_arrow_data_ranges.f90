program test_streamplot_arrow_data_ranges
    !! Verify streamplot with arrowsize>0 keeps the user's data range on the
    !! visible axis. Regression test for the bug where arrow-only mode added
    !! no plot entries, so calculate_figure_data_ranges fell back to its
    !! default 0..1 range and the rendered axes collapsed away from the
    !! actual x/y span.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, streamplot, xlabel, ylabel, title, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: nx = 20, ny = 20
    real(wp) :: x(nx), y(ny), u(nx, ny), v(nx, ny)
    integer :: i, j
    character(len=*), parameter :: outfile = &
        'build/test/output/streamplot_arrow_data_ranges.txt'
    logical :: dir_ok, file_exists
    integer :: file_size, unit, ios
    character(len=512) :: line
    logical :: has_neg_range, has_pos_range, found_content

    call create_directory_runtime('build/test/output', dir_ok)

    do i = 1, nx
        x(i) = -2.0_wp + 4.0_wp * real(i - 1, wp) / real(nx - 1, wp)
    end do
    do j = 1, ny
        y(j) = -2.0_wp + 4.0_wp * real(j - 1, wp) / real(ny - 1, wp)
    end do

    do j = 1, ny
        do i = 1, nx
            u(i, j) = -y(j)
            v(i, j) =  x(i)
        end do
    end do

    call figure(figsize=[8.0_wp, 6.0_wp])
    call streamplot(x, y, u, v, density=1.0_wp, arrowsize=1.5_wp, arrowstyle='->')
    call xlabel('X')
    call ylabel('Y')
    call title('Streamplot Arrow Data Range Test')
    call savefig(outfile)

    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "FAIL: streamplot savefig did not create ", outfile
        stop 1
    end if
    if (file_size <= 0) then
        print *, "FAIL: streamplot output file is empty"
        stop 1
    end if

    has_neg_range = .false.
    has_pos_range = .false.
    found_content = .false.

    open(newunit=unit, file=outfile, status='old', action='read', iostat=ios)
    if (ios /= 0) then
        print *, "FAIL: cannot read ", outfile
        stop 1
    end if
    do
        read(unit, '(A)', iostat=ios) line
        if (ios /= 0) exit
        found_content = .true.
        if (index(line, '-2.0') > 0 .or. index(line, '-1.5') > 0 .or. &
            index(line, '-1.0') > 0 .or. index(line, '-0.5') > 0) then
            has_neg_range = .true.
        end if
        if (index(line, '2.0') > 0 .or. index(line, '1.5') > 0) then
            has_pos_range = .true.
        end if
    end do
    close(unit)

    if (.not. found_content) then
        print *, "FAIL: streamplot output has no readable content"
        stop 1
    end if
    if (.not. has_neg_range) then
        print *, "FAIL: streamplot output missing negative axis labels"
        print *, "      Expected labels such as -2.0, -1.5, -1.0, -0.5"
        stop 1
    end if
    if (.not. has_pos_range) then
        print *, "FAIL: streamplot output missing positive axis labels"
        print *, "      Expected labels such as 2.0, 1.5"
        stop 1
    end if

    print *, "PASS: streamplot arrow-mode data range preserved"

end program test_streamplot_arrow_data_ranges
