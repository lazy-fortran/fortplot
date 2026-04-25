program test_quiver_data_ranges
    !! Verify quiver plot computes correct axis ranges from vector data
    !! Regression test for issue #1710: quiver was missing from data range
    !! calculation, causing default [0,1] range instead of actual data range.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, quiver, xlabel, ylabel, title, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: nx = 10, ny = 10
    real(wp), dimension(nx*ny) :: x, y, u, v
    integer :: i, j, k
    real(wp) :: xi, yj
    character(len=*), parameter :: outfile = 'build/test/output/quiver_data_ranges.txt'
    logical :: dir_ok, file_exists
    integer :: file_size, unit, ios
    character(len=512) :: line
    logical :: has_neg_range, has_pos_range, found_content

    call create_directory_runtime('build/test/output', dir_ok)

    ! Create grid spanning [-2, 2]
    k = 0
    do j = 1, ny
        do i = 1, nx
            k = k + 1
            xi = -2.0_wp + 4.0_wp * real(i-1, wp) / real(nx-1, wp)
            yj = -2.0_wp + 4.0_wp * real(j-1, wp) / real(ny-1, wp)
            x(k) = xi
            y(k) = yj
            u(k) = -yj
            v(k) = xi
        end do
    end do

    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v)
    call xlabel('X')
    call ylabel('Y')
    call title('Quiver Data Range Test')
    call savefig(outfile)

    ! Verify file exists and has content
    inquire(file=outfile, exist=file_exists, size=file_size)
    if (.not. file_exists) then
        print *, "FAIL: quiver savefig did not create ", outfile
        stop 1
    end if
    if (file_size <= 0) then
        print *, "FAIL: quiver output file is empty"
        stop 1
    end if
    print *, "PASS: quiver file created (", file_size, " bytes)"

    ! Verify axis range labels show correct data range
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
        ! Check for negative axis labels (e.g., "-2.0", "-1.5")
        if (index(line, '-2.0') > 0 .or. index(line, '-1.5') > 0 .or. &
            index(line, '-1.0') > 0 .or. index(line, '-0.5') > 0) then
            has_neg_range = .true.
        end if
        ! Check for positive axis labels (e.g., "2.0", "1.5")
        if (index(line, '2.0') > 0 .or. index(line, '1.5') > 0) then
            has_pos_range = .true.
        end if
    end do
    close(unit)

    if (.not. found_content) then
        print *, "FAIL: quiver output has no readable content"
        stop 1
    end if

    if (.not. has_neg_range) then
        print *, "FAIL: quiver output missing negative axis labels"
        print *, "      Expected labels such as -2.0, -1.5, -1.0, -0.5"
        stop 1
    end if
    print *, "PASS: negative axis labels present"

    if (.not. has_pos_range) then
        print *, "FAIL: quiver output missing positive axis labels"
        print *, "      Expected labels such as 2.0, 1.5"
        stop 1
    end if
    print *, "PASS: positive axis labels present"

    print *, "PASS: quiver data range test passed"

end program test_quiver_data_ranges
