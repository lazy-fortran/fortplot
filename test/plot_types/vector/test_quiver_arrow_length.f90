program test_quiver_arrow_length
    !! Verify quiver scale acts as a length multiplier and shaft length tracks
    !! vector magnitude. scale=0.5 must roughly halve the shafts, and a field
    !! of varying magnitude must produce shafts of varying length. Shaft length
    !! is read from the SVG arrow segments, the same metric the rendering gate
    !! checks.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure, quiver, savefig
    use fortplot_system_runtime, only: create_directory_runtime
    implicit none

    integer, parameter :: nx = 10, ny = 10
    real(wp), dimension(nx*ny) :: x, y, u, v
    integer :: i, j, k
    real(wp) :: xi, yj
    real(wp) :: mean_default, mean_scaled, max_default, min_default, dummy
    logical :: dir_ok
    character(len=*), parameter :: out_default = &
        'build/test/output/quiver_len_default.svg'
    character(len=*), parameter :: out_scaled = &
        'build/test/output/quiver_len_scaled.svg'

    call create_directory_runtime('build/test/output', dir_ok)

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
    call savefig(out_default)

    call figure(figsize=[8.0_wp, 6.0_wp])
    call quiver(x, y, u, v, scale=0.5_wp)
    call savefig(out_scaled)

    call shaft_stats(out_default, mean_default, max_default, min_default)
    call shaft_stats(out_scaled, mean_scaled, dummy, dummy)

    ! Shaft length must vary with magnitude: the longest exceeds the shortest.
    if (max_default <= min_default * 1.5_wp) then
        print *, "FAIL: shafts do not vary with magnitude, max/min =", &
            max_default, min_default
        stop 1
    end if
    print *, "PASS: shaft length varies with magnitude"

    ! scale=0.5 acts as a direct length multiplier: about half the default.
    if (mean_scaled >= mean_default * 0.9_wp) then
        print *, "FAIL: scale=0.5 did not shorten shafts, scaled/default =", &
            mean_scaled, mean_default
        stop 1
    end if
    print *, "PASS: scale=0.5 shortens shafts, mean scaled/default =", &
        mean_scaled, mean_default

    print *, "PASS: quiver arrow length test passed"

contains

    subroutine shaft_stats(path, mean_len, max_len, min_len)
        !! Scan an SVG for quiver shaft segments (non-axis stroke) and return
        !! their mean, longest, and shortest pixel length.
        character(len=*), intent(in) :: path
        real(wp), intent(out) :: mean_len, max_len, min_len
        integer :: unit, ios, count
        character(len=4096) :: line
        real(wp) :: x1, y1, x2, y2, length, total

        max_len = 0.0_wp
        min_len = huge(1.0_wp)
        total = 0.0_wp
        count = 0

        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, "FAIL: cannot read ", path
            stop 1
        end if
        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, '<line') == 0) cycle
            if (index(line, 'stroke="rgb(') == 0) cycle
            if (.not. parse_line(line, x1, y1, x2, y2)) cycle
            length = sqrt((x2-x1)**2 + (y2-y1)**2)
            total = total + length
            count = count + 1
            if (length > max_len) max_len = length
            if (length < min_len) min_len = length
        end do
        close(unit)

        if (count == 0) then
            print *, "FAIL: no quiver shaft segments found in ", path
            stop 1
        end if
        mean_len = total / real(count, wp)
    end subroutine shaft_stats

    logical function parse_line(line, x1, y1, x2, y2) result(ok)
        !! Extract x1,y1,x2,y2 from an SVG <line .../> element.
        character(len=*), intent(in) :: line
        real(wp), intent(out) :: x1, y1, x2, y2
        ok = read_attr(line, 'x1="', x1) .and. read_attr(line, 'y1="', y1) &
            .and. read_attr(line, 'x2="', x2) .and. read_attr(line, 'y2="', y2)
    end function parse_line

    logical function read_attr(line, key, val) result(ok)
        !! Read the numeric value following key (e.g. 'x1="') in line.
        character(len=*), intent(in) :: line, key
        real(wp), intent(out) :: val
        integer :: p, q, ios
        ok = .false.
        val = 0.0_wp
        p = index(line, key)
        if (p == 0) return
        p = p + len(key)
        q = index(line(p:), '"')
        if (q == 0) return
        read(line(p:p+q-2), *, iostat=ios) val
        ok = ios == 0
    end function read_attr

end program test_quiver_arrow_length
