module fortplot_legend_overlap
    !! Matplotlib-style overlap cost for points, paths and rectangular patches.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    implicit none
    private
    public :: legend_overlap_cost

contains

    function legend_overlap_cost(bounds, x, y, paths, rectangles) result(cost)
        real(wp), intent(in) :: bounds(4), x(:), y(:)
        integer, intent(in), optional :: paths(:)
        real(wp), intent(in), optional :: rectangles(:, :)
        integer :: cost, i, path, n
        logical :: intersects

        cost = 0
        n = min(size(x), size(y))
        do i = 1, n
            if (.not. ieee_is_finite(x(i))) cycle
            if (.not. ieee_is_finite(y(i))) cycle
            if (x(i) >= bounds(1) .and. x(i) <= bounds(3) .and. &
                y(i) >= bounds(2) .and. y(i) <= bounds(4)) cost = cost + 1
        end do
        if (present(paths)) then
            n = min(n, size(paths))
            if (n > 0) then
                do path = 1, maxval(paths(1:n))
                    intersects = .false.
                    do i = 2, n
                        if (paths(i) /= path .or. paths(i - 1) /= path) cycle
                        if (segment_intersects(bounds, [x(i - 1), y(i - 1)], &
                            [x(i), y(i)])) intersects = .true.
                        if (intersects) exit
                    end do
                    if (intersects) cost = cost + 1
                end do
            end if
        end if
        if (present(rectangles)) then
            do i = 1, size(rectangles, 2)
                if (rectangles(1, i) <= bounds(3) .and. &
                    rectangles(3, i) >= bounds(1) .and. &
                    rectangles(2, i) <= bounds(4) .and. &
                    rectangles(4, i) >= bounds(2)) cost = cost + 1
            end do
        end if
    end function legend_overlap_cost

    pure function segment_intersects(bounds, start, finish) result(intersects)
        real(wp), intent(in) :: bounds(4), start(2), finish(2)
        logical :: intersects
        real(wp) :: lower, upper, delta, t0, t1
        integer :: axis

        intersects = .false.
        if (.not. all(ieee_is_finite(start))) return
        if (.not. all(ieee_is_finite(finish))) return
        lower = 0.0_wp
        upper = 1.0_wp
        do axis = 1, 2
            delta = finish(axis) - start(axis)
            if (abs(delta) <= tiny(delta)) then
                if (start(axis) < bounds(axis)) return
                if (start(axis) > bounds(axis + 2)) return
            else
                t0 = (bounds(axis) - start(axis))/delta
                t1 = (bounds(axis + 2) - start(axis))/delta
                lower = max(lower, min(t0, t1))
                upper = min(upper, max(t0, t1))
                if (lower > upper) return
            end if
        end do
        intersects = .true.
    end function segment_intersects

end module fortplot_legend_overlap
