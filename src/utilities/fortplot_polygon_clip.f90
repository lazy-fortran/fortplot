module fortplot_polygon_clip
    !! Clip device-space polygons to a rectangle before they are filled.
    !! The polygon counterpart of fortplot_segment_clip: backends call this
    !! so that vertices far outside the visible area (e.g. y ~ 1e21 under a
    !! small ylim) neither overflow integer pixel conversion nor reach the
    !! PDF stream, and the fill covers exactly the visible part.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    private
    public :: clip_polygon, clip_polygon_capacity

contains

    pure integer function clip_polygon_capacity(n) result(cap)
        !! Output size that always holds the clip of an n-vertex polygon:
        !! one half-plane stage turns k vertices into at most 3k/2, so four
        !! stages give at most (3/2)**4 n < 6 n.
        integer, intent(in) :: n
        cap = 6*max(n, 1)
    end function clip_polygon_capacity

    pure subroutine clip_polygon(px, py, lo, hi, cx, cy, m)
        !! Sutherland-Hodgman clip of the closed polygon (px, py) to
        !! [lo(1),hi(1)] x [lo(2),hi(2)]. Non-finite vertices are dropped
        !! first. On return (cx(1:m), cy(1:m)) is the clipped polygon; m = 0
        !! when fewer than three vertices remain. cx, cy need
        !! clip_polygon_capacity(size(px)) elements. A vertex created on an
        !! edge lies exactly on it.
        real(wp), intent(in) :: px(:), py(:), lo(2), hi(2)
        real(wp), intent(out) :: cx(:), cy(:)
        integer, intent(out) :: m
        real(wp) :: bx(size(cx)), by(size(cx))
        integer :: i, k

        m = 0
        do i = 1, min(size(px), size(py))
            if (abs(px(i)) < huge(1.0_wp) .and. abs(py(i)) < huge(1.0_wp)) then
                m = m + 1
                cx(m) = px(i); cy(m) = py(i)
            end if
        end do
        do k = 1, 4
            if (m < 3) exit
            bx(1:m) = cx(1:m); by(1:m) = cy(1:m)
            call clip_half_plane(bx(1:m), by(1:m), mod(k - 1, 2) + 1, &
                                 merge(lo, hi, k <= 2), k <= 2, cx, cy, m)
        end do
        if (m < 3) m = 0
    end subroutine clip_polygon

    pure subroutine clip_half_plane(ax, ay, axis, bounds, keep_above, cx, cy, m)
        !! Keep the part of the polygon with coordinate(axis) >= bound
        !! (keep_above) or <= bound, bound = bounds(axis).
        real(wp), intent(in) :: ax(:), ay(:), bounds(2)
        integer, intent(in) :: axis
        logical, intent(in) :: keep_above
        real(wp), intent(inout) :: cx(:), cy(:)
        integer, intent(out) :: m
        real(wp) :: a(2), b(2), q(2), bound
        logical :: a_in, b_in
        integer :: i, n

        n = size(ax)
        bound = bounds(axis)
        m = 0
        a = [ax(n), ay(n)]
        a_in = inside(a)
        do i = 1, n
            b = [ax(i), ay(i)]
            b_in = inside(b)
            if (a_in .neqv. b_in) then
                q = crossing(a, b)
                m = m + 1
                cx(m) = q(1); cy(m) = q(2)
            end if
            if (b_in) then
                m = m + 1
                cx(m) = b(1); cy(m) = b(2)
            end if
            a = b; a_in = b_in
        end do

    contains

        pure logical function inside(p)
            real(wp), intent(in) :: p(2)
            if (keep_above) then
                inside = p(axis) >= bound
            else
                inside = p(axis) <= bound
            end if
        end function inside

        pure function crossing(p, r) result(s)
            !! Point of segment p-r on the bound, interpolated from the
            !! nearer end so a huge far end does not cancel its digits.
            real(wp), intent(in) :: p(2), r(2)
            real(wp) :: s(2), t

            t = (bound - p(axis))/(r(axis) - p(axis))
            if (t <= 0.5_wp) then
                s = p + t*(r - p)
            else
                s = r + (t - 1.0_wp)*(r - p)
            end if
            s(axis) = bound
        end function crossing
    end subroutine clip_half_plane

end module fortplot_polygon_clip
