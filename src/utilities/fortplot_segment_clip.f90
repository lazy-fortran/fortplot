module fortplot_segment_clip
    !! Clip device-space line segments to a rectangle before they are drawn.
    !! Backends call this so that data far outside the visible area (e.g.
    !! y ~ 1e21 under a small ylim) cost work proportional to what is seen.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    private
    public :: clip_segment

contains

    pure subroutine clip_segment(px1, py1, px2, py2, lo, hi, x0, y0, x1, y1, &
            t_start, visible)
        !! Liang-Barsky clip of (px1,py1)-(px2,py2) to [lo(1),hi(1)] x
        !! [lo(2),hi(2)]. A clipped end is placed exactly on the edge it
        !! crosses; t_start is the segment parameter of (x0, y0).
        real(wp), intent(in) :: px1, py1, px2, py2, lo(2), hi(2)
        real(wp), intent(out) :: x0, y0, x1, y1, t_start
        logical, intent(out) :: visible
        real(wp) :: p0(2), d(2), t0, t1, t
        integer :: k, edge0, edge1

        x0 = px1; y0 = py1; x1 = px2; y1 = py2; t_start = 0.0_wp
        visible = .false.
        p0 = [px1, py1]
        d = [px2 - px1, py2 - py1]
        if (any(.not. (abs([p0, d]) < huge(1.0_wp)))) return
        t0 = 0.0_wp; t1 = 1.0_wp; edge0 = 0; edge1 = 0
        do k = 1, 2
            if (d(k) == 0.0_wp) then
                if (p0(k) < lo(k) .or. p0(k) > hi(k)) return
                cycle
            end if
            t = (merge(lo(k), hi(k), d(k) > 0.0_wp) - p0(k))/d(k)
            if (t > t0) then
                t0 = t; edge0 = k
            end if
            t = (merge(hi(k), lo(k), d(k) > 0.0_wp) - p0(k))/d(k)
            if (t < t1) then
                t1 = t; edge1 = k
            end if
        end do
        if (t0 > t1) return
        visible = .true.
        t_start = t0
        if (edge0 /= 0) call place(edge0, t0, .true., x0, y0)
        if (edge1 /= 0) call place(edge1, t1, .false., x1, y1)

    contains

        pure subroutine place(edge, t, entering, x, y)
            integer, intent(in) :: edge
            real(wp), intent(in) :: t
            logical, intent(in) :: entering
            real(wp), intent(out) :: x, y
            real(wp) :: q(2)

            q = p0 + t*d
            ! t*d may lose digits when d is huge; the crossed coordinate is
            ! known exactly.
            q(edge) = merge(lo(edge), hi(edge), (d(edge) > 0.0_wp) .eqv. entering)
            q = min(max(q, lo), hi)
            x = q(1); y = q(2)
        end subroutine place
    end subroutine clip_segment

end module fortplot_segment_clip
