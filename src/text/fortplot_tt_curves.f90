module fortplot_tt_curves
    !! TrueType glyph outline curve tessellation.
    !! Converts quadratic/cubic Bezier curves from glyph outlines into flat
    !! line segments suitable for rasterization. Fortran port of
    !! stbtt_FlattenCurves and related functions from stb_truetype.h
    !! lines 3547-3693.
    use fortplot_tt_outlines, only: tt_vertex_t, TT_VMOVE, TT_VLINE, TT_VCURVE
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: tt_point_t, tt_flatten_curves

    integer, parameter :: MAX_RECURSION_DEPTH = 16

    type :: tt_point_t
        real(dp) :: x
        real(dp) :: y
    end type tt_point_t

contains

    recursive subroutine tesselate_quadratic(fill, points, num_points, &
            x0, y0, x1, y1, x2, y2, flatness_sq, depth)
        !! Subdivide a quadratic Bezier curve until flat enough.
        !! Port of stbtt__tesselate_curve (stb_truetype.h lines 3555-3572).
        !! When fill is false, only counts points without writing.
        logical, intent(in) :: fill
        type(tt_point_t), intent(inout) :: points(:)
        integer, intent(inout) :: num_points
        real(dp), intent(in) :: x0, y0, x1, y1, x2, y2, flatness_sq
        integer, intent(in) :: depth
        real(dp) :: mx, my, dx, dy

        mx = (x0 + 2.0_dp * x1 + x2) / 4.0_dp
        my = (y0 + 2.0_dp * y1 + y2) / 4.0_dp
        dx = (x0 + x2) / 2.0_dp - mx
        dy = (y0 + y2) / 2.0_dp - my

        if (depth > MAX_RECURSION_DEPTH) return

        if (dx * dx + dy * dy > flatness_sq) then
            call tesselate_quadratic(fill, points, num_points, &
                x0, y0, (x0 + x1) / 2.0_dp, (y0 + y1) / 2.0_dp, &
                mx, my, flatness_sq, depth + 1)
            call tesselate_quadratic(fill, points, num_points, &
                mx, my, (x1 + x2) / 2.0_dp, (y1 + y2) / 2.0_dp, &
                x2, y2, flatness_sq, depth + 1)
        else
            num_points = num_points + 1
            if (fill) points(num_points) = tt_point_t(x2, y2)
        end if
    end subroutine tesselate_quadratic

    recursive subroutine tesselate_cubic(fill, points, num_points, &
            x0, y0, x1, y1, x2, y2, x3, y3, flatness_sq, depth)
        !! Subdivide a cubic Bezier curve until flat enough.
        !! Port of stbtt__tesselate_cubic (stb_truetype.h lines 3575-3614).
        !! When fill is false, only counts points without writing.
        logical, intent(in) :: fill
        type(tt_point_t), intent(inout) :: points(:)
        integer, intent(inout) :: num_points
        real(dp), intent(in) :: x0, y0, x1, y1, x2, y2, x3, y3, flatness_sq
        integer, intent(in) :: depth
        real(dp) :: dx0, dy0, dx1, dy1, dx2, dy2, dx, dy
        real(dp) :: longlen, shortlen
        real(dp) :: x01, y01, x12, y12, x23, y23, xa, ya, xb, yb, mx, my

        if (depth > MAX_RECURSION_DEPTH) return

        dx0 = x1 - x0; dy0 = y1 - y0
        dx1 = x2 - x1; dy1 = y2 - y1
        dx2 = x3 - x2; dy2 = y3 - y2
        dx = x3 - x0; dy = y3 - y0

        longlen = sqrt(dx0 * dx0 + dy0 * dy0) &
            + sqrt(dx1 * dx1 + dy1 * dy1) &
            + sqrt(dx2 * dx2 + dy2 * dy2)
        shortlen = sqrt(dx * dx + dy * dy)

        if (longlen * longlen - shortlen * shortlen > flatness_sq) then
            x01 = (x0 + x1) / 2.0_dp; y01 = (y0 + y1) / 2.0_dp
            x12 = (x1 + x2) / 2.0_dp; y12 = (y1 + y2) / 2.0_dp
            x23 = (x2 + x3) / 2.0_dp; y23 = (y2 + y3) / 2.0_dp
            xa = (x01 + x12) / 2.0_dp; ya = (y01 + y12) / 2.0_dp
            xb = (x12 + x23) / 2.0_dp; yb = (y12 + y23) / 2.0_dp
            mx = (xa + xb) / 2.0_dp; my = (ya + yb) / 2.0_dp

            call tesselate_cubic(fill, points, num_points, &
                x0, y0, x01, y01, xa, ya, mx, my, flatness_sq, depth + 1)
            call tesselate_cubic(fill, points, num_points, &
                mx, my, xb, yb, x23, y23, x3, y3, flatness_sq, depth + 1)
        else
            num_points = num_points + 1
            if (fill) points(num_points) = tt_point_t(x3, y3)
        end if
    end subroutine tesselate_cubic

    subroutine tt_flatten_curves(vertices, num_verts, objspace_flatness, &
            points, contour_lengths, num_contours)
        !! Convert glyph vertex array to flattened point arrays with contour info.
        !! Uses a two-pass algorithm: pass 1 counts points, pass 2 fills them.
        !! Port of stbtt_FlattenCurves (stb_truetype.h lines 3618-3693).
        type(tt_vertex_t), intent(in) :: vertices(:)
        integer, intent(in) :: num_verts
        real(dp), intent(in) :: objspace_flatness
        type(tt_point_t), allocatable, intent(out) :: points(:)
        integer, allocatable, intent(out) :: contour_lengths(:)
        integer, intent(out) :: num_contours

        type(tt_point_t) :: dummy_points(1)
        real(dp) :: flatness_sq, x, y
        integer :: i, n, start, num_points, pass
        logical :: fill

        num_contours = 0
        flatness_sq = objspace_flatness * objspace_flatness

        ! Count contours (number of VMOVE vertices)
        do i = 1, num_verts
            if (vertices(i)%vtype == TT_VMOVE) num_contours = num_contours + 1
        end do

        if (num_contours == 0) return

        allocate(contour_lengths(num_contours))

        ! Two passes: first count points, then allocate and fill
        do pass = 1, 2
            fill = (pass == 2)
            if (pass == 2) allocate(points(num_points))
            num_points = 0
            n = 0
            start = 0
            x = 0.0_dp
            y = 0.0_dp

            do i = 1, num_verts
                select case (vertices(i)%vtype)
                case (TT_VMOVE)
                    if (n > 0) contour_lengths(n) = num_points - start
                    n = n + 1
                    start = num_points
                    x = real(vertices(i)%x, dp)
                    y = real(vertices(i)%y, dp)
                    num_points = num_points + 1
                    if (fill) points(num_points) = tt_point_t(x, y)

                case (TT_VLINE)
                    x = real(vertices(i)%x, dp)
                    y = real(vertices(i)%y, dp)
                    num_points = num_points + 1
                    if (fill) points(num_points) = tt_point_t(x, y)

                case (TT_VCURVE)
                    if (fill) then
                        call tesselate_quadratic(.true., points, num_points, &
                            x, y, &
                            real(vertices(i)%cx, dp), &
                            real(vertices(i)%cy, dp), &
                            real(vertices(i)%x, dp), &
                            real(vertices(i)%y, dp), &
                            flatness_sq, 0)
                    else
                        call tesselate_quadratic(.false., dummy_points, &
                            num_points, x, y, &
                            real(vertices(i)%cx, dp), &
                            real(vertices(i)%cy, dp), &
                            real(vertices(i)%x, dp), &
                            real(vertices(i)%y, dp), &
                            flatness_sq, 0)
                    end if
                    x = real(vertices(i)%x, dp)
                    y = real(vertices(i)%y, dp)

                end select
            end do

            ! Close last contour
            if (n > 0) contour_lengths(n) = num_points - start
        end do
    end subroutine tt_flatten_curves

end module fortplot_tt_curves
