module fortplot_contour_smoothing
    !! Contour line smoothing using Catmull-Rom spline interpolation
    !!
    !! This module provides smoothing for contour lines to reduce the
    !! polygonal/jagged appearance from marching squares output.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: smooth_contour_chain
    public :: catmull_rom_interpolate

    integer, parameter :: DEFAULT_SUBDIVISIONS = 4

contains

    subroutine smooth_contour_chain(n_in, x_in, y_in, subdivisions, &
                                    n_out, x_out, y_out)
        !! Smooth a polyline using Catmull-Rom spline interpolation
        integer, intent(in) :: n_in
        real(wp), intent(in) :: x_in(:), y_in(:)
        integer, intent(in) :: subdivisions
        integer, intent(out) :: n_out
        real(wp), allocatable, intent(out) :: x_out(:), y_out(:)

        integer :: i, j, idx, n_subdiv, n_segments
        real(wp) :: t, x0, y0, x1, y1, x2, y2, x3, y3
        real(wp) :: px, py

        if (n_in < 2) then
            n_out = n_in
            allocate (x_out(n_in), y_out(n_in))
            if (n_in >= 1) then
                x_out(1:n_in) = x_in(1:n_in)
                y_out(1:n_in) = y_in(1:n_in)
            end if
            return
        end if

        n_subdiv = max(1, subdivisions)
        n_segments = n_in - 1
        n_out = n_segments*n_subdiv + 1

        allocate (x_out(n_out), y_out(n_out))

        idx = 0
        do i = 1, n_segments
            x1 = x_in(i)
            y1 = y_in(i)
            x2 = x_in(i + 1)
            y2 = y_in(i + 1)

            if (i == 1) then
                x0 = 2.0_wp*x1 - x2
                y0 = 2.0_wp*y1 - y2
            else
                x0 = x_in(i - 1)
                y0 = y_in(i - 1)
            end if

            if (i == n_segments) then
                x3 = 2.0_wp*x2 - x1
                y3 = 2.0_wp*y2 - y1
            else
                x3 = x_in(i + 2)
                y3 = y_in(i + 2)
            end if

            do j = 0, n_subdiv - 1
                t = real(j, wp)/real(n_subdiv, wp)
                call catmull_rom_interpolate(t, x0, y0, x1, y1, x2, y2, &
                                             x3, y3, px, py)
                idx = idx + 1
                x_out(idx) = px
                y_out(idx) = py
            end do
        end do

        idx = idx + 1
        x_out(idx) = x_in(n_in)
        y_out(idx) = y_in(n_in)
        n_out = idx
    end subroutine smooth_contour_chain

    pure subroutine catmull_rom_interpolate(t, x0, y0, x1, y1, x2, y2, &
                                            x3, y3, px, py)
        !! Catmull-Rom spline interpolation between P1 and P2
        !! t in [0,1], P0/P3 are control points
        real(wp), intent(in) :: t
        real(wp), intent(in) :: x0, y0, x1, y1, x2, y2, x3, y3
        real(wp), intent(out) :: px, py

        real(wp) :: t2, t3
        real(wp) :: c0, c1, c2, c3

        t2 = t*t
        t3 = t2*t

        c0 = -0.5_wp*t3 + t2 - 0.5_wp*t
        c1 = 1.5_wp*t3 - 2.5_wp*t2 + 1.0_wp
        c2 = -1.5_wp*t3 + 2.0_wp*t2 + 0.5_wp*t
        c3 = 0.5_wp*t3 - 0.5_wp*t2

        px = c0*x0 + c1*x1 + c2*x2 + c3*x3
        py = c0*y0 + c1*y1 + c2*y2 + c3*y3
    end subroutine catmull_rom_interpolate

end module fortplot_contour_smoothing
