module fortplot_contour_algorithms
    !! Contour plotting algorithms module
    !!
    !! This module implements the marching squares algorithm and related
    !! functions for contour line extraction and rendering.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: calculate_marching_squares_config
    public :: get_contour_lines
    public :: interpolate_edge_crossings
    public :: apply_marching_squares_lookup
    public :: smooth_contour_chain
    public :: catmull_rom_interpolate

contains

    subroutine calculate_marching_squares_config(z1, z2, z3, z4, level, config)
        !! Calculate marching squares configuration for a cell
        real(wp), intent(in) :: z1, z2, z3, z4, level
        integer, intent(out) :: config

        config = 0
        if (z1 >= level) config = config + 1
        if (z2 >= level) config = config + 2
        if (z3 >= level) config = config + 4
        if (z4 >= level) config = config + 8
    end subroutine calculate_marching_squares_config

    subroutine get_contour_lines(config, x1, y1, x2, y2, x3, y3, x4, y4, &
                                 z1, z2, z3, z4, level, line_points, num_lines)
        !! Extract contour lines from a cell using marching squares
        integer, intent(in) :: config
        real(wp), intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp), intent(in) :: z1, z2, z3, z4, level
        real(wp), intent(out) :: line_points(8)
        integer, intent(out) :: num_lines

        real(wp) :: xa, ya, xb, yb, xc, yc, xd, yd

        call interpolate_edge_crossings(x1, y1, x2, y2, x3, y3, x4, y4, &
                                        z1, z2, z3, z4, level, xa, ya, xb, yb, xc, &
                                        yc, xd, yd)
        call apply_marching_squares_lookup(config, xa, ya, xb, yb, xc, yc, xd, yd, &
                                           line_points, num_lines)
    end subroutine get_contour_lines

    subroutine interpolate_edge_crossings(x1, y1, x2, y2, x3, y3, x4, y4, &
                                          z1, z2, z3, z4, level, xa, ya, xb, yb, xc, &
                                          yc, xd, yd)
        !! Interpolate the positions where contour crosses cell edges
        real(wp), intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp), intent(in) :: z1, z2, z3, z4, level
        real(wp), intent(out) :: xa, ya, xb, yb, xc, yc, xd, yd

        real(wp) :: t

        ! Edge 1-2 (bottom)
        if (abs(z2 - z1) > epsilon(1.0_wp)) then
            t = (level - z1)/(z2 - z1)
            xa = x1 + t*(x2 - x1)
            ya = y1 + t*(y2 - y1)
        else
            xa = 0.5_wp*(x1 + x2)
            ya = 0.5_wp*(y1 + y2)
        end if

        ! Edge 2-3 (right)
        if (abs(z3 - z2) > epsilon(1.0_wp)) then
            t = (level - z2)/(z3 - z2)
            xb = x2 + t*(x3 - x2)
            yb = y2 + t*(y3 - y2)
        else
            xb = 0.5_wp*(x2 + x3)
            yb = 0.5_wp*(y2 + y3)
        end if

        ! Edge 3-4 (top)
        if (abs(z4 - z3) > epsilon(1.0_wp)) then
            t = (level - z3)/(z4 - z3)
            xc = x3 + t*(x4 - x3)
            yc = y3 + t*(y4 - y3)
        else
            xc = 0.5_wp*(x3 + x4)
            yc = 0.5_wp*(y3 + y4)
        end if

        ! Edge 4-1 (left)
        if (abs(z1 - z4) > epsilon(1.0_wp)) then
            t = (level - z4)/(z1 - z4)
            xd = x4 + t*(x1 - x4)
            yd = y4 + t*(y1 - y4)
        else
            xd = 0.5_wp*(x4 + x1)
            yd = 0.5_wp*(y4 + y1)
        end if
    end subroutine interpolate_edge_crossings

    subroutine apply_marching_squares_lookup(config, xa, ya, xb, yb, xc, yc, xd, yd, &
                                             line_points, num_lines)
        !! Apply marching squares lookup table to get line segments
        integer, intent(in) :: config
        real(wp), intent(in) :: xa, ya, xb, yb, xc, yc, xd, yd
        real(wp), intent(out) :: line_points(8)
        integer, intent(out) :: num_lines

        num_lines = 0

        select case (config)
        case (0, 15)
            ! No contour or all inside - no lines
            num_lines = 0
        case (1, 14)
            ! Corner 1 isolated
            line_points(1:4) = [xa, ya, xd, yd]
            num_lines = 1
        case (2, 13)
            ! Corner 2 isolated
            line_points(1:4) = [xa, ya, xb, yb]
            num_lines = 1
        case (3, 12)
            ! Corners 1 and 2
            line_points(1:4) = [xd, yd, xb, yb]
            num_lines = 1
        case (4, 11)
            ! Corner 3 isolated
            line_points(1:4) = [xb, yb, xc, yc]
            num_lines = 1
        case (5)
            ! Corners 1 and 3 - saddle point (choose connection)
            line_points(1:8) = [xa, ya, xd, yd, xb, yb, xc, yc]
            num_lines = 2
        case (10)
            ! Corners 2 and 4 - saddle point (choose connection)
            line_points(1:8) = [xa, ya, xb, yb, xc, yc, xd, yd]
            num_lines = 2
        case (6, 9)
            ! Corners 2 and 3
            line_points(1:4) = [xa, ya, xc, yc]
            num_lines = 1
        case (7, 8)
            ! Corners 1, 2 and 3
            line_points(1:4) = [xd, yd, xc, yc]
            num_lines = 1
        end select
    end subroutine apply_marching_squares_lookup

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

end module fortplot_contour_algorithms
