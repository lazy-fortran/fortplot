module fortplot_contour_algorithms
    !! Contour generation algorithms using marching squares
    !! 
    !! This module contains the marching squares implementation and contour
    !! tracing algorithms following the Single Responsibility Principle.
    !! 
    !! SOLID: Single responsibility for contour generation algorithms
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: calculate_marching_squares_config, interpolate_edge_crossings
    public :: apply_marching_squares_lookup

contains

    subroutine calculate_marching_squares_config(z1, z2, z3, z4, level, config)
        !! Calculate marching squares configuration for a cell
        !! Following KISS principle - simple binary configuration
        real(wp), intent(in) :: z1, z2, z3, z4, level
        integer, intent(out) :: config

        config = 0
        if (z1 >= level) config = config + 1
        if (z2 >= level) config = config + 2
        if (z3 >= level) config = config + 4
        if (z4 >= level) config = config + 8
        
    end subroutine calculate_marching_squares_config

    subroutine interpolate_edge_crossings(x1, y1, x2, y2, x3, y3, x4, y4, &
                                         z1, z2, z3, z4, level, xa, ya, xb, yb, xc, yc, xd, yd)
        !! Interpolate where contour level crosses cell edges
        !! Following SRP - handles only edge crossing interpolation
        real(wp), intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp), intent(in) :: z1, z2, z3, z4, level
        real(wp), intent(out) :: xa, ya, xb, yb, xc, yc, xd, yd

        ! Edge 1-2 (bottom)
        if (abs(z2 - z1) > 1e-10_wp) then
            xa = x1 + (level - z1) / (z2 - z1) * (x2 - x1)
            ya = y1 + (level - z1) / (z2 - z1) * (y2 - y1)
        else
            xa = (x1 + x2) * 0.5_wp
            ya = (y1 + y2) * 0.5_wp
        end if

        ! Edge 2-3 (right)
        if (abs(z3 - z2) > 1e-10_wp) then
            xb = x2 + (level - z2) / (z3 - z2) * (x3 - x2)
            yb = y2 + (level - z2) / (z3 - z2) * (y3 - y2)
        else
            xb = (x2 + x3) * 0.5_wp
            yb = (y2 + y3) * 0.5_wp
        end if

        ! Edge 3-4 (top)
        if (abs(z4 - z3) > 1e-10_wp) then
            xc = x3 + (level - z3) / (z4 - z3) * (x4 - x3)
            yc = y3 + (level - z3) / (z4 - z3) * (y4 - y3)
        else
            xc = (x3 + x4) * 0.5_wp
            yc = (y3 + y4) * 0.5_wp
        end if

        ! Edge 4-1 (left)
        if (abs(z1 - z4) > 1e-10_wp) then
            xd = x4 + (level - z4) / (z1 - z4) * (x1 - x4)
            yd = y4 + (level - z4) / (z1 - z4) * (y1 - y4)
        else
            xd = (x4 + x1) * 0.5_wp
            yd = (y4 + y1) * 0.5_wp
        end if
        
    end subroutine interpolate_edge_crossings

    subroutine apply_marching_squares_lookup(config, xa, ya, xb, yb, xc, yc, xd, yd, line_points, num_lines)
        !! Apply marching squares lookup table to get line segments
        !! Following SRP - handles only lookup table application
        integer, intent(in) :: config
        real(wp), intent(in) :: xa, ya, xb, yb, xc, yc, xd, yd
        real(wp), dimension(8), intent(out) :: line_points
        integer, intent(out) :: num_lines

        select case (config)
        case (0, 15)
            num_lines = 0
        case (1, 14)
            line_points(1:4) = [xd, yd, xa, ya]
            num_lines = 1
        case (2, 13)
            line_points(1:4) = [xa, ya, xb, yb]
            num_lines = 1
        case (3, 12)
            line_points(1:4) = [xd, yd, xb, yb]
            num_lines = 1
        case (4, 11)
            line_points(1:4) = [xb, yb, xc, yc]
            num_lines = 1
        case (5)
            line_points(1:4) = [xd, yd, xa, ya]
            line_points(5:8) = [xb, yb, xc, yc]
            num_lines = 2
        case (6, 9)
            line_points(1:4) = [xa, ya, xc, yc]
            num_lines = 1
        case (7, 8)
            line_points(1:4) = [xd, yd, xc, yc]
            num_lines = 1
        case (10)
            line_points(1:4) = [xa, ya, xb, yb]
            line_points(5:8) = [xd, yd, xc, yc]
            num_lines = 2
        case default
            num_lines = 0
        end select
        
    end subroutine apply_marching_squares_lookup

end module fortplot_contour_algorithms