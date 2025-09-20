module fortplot_3d_axes
    !! 3D axes rendering module for projecting 3D axis frames to 2D
    !!
    !! Provides clean, efficient routines for rendering 3D coordinate frames
    !! with proper tick marks and labels aligned to visible axis segments.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_tick_calculation, only: find_nice_tick_locations, &
                                       format_tick_value_consistent, &
                                       determine_decimal_places_from_step
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    implicit none
    
    private
    public :: draw_3d_axes
    
    ! Constants for 3D visualization - percentage of axis length for true consistency
    integer, parameter :: MAX_TICKS_PER_AXIS = 10
    ! Constants for visually consistent tick appearance (percentages of rendered axis length)
    real(wp), parameter :: VISUAL_TICK_PERCENT = 0.04_wp      ! Preferred tick length as 4% of rendered axis length
    real(wp), parameter :: VISUAL_PADDING_PERCENT = 0.06_wp   ! Preferred label padding as 6% of rendered axis length
    real(wp), parameter :: VISUAL_Z_EXTRA_PERCENT = 0.03_wp   ! Preferred extra Z-axis spacing as 3% of rendered axis length
    ! Hard clamps to avoid extremes (fractions of edge length)
    real(wp), parameter :: VISUAL_TICK_MIN = 0.01_wp
    real(wp), parameter :: VISUAL_TICK_MAX = 0.06_wp
    real(wp), parameter :: VISUAL_PADDING_MIN = 0.03_wp
    real(wp), parameter :: VISUAL_PADDING_MAX = 0.12_wp
    real(wp), parameter :: EPSILON = 1.0e-12_wp            ! Numerical epsilon for divisions
    
    ! Axis identification
    integer, parameter :: X_AXIS = 1, Y_AXIS = 2, Z_AXIS = 3
    
    ! Corner indices for readability
    integer, parameter :: &
        CORNER_MIN_MIN_MIN = 1, &  ! (x_min, y_min, z_min)
        CORNER_MAX_MIN_MIN = 2, &  ! (x_max, y_min, z_min) 
        CORNER_MAX_MAX_MIN = 3, &  ! (x_max, y_max, z_min)
        CORNER_MIN_MAX_MIN = 4, &  ! (x_min, y_max, z_min)
        CORNER_MIN_MIN_MAX = 5, &  ! (x_min, y_min, z_max)
        CORNER_MAX_MIN_MAX = 6, &  ! (x_max, y_min, z_max)
        CORNER_MAX_MAX_MAX = 7, &  ! (x_max, y_max, z_max)
        CORNER_MIN_MAX_MAX = 8     ! (x_min, y_max, z_max)

contains

    subroutine draw_3d_axes(ctx, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw complete 3D axes frame with ticks and labels
        !! 
        !! This is the main entry point that handles all 3D axis rendering:
        !! - Projects 3D bounding box to 2D coordinates
        !! - Draws visible axis segments 
        !! - Places tick marks and labels at appropriate positions
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        real(wp) :: corners_3d(3,8), corners_2d(2,8)
        real(wp) :: azim, elev, dist
        
        ! Validate input ranges
        if (x_max <= x_min .or. y_max <= y_min .or. z_max <= z_min) return
        
        ! Set up 3D projection
        call get_default_view_angles(azim, elev, dist)
        call create_unit_cube(corners_3d)
        call project_to_2d(corners_3d, azim, elev, dist, corners_2d)
        call scale_to_data_range(corners_2d, x_min, x_max, y_min, y_max)
        
        ! Draw the three visible axis segments
        call draw_axis_lines(ctx, corners_2d)
        
        ! Draw ticks and labels on each axis
        call draw_all_axis_ticks(ctx, corners_2d, x_min, x_max, y_min, y_max, z_min, z_max)
    end subroutine draw_3d_axes

    subroutine create_unit_cube(corners_3d)
        !! Create unit cube vertices in normalized [0,1]³ space
        real(wp), intent(out) :: corners_3d(3,8)
        
        ! Define all 8 corners of unit cube systematically
        corners_3d(:,CORNER_MIN_MIN_MIN) = [0.0_wp, 0.0_wp, 0.0_wp]
        corners_3d(:,CORNER_MAX_MIN_MIN) = [1.0_wp, 0.0_wp, 0.0_wp]
        corners_3d(:,CORNER_MAX_MAX_MIN) = [1.0_wp, 1.0_wp, 0.0_wp]
        corners_3d(:,CORNER_MIN_MAX_MIN) = [0.0_wp, 1.0_wp, 0.0_wp]
        corners_3d(:,CORNER_MIN_MIN_MAX) = [0.0_wp, 0.0_wp, 1.0_wp]
        corners_3d(:,CORNER_MAX_MIN_MAX) = [1.0_wp, 0.0_wp, 1.0_wp]
        corners_3d(:,CORNER_MAX_MAX_MAX) = [1.0_wp, 1.0_wp, 1.0_wp]
        corners_3d(:,CORNER_MIN_MAX_MAX) = [0.0_wp, 1.0_wp, 1.0_wp]
    end subroutine create_unit_cube

    subroutine project_to_2d(corners_3d, azim, elev, dist, corners_2d)
        !! Project 3D corners to 2D using standard viewing transformation
        real(wp), intent(in) :: corners_3d(3,8), azim, elev, dist
        real(wp), intent(out) :: corners_2d(2,8)
        
        real(wp) :: x3d(8), y3d(8), z3d(8), x2d(8), y2d(8)
        
        ! Extract coordinates for projection
        x3d = corners_3d(1,:)
        y3d = corners_3d(2,:)
        z3d = corners_3d(3,:)
        
        call project_3d_to_2d(x3d, y3d, z3d, azim, elev, dist, x2d, y2d)
        
        corners_2d(1,:) = x2d
        corners_2d(2,:) = y2d
    end subroutine project_to_2d

    subroutine scale_to_data_range(corners_2d, x_min, x_max, y_min, y_max)
        !! Scale projected coordinates to actual data ranges
        real(wp), intent(inout) :: corners_2d(2,8)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        real(wp) :: proj_bounds(4), data_ranges(2)
        integer :: i
        
        ! Find projection bounds
        proj_bounds(1) = minval(corners_2d(1,:))  ! proj_x_min
        proj_bounds(2) = maxval(corners_2d(1,:))  ! proj_x_max
        proj_bounds(3) = minval(corners_2d(2,:))  ! proj_y_min
        proj_bounds(4) = maxval(corners_2d(2,:))  ! proj_y_max
        
        ! Calculate scaling factors
        data_ranges(1) = max(EPSILON, x_max - x_min)
        data_ranges(2) = max(EPSILON, y_max - y_min)
        
        ! Scale all corners
        do i = 1, 8
            corners_2d(1,i) = x_min + (corners_2d(1,i) - proj_bounds(1)) / &
                              max(EPSILON, proj_bounds(2) - proj_bounds(1)) * data_ranges(1)
            corners_2d(2,i) = y_min + (corners_2d(2,i) - proj_bounds(3)) / &
                              max(EPSILON, proj_bounds(4) - proj_bounds(3)) * data_ranges(2)
        end do
    end subroutine scale_to_data_range

    subroutine draw_axis_lines(ctx, corners_2d)
        !! Draw the three visible axis lines forming the 3D coordinate frame
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2,8)
        
        ! X-axis: front bottom edge
        call ctx%line(corners_2d(1,CORNER_MIN_MIN_MIN), corners_2d(2,CORNER_MIN_MIN_MIN), &
                      corners_2d(1,CORNER_MAX_MIN_MIN), corners_2d(2,CORNER_MAX_MIN_MIN))
        
        ! Y-axis: front right edge  
        call ctx%line(corners_2d(1,CORNER_MAX_MIN_MIN), corners_2d(2,CORNER_MAX_MIN_MIN), &
                      corners_2d(1,CORNER_MAX_MAX_MIN), corners_2d(2,CORNER_MAX_MAX_MIN))
        
        ! Z-axis: front left vertical
        call ctx%line(corners_2d(1,CORNER_MIN_MIN_MIN), corners_2d(2,CORNER_MIN_MIN_MIN), &
                      corners_2d(1,CORNER_MIN_MIN_MAX), corners_2d(2,CORNER_MIN_MIN_MAX))
    end subroutine draw_axis_lines

    subroutine draw_all_axis_ticks(ctx, corners_2d, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw ticks and labels for all three axes
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2,8)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        ! Draw each axis independently using the same pattern
        call draw_single_axis_ticks(ctx, corners_2d, X_AXIS, x_min, x_max, x_min, x_max, y_min, y_max, z_min, z_max)
        call draw_single_axis_ticks(ctx, corners_2d, Y_AXIS, y_min, y_max, x_min, x_max, y_min, y_max, z_min, z_max)  
        call draw_single_axis_ticks(ctx, corners_2d, Z_AXIS, z_min, z_max, x_min, x_max, y_min, y_max, z_min, z_max)
    end subroutine draw_all_axis_ticks

    subroutine draw_single_axis_ticks(ctx, corners_2d, axis_id, axis_min, axis_max, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw ticks and labels for a single axis
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2,8)
        integer, intent(in) :: axis_id
        real(wp), intent(in) :: axis_min, axis_max, x_min, x_max, y_min, y_max, z_min, z_max
        
        real(wp) :: tick_values(MAX_TICKS_PER_AXIS), step_size
        real(wp) :: nice_min, nice_max
        integer :: n_ticks, decimals, corner1, corner2
        
        ! Get nice tick locations
        call find_nice_tick_locations(axis_min, axis_max, 5, nice_min, nice_max, &
                                     step_size, tick_values, n_ticks)
        decimals = determine_decimal_places_from_step(step_size)
        
        ! Determine corner indices for this axis
        select case (axis_id)
        case (X_AXIS)
            corner1 = CORNER_MIN_MIN_MIN; corner2 = CORNER_MAX_MIN_MIN
        case (Y_AXIS) 
            corner1 = CORNER_MAX_MIN_MIN; corner2 = CORNER_MAX_MAX_MIN
        case (Z_AXIS)
            corner1 = CORNER_MIN_MIN_MIN; corner2 = CORNER_MIN_MIN_MAX
        end select
        
        call draw_ticks_on_edge(ctx, corners_2d, corner1, corner2, tick_values, n_ticks, &
                               axis_min, axis_max, x_min, x_max, y_min, y_max, z_min, z_max, decimals, axis_id)
    end subroutine draw_single_axis_ticks

    subroutine draw_ticks_on_edge(ctx, corners_2d, corner1, corner2, tick_values, n_ticks, &
                                 axis_min, axis_max, x_min, x_max, y_min, y_max, z_min, z_max, decimals, axis_id)
        !! Draw tick marks and labels along a specific edge with visually consistent lengths
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2,8)
        integer, intent(in) :: corner1, corner2, n_ticks, decimals, axis_id
        real(wp), intent(in) :: tick_values(:), axis_min, axis_max
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max

        real(wp) :: tick_pos(2), tick_end(2), label_pos(2)
        real(wp) :: range_factor
        real(wp) :: edge_vec(2), edge_len, normal_vec(2), edge_mid(2), plot_center(2)
        real(wp) :: tick_length_screen, padding_screen, extra_screen
        character(len=32) :: label
        integer :: i
    ! Pixel/back-end scale and temporary deltas (declare here per Fortran rules)
    real(wp) :: width_scale, height_scale, canvas_w_px, canvas_h_px
    real(wp) :: tick_px, pad_px, extra_px
    real(wp) :: dx, dx_pad, dy, dy_pad

        ! Compute edge direction and its rendered length (in current 2D drawing coords)
        edge_vec(1) = corners_2d(1,corner2) - corners_2d(1,corner1)
        edge_vec(2) = corners_2d(2,corner2) - corners_2d(2,corner1)
        edge_len    = sqrt(edge_vec(1)**2 + edge_vec(2)**2)
        if (edge_len <= EPSILON) return

        ! Unit outward normal (choose the one pointing away from the box center)
        normal_vec = [ -edge_vec(2)/edge_len, edge_vec(1)/edge_len ]
        edge_mid   = 0.5_wp * [ corners_2d(1,corner1)+corners_2d(1,corner2), &
                                 corners_2d(2,corner1)+corners_2d(2,corner2) ]
        plot_center = [ sum(corners_2d(1,:))/8.0_wp, sum(corners_2d(2,:))/8.0_wp ]
        if ( (normal_vec(1)*(edge_mid(1)-plot_center(1)) + normal_vec(2)*(edge_mid(2)-plot_center(2))) < 0.0_wp ) then
            normal_vec = -normal_vec
        end if

    ! Compute pixel dimensions of the whole plot area via backend scales
    width_scale  = ctx%get_width_scale()
    height_scale = ctx%get_height_scale()
    canvas_w_px  = width_scale  * (x_max - x_min)    ! approx canvas width in pixels
    canvas_h_px  = height_scale * (y_max - y_min)    ! approx canvas height in pixels

    ! Desired tick length in pixels (fraction of smaller canvas dimension), clamped
    tick_px = max(4.0_wp, min(12.0_wp, VISUAL_TICK_PERCENT * min(canvas_w_px, canvas_h_px)))
    pad_px  = max(6.0_wp, min(24.0_wp, VISUAL_PADDING_PERCENT * min(canvas_w_px, canvas_h_px)))
    extra_px = merge(max(0.0_wp, VISUAL_Z_EXTRA_PERCENT) * min(canvas_w_px, canvas_h_px), 0.0_wp, axis_id == Z_AXIS)
        
        do i = 1, n_ticks
            ! Skip ticks outside axis range
            if (tick_values(i) < axis_min .or. tick_values(i) > axis_max) cycle
            
            ! Interpolate position along edge
            range_factor = (tick_values(i) - axis_min) / max(EPSILON, axis_max - axis_min)
            tick_pos(1) = corners_2d(1,corner1) + range_factor * (corners_2d(1,corner2) - corners_2d(1,corner1))
            tick_pos(2) = corners_2d(2,corner1) + range_factor * (corners_2d(2,corner2) - corners_2d(2,corner1))
            
            ! Convert pixel lengths to data-space deltas and place axis-aligned ticks
            if (axis_id == Z_AXIS) then
                ! horizontal ticks: convert pixel -> data-x using width_scale
                dx = sign(1.0_wp, normal_vec(1)) * (tick_px / max(EPSILON, width_scale))
                dx_pad = sign(1.0_wp, normal_vec(1)) * ((pad_px + extra_px) / max(EPSILON, width_scale))
                tick_end(1) = tick_pos(1) + dx
                tick_end(2) = tick_pos(2)
                label_pos(1) = tick_end(1) + dx_pad
                label_pos(2) = tick_pos(2)
            else
                ! vertical ticks for X/Y: convert pixel -> data-y using height_scale
                dy = sign(1.0_wp, normal_vec(2)) * (tick_px / max(EPSILON, height_scale))
                dy_pad = sign(1.0_wp, normal_vec(2)) * ((pad_px + extra_px) / max(EPSILON, height_scale))
                tick_end(1) = tick_pos(1)
                tick_end(2) = tick_pos(2) + dy
                label_pos(1) = tick_pos(1)
                label_pos(2) = tick_end(2) + dy_pad
            end if
            
            ! Draw tick mark
            call ctx%line(tick_pos(1), tick_pos(2), tick_end(1), tick_end(2))
            
            ! Draw label
            label = format_tick_value_consistent(tick_values(i), decimals)
            call ctx%text(label_pos(1), label_pos(2), trim(adjustl(label)))
        end do
    end subroutine draw_ticks_on_edge

    ! ...existing code...

end module fortplot_3d_axes
