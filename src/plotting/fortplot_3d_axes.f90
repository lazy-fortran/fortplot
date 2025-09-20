module fortplot_3d_axes
    !! 3D axes rendering module for projecting 3D axis frames to 2D
    !!
    !! Provides clean, efficient routines for rendering 3D coordinate frames
    !! with proper tick marks and labels aligned to visible axis segments.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_tick_calculation, only: find_nice_tick_locations, &
                                       format_tick_value_consistent, &
                                       determine_decimal_places_from_step
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    implicit none
    
    private
    public :: draw_3d_axes
    
    ! Constants for 3D visualization
    integer, parameter :: MAX_TICKS_PER_AXIS = 10
    real(wp), parameter :: TICK_LENGTH_RATIO = 0.02_wp    ! Tick length as fraction of range
    real(wp), parameter :: LABEL_PADDING_RATIO = 0.02_wp ! Label padding as fraction of range
    real(wp), parameter :: LABEL_OFFSET_Y = 0.25_wp      ! Y offset for label positioning
    real(wp), parameter :: LABEL_OFFSET_X = 0.5_wp       ! X offset for label centering
    real(wp), parameter :: EPSILON = 1.0e-12_wp          ! Numerical epsilon for divisions
    
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

    subroutine create_3d_axis_corners(x_min, x_max, y_min, y_max, z_min, z_max, corners_3d)
        !! Generate 8 corners of 3D bounding box
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        real(wp), intent(out) :: corners_3d(3,8)
        
        ! Corner 1: (x_min, y_min, z_min)
        corners_3d(:,1) = [x_min, y_min, z_min]
        ! Corner 2: (x_max, y_min, z_min)
        corners_3d(:,2) = [x_max, y_min, z_min]
        ! Corner 3: (x_max, y_max, z_min)
        corners_3d(:,3) = [x_max, y_max, z_min]
        ! Corner 4: (x_min, y_max, z_min)
        corners_3d(:,4) = [x_min, y_max, z_min]
        ! Corner 5: (x_min, y_min, z_max)
        corners_3d(:,5) = [x_min, y_min, z_max]
        ! Corner 6: (x_max, y_min, z_max)
        corners_3d(:,6) = [x_max, y_min, z_max]
        ! Corner 7: (x_max, y_max, z_max)
        corners_3d(:,7) = [x_max, y_max, z_max]
        ! Corner 8: (x_min, y_max, z_max)
        corners_3d(:,8) = [x_min, y_max, z_max]
    end subroutine create_3d_axis_corners

    subroutine project_3d_corners_to_2d(corners_3d, azim, elev, dist, corners_2d)
        !! Project 8 3D corners to 2D screen coordinates
        real(wp), intent(in) :: corners_3d(3,8)
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(out) :: corners_2d(2,8)
        
        real(wp) :: x3d(8), y3d(8), z3d(8), x2d(8), y2d(8)
        
        x3d = corners_3d(1,:)
        y3d = corners_3d(2,:)
        z3d = corners_3d(3,:)
        
        call project_3d_to_2d(x3d, y3d, z3d, azim, elev, dist, x2d, y2d)
        
        corners_2d(1,:) = x2d
        corners_2d(2,:) = y2d
    end subroutine project_3d_corners_to_2d

    subroutine create_3d_axis_lines(x_min, x_max, y_min, y_max, z_min, z_max, axis_lines_3d)
        !! Generate 3D axis lines (X, Y, Z axes)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        real(wp), intent(out) :: axis_lines_3d(3,6)
        
        ! X-axis line: from (x_min, y_min, z_min) to (x_max, y_min, z_min)
        axis_lines_3d(:,1) = [x_min, y_min, z_min]
        axis_lines_3d(:,2) = [x_max, y_min, z_min]
        
        ! Y-axis line: from (x_min, y_min, z_min) to (x_min, y_max, z_min)
        axis_lines_3d(:,3) = [x_min, y_min, z_min]
        axis_lines_3d(:,4) = [x_min, y_max, z_min]
        
        ! Z-axis line: from (x_min, y_min, z_min) to (x_min, y_min, z_max)
        axis_lines_3d(:,5) = [x_min, y_min, z_min]
        axis_lines_3d(:,6) = [x_min, y_min, z_max]
    end subroutine create_3d_axis_lines

    subroutine project_3d_axis_lines(axis_lines_3d, azim, elev, dist, axis_lines_2d)
        !! Project 3D axis lines to 2D screen coordinates
        real(wp), intent(in) :: axis_lines_3d(3,6)
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(out) :: axis_lines_2d(2,6)
        
        real(wp) :: x3d(6), y3d(6), z3d(6), x2d(6), y2d(6)
        
        x3d = axis_lines_3d(1,:)
        y3d = axis_lines_3d(2,:)
        z3d = axis_lines_3d(3,:)
        
        call project_3d_to_2d(x3d, y3d, z3d, azim, elev, dist, x2d, y2d)
        
        axis_lines_2d(1,:) = x2d
        axis_lines_2d(2,:) = y2d
    end subroutine project_3d_axis_lines

    subroutine create_3d_tick_positions(x_min, x_max, y_min, y_max, z_min, z_max, &
                                       tick_3d, n_ticks)
        !! Generate 3D tick mark positions on axes
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        real(wp), intent(out) :: tick_3d(3,10)
        integer, intent(out) :: n_ticks
        
        real(wp) :: x_step, y_step, z_step
        integer :: i
        
        n_ticks = 0
        
        ! Create 3 ticks on X-axis
        x_step = (x_max - x_min) / 2.0_wp
        do i = 0, 2
            n_ticks = n_ticks + 1
            tick_3d(:,n_ticks) = [x_min + i * x_step, y_min, z_min]
        end do
        
        ! Create 3 ticks on Y-axis
        y_step = (y_max - y_min) / 2.0_wp
        do i = 0, 2
            n_ticks = n_ticks + 1
            tick_3d(:,n_ticks) = [x_min, y_min + i * y_step, z_min]
        end do
        
        ! Create 3 ticks on Z-axis
        z_step = (z_max - z_min) / 2.0_wp
        do i = 0, 2
            n_ticks = n_ticks + 1
            tick_3d(:,n_ticks) = [x_min, y_min, z_min + i * z_step]
        end do
    end subroutine create_3d_tick_positions

    subroutine draw_3d_axes(ctx, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw 3D axes frame using the generic plotting context
        !! The backend maps data -> device coordinates via ctx methods
        use fortplot_context, only: plot_context
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        real(wp) :: azim, elev, dist
        real(wp) :: corners_3d(3,8), corners_2d(2,8)
        real(wp) :: x1, y1, x2, y2
        real(wp) :: x_data, y_data, x_screen, y_screen
        
        ! Get viewing angles
        call get_default_view_angles(azim, elev, dist)
        
        ! Create 3D corners in normalized space [0,1]
        ! This matches the normalization used for data rendering
        call create_3d_axis_corners(0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, corners_3d)
        
        ! Project to 2D (still in data space)
        call project_3d_corners_to_2d(corners_3d, azim, elev, dist, corners_2d)
        
        ! Map projected coordinates into current data ranges; backend maps data->device
        call transform_corners_to_data(corners_2d, x_min, x_max, y_min, y_max)
        
        ! Draw axes matplotlib/MATLAB style - forming a corner shape
        ! Not all axes meet at the same point for proper 3D visualization
        
        ! X-axis: corner 1 to corner 2 (bottom front edge)
        x1 = corners_2d(1, 1)
        y1 = corners_2d(2, 1)
        x2 = corners_2d(1, 2)
        y2 = corners_2d(2, 2)
        call ctx%line(x1, y1, x2, y2)
        
        ! Z-axis: corner 1 to corner 5 (front left vertical)
        x1 = corners_2d(1, 1)
        y1 = corners_2d(2, 1)
        x2 = corners_2d(1, 5)
        y2 = corners_2d(2, 5)
        call ctx%line(x1, y1, x2, y2)
        
        ! Y-axis: corner 2 to corner 3 (front right edge, parallel to Y)
        ! This creates the offset Y-axis that forms the 3D corner
        x1 = corners_2d(1, 2)
        y1 = corners_2d(2, 2)
        x2 = corners_2d(1, 3)
        y2 = corners_2d(2, 3)
        call ctx%line(x1, y1, x2, y2)
        
        ! Draw ticks and labels on the three axes
        call draw_3d_axis_ticks_and_labels(ctx, corners_2d, x_min, x_max, y_min, y_max, z_min, z_max)
    end subroutine draw_3d_axes

    subroutine transform_corners_to_data(corners_2d, x_min, x_max, y_min, y_max)
        !! Transform projected corners from projection space to current data ranges
        real(wp), intent(inout) :: corners_2d(:,:)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        real(wp) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        real(wp) :: x_range_proj, y_range_proj
        real(wp) :: x_range_data, y_range_data
        integer :: i
        
        proj_x_min = minval(corners_2d(1,:))
        proj_x_max = maxval(corners_2d(1,:))
        proj_y_min = minval(corners_2d(2,:))
        proj_y_max = maxval(corners_2d(2,:))
        
        x_range_proj = max(1.0e-12_wp, proj_x_max - proj_x_min)
        y_range_proj = max(1.0e-12_wp, proj_y_max - proj_y_min)
        
        x_range_data = max(1.0e-12_wp, x_max - x_min)
        y_range_data = max(1.0e-12_wp, y_max - y_min)
        
        do i = 1, size(corners_2d, 2)
            corners_2d(1,i) = x_min + (corners_2d(1,i) - proj_x_min) / x_range_proj * x_range_data
            corners_2d(2,i) = y_min + (corners_2d(2,i) - proj_y_min) / y_range_proj * y_range_data
        end do
    end subroutine transform_corners_to_data
    
    subroutine draw_3d_axis_ticks_and_labels(ctx, corners_2d, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw tick marks and labels on the visible 3D axes
        use fortplot_context, only: plot_context
        use fortplot_text, only: render_text_to_image, calculate_text_width
        use fortplot_tick_calculation, only: find_nice_tick_locations, format_tick_value_consistent, &
                                           determine_decimal_places_from_step
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2,8)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        real(wp) :: x_pos, y_pos
        character(len=32) :: label
        integer :: i, n_ticks
        real(wp) :: value, step
        real(wp) :: x_range, y_range
        real(wp) :: tick_len_y, tick_len_x
        real(wp) :: pad_x, pad_y
        real(wp) :: tick_y_end, label_y_pos
        real(wp) :: y_span
        integer :: decimals_x, decimals_y, decimals_z
        
        ! Arrays for tick positions and values
        real(wp) :: x_tick_vals(10), y_tick_vals(10), z_tick_vals(10)
        real(wp) :: nice_x_min, nice_x_max, nice_y_min, nice_y_max, nice_z_min, nice_z_max
        real(wp) :: x_step, y_step, z_step
        integer :: n_x_ticks, n_y_ticks, n_z_ticks
        
        ! Use fractions of the current data ranges for tick lengths and padding
        x_range = max(1.0e-12_wp, abs(x_max - x_min))
        y_span = y_max - y_min
        y_range = max(1.0e-12_wp, abs(y_span))
        tick_len_y = 0.02_wp * y_range   ! vertical tick length (in data units)
        tick_len_x = 0.02_wp * x_range   ! horizontal tick length (in data units)
        pad_x = 0.02_wp * x_range        ! horizontal text padding (data units)
        pad_y = 0.02_wp * y_range        ! vertical text padding (data units)
        n_ticks = 5  ! Number of ticks per axis

        ! Find nice tick locations for each axis using the same algorithm as 2D plots
        call find_nice_tick_locations(x_min, x_max, n_ticks, &
                                     nice_x_min, nice_x_max, x_step, &
                                     x_tick_vals, n_x_ticks)
        
        call find_nice_tick_locations(y_min, y_max, n_ticks, &
                                     nice_y_min, nice_y_max, y_step, &
                                     y_tick_vals, n_y_ticks)
        
        call find_nice_tick_locations(z_min, z_max, n_ticks, &
                                     nice_z_min, nice_z_max, z_step, &
                                     z_tick_vals, n_z_ticks)
        
        ! Determine decimal places for consistent formatting
        decimals_x = determine_decimal_places_from_step(x_step)
        decimals_y = determine_decimal_places_from_step(y_step)
        decimals_z = determine_decimal_places_from_step(z_step)
        
        ! X-axis ticks and labels (edge from corner 1 to corner 2)
        ! Position ticks proportionally along the edge based on their data values
        do i = 1, n_x_ticks
            value = x_tick_vals(i)
            ! Only draw ticks that fall within the axis range
            if (value < x_min .or. value > x_max) cycle
            
            ! Map value to position along edge from corner 1 to corner 2
            ! Using linear interpolation: pos = corner1 + (value - x_min) / (x_max - x_min) * (corner2 - corner1)
            x_pos = corners_2d(1,1) + (value - x_min) / max(1.0e-12_wp, x_max - x_min) * (corners_2d(1,2) - corners_2d(1,1))
            y_pos = corners_2d(2,1) + (value - x_min) / max(1.0e-12_wp, x_max - x_min) * (corners_2d(2,2) - corners_2d(2,1))

            ! Determine downward direction in device space based on Y span sign
            if (y_span >= 0.0_wp) then
                tick_y_end = y_pos - tick_len_y
                label_y_pos = tick_y_end - pad_y
            else
                tick_y_end = y_pos + tick_len_y
                label_y_pos = tick_y_end + pad_y
            end if

            ! Draw tick mark pointing down in device space
            call ctx%line(x_pos, y_pos, x_pos, tick_y_end)

            ! Draw label using consistent decimal places across the axis
            label = format_tick_value_consistent(value, decimals_x)
            call render_text_to_ctx(ctx, x_pos - 0.5_wp*pad_x, label_y_pos, trim(adjustl(label)))
        end do
        
        ! Y-axis ticks and labels (edge from corner 2 to corner 3)
        ! Position ticks proportionally along the edge based on their data values
        ! This matches the actual Y-axis line drawn above
        do i = 1, n_y_ticks
            value = y_tick_vals(i)
            ! Only draw ticks that fall within the axis range
            if (value < y_min .or. value > y_max) cycle
            
            ! Map value to position along edge from corner 2 to corner 3
            x_pos = corners_2d(1,2) + (value - y_min) / max(1.0e-12_wp, y_max - y_min) * (corners_2d(1,3) - corners_2d(1,2))
            y_pos = corners_2d(2,2) + (value - y_min) / max(1.0e-12_wp, y_max - y_min) * (corners_2d(2,3) - corners_2d(2,2))
            
            ! Draw tick mark pointing left (in data units)
            call ctx%line(x_pos, y_pos, x_pos - tick_len_x, y_pos)
            
            ! Draw label using consistent decimal places across the axis
            label = format_tick_value_consistent(value, decimals_y)
            call render_text_to_ctx(ctx, x_pos - tick_len_x - pad_x, y_pos + 0.25_wp*pad_y, trim(adjustl(label)))
        end do
        
        ! Z-axis ticks and labels (edge from corner 1 to corner 5)
        ! Position ticks proportionally along the edge based on their data values
        do i = 1, n_z_ticks
            value = z_tick_vals(i)
            ! Only draw ticks that fall within the axis range
            if (value < z_min .or. value > z_max) cycle
            
            ! Map value to position along edge from corner 1 to corner 5
            x_pos = corners_2d(1,1) + (value - z_min) / max(1.0e-12_wp, z_max - z_min) * (corners_2d(1,5) - corners_2d(1,1))
            y_pos = corners_2d(2,1) + (value - z_min) / max(1.0e-12_wp, z_max - z_min) * (corners_2d(2,5) - corners_2d(2,1))
            
            ! Draw tick mark pointing left (in data units)
            call ctx%line(x_pos, y_pos, x_pos - tick_len_x, y_pos)
            
            ! Draw label using consistent decimal places across the axis
            label = format_tick_value_consistent(value, decimals_z)
            call render_text_to_ctx(ctx, x_pos - tick_len_x - pad_x, y_pos + 0.25_wp*pad_y, trim(adjustl(label)))
        end do
    end subroutine draw_3d_axis_ticks_and_labels
    
    subroutine render_text_to_ctx(ctx, x, y, text)
        !! Helper to render text using the active backend context
        use fortplot_context, only: plot_context
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text

        call ctx%text(x, y, text)
    end subroutine render_text_to_ctx

end module fortplot_3d_axes
