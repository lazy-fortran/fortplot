module fortplot_3d_axes
    !! 3D axes rendering module for projecting 3D axis frames to 2D
    !!
    !! Provides routines for generating 3D bounding boxes, axis lines, 
    !! tick positions and projecting them to 2D coordinates
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    implicit none
    
    private
    public :: create_3d_axis_corners, project_3d_corners_to_2d
    public :: create_3d_axis_lines, project_3d_axis_lines
    public :: create_3d_tick_positions
    public :: draw_3d_axes, transform_corners_to_data
    
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
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2,8)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        real(wp) :: tick_length, x_pos, y_pos, dx, dy
        character(len=32) :: label
        integer :: i, n_ticks
        real(wp) :: value, step
        
        tick_length = 4.0_wp  ! Tick length in pixels
        n_ticks = 5  ! Number of ticks per axis
        
        ! X-axis ticks and labels (edge from corner 1 to corner 2)
        step = (x_max - x_min) / real(n_ticks - 1, wp)
        do i = 1, n_ticks
            value = x_min + real(i-1, wp) * step
            ! Interpolate position along edge
            x_pos = corners_2d(1,1) + (corners_2d(1,2) - corners_2d(1,1)) * real(i-1, wp) / real(n_ticks-1, wp)
            y_pos = corners_2d(2,1) + (corners_2d(2,2) - corners_2d(2,1)) * real(i-1, wp) / real(n_ticks-1, wp)
            
            ! Draw tick mark pointing down
            call ctx%line(x_pos, y_pos, x_pos, y_pos + tick_length)
            
            ! Draw label
            write(label, '(F6.1)') value
            call render_text_to_ctx(ctx, x_pos - 10.0_wp, y_pos + tick_length + 5.0_wp, trim(adjustl(label)))
        end do
        
        ! Y-axis ticks and labels (edge from corner 1 to corner 4)
        step = (y_max - y_min) / real(n_ticks - 1, wp)
        do i = 1, n_ticks
            value = y_min + real(i-1, wp) * step
            x_pos = corners_2d(1,1) + (corners_2d(1,4) - corners_2d(1,1)) * real(i-1, wp) / real(n_ticks-1, wp)
            y_pos = corners_2d(2,1) + (corners_2d(2,4) - corners_2d(2,1)) * real(i-1, wp) / real(n_ticks-1, wp)
            
            ! Draw tick mark pointing left
            call ctx%line(x_pos, y_pos, x_pos - tick_length, y_pos)
            
            ! Draw label
            write(label, '(F6.1)') value
            call render_text_to_ctx(ctx, x_pos - tick_length - 30.0_wp, y_pos + 5.0_wp, trim(adjustl(label)))
        end do
        
        ! Z-axis ticks and labels (edge from corner 1 to corner 5)
        step = (z_max - z_min) / real(n_ticks - 1, wp)
        do i = 1, n_ticks
            value = z_min + real(i-1, wp) * step
            x_pos = corners_2d(1,1) + (corners_2d(1,5) - corners_2d(1,1)) * real(i-1, wp) / real(n_ticks-1, wp)
            y_pos = corners_2d(2,1) + (corners_2d(2,5) - corners_2d(2,1)) * real(i-1, wp) / real(n_ticks-1, wp)
            
            ! Draw tick mark pointing left
            call ctx%line(x_pos, y_pos, x_pos - tick_length, y_pos)
            
            ! Draw label
            write(label, '(F6.1)') value
            call render_text_to_ctx(ctx, x_pos - tick_length - 30.0_wp, y_pos + 5.0_wp, trim(adjustl(label)))
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
