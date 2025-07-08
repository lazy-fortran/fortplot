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
    public :: draw_3d_axes_to_raster
    
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

    subroutine draw_3d_axes_to_raster(ctx, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw 3D axes frame to raster backend - matplotlib style
        use fortplot_context, only: plot_context
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        real(wp) :: azim, elev, dist
        real(wp) :: corners_3d(3,8), corners_2d(2,8)
        real(wp) :: x1, y1, x2, y2
        
        call get_default_view_angles(azim, elev, dist)
        call create_3d_axis_corners(x_min, x_max, y_min, y_max, z_min, z_max, corners_3d)
        call project_3d_corners_to_2d(corners_3d, azim, elev, dist, corners_2d)
        
        ! Scale projected coordinates to plot area
        call scale_2d_to_plot_area(corners_2d, ctx, x_min, x_max, y_min, y_max)
        
        ! Draw the three main axes like matplotlib
        ! For default view (azim=-60, elev=30), matplotlib draws the "back" edges:
        ! X-axis: edge from corner 3 to corner 2 (back of bottom face)
        ! Y-axis: edge from corner 4 to corner 3 (left of bottom face)  
        ! Z-axis: edge from corner 3 to corner 7 (back left vertical)
        ! All three axes meet at corner 3 (x_max, y_max, z_min)
        
        ! X-axis (corner 3 to corner 2)
        x1 = corners_2d(1, 3)
        y1 = corners_2d(2, 3)
        x2 = corners_2d(1, 2)
        y2 = corners_2d(2, 2)
        call ctx%line(x1, y1, x2, y2)
        
        ! Y-axis (corner 4 to corner 3)
        x1 = corners_2d(1, 4)
        y1 = corners_2d(2, 4)
        x2 = corners_2d(1, 3)
        y2 = corners_2d(2, 3)
        call ctx%line(x1, y1, x2, y2)
        
        ! Z-axis (corner 3 to corner 7)
        x1 = corners_2d(1, 3)
        y1 = corners_2d(2, 3)
        x2 = corners_2d(1, 7)
        y2 = corners_2d(2, 7)
        call ctx%line(x1, y1, x2, y2)
        
        ! Draw ticks and labels on the three axes
        call draw_3d_axis_ticks_and_labels(ctx, corners_2d, x_min, x_max, y_min, y_max, z_min, z_max)
    end subroutine draw_3d_axes_to_raster

    subroutine scale_2d_to_plot_area(points_2d, ctx, x_min, x_max, y_min, y_max)
        !! Scale projected 2D coordinates to plot area
        use fortplot_context, only: plot_context
        real(wp), intent(inout) :: points_2d(:,:)
        class(plot_context), intent(in) :: ctx
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        real(wp) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        real(wp) :: scale_x, scale_y
        integer :: i
        
        ! Find bounds of projected coordinates
        proj_x_min = minval(points_2d(1,:))
        proj_x_max = maxval(points_2d(1,:))
        proj_y_min = minval(points_2d(2,:))
        proj_y_max = maxval(points_2d(2,:))
        
        ! Scale to plot area using context bounds
        scale_x = (ctx%x_max - ctx%x_min) / (proj_x_max - proj_x_min)
        scale_y = (ctx%y_max - ctx%y_min) / (proj_y_max - proj_y_min)
        
        do i = 1, size(points_2d, 2)
            points_2d(1,i) = ctx%x_min + (points_2d(1,i) - proj_x_min) * scale_x
            points_2d(2,i) = ctx%y_min + (points_2d(2,i) - proj_y_min) * scale_y
        end do
    end subroutine scale_2d_to_plot_area
    
    subroutine draw_3d_axis_ticks_and_labels(ctx, corners_2d, x_min, x_max, y_min, y_max, z_min, z_max)
        !! Draw tick marks and labels on the visible 3D axes
        use fortplot_context, only: plot_context
        use fortplot_text, only: render_text_to_image, calculate_text_width
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: corners_2d(2,8)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        real(wp) :: tick_length, x_pos, y_pos
        character(len=32) :: label
        integer :: i, n_ticks
        real(wp) :: value, step
        
        tick_length = 5.0_wp  ! Tick length in pixels
        n_ticks = 5  ! Number of ticks per axis
        
        ! X-axis ticks and labels (edge 1: corner 1 to corner 2)
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
        
        ! Y-axis ticks and labels (edge 4: corner 4 to corner 1, but we go 1 to 4)
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
        
        ! Z-axis ticks and labels (edge 9: corner 1 to corner 5)
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
        !! Helper to render text to context (placeholder)
        use fortplot_context, only: plot_context
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        
        ! This is a placeholder - actual implementation would depend on backend
        ! For now, we'll skip text rendering in 3D axes
    end subroutine render_text_to_ctx

end module fortplot_3d_axes