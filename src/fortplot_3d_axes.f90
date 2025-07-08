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
        !! Draw 3D axes frame to raster backend
        use fortplot_context, only: plot_context
        class(plot_context), intent(inout) :: ctx
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        
        real(wp) :: azim, elev, dist
        real(wp) :: axis_lines_3d(3,6), axis_lines_2d(2,6)
        real(wp) :: corners_3d(3,8), corners_2d(2,8)
        real(wp) :: x1, y1, x2, y2
        integer :: i
        
        call get_default_view_angles(azim, elev, dist)
        call create_3d_axis_lines(x_min, x_max, y_min, y_max, z_min, z_max, axis_lines_3d)
        call project_3d_axis_lines(axis_lines_3d, azim, elev, dist, axis_lines_2d)
        
        ! Scale projected coordinates to plot area
        call scale_2d_to_plot_area(axis_lines_2d, ctx, x_min, x_max, y_min, y_max)
        
        ! Draw three axis lines
        do i = 1, 3
            x1 = axis_lines_2d(1, 2*i-1)
            y1 = axis_lines_2d(2, 2*i-1)
            x2 = axis_lines_2d(1, 2*i)
            y2 = axis_lines_2d(2, 2*i)
            call ctx%line(x1, y1, x2, y2)
        end do
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

end module fortplot_3d_axes