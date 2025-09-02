module fortplot_contour_rendering
    !! Contour plot rendering module
    !! 
    !! This module handles all contour plot rendering operations including
    !! contour level tracing, marching squares algorithm, and contour line drawing.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_colormap
    use fortplot_contour_algorithms
    use fortplot_plot_data
    implicit none
    
    private
    public :: render_contour_plot
    
contains
    
    subroutine render_contour_plot(backend, plot_data, x_min_t, x_max_t, y_min_t, y_max_t, &
                                  xscale, yscale, symlog_threshold, width, height, &
                                  margin_left, margin_right, margin_bottom, margin_top)
        !! Render a contour plot
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        
        real(wp) :: z_min, z_max
        real(wp), dimension(3) :: level_color
        integer :: i, j, nx, ny, nlev
        real(wp) :: level
        
        ! Get data ranges
        z_min = minval(plot_data%z_grid)
        z_max = maxval(plot_data%z_grid)
        
        nx = size(plot_data%x_grid)
        ny = size(plot_data%y_grid)
        
        ! If colored (filled) contours requested, render a filled background
        ! using the backend heatmap fill for a smooth colormap field. This
        ! provides a contourf-like visual while keeping implementation simple
        ! and backend-agnostic. Contour lines are then overlaid (if present).
        if (plot_data%use_color_levels) then
            call backend%fill_heatmap(plot_data%x_grid, plot_data%y_grid, plot_data%z_grid, z_min, z_max)
        end if

        ! Render contour levels (lines)
        if (allocated(plot_data%contour_levels)) then
            nlev = size(plot_data%contour_levels)
            do i = 1, nlev
                level = plot_data%contour_levels(i)
                
                ! Set color based on contour level if using color levels
                if (plot_data%use_color_levels) then
                    call colormap_value_to_color(level, z_min, z_max, &
                                               plot_data%colormap, level_color)
                    call backend%color(level_color(1), level_color(2), level_color(3))
                else
                    call backend%color(plot_data%color(1), plot_data%color(2), plot_data%color(3))
                end if
                
                ! Trace this contour level
                call trace_contour_level(backend, plot_data, level, xscale, yscale, &
                                       symlog_threshold, x_min_t, x_max_t, y_min_t, y_max_t)
            end do
        else
            ! Use default 3 levels
            call render_default_contour_levels(backend, plot_data, z_min, z_max, &
                                             xscale, yscale, symlog_threshold, &
                                             x_min_t, x_max_t, y_min_t, y_max_t)
        end if
        
    end subroutine render_contour_plot

    ! Note: filled polygon rendering to be implemented using
    ! extract_contour_regions() once polygon fill support exists.

    subroutine render_default_contour_levels(backend, plot_data, z_min, z_max, &
                                           xscale, yscale, symlog_threshold, &
                                           x_min_t, x_max_t, y_min_t, y_max_t)
        !! Render default contour levels
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: z_min, z_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        
        real(wp), dimension(3) :: level_color
        real(wp) :: level_values(3)
        integer :: i
        
        level_values = [z_min + 0.2_wp * (z_max - z_min), &
                       z_min + 0.5_wp * (z_max - z_min), &
                       z_min + 0.8_wp * (z_max - z_min)]
        
        do i = 1, 3
            if (plot_data%use_color_levels) then
                call colormap_value_to_color(level_values(i), z_min, z_max, &
                                           plot_data%colormap, level_color)
                call backend%color(level_color(1), level_color(2), level_color(3))
            end if
            
            call trace_contour_level(backend, plot_data, level_values(i), &
                                   xscale, yscale, symlog_threshold, &
                                   x_min_t, x_max_t, y_min_t, y_max_t)
        end do
    end subroutine render_default_contour_levels
    
    subroutine trace_contour_level(backend, plot_data, level, xscale, yscale, &
                                  symlog_threshold, x_min_t, x_max_t, y_min_t, y_max_t)
        !! Trace a single contour level using marching squares
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: level
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        
        integer :: nx, ny, i, j
        
        nx = size(plot_data%x_grid)
        ny = size(plot_data%y_grid)
        
        do i = 1, nx-1
            do j = 1, ny-1
                call process_contour_cell(backend, plot_data, i, j, level, &
                                        xscale, yscale, symlog_threshold)
            end do
        end do
    end subroutine trace_contour_level
    
    subroutine process_contour_cell(backend, plot_data, i, j, level, xscale, yscale, symlog_threshold)
        !! Process a single grid cell for contour extraction
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        integer, intent(in) :: i, j
        real(wp), intent(in) :: level
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        real(wp) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp) :: z1, z2, z3, z4
        integer :: config
        real(wp), dimension(8) :: line_points
        integer :: num_lines
        
        ! Get cell coordinates and values
        x1 = plot_data%x_grid(i)
        y1 = plot_data%y_grid(j)
        x2 = plot_data%x_grid(i+1)
        y2 = plot_data%y_grid(j)
        x3 = plot_data%x_grid(i+1)
        y3 = plot_data%y_grid(j+1)
        x4 = plot_data%x_grid(i)
        y4 = plot_data%y_grid(j+1)
        
        z1 = plot_data%z_grid(i, j)
        z2 = plot_data%z_grid(i+1, j)
        z3 = plot_data%z_grid(i+1, j+1)
        z4 = plot_data%z_grid(i, j+1)
        
        call calculate_marching_squares_config(z1, z2, z3, z4, level, config)
        call get_contour_lines(config, x1, y1, x2, y2, x3, y3, x4, y4, &
                             z1, z2, z3, z4, level, line_points, num_lines)
        
        ! Draw contour lines
        if (num_lines > 0) then
            call draw_contour_lines(backend, line_points, num_lines, xscale, yscale, symlog_threshold)
        end if
    end subroutine process_contour_cell
    
    subroutine draw_contour_lines(backend, line_points, num_lines, xscale, yscale, symlog_threshold)
        !! Draw contour line segments
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: line_points(8)
        integer, intent(in) :: num_lines
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        real(wp) :: x1, y1, x2, y2
        
        if (num_lines >= 1) then
            x1 = apply_scale_transform(line_points(1), xscale, symlog_threshold)
            y1 = apply_scale_transform(line_points(2), yscale, symlog_threshold)
            x2 = apply_scale_transform(line_points(3), xscale, symlog_threshold)
            y2 = apply_scale_transform(line_points(4), yscale, symlog_threshold)
            
            call backend%line(x1, y1, x2, y2)
        end if
        
        if (num_lines >= 2) then
            x1 = apply_scale_transform(line_points(5), xscale, symlog_threshold)
            y1 = apply_scale_transform(line_points(6), yscale, symlog_threshold)
            x2 = apply_scale_transform(line_points(7), xscale, symlog_threshold)
            y2 = apply_scale_transform(line_points(8), yscale, symlog_threshold)
            
            call backend%line(x1, y1, x2, y2)
        end if
    end subroutine draw_contour_lines

end module fortplot_contour_rendering
