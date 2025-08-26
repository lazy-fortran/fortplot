module fortplot_rendering
    !! Figure rendering pipeline module
    !! 
    !! This module handles the rendering pipeline for all plot types,
    !! including coordinate transformations and drawing operations.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_utils
    use fortplot_colormap
    use fortplot_contour_algorithms
    use fortplot_plot_data
    use fortplot_format_parser, only: parse_format_string
    implicit none
    
    private
    public :: render_line_plot
    public :: render_contour_plot
    public :: render_pcolormesh_plot
    public :: render_markers
    public :: draw_line_with_style
    public :: render_solid_line
    public :: render_patterned_line
    public :: transform_quad_to_screen
    public :: draw_filled_quad
    public :: draw_quad_edges
    
contains
    
    subroutine render_line_plot(backend, plot_data, plot_idx, x_min_t, x_max_t, y_min_t, y_max_t, xscale, yscale, symlog_threshold)
        !! Render a line plot with proper scaling and clipping
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        integer, intent(in) :: plot_idx
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        real(wp), allocatable :: x_scaled(:), y_scaled(:)
        integer :: i, n
        
        ! Validate input data
        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) return
        if (size(plot_data%x) == 0 .or. size(plot_data%y) == 0) return
        if (size(plot_data%x) /= size(plot_data%y)) return
        
        n = size(plot_data%x)
        allocate(x_scaled(n), y_scaled(n))
        
        ! Transform coordinates based on scale
        do i = 1, n
            x_scaled(i) = apply_scale_transform(plot_data%x(i), xscale, symlog_threshold)
            y_scaled(i) = apply_scale_transform(plot_data%y(i), yscale, symlog_threshold)
        end do
        
        ! Set color
        call backend%color(plot_data%color(1), plot_data%color(2), plot_data%color(3))
        
        ! Check if we should draw lines at all
        if (allocated(plot_data%linestyle)) then
            ! Skip line drawing if linestyle is 'None'
            if (trim(plot_data%linestyle) == 'None' .or. &
                trim(plot_data%linestyle) == 'none' .or. &
                trim(plot_data%linestyle) == '') then
                ! No lines to draw, only markers
                deallocate(x_scaled, y_scaled)
                return
            end if
            call backend%set_line_style(plot_data%linestyle)
        end if
        
        ! Draw the line segments
        do i = 1, n-1
            call backend%line(x_scaled(i), y_scaled(i), x_scaled(i+1), y_scaled(i+1))
        end do
        
        deallocate(x_scaled, y_scaled)
    end subroutine render_line_plot
    
    subroutine render_markers(backend, plot_data, x_min_t, x_max_t, y_min_t, y_max_t, xscale, yscale, symlog_threshold)
        !! Render markers for a plot
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        real(wp) :: x_scaled, y_scaled
        integer :: i
        
        if (.not. allocated(plot_data%marker)) return
        if (len_trim(plot_data%marker) == 0) return
        if (trim(plot_data%marker) == 'None' .or. trim(plot_data%marker) == 'none') return
        
        ! Validate input data
        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) return
        if (size(plot_data%x) == 0 .or. size(plot_data%y) == 0) return
        if (size(plot_data%x) /= size(plot_data%y)) return
        
        ! Draw markers
        call backend%color(plot_data%color(1), plot_data%color(2), plot_data%color(3))
        
        do i = 1, size(plot_data%x)
            x_scaled = apply_scale_transform(plot_data%x(i), xscale, symlog_threshold)
            y_scaled = apply_scale_transform(plot_data%y(i), yscale, symlog_threshold)
            call backend%draw_marker(x_scaled, y_scaled, plot_data%marker)
        end do
    end subroutine render_markers
    
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
        
        ! Render contour levels
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
        
        ! Colorbar rendering handled elsewhere if needed
    end subroutine render_contour_plot
    
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
    
    subroutine render_pcolormesh_plot(backend, plot_data, x_min_t, x_max_t, y_min_t, y_max_t, &
                                     xscale, yscale, symlog_threshold, width, height, margin_right)
        !! Render a pcolormesh plot
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_right
        
        real(wp) :: x_quad(4), y_quad(4), x_screen(4), y_screen(4)
        real(wp), dimension(3) :: quad_color
        real(wp) :: c_value, vmin, vmax
        integer :: i, j, nx, ny
        
        nx = size(plot_data%pcolormesh_data%c_values, 2)
        ny = size(plot_data%pcolormesh_data%c_values, 1)
        
        vmin = plot_data%pcolormesh_data%vmin
        vmax = plot_data%pcolormesh_data%vmax
        
        ! Render each quad
        do i = 1, nx
            do j = 1, ny
                ! Get quad corners from vertices arrays
                x_quad = [plot_data%pcolormesh_data%x_vertices(j, i), &
                         plot_data%pcolormesh_data%x_vertices(j, i+1), &
                         plot_data%pcolormesh_data%x_vertices(j+1, i+1), &
                         plot_data%pcolormesh_data%x_vertices(j+1, i)]
                         
                y_quad = [plot_data%pcolormesh_data%y_vertices(j, i), &
                         plot_data%pcolormesh_data%y_vertices(j, i+1), &
                         plot_data%pcolormesh_data%y_vertices(j+1, i+1), &
                         plot_data%pcolormesh_data%y_vertices(j+1, i)]
                
                ! Transform to screen coordinates
                call transform_quad_to_screen(x_quad, y_quad, x_screen, y_screen, &
                                            xscale, yscale, symlog_threshold)
                
                ! Get color for this quad
                c_value = plot_data%pcolormesh_data%c_values(j, i)
                call colormap_value_to_color(c_value, vmin, vmax, &
                                           plot_data%pcolormesh_data%colormap_name, quad_color)
                
                ! Draw filled quad
                call backend%color(quad_color(1), quad_color(2), quad_color(3))
                call draw_filled_quad(backend, x_screen, y_screen)
                
                ! Draw edges if requested
                if (plot_data%pcolormesh_data%show_edges) then
                    call backend%color(plot_data%pcolormesh_data%edge_color(1), &
                                     plot_data%pcolormesh_data%edge_color(2), &
                                     plot_data%pcolormesh_data%edge_color(3))
                    call draw_quad_edges(backend, x_screen, y_screen, &
                                       plot_data%pcolormesh_data%edge_width)
                end if
            end do
        end do
        
        ! Colorbar rendering handled elsewhere if needed
    end subroutine render_pcolormesh_plot
    
    subroutine draw_line_with_style(backend, x, y, linestyle, color)
        !! Draw a line with the specified style
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: linestyle
        real(wp), intent(in), optional :: color(3)
        
        if (present(color)) then
            call backend%color(color(1), color(2), color(3))
        end if
        
        if (present(linestyle)) then
            select case (trim(linestyle))
            case ('--', 'dashed')
                call render_patterned_line(backend, x, y, '--')
            case (':', 'dotted')
                call render_patterned_line(backend, x, y, ':')
            case ('-.', 'dashdot')
                call render_patterned_line(backend, x, y, '-.')
            case default
                call render_solid_line(backend, x, y)
            end select
        else
            call render_solid_line(backend, x, y)
        end if
    end subroutine draw_line_with_style
    
    subroutine render_solid_line(backend, x, y)
        !! Render a solid line
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x(:), y(:)
        integer :: i
        
        if (size(x) < 2) return
        
        do i = 1, size(x)-1
            call backend%line(x(i), y(i), x(i+1), y(i+1))
        end do
    end subroutine render_solid_line
    
    subroutine render_patterned_line(backend, x, y, pattern)
        !! Render a line with dash patterns
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: pattern
        
        real(wp) :: dash_length, gap_length, dot_length
        real(wp) :: segment_length, accumulated_length
        real(wp) :: dx, dy, x1, y1, x2, y2
        logical :: drawing
        integer :: i
        
        ! Define pattern parameters
        select case (trim(pattern))
        case ('--', 'dashed')
            dash_length = 6.0_wp
            gap_length = 4.0_wp
            dot_length = 0.0_wp
        case (':', 'dotted')
            dash_length = 2.0_wp
            gap_length = 2.0_wp
            dot_length = 0.0_wp
        case ('-.', 'dashdot')
            dash_length = 6.0_wp
            gap_length = 2.0_wp
            dot_length = 2.0_wp
        case default
            call render_solid_line(backend, x, y)
            return
        end select
        
        ! Render with pattern
        drawing = .true.
        accumulated_length = 0.0_wp
        
        do i = 1, size(x) - 1
            dx = x(i+1) - x(i)
            dy = y(i+1) - y(i)
            segment_length = sqrt(dx**2 + dy**2)
            
            call render_segment_with_pattern(backend, x(i), y(i), x(i+1), y(i+1), &
                                           segment_length, accumulated_length, &
                                           dash_length, gap_length, drawing)
        end do
    end subroutine render_patterned_line
    
    subroutine render_segment_with_pattern(backend, x1, y1, x2, y2, segment_length, &
                                         accumulated_length, dash_length, gap_length, drawing)
        !! Render a single line segment with pattern
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x1, y1, x2, y2, segment_length
        real(wp), intent(inout) :: accumulated_length
        real(wp), intent(in) :: dash_length, gap_length
        logical, intent(inout) :: drawing
        
        real(wp) :: remaining, pattern_period, t, x_start, y_start, x_end, y_end
        real(wp) :: dx, dy
        
        pattern_period = dash_length + gap_length
        remaining = segment_length
        dx = x2 - x1
        dy = y2 - y1
        
        x_start = x1
        y_start = y1
        
        do while (remaining > epsilon(1.0_wp))
            if (drawing) then
                ! Currently drawing
                if (accumulated_length + remaining <= dash_length) then
                    ! Can draw entire remaining segment
                    call backend%line(x_start, y_start, x2, y2)
                    accumulated_length = accumulated_length + remaining
                    remaining = 0.0_wp
                else
                    ! Draw partial segment
                    t = (dash_length - accumulated_length) / segment_length
                    x_end = x_start + t * dx
                    y_end = y_start + t * dy
                    
                    call backend%line(x_start, y_start, x_end, y_end)
                    
                    remaining = remaining - (dash_length - accumulated_length)
                    x_start = x_end
                    y_start = y_end
                    accumulated_length = 0.0_wp
                    drawing = .false.
                end if
            else
                ! Currently in gap
                if (accumulated_length + remaining <= gap_length) then
                    ! Entire remaining segment is in gap
                    accumulated_length = accumulated_length + remaining
                    remaining = 0.0_wp
                else
                    ! Skip gap portion
                    t = (gap_length - accumulated_length) / segment_length
                    x_start = x_start + t * dx
                    y_start = y_start + t * dy
                    
                    remaining = remaining - (gap_length - accumulated_length)
                    accumulated_length = 0.0_wp
                    drawing = .true.
                end if
            end if
        end do
    end subroutine render_segment_with_pattern
    
    subroutine transform_quad_to_screen(x_quad, y_quad, x_screen, y_screen, &
                                       xscale, yscale, symlog_threshold)
        !! Transform quad coordinates to screen space
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp), intent(out) :: x_screen(4), y_screen(4)
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer :: i
        
        do i = 1, 4
            x_screen(i) = apply_scale_transform(x_quad(i), xscale, symlog_threshold)
            y_screen(i) = apply_scale_transform(y_quad(i), yscale, symlog_threshold)
        end do
    end subroutine transform_quad_to_screen
    
    subroutine draw_filled_quad(backend, x_screen, y_screen)
        !! Draw a filled quadrilateral
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x_screen(4), y_screen(4)
        
        ! Use fill_quad if available
        call backend%fill_quad(x_screen, y_screen)
    end subroutine draw_filled_quad
    
    subroutine draw_quad_edges(backend, x_screen, y_screen, line_width)
        !! Draw quadrilateral edges
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x_screen(4), y_screen(4)
        real(wp), intent(in) :: line_width
        
        call backend%set_line_width(line_width)
        call backend%line(x_screen(1), y_screen(1), x_screen(2), y_screen(2))
        call backend%line(x_screen(2), y_screen(2), x_screen(3), y_screen(3))
        call backend%line(x_screen(3), y_screen(3), x_screen(4), y_screen(4))
        call backend%line(x_screen(4), y_screen(4), x_screen(1), y_screen(1))
    end subroutine draw_quad_edges
    
end module fortplot_rendering