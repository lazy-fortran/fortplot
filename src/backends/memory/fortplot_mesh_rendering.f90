module fortplot_mesh_rendering
    !! Mesh (pcolormesh) rendering module
    !! 
    !! This module handles all mesh-based rendering operations including
    !! pcolormesh plots, quad transformations, and filled quad rendering.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_colormap
    use fortplot_plot_data
    implicit none
    
    private
    public :: render_pcolormesh_plot
    public :: transform_quad_to_screen
    public :: draw_filled_quad
    public :: draw_quad_edges
    
contains
    
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
        
        ! Safety check: ensure pcolormesh data is properly initialized
        if (.not. allocated(plot_data%pcolormesh_data%c_values) .or. &
            .not. allocated(plot_data%pcolormesh_data%x_vertices) .or. &
            .not. allocated(plot_data%pcolormesh_data%y_vertices)) then
            ! Pcolormesh data not properly initialized - skip rendering
            return
        end if
        
        nx = size(plot_data%pcolormesh_data%c_values, 2)
        ny = size(plot_data%pcolormesh_data%c_values, 1)
        
        ! Additional safety: ensure arrays have consistent sizes
        if (nx <= 0 .or. ny <= 0) then
            ! Invalid array dimensions - skip rendering
            return
        end if
        
        ! Verify vertex arrays have correct dimensions (ny+1, nx+1)
        if (size(plot_data%pcolormesh_data%x_vertices, 1) /= ny + 1 .or. &
            size(plot_data%pcolormesh_data%x_vertices, 2) /= nx + 1 .or. &
            size(plot_data%pcolormesh_data%y_vertices, 1) /= ny + 1 .or. &
            size(plot_data%pcolormesh_data%y_vertices, 2) /= nx + 1) then
            ! Vertex arrays have incorrect dimensions - skip rendering
            return
        end if
        
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

end module fortplot_mesh_rendering