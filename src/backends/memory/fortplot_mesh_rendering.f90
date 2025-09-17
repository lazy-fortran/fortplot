module fortplot_mesh_rendering
    !! Mesh (pcolormesh) rendering module
    !! 
    !! This module handles all mesh-based rendering operations including
    !! pcolormesh plots, quad transformations, and filled quad rendering.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_pdf, only: pdf_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_colormap
    use fortplot_plot_data
    implicit none
    
    private
    public :: render_pcolormesh_plot
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
        
        integer :: nx, ny
        ! PDF specialization temporaries
        real(wp), allocatable :: xg(:), yg(:)
        real(wp) :: vmin, vmax
        integer :: i
        ! Reference otherwise-unused parameters to keep interface stable
        associate(dummy_xmin => x_min_t, dummy_xmax => x_max_t, dummy_ymin => y_min_t, dummy_ymax => y_max_t); end associate
        associate(dummy_xs => len_trim(xscale), dummy_ys => len_trim(yscale)); end associate
        associate(dummy_slt => symlog_threshold); end associate
        associate(dummy_w => width, dummy_h => height); end associate
        associate(dummy_mr => margin_right); end associate
        
        ! Validate mesh data and get dimensions
        if (.not. validate_mesh_data(plot_data, nx, ny)) return
        
        ! PDF specialization: render as a single Image XObject for seam-free output
        select type (backend)
        type is (pdf_context)
            allocate(xg(nx), yg(ny))
            do i = 1, nx
                xg(i) = plot_data%pcolormesh_data%x_vertices(1, i)
            end do
            do i = 1, ny
                yg(i) = plot_data%pcolormesh_data%y_vertices(i, 1)
            end do
            vmin = minval(plot_data%pcolormesh_data%c_values)
            vmax = maxval(plot_data%pcolormesh_data%c_values)
            if (vmax <= vmin) vmax = vmin + 1.0_wp
            call backend%fill_heatmap(xg, yg, plot_data%pcolormesh_data%c_values, vmin, vmax)
            return
        class default
            continue
        end select

        ! Render the mesh (default path)
        call render_mesh_quads(backend, plot_data, nx, ny)
    end subroutine render_pcolormesh_plot
    
    logical function validate_mesh_data(plot_data, nx, ny) result(valid)
        !! Validate pcolormesh data arrays
        type(plot_data_t), intent(in) :: plot_data
        integer, intent(out) :: nx, ny
        
        valid = .false.
        
        ! Safety check: ensure pcolormesh data is properly initialized
        if (.not. allocated(plot_data%pcolormesh_data%c_values) .or. &
            .not. allocated(plot_data%pcolormesh_data%x_vertices) .or. &
            .not. allocated(plot_data%pcolormesh_data%y_vertices)) return
        
        nx = size(plot_data%pcolormesh_data%c_values, 2)
        ny = size(plot_data%pcolormesh_data%c_values, 1)
        
        ! Additional safety: ensure arrays have consistent sizes
        if (nx <= 0 .or. ny <= 0) return
        
        ! Verify vertex arrays have correct dimensions (ny+1, nx+1)
        if (size(plot_data%pcolormesh_data%x_vertices, 1) /= ny + 1 .or. &
            size(plot_data%pcolormesh_data%x_vertices, 2) /= nx + 1 .or. &
            size(plot_data%pcolormesh_data%y_vertices, 1) /= ny + 1 .or. &
            size(plot_data%pcolormesh_data%y_vertices, 2) /= nx + 1) return
        
        valid = .true.
    end function validate_mesh_data
    
    subroutine render_mesh_quads(backend, plot_data, nx, ny)
        !! Render all quadrilaterals in the mesh
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        integer, intent(in) :: nx, ny
        
        real(wp) :: x_quad(4), y_quad(4)
        real(wp), dimension(3) :: quad_color
        real(wp) :: c_value, vmin, vmax
        integer :: i, j
        
        ! Robust normalization: span actual data range (matplotlib behavior)
        vmin = minval(plot_data%pcolormesh_data%c_values)
        vmax = maxval(plot_data%pcolormesh_data%c_values)
        if (vmax <= vmin) then
            vmax = vmin + 1.0_wp
        end if
        
        ! Render each quad
        do i = 1, nx
            do j = 1, ny
                call render_single_quad(backend, plot_data, i, j, vmin, vmax)
            end do
        end do
    end subroutine render_mesh_quads
    
    subroutine render_single_quad(backend, plot_data, i, j, vmin, vmax)
        !! Render a single quadrilateral
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        integer, intent(in) :: i, j
        real(wp), intent(in) :: vmin, vmax
        
        real(wp) :: x_quad(4), y_quad(4)
        real(wp), dimension(3) :: quad_color
        real(wp) :: c_value
        
        ! Get quad corners from vertices arrays
        x_quad = [plot_data%pcolormesh_data%x_vertices(j, i), &
                 plot_data%pcolormesh_data%x_vertices(j, i+1), &
                 plot_data%pcolormesh_data%x_vertices(j+1, i+1), &
                 plot_data%pcolormesh_data%x_vertices(j+1, i)]
                 
        y_quad = [plot_data%pcolormesh_data%y_vertices(j, i), &
                 plot_data%pcolormesh_data%y_vertices(j, i+1), &
                 plot_data%pcolormesh_data%y_vertices(j+1, i+1), &
                 plot_data%pcolormesh_data%y_vertices(j+1, i)]
        
        ! Get color for this quad
        c_value = plot_data%pcolormesh_data%c_values(j, i)
        call colormap_value_to_color(c_value, vmin, vmax, &
                                   plot_data%pcolormesh_data%colormap_name, quad_color)
        
        ! Draw filled quad
        call backend%color(quad_color(1), quad_color(2), quad_color(3))
        call draw_filled_quad(backend, x_quad, y_quad)
        
        ! Draw edges if requested
        if (plot_data%pcolormesh_data%show_edges) then
            call backend%color(plot_data%pcolormesh_data%edge_color(1), &
                             plot_data%pcolormesh_data%edge_color(2), &
                             plot_data%pcolormesh_data%edge_color(3))
            call draw_quad_edges(backend, x_quad, y_quad, &
                               plot_data%pcolormesh_data%edge_width)
        end if
    end subroutine render_single_quad

    ! Note: Do not pre-transform quads here; backends apply coordinate transforms.
    
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
