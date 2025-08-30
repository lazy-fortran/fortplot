module fortplot_matplotlib_contour
    !! Contour and field plotting functions for matplotlib-compatible API
    !! Contains contour, pcolormesh, streamplot, and surface operations
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error, log_warning, log_info
    
    implicit none
    private
    
    ! Export contour and field functions
    public :: contour, contour_filled, pcolormesh, streamplot
    public :: add_contour, add_contour_filled, add_pcolormesh
    public :: add_surface
    
contains

    subroutine ensure_fig_init()
        !! Internal helper to ensure global figure is initialized
        if (.not. allocated(fig)) then
            allocate(figure_t :: fig)
        end if
        if (.not. fig%backend_associated()) then
            call fig%initialize()
        end if
    end subroutine ensure_fig_init

    subroutine contour(x, y, z, levels, label)
        !! Add a contour plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label
        
        call ensure_fig_init()
        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine contour

    subroutine contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        !! Add a filled contour plot with color levels to the global figure
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        real(wp), allocatable :: wp_x(:), wp_y(:), wp_z(:,:), wp_levels(:)
        
        call ensure_fig_init()
        
        ! Convert input arrays to working precision
        call convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, wp_levels)
        
        ! Forward to helper function with converted arrays
        call forward_contour_filled_params(fig, wp_x, wp_y, wp_z, wp_levels, &
                                          colormap, show_colorbar, label)
    end subroutine contour_filled

    subroutine pcolormesh(x, y, z, shading, colormap, show_colorbar, label, &
                         vmin, vmax, edgecolors, linewidths)
        !! Add a pseudocolor plot to the global figure (pyplot-style)
        !! Creates a colored mesh representation of 2D data
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        character(len=*), intent(in), optional :: shading, colormap, label
        logical, intent(in), optional :: show_colorbar
        real(8), intent(in), optional :: vmin, vmax
        real(8), dimension(3), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: linewidths
        
        character(len=64) :: shading_local, colormap_local, label_local
        logical :: show_colorbar_local
        real(8) :: vmin_local, vmax_local, linewidths_local
        integer :: nx, ny
        
        call ensure_fig_init()
        
        ! Validate input dimensions
        nx = size(x)
        ny = size(y)
        
        ! For pcolormesh, z should have dimensions (ny-1, nx-1) for cell-centered data
        ! or (ny, nx) for vertex-centered data (following Fortran column-major order)
        if (size(z, 1) /= ny-1 .or. size(z, 2) /= nx-1) then
            ! Check for vertex-centered data
            if (size(z, 1) == ny .and. size(z, 2) == nx) then
                call log_info("pcolormesh: using vertex-centered data")
            ! Check for incorrect C-style ordering
            elseif (size(z, 1) == nx-1 .and. size(z, 2) == ny-1) then
                call log_warning("pcolormesh: z dimensions appear to use C-style (row-major) ordering")
                call log_warning("pcolormesh: expected Fortran-style z(ny-1, nx-1), got z(nx-1, ny-1)")
            elseif (size(z, 1) == nx .and. size(z, 2) == ny) then
                call log_warning("pcolormesh: vertex-centered data uses C-style ordering")
                call log_warning("pcolormesh: expected Fortran-style z(ny, nx), got z(nx, ny)")
            else
                call log_error("pcolormesh: z must have dimensions (ny-1, nx-1) for " // &
                              "cell-centered or (ny, nx) for vertex-centered data")
                return
            end if
        end if
        
        ! Set default values
        shading_local = 'flat'
        if (present(shading)) shading_local = shading
        
        colormap_local = 'viridis'
        if (present(colormap)) colormap_local = colormap
        
        show_colorbar_local = .false.
        if (present(show_colorbar)) show_colorbar_local = show_colorbar
        
        label_local = ''
        if (present(label)) label_local = label
        
        ! Handle edgecolors (color array not used in placeholder implementation)
        
        linewidths_local = 1.0d0
        if (present(linewidths)) linewidths_local = linewidths
        
        ! Calculate default vmin/vmax if not provided
        if (present(vmin)) then
            vmin_local = vmin
        else
            vmin_local = minval(z)
        end if
        
        if (present(vmax)) then
            vmax_local = vmax
        else
            vmax_local = maxval(z)
        end if
        
        ! Forward to figure method (simplified parameters)
        call fig%add_pcolormesh(x, y, z, colormap=colormap_local, &
                              vmin=vmin_local, vmax=vmax_local, &
                              linewidths=linewidths_local)
    end subroutine pcolormesh

    subroutine streamplot(x, y, u, v, density, linewidth_scale, arrow_scale, colormap, label)
        !! Add a streamplot to the global figure (pyplot-style)
        !! Creates streamlines from a vector field
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: u, v
        real(8), intent(in), optional :: density, linewidth_scale, arrow_scale
        character(len=*), intent(in), optional :: colormap, label
        
        real(8) :: density_local, linewidth_scale_local, arrow_scale_local
        character(len=64) :: colormap_local, label_local
        integer :: nx, ny
        
        call ensure_fig_init()
        
        ! Validate input dimensions
        nx = size(x)
        ny = size(y)
        
        if (size(u, 1) /= nx .or. size(u, 2) /= ny) then
            call log_error("streamplot: u dimensions must match x and y")
            return
        end if
        
        if (size(v, 1) /= nx .or. size(v, 2) /= ny) then
            call log_error("streamplot: v dimensions must match x and y")
            return
        end if
        
        ! Set default values
        density_local = 1.0d0
        if (present(density)) density_local = density
        
        linewidth_scale_local = 1.0d0
        if (present(linewidth_scale)) linewidth_scale_local = linewidth_scale
        
        arrow_scale_local = 1.0d0
        if (present(arrow_scale)) arrow_scale_local = arrow_scale
        
        colormap_local = 'viridis'
        if (present(colormap)) colormap_local = colormap
        
        label_local = ''
        if (present(label)) label_local = label
        
        ! Create streamlines and add to figure
        call create_and_add_streamlines(x, y, u, v, density_local, &
                                       linewidth_scale_local, arrow_scale_local, &
                                       colormap_local, label_local)
    end subroutine streamplot

    subroutine add_contour(x, y, z, levels, label)
        !! Direct interface to figure's add_contour method for testing
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label
        
        call ensure_fig_init()
        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine add_contour

    subroutine add_contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        !! Direct interface to figure's add_contour_filled for testing
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        real(wp), allocatable :: wp_x(:), wp_y(:), wp_z(:,:), wp_levels(:)
        
        call ensure_fig_init()
        
        ! Convert input arrays to working precision
        call convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, wp_levels)
        
        ! Forward to helper function
        call forward_contour_filled_params(fig, wp_x, wp_y, wp_z, wp_levels, &
                                          colormap, show_colorbar, label)
    end subroutine add_contour_filled

    subroutine add_pcolormesh(x, y, z, shading, colormap, show_colorbar, label, &
                             vmin, vmax, edgecolors, linewidths)
        !! Direct interface to figure's add_pcolormesh for testing
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        character(len=*), intent(in), optional :: shading, colormap, label
        logical, intent(in), optional :: show_colorbar
        real(8), intent(in), optional :: vmin, vmax
        real(8), dimension(3), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: linewidths
        
        character(len=64) :: shading_local, colormap_local, label_local
        logical :: show_colorbar_local
        real(8) :: vmin_local, vmax_local, linewidths_local
        
        call ensure_fig_init()
        
        ! Set default values
        shading_local = 'flat'
        if (present(shading)) shading_local = shading
        
        colormap_local = 'viridis'
        if (present(colormap)) colormap_local = colormap
        
        show_colorbar_local = .false.
        if (present(show_colorbar)) show_colorbar_local = show_colorbar
        
        label_local = ''
        if (present(label)) label_local = label
        
        ! Handle edgecolors (color array not used in placeholder implementation)
        
        linewidths_local = 1.0d0
        if (present(linewidths)) linewidths_local = linewidths
        
        ! Calculate default vmin/vmax if not provided
        if (present(vmin)) then
            vmin_local = vmin
        else
            vmin_local = minval(z)
        end if
        
        if (present(vmax)) then
            vmax_local = vmax
        else
            vmax_local = maxval(z)
        end if
        
        ! Forward to figure method (simplified parameters)
        call fig%add_pcolormesh(x, y, z, colormap=colormap_local, &
                              vmin=vmin_local, vmax=vmax_local, &
                              linewidths=linewidths_local)
    end subroutine add_pcolormesh

    subroutine add_surface(x, y, z, colormap, show_colorbar, alpha, edgecolor, linewidth, label)
        !! Add a 3D surface plot with optional colormapping
        !! Provides direct access to figure's 3D surface capabilities
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        real(8), intent(in), optional :: alpha, linewidth
        real(8), dimension(3), intent(in), optional :: edgecolor
        
        character(len=64) :: colormap_local, label_local
        logical :: show_colorbar_local
        real(8) :: alpha_local, linewidth_local
        integer :: nx, ny, i, j
        
        call ensure_fig_init()
        
        ! Validate input dimensions
        nx = size(x)
        ny = size(y)
        
        ! For surface plots, z should have dimensions (ny-1, nx-1) for cell-centered data
        ! or (ny, nx) for vertex-centered data (following Fortran column-major order)
        if (size(z, 1) /= ny-1 .or. size(z, 2) /= nx-1) then
            ! Check for vertex-centered data
            if (size(z, 1) == ny .and. size(z, 2) == nx) then
                call log_info("add_surface: using vertex-centered data")
            ! Check for incorrect C-style ordering
            elseif (size(z, 1) == nx-1 .and. size(z, 2) == ny-1) then
                call log_warning("add_surface: z dimensions appear to use C-style (row-major) ordering")
                call log_warning("add_surface: expected Fortran-style z(ny-1, nx-1), got z(nx-1, ny-1)")
            elseif (size(z, 1) == nx .and. size(z, 2) == ny) then
                call log_warning("add_surface: vertex-centered data uses C-style ordering")
                call log_warning("add_surface: expected Fortran-style z(ny, nx), got z(nx, ny)")
            else
                call log_error("add_surface: z must have dimensions (ny-1, nx-1) for " // &
                              "cell-centered or (ny, nx) for vertex-centered data")
                return
            end if
        end if
        
        ! Set default values
        colormap_local = 'viridis'
        if (present(colormap)) colormap_local = colormap
        
        show_colorbar_local = .false.
        if (present(show_colorbar)) show_colorbar_local = show_colorbar
        
        alpha_local = 1.0d0
        if (present(alpha)) alpha_local = alpha
        
        ! Handle edgecolor (color array not used in placeholder implementation)
        
        linewidth_local = 0.5d0
        if (present(linewidth)) linewidth_local = linewidth
        
        label_local = ''
        if (present(label)) label_local = label
        
        ! 3D surface plot not yet implemented in figure_core - use contour as placeholder
        call fig%add_contour_filled(x, y, z, colormap=colormap_local, &
                                   show_colorbar=show_colorbar_local, &
                                   label=label_local)
    end subroutine add_surface

    ! Helper routines
    
    subroutine convert_contour_arrays(x, y, z, levels, wp_x, wp_y, wp_z, wp_levels)
        !! Convert input arrays to working precision for contour operations
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        real(wp), allocatable, intent(out) :: wp_x(:), wp_y(:), wp_z(:,:), wp_levels(:)
        
        integer :: i, j
        
        ! Allocate and convert arrays
        allocate(wp_x(size(x)))
        allocate(wp_y(size(y)))
        allocate(wp_z(size(z,1), size(z,2)))
        
        wp_x = real(x, wp)
        wp_y = real(y, wp)
        wp_z = real(z, wp)
        
        if (present(levels)) then
            allocate(wp_levels(size(levels)))
            wp_levels = real(levels, wp)
        end if
    end subroutine convert_contour_arrays

    subroutine forward_contour_filled_params(figure_obj, x, y, z, levels, &
                                            colormap, show_colorbar, label)
        !! Forward contour_filled parameters to figure object
        class(figure_t), intent(inout) :: figure_obj
        real(wp), dimension(:), intent(in) :: x, y
        real(wp), dimension(:,:), intent(in) :: z
        real(wp), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        ! Forward all parameters to figure's add_contour_filled method
        call figure_obj%add_contour_filled(x, y, z, levels=levels, &
                                          colormap=colormap, &
                                          show_colorbar=show_colorbar, &
                                          label=label)
    end subroutine forward_contour_filled_params

    subroutine create_and_add_streamlines(x, y, u, v, density, &
                                         linewidth_scale, arrow_scale, &
                                         colormap, label)
        !! Create streamlines and add them to the figure
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: u, v
        real(8), intent(in) :: density, linewidth_scale, arrow_scale
        character(len=*), intent(in) :: colormap, label
        
        ! Simplified streamplot - just create a few representative streamlines
        integer :: i, j, nx, ny, n_streams
        real(8), allocatable :: stream_x(:), stream_y(:)
        real(8) :: dx, dy, x0, y0
        
        nx = size(x)
        ny = size(y)
        
        ! Create a simple grid of starting points
        n_streams = max(1, int(density * 10))
        
        do i = 1, n_streams
            do j = 1, n_streams
                x0 = x(1) + (x(nx) - x(1)) * real(i-1) / real(n_streams-1)
                y0 = y(1) + (y(ny) - y(1)) * real(j-1) / real(n_streams-1)
                
                ! Create a simple streamline (placeholder implementation)
                allocate(stream_x(20), stream_y(20))
                call create_simple_streamline(x0, y0, x, y, u, v, stream_x, stream_y)
                
                ! Add as a line plot
                if (i == 1 .and. j == 1 .and. len_trim(label) > 0) then
                    call fig%add_plot(stream_x, stream_y, label=label, linestyle='-')
                else
                    call fig%add_plot(stream_x, stream_y, linestyle='-')
                end if
                
                deallocate(stream_x, stream_y)
            end do
        end do
    end subroutine create_and_add_streamlines
    
    subroutine create_simple_streamline(x0, y0, x_grid, y_grid, u, v, stream_x, stream_y)
        !! Create a simple streamline trajectory (placeholder)
        real(8), intent(in) :: x0, y0
        real(8), dimension(:), intent(in) :: x_grid, y_grid
        real(8), dimension(:,:), intent(in) :: u, v
        real(8), dimension(:), intent(out) :: stream_x, stream_y
        
        integer :: i, n
        real(8) :: dt
        
        n = size(stream_x)
        dt = 0.1d0
        
        ! Simple Euler integration for streamline
        stream_x(1) = x0
        stream_y(1) = y0
        
        do i = 2, n
            ! Simplified - just use average velocity
            stream_x(i) = stream_x(i-1) + dt * sum(u) / size(u)
            stream_y(i) = stream_y(i-1) + dt * sum(v) / size(v)
        end do
    end subroutine create_simple_streamline

end module fortplot_matplotlib_contour