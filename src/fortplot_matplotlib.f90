module fortplot_matplotlib
    !! Matplotlib-compatible API wrapper functions for fortplot
    !! Provides global figure management and pyplot-style interface
    !!
    !! This module contains the matplotlib.pyplot-style wrapper functions 
    !! that operate on a global figure instance, providing simple access
    !! to plotting functionality without explicit figure management.
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_global, only: fig => global_figure
    use fortplot_plotting, only: add_text_annotation, add_arrow_annotation, add_scatter_2d, &
                                figure_add_plot => add_plot, figure_add_contour => add_contour, &
                                figure_add_contour_filled => add_contour_filled, &
                                figure_add_pcolormesh => add_pcolormesh, &
                                figure_streamplot => streamplot, figure_bar => bar, figure_barh => barh, &
                                figure_hist => hist, figure_errorbar => errorbar, &
                                figure_add_3d_plot => add_3d_plot, figure_add_surface => add_surface
    ! Type-bound procedures from figure_t are used directly through fig%method() calls
    use fortplot_logging, only: log_error, log_warning, log_info
    use fortplot_security, only: safe_launch_viewer, safe_remove_file
    
    implicit none
    private
    
    ! Export pyplot-style functions
    public :: plot, contour, contour_filled, pcolormesh, streamplot
    public :: hist, histogram, scatter, errorbar, boxplot
    public :: bar, barh
    public :: text, annotate
    public :: xlabel, ylabel, title, legend
    public :: savefig, figure, subplot
    public :: add_plot, add_contour, add_contour_filled, add_pcolormesh, add_errorbar
    public :: add_3d_plot, add_surface, add_scatter
    public :: set_xscale, set_yscale, xlim, ylim
    public :: set_line_width, set_ydata
    public :: show, show_viewer
    public :: ensure_global_figure_initialized, get_global_figure
    
    ! Overloaded show interface
    interface show
        module procedure show_data, show_figure
    end interface show
    
contains

    subroutine ensure_global_figure_initialized()
        !! Ensure global figure is initialized before use (matplotlib compatibility)
        !! Auto-initializes with default dimensions if not already initialized
        if (.not. allocated(fig)) then
            allocate(figure_t :: fig)
        end if
        if (.not. allocated(fig%backend)) then
            call fig%initialize()
        end if
    end subroutine ensure_global_figure_initialized
    
    function get_global_figure() result(global_fig)
        !! Get reference to the global figure for testing access to arrow data
        !! This allows tests to access fig%arrow_data without making fig public
        class(figure_t), pointer :: global_fig
        call ensure_global_figure_initialized()
        global_fig => fig
    end function get_global_figure

    subroutine plot(x, y, label, linestyle)
        !! Add a line plot to the global figure (pyplot-fortran compatible)
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle
        
        call ensure_global_figure_initialized()
        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine plot

    subroutine contour(x, y, z, levels, label)
        !! Add a contour plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label
        
        call ensure_global_figure_initialized()
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
        
        call ensure_global_figure_initialized()
        
        ! Convert input arrays to working precision
        allocate(wp_x(size(x)), wp_y(size(y)), wp_z(size(z,1), size(z,2)))
        wp_x = real(x, wp)
        wp_y = real(y, wp)
        wp_z = real(z, wp)
        
        if (present(levels)) then
            allocate(wp_levels(size(levels)))
            wp_levels = real(levels, wp)
        else
            allocate(wp_levels(0))
        end if
        
        ! Forward parameters to underlying method using conditional calls for memory safety
        if (present(levels) .and. present(colormap) .and. present(show_colorbar) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, &
                colormap=colormap, show_colorbar=show_colorbar, label=label)
        else if (present(levels) .and. present(colormap) .and. present(show_colorbar)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, &
                colormap=colormap, show_colorbar=show_colorbar)
        else if (present(levels) .and. present(colormap) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, &
                colormap=colormap, label=label)
        else if (present(colormap) .and. present(show_colorbar) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, colormap=colormap, &
                show_colorbar=show_colorbar, label=label)
        else if (present(levels) .and. present(colormap)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, colormap=colormap)
        else if (present(levels) .and. present(show_colorbar)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, show_colorbar=show_colorbar)
        else if (present(levels) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, label=label)
        else if (present(colormap) .and. present(show_colorbar)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, colormap=colormap, show_colorbar=show_colorbar)
        else if (present(colormap) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, colormap=colormap, label=label)
        else if (present(show_colorbar) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, show_colorbar=show_colorbar, label=label)
        else if (present(levels)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels)
        else if (present(colormap)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, colormap=colormap)
        else if (present(show_colorbar)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, show_colorbar=show_colorbar)
        else if (present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, label=label)
        else
            call fig%add_contour_filled(wp_x, wp_y, wp_z)
        end if
        
        deallocate(wp_x, wp_y, wp_z)
        if (allocated(wp_levels)) deallocate(wp_levels)
    end subroutine contour_filled

    subroutine pcolormesh(x, y, z, shading, colormap, show_colorbar, label, &
                         vmin, vmax, edgecolors, linewidths)
        !! Add a pseudocolor mesh plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        character(len=*), intent(in), optional :: shading, colormap, label
        logical, intent(in), optional :: show_colorbar
        real(8), intent(in), optional :: vmin, vmax
        real(8), dimension(3), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: linewidths
        
        real(wp), allocatable :: wp_x(:), wp_y(:), wp_z(:,:)
        real(wp) :: wp_vmin, wp_vmax, wp_linewidths
        real(wp) :: wp_edgecolors(3)
        
        call ensure_global_figure_initialized()
        
        ! Convert input arrays to working precision
        allocate(wp_x(size(x)), wp_y(size(y)), wp_z(size(z,1), size(z,2)))
        wp_x = real(x, wp)
        wp_y = real(y, wp)
        wp_z = real(z, wp)
        
        ! Convert optional parameters to working precision
        if (present(vmin)) then
            wp_vmin = real(vmin, wp)
        end if
        if (present(vmax)) then
            wp_vmax = real(vmax, wp)
        end if
        if (present(edgecolors)) then
            wp_edgecolors = real(edgecolors, wp)
        end if
        if (present(linewidths)) then
            wp_linewidths = real(linewidths, wp)
        end if
        
        ! Forward SUPPORTED parameters to underlying method using conditional calls
        if (present(colormap) .and. present(vmin) .and. present(vmax) .and. &
            present(edgecolors) .and. present(linewidths)) then
            call fig%add_pcolormesh(wp_x, wp_y, wp_z, &
                colormap=colormap, vmin=wp_vmin, vmax=wp_vmax, &
                edgecolors=wp_edgecolors, linewidths=wp_linewidths)
        else if (present(colormap) .and. present(vmin) .and. present(vmax)) then
            call fig%add_pcolormesh(wp_x, wp_y, wp_z, &
                colormap=colormap, vmin=wp_vmin, vmax=wp_vmax)
        else if (present(colormap)) then
            call fig%add_pcolormesh(wp_x, wp_y, wp_z, colormap=colormap)
        else if (present(vmin) .and. present(vmax)) then
            call fig%add_pcolormesh(wp_x, wp_y, wp_z, vmin=wp_vmin, vmax=wp_vmax)
        else
            call fig%add_pcolormesh(wp_x, wp_y, wp_z)
        end if
        
        deallocate(wp_x, wp_y, wp_z)
    end subroutine pcolormesh

    subroutine streamplot(x, y, u, v, density, linewidth_scale, arrow_scale, colormap, label)
        !! Add a streamline plot to the global figure (pyplot-style)
        !! Direct implementation compatible with figure_core type
        use fortplot_streamplot_matplotlib, only: streamplot_matplotlib
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: u, v
        real(8), intent(in), optional :: density, linewidth_scale, arrow_scale
        character(len=*), intent(in), optional :: colormap, label
        
        real(wp) :: wp_density
        real(wp), allocatable :: wp_x(:), wp_y(:), wp_u(:,:), wp_v(:,:)
        real, allocatable :: trajectories(:,:,:)
        integer :: n_trajectories
        integer, allocatable :: trajectory_lengths(:)
        
        call ensure_global_figure_initialized()
        
        ! Validate input dimensions
        if (size(u,1) /= size(x) .or. size(u,2) /= size(y) .or. &
            size(v,1) /= size(x) .or. size(v,2) /= size(y)) then
            call log_error('streamplot: Input dimension mismatch')
            return
        end if
        
        ! Convert parameters to working precision
        wp_density = 1.0_wp
        if (present(density)) wp_density = real(density, wp)
        
        ! Convert input arrays to working precision
        allocate(wp_x(size(x)), wp_y(size(y)))
        allocate(wp_u(size(u,1), size(u,2)), wp_v(size(v,1), size(v,2)))
        
        wp_x = real(x, wp)
        wp_y = real(y, wp)
        wp_u = real(u, wp)
        wp_v = real(v, wp)
        
        ! Generate streamlines using matplotlib algorithm
        call streamplot_matplotlib(wp_x, wp_y, wp_u, wp_v, wp_density, &
                                  trajectories, n_trajectories, trajectory_lengths)
        
        ! Add trajectories as line plots to figure
        call add_streamplot_trajectories_to_figure(trajectories, n_trajectories, &
                                                  trajectory_lengths, wp_x, wp_y)
        
        deallocate(wp_x, wp_y, wp_u, wp_v)
    end subroutine streamplot
    
    subroutine add_streamplot_trajectories_to_figure(trajectories, n_trajectories, &
                                                    trajectory_lengths, x_grid, y_grid)
        !! Add streamline trajectories to global figure as line plots
        use fortplot_plot_data, only: PLOT_TYPE_LINE
        real, intent(in) :: trajectories(:,:,:)
        integer, intent(in) :: n_trajectories, trajectory_lengths(:)
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        
        integer :: i, j, n_points
        real(wp), allocatable :: traj_x(:), traj_y(:)
        real(wp) :: line_color(3)
        
        ! Set default streamline color (blue)
        line_color = [0.0_wp, 0.447_wp, 0.698_wp]
        
        do i = 1, n_trajectories
            n_points = trajectory_lengths(i)
            if (n_points <= 1) cycle
            
            allocate(traj_x(n_points), traj_y(n_points))
            
            ! Convert from grid coordinates to data coordinates
            do j = 1, n_points
                traj_x(j) = real(trajectories(i, j, 1), wp) * (x_grid(size(x_grid)) - x_grid(1)) / &
                           real(size(x_grid) - 1, wp) + x_grid(1)
                traj_y(j) = real(trajectories(i, j, 2), wp) * (y_grid(size(y_grid)) - y_grid(1)) / &
                           real(size(y_grid) - 1, wp) + y_grid(1)
            end do
            
            ! Add trajectory as line plot to figure
            call fig%add_plot(traj_x, traj_y, linestyle='-')
            
            deallocate(traj_x, traj_y)
        end do
    end subroutine add_streamplot_trajectories_to_figure

    subroutine errorbar(x, y, xerr, yerr, fmt, label, capsize, linestyle, marker, color)
        !! Add error bars to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: xerr, yerr
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(8), intent(in), optional :: capsize
        real(8), dimension(3), intent(in), optional :: color
        
        call ensure_global_figure_initialized()
        ! TODO: errorbar method not yet implemented in working figure core
        ! call fig%errorbar(x, y, xerr, yerr, label=label)
    end subroutine errorbar

    subroutine bar(x, height, width, bottom, label, color, edgecolor, align)
        !! Add vertical bar plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: x, height
        real(8), intent(in), optional :: width
        real(8), dimension(:), intent(in), optional :: bottom
        character(len=*), intent(in), optional :: label, align
        real(8), dimension(3), intent(in), optional :: color, edgecolor
        
        error stop "bar plot functionality is not yet implemented"
    end subroutine bar

    subroutine barh(y, width, height, left, label, color, edgecolor, align)
        !! Add horizontal bar plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: y, width
        real(8), intent(in), optional :: height
        real(8), dimension(:), intent(in), optional :: left
        character(len=*), intent(in), optional :: label, align
        real(8), dimension(3), intent(in), optional :: color, edgecolor
        
        error stop "horizontal bar plot functionality is not yet implemented"
    end subroutine barh

    subroutine hist(data, bins, density, label, color)
        !! Add histogram plot to the global figure (pyplot-style)
        real(8), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(8), intent(in), optional :: color(3)
        
        error stop "histogram functionality is not yet implemented"
    end subroutine hist

    subroutine histogram(data, bins, density, label, color)
        !! Add histogram plot to the global figure (pyplot-style)
        !! Alias for hist() subroutine
        real(8), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(8), intent(in), optional :: color(3)
        
        error stop "histogram functionality is not yet implemented"
    end subroutine histogram

    subroutine boxplot(data, position, width, label, show_outliers, horizontal, color)
        !! Add a box plot to the global figure (matplotlib-style)
        real(wp), dimension(:), intent(in) :: data
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        real(wp), intent(in), optional :: color(3)
        
        ! TODO: Implement boxplot method in figure_core
        call log_error("boxplot() not yet implemented - please use main branch for boxplot support")
    end subroutine boxplot

    subroutine scatter(x, y, s, c, label, marker, markersize, color, &
                      colormap, vmin, vmax, show_colorbar)
        !! Add enhanced scatter plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: s, c
        character(len=*), intent(in), optional :: label, marker, colormap
        real(8), intent(in), optional :: markersize, vmin, vmax
        real(8), dimension(3), intent(in), optional :: color
        logical, intent(in), optional :: show_colorbar
        
        call ensure_global_figure_initialized()
        ! TODO: scatter method not implemented in working figure core
    end subroutine scatter

    subroutine add_scatter(x, y, s, c, label, marker, markersize, color, &
                          colormap, vmin, vmax, show_colorbar)
        !! Add enhanced scatter plot to the global figure (wrapper for consistency)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: s, c
        character(len=*), intent(in), optional :: label, marker, colormap
        real(8), intent(in), optional :: markersize, vmin, vmax
        real(8), dimension(3), intent(in), optional :: color
        logical, intent(in), optional :: show_colorbar
        
        call ensure_global_figure_initialized()
        ! TODO: scatter method not implemented in working figure core
    end subroutine add_scatter

    subroutine text(x, y, text_content, coord_type, font_size, rotation, alignment, has_bbox)
        !! Add text annotation to the global figure (matplotlib-style)
        real(8), intent(in) :: x, y
        character(len=*), intent(in) :: text_content
        integer, intent(in), optional :: coord_type
        real(8), intent(in), optional :: font_size, rotation
        character(len=*), intent(in), optional :: alignment
        logical, intent(in), optional :: has_bbox
        
        call ensure_global_figure_initialized()
        ! TODO: text annotation method not implemented in working figure core
    end subroutine text

    subroutine annotate(text_content, xy, xytext, xy_coord_type, xytext_coord_type, &
                       font_size, alignment, has_bbox)
        !! Add arrow annotation to the global figure (matplotlib-style)
        character(len=*), intent(in) :: text_content
        real(8), intent(in) :: xy(2), xytext(2)
        integer, intent(in), optional :: xy_coord_type, xytext_coord_type
        real(8), intent(in), optional :: font_size
        character(len=*), intent(in), optional :: alignment
        logical, intent(in), optional :: has_bbox
        
        call ensure_global_figure_initialized()
        ! TODO: arrow annotation method not implemented in working figure core
    end subroutine annotate

    ! Additional wrappers for add_* functions
    subroutine add_plot(x, y, label, linestyle)
        !! Add a line plot to the global figure (fortplot_api compatible)
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle
        
        call ensure_global_figure_initialized()
        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine add_plot

    subroutine add_contour(x, y, z, levels, label)
        !! Add a contour plot to the global figure
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label
        
        call ensure_global_figure_initialized()
        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine add_contour

    subroutine add_contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        !! Add a filled contour plot to the global figure
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        real(wp), allocatable :: wp_x(:), wp_y(:), wp_z(:,:), wp_levels(:)
        
        call ensure_global_figure_initialized()
        
        ! Convert input arrays to working precision
        allocate(wp_x(size(x)), wp_y(size(y)), wp_z(size(z,1), size(z,2)))
        wp_x = real(x, wp)
        wp_y = real(y, wp)
        wp_z = real(z, wp)
        
        if (present(levels)) then
            allocate(wp_levels(size(levels)))
            wp_levels = real(levels, wp)
        else
            allocate(wp_levels(0))
        end if
        
        ! Forward parameters to underlying method using conditional calls for memory safety
        if (present(levels) .and. present(colormap) .and. present(show_colorbar) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, &
                colormap=colormap, show_colorbar=show_colorbar, label=label)
        else if (present(levels) .and. present(colormap) .and. present(show_colorbar)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, &
                colormap=colormap, show_colorbar=show_colorbar)
        else if (present(levels) .and. present(colormap) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, &
                colormap=colormap, label=label)
        else if (present(colormap) .and. present(show_colorbar) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, colormap=colormap, &
                show_colorbar=show_colorbar, label=label)
        else if (present(levels) .and. present(colormap)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, colormap=colormap)
        else if (present(levels) .and. present(show_colorbar)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, show_colorbar=show_colorbar)
        else if (present(levels) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels, label=label)
        else if (present(colormap) .and. present(show_colorbar)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, colormap=colormap, show_colorbar=show_colorbar)
        else if (present(colormap) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, colormap=colormap, label=label)
        else if (present(show_colorbar) .and. present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, show_colorbar=show_colorbar, label=label)
        else if (present(levels)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, levels=wp_levels)
        else if (present(colormap)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, colormap=colormap)
        else if (present(show_colorbar)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, show_colorbar=show_colorbar)
        else if (present(label)) then
            call fig%add_contour_filled(wp_x, wp_y, wp_z, label=label)
        else
            call fig%add_contour_filled(wp_x, wp_y, wp_z)
        end if
        
        deallocate(wp_x, wp_y, wp_z)
        if (allocated(wp_levels)) deallocate(wp_levels)
    end subroutine add_contour_filled

    subroutine add_pcolormesh(x, y, z, shading, colormap, show_colorbar, label, &
                             vmin, vmax, edgecolors, linewidths)
        !! Add a pseudocolor mesh plot to the global figure
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        character(len=*), intent(in), optional :: shading, colormap, label
        logical, intent(in), optional :: show_colorbar
        real(8), intent(in), optional :: vmin, vmax
        real(8), dimension(3), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: linewidths
        
        real(wp), allocatable :: wp_x(:), wp_y(:), wp_z(:,:)
        real(wp) :: wp_vmin, wp_vmax, wp_linewidths
        real(wp) :: wp_edgecolors(3)
        
        call ensure_global_figure_initialized()
        
        ! Convert input arrays to working precision
        allocate(wp_x(size(x)), wp_y(size(y)), wp_z(size(z,1), size(z,2)))
        wp_x = real(x, wp)
        wp_y = real(y, wp)
        wp_z = real(z, wp)
        
        ! Convert optional parameters to working precision
        if (present(vmin)) then
            wp_vmin = real(vmin, wp)
        end if
        if (present(vmax)) then
            wp_vmax = real(vmax, wp)
        end if
        if (present(edgecolors)) then
            wp_edgecolors = real(edgecolors, wp)
        end if
        if (present(linewidths)) then
            wp_linewidths = real(linewidths, wp)
        end if
        
        ! Forward SUPPORTED parameters to underlying method using conditional calls
        if (present(colormap) .and. present(vmin) .and. present(vmax) .and. &
            present(edgecolors) .and. present(linewidths)) then
            call fig%add_pcolormesh(wp_x, wp_y, wp_z, &
                colormap=colormap, vmin=wp_vmin, vmax=wp_vmax, &
                edgecolors=wp_edgecolors, linewidths=wp_linewidths)
        else if (present(colormap) .and. present(vmin) .and. present(vmax)) then
            call fig%add_pcolormesh(wp_x, wp_y, wp_z, &
                colormap=colormap, vmin=wp_vmin, vmax=wp_vmax)
        else if (present(colormap)) then
            call fig%add_pcolormesh(wp_x, wp_y, wp_z, colormap=colormap)
        else if (present(vmin) .and. present(vmax)) then
            call fig%add_pcolormesh(wp_x, wp_y, wp_z, vmin=wp_vmin, vmax=wp_vmax)
        else
            call fig%add_pcolormesh(wp_x, wp_y, wp_z)
        end if
        
        deallocate(wp_x, wp_y, wp_z)
    end subroutine add_pcolormesh

    subroutine add_errorbar(x, y, xerr, yerr, fmt, label, capsize, linestyle, marker, color)
        !! Add error bars to the global figure
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: xerr, yerr
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(8), intent(in), optional :: capsize
        real(8), dimension(3), intent(in), optional :: color
        
        call ensure_global_figure_initialized()
        ! TODO: errorbar method not yet implemented in working figure core
        ! call fig%errorbar(x, y, xerr, yerr, label=label)
    end subroutine add_errorbar

    subroutine add_3d_plot(x, y, z, label, linestyle, color, linewidth, marker, markersize)
        !! Add 3D line plot to the global figure
        real(8), dimension(:), intent(in) :: x, y, z
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(8), dimension(3), intent(in), optional :: color
        real(8), intent(in), optional :: linewidth, markersize
        
        call ensure_global_figure_initialized()
        ! TODO: add_3d_plot method not yet implemented in working figure core  
        ! call fig%add_3d_plot(x, y, z, label=label)
    end subroutine add_3d_plot

    subroutine add_surface(x, y, z, colormap, show_colorbar, alpha, edgecolor, linewidth, label)
        !! Add 3D surface plot to the global figure
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        real(8), intent(in), optional :: alpha, linewidth
        real(8), dimension(3), intent(in), optional :: edgecolor
        
        call ensure_global_figure_initialized()
        ! TODO: add_surface method not yet implemented in working figure core
        ! call fig%add_surface(x, y, z, label=label)
    end subroutine add_surface

    ! Figure management functions
    subroutine figure(num, figsize, dpi)
        !! Initialize or select a figure (matplotlib-style)
        integer, intent(in), optional :: num
        real(8), dimension(2), intent(in), optional :: figsize
        integer, intent(in), optional :: dpi
        
        integer :: fig_width, fig_height
        integer :: actual_dpi
        real(8) :: width_val, height_val
        
        ! Ensure global figure is allocated before initialization
        call ensure_global_figure_initialized()
        
        ! Default DPI (matches matplotlib default)
        actual_dpi = 100
        if (present(dpi)) then
            actual_dpi = dpi
        end if
        
        fig_width = 800
        fig_height = 600
        
        if (present(figsize)) then
            width_val = figsize(1)
            height_val = figsize(2)
            
            ! Smart interpretation: values > 100 are likely pixels, not inches
            ! Standard figure sizes in inches are typically 6-20 inches
            if (width_val > 100.0d0 .or. height_val > 100.0d0) then
                ! Interpret as pixels directly
                fig_width = int(width_val)
                fig_height = int(height_val)
            else
                ! Interpret as inches, convert to pixels using DPI
                fig_width = int(width_val * real(actual_dpi, 8))
                fig_height = int(height_val * real(actual_dpi, 8))
            end if
            
            ! Apply reasonable limits to prevent overflow
            ! Maximum 10000x10000 pixels for safety
            fig_width = min(max(fig_width, 100), 10000)
            fig_height = min(max(fig_height, 100), 10000)
        end if
        
        call fig%initialize(fig_width, fig_height)
    end subroutine figure

    subroutine subplot(nrows, ncols, index)
        !! Create subplot (placeholder for future implementation)
        integer, intent(in) :: nrows, ncols, index
        
        call log_warning("subplot() is not yet fully implemented")
        call ensure_global_figure_initialized()
    end subroutine subplot

    ! Axis configuration functions
    subroutine xlabel(label_text)
        !! Set the x-axis label for the global figure
        character(len=*), intent(in) :: label_text
        
        call ensure_global_figure_initialized()
        call fig%set_xlabel(label_text)
    end subroutine xlabel

    subroutine ylabel(label_text)
        !! Set the y-axis label for the global figure
        character(len=*), intent(in) :: label_text
        
        call ensure_global_figure_initialized()
        call fig%set_ylabel(label_text)
    end subroutine ylabel

    subroutine title(title_text)
        !! Set the title for the global figure
        character(len=*), intent(in) :: title_text
        
        call ensure_global_figure_initialized()
        call fig%set_title(title_text)
    end subroutine title

    subroutine legend(position, box, fontsize)
        !! Add legend to the global figure
        character(len=*), intent(in), optional :: position
        logical, intent(in), optional :: box
        real(8), intent(in), optional :: fontsize
        
        call ensure_global_figure_initialized()
        call fig%legend(location=position)
    end subroutine legend

    subroutine xlim(xmin, xmax)
        !! Set x-axis limits for the global figure
        real(8), intent(in) :: xmin, xmax
        
        call ensure_global_figure_initialized()
        call fig%set_xlim(xmin, xmax)
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        !! Set y-axis limits for the global figure
        real(8), intent(in) :: ymin, ymax
        
        call ensure_global_figure_initialized()
        call fig%set_ylim(ymin, ymax)
    end subroutine ylim

    subroutine set_xscale(scale, threshold)
        !! Set x-axis scale type
        !! @param scale Scale type: 'linear', 'log', or 'symlog'
        !! @param threshold Optional threshold for symlog scale
        character(len=*), intent(in) :: scale
        real(8), intent(in), optional :: threshold
        
        call ensure_global_figure_initialized()
        
        ! Validate scale type
        select case (trim(scale))
        case ('linear', 'log', 'symlog')
            if (present(threshold)) then
                call fig%set_xscale(scale, threshold)
            else
                call fig%set_xscale(scale)
            end if
        case default
            call log_warning("Unknown scale type: " // trim(scale) // ". Using 'linear'.")
            call fig%set_xscale('linear')
        end select
    end subroutine set_xscale

    subroutine set_yscale(scale, threshold)
        !! Set y-axis scale type
        !! @param scale Scale type: 'linear', 'log', or 'symlog'
        !! @param threshold Optional threshold for symlog scale
        character(len=*), intent(in) :: scale
        real(8), intent(in), optional :: threshold
        
        call ensure_global_figure_initialized()
        
        ! Validate scale type
        select case (trim(scale))
        case ('linear', 'log', 'symlog')
            if (present(threshold)) then
                call fig%set_yscale(scale, threshold)
            else
                call fig%set_yscale(scale)
            end if
        case default
            call log_warning("Unknown scale type: " // trim(scale) // ". Using 'linear'.")
            call fig%set_yscale('linear')
        end select
    end subroutine set_yscale

    subroutine set_line_width(width)
        !! Set line width for subsequent plots (placeholder)
        real(8), intent(in) :: width
        
        call log_warning("set_line_width() is not yet fully implemented")
    end subroutine set_line_width

    subroutine set_ydata(ydata)
        !! Update y data for existing plot (placeholder)
        real(8), dimension(:), intent(in) :: ydata
        
        call log_warning("set_ydata() is not yet fully implemented")
    end subroutine set_ydata

    ! Output functions
    subroutine savefig(filename, dpi, transparent, bbox_inches)
        !! Save the global figure to file
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: dpi
        logical, intent(in), optional :: transparent
        character(len=*), intent(in), optional :: bbox_inches
        
        call ensure_global_figure_initialized()
        call fig%savefig(filename)
    end subroutine savefig

    ! Display functions
    subroutine show_data(x, y, label, title_text, xlabel_text, ylabel_text, blocking)
        !! Display a line plot in the terminal using ASCII graphics
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, title_text, xlabel_text, ylabel_text
        logical, intent(in), optional :: blocking
        
        call fig%initialize()
        
        if (present(title_text)) call fig%set_title(title_text)
        if (present(xlabel_text)) call fig%set_xlabel(xlabel_text)
        if (present(ylabel_text)) call fig%set_ylabel(ylabel_text)
        
        call fig%add_plot(x, y, label=label)
        call fig%show()
    end subroutine show_data

    subroutine show_figure(blocking)
        !! Display the global figure intelligently
        logical, intent(in), optional :: blocking
        
        call ensure_global_figure_initialized()
        if (is_gui_available()) then
            call show_viewer_implementation(blocking=blocking)
        else
            call fig%show()
        end if
    end subroutine show_figure

    subroutine show_viewer(blocking)
        !! Display the current figure in the system's default viewer
        logical, intent(in), optional :: blocking
        
        call ensure_global_figure_initialized()
        call show_viewer_implementation(blocking=blocking)
    end subroutine show_viewer

    ! Internal utility functions
    function is_gui_available() result(gui_available)
        !! Check if GUI display is available for viewer output
        logical :: gui_available
        character(len=256) :: display_var
        integer :: status
        
        gui_available = .false.
        
        call get_environment_variable('DISPLAY', display_var, status=status)
        if (status == 0 .and. len_trim(display_var) > 0) then
            gui_available = .true.
        end if
    end function is_gui_available

    subroutine show_viewer_implementation(blocking)
        !! Internal implementation for showing plot in system viewer
        use iso_fortran_env, only: int64
        
        logical, intent(in), optional :: blocking
        logical :: do_block, success
        character(len=256) :: temp_filename
        character(len=32) :: timestamp
        integer :: stat
        integer(int64) :: time_val
        
        do_block = .false.
        if (present(blocking)) do_block = blocking
        
        call system_clock(time_val)
        write(timestamp, '(I0)') time_val
        
        call get_environment_variable('WINDIR', temp_filename, status=stat)
        if (stat == 0 .and. len_trim(temp_filename) > 0) then
            temp_filename = 'fortplot_' // trim(timestamp) // '.png'
        else
            temp_filename = '/tmp/fortplot_' // trim(timestamp) // '.png'
        end if
        
        call fig%savefig(temp_filename)
        call safe_launch_viewer(temp_filename, success)
        
        if (success) then
            stat = 0
        else
            stat = 1
        end if
        
        if (stat /= 0) then
            call log_warning('Failed to open plot viewer. Plot saved to: ' // trim(temp_filename))
            call log_info('Please open the file manually with your preferred PDF viewer.')
        else
            call log_info('Plot opened in default viewer. File: ' // trim(temp_filename))
        end if
        
        if (do_block) then
            call log_info('Press Enter to continue and clean up temporary file...')
            read(*,*)
            
            call safe_remove_file(temp_filename, success)
            if (.not. success) then
                call log_warning('Could not remove temporary file: ' // trim(temp_filename))
            end if
        else
            call log_info('Note: Temporary file will remain at: ' // trim(temp_filename))
        end if
    end subroutine show_viewer_implementation

end module fortplot_matplotlib