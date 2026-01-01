module fortplot_scatter_plots
    !! Scatter plot operations module
    !!
    !! This module handles all scatter plot operations including 2D and 3D
    !! scatter plots with size and color mapping capabilities.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_figure_core_advanced, only: core_scatter
    use fortplot_figure_plot_management, only: next_plot_color
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    use fortplot_logging, only: log_error

    implicit none

    private
    public :: add_scatter_2d_impl
    public :: add_scatter_3d_impl
    public :: add_scatter_plot_data

    interface add_scatter_2d
        module procedure add_scatter_2d_impl
    end interface
    public :: add_scatter_2d

    interface add_scatter_3d
        module procedure add_scatter_3d_impl
    end interface
    public :: add_scatter_3d

contains

    subroutine add_scatter_2d_impl(self, x, y, s, c, label, marker, markersize, color, &
                                   colormap, vmin, vmax, show_colorbar, alpha)
        !! Add 2D scatter plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:)  ! size array
        real(wp), intent(in), optional :: c(:)  ! color array
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)  ! single RGB color
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: alpha

        call add_scatter_plot_data(self, x, y, s=s, c=c, label=label, &
                                   marker=marker, markersize=markersize, color=color, &
                                   colormap=colormap, vmin=vmin, vmax=vmax, &
                                   show_colorbar=show_colorbar, alpha=alpha)
    end subroutine add_scatter_2d_impl

    subroutine add_scatter_3d_impl(self, x, y, z, s, c, label, marker, &
                                   markersize, &
                                   color, colormap, vmin, vmax, &
                                   show_colorbar, alpha)
        !! Add 3D scatter plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)
        real(wp), intent(in), optional :: s(:)  ! size array
        real(wp), intent(in), optional :: c(:)  ! color array
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)  ! single RGB color
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: alpha

        if (present(alpha)) then
            associate (unused => alpha); end associate
        end if
        call add_scatter_plot_data(self, x, y, z, s, c, label, marker, &
                                   markersize, &
                                   color, colormap, vmin, vmax, &
                                   show_colorbar, alpha)
    end subroutine add_scatter_3d_impl

    subroutine add_scatter_plot_data(self, x, y, z, s, c, label, marker, markersize, &
                                     color, colormap, vmin, vmax, show_colorbar, alpha)
        !! Add scatter plot data with optional properties
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: z(:), s(:), c(:), markersize
        real(wp), intent(in), optional :: color(3), vmin, vmax, alpha
        character(len=*), intent(in), optional :: label, marker, colormap
        logical, intent(in), optional :: show_colorbar

        real(wp) :: default_color(3)
        real(wp), allocatable :: x_proj(:), y_proj(:)
        real(wp) :: azim, elev, dist
        logical :: use_projection
        integer :: plot_idx, n_points, previous_count

        use_projection = present(z)
        if (.not. allocated(self%plots)) then
            allocate (self%plots(self%state%max_plots))
        end if

        self%state%plot_count = self%plot_count
        default_color = next_plot_color(self%state)

        if (use_projection) then
            n_points = size(x)
            if (size(y) /= n_points .or. size(z) /= n_points) then
                call log_error('add_scatter: x, y, and z must have matching sizes')
                return
            end if
            if (n_points == 0) then
                call log_error('add_scatter: coordinate arrays must not be empty')
                return
            end if

            allocate (x_proj(n_points))
            allocate (y_proj(n_points))
            call get_default_view_angles(azim, elev, dist)
            call project_3d_to_2d(x, y, z, azim, elev, dist, x_proj, y_proj)
        end if

        previous_count = self%plot_count

        if (use_projection) then
            call core_scatter(self%plots, self%state, self%plot_count, x_proj, y_proj, &
                              s=s, c=c, marker=marker, markersize=markersize, &
                              color=color, colormap=colormap, vmin=vmin, vmax=vmax, &
                              label=label, show_colorbar=show_colorbar, &
                              default_color=default_color)
        else
            call core_scatter(self%plots, self%state, self%plot_count, x, y, s=s, c=c, &
                              marker=marker, markersize=markersize, color=color, &
                              colormap=colormap, vmin=vmin, vmax=vmax, label=label, &
                              show_colorbar=show_colorbar, default_color=default_color)
        end if

        if (self%plot_count <= previous_count) then
            return
        end if

        if (.not. allocated(self%plots)) then
            return
        end if

        plot_idx = self%plot_count
        if (plot_idx < 1 .or. plot_idx > size(self%plots)) then
            return
        end if

        if (present(z)) then
            self%plots(plot_idx)%z = z
        end if

        if (present(alpha)) then
            ! Alpha retained for API parity; currently ignored
            associate (unused => alpha); end associate
        end if

        self%state%rendered = .false.
    end subroutine add_scatter_plot_data

end module fortplot_scatter_plots
