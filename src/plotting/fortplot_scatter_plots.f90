module fortplot_scatter_plots
    !! Scatter plot operations module
    !! 
    !! This module handles all scatter plot operations including 2D and 3D
    !! scatter plots with size and color mapping capabilities.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_figure_core_advanced, only: core_scatter
    use fortplot_figure_plot_management, only: next_plot_color

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

    subroutine add_scatter_3d_impl(self, x, y, z, s, c, label, marker, markersize, &
                                   color, colormap, vmin, vmax, show_colorbar, alpha)
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
        real(wp) :: alpha_dummy1
        if (present(alpha)) alpha_dummy1 = alpha
        call add_scatter_plot_data(self, x, y, z, s, c, label, marker, markersize, &
                                   color, colormap, vmin, vmax, show_colorbar, alpha)
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

        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        end if

        self%state%plot_count = self%plot_count
        default_color = next_plot_color(self%state)

        call core_scatter(self%plots, self%state, self%plot_count, x, y, s=s, c=c, &
                          marker=marker, markersize=markersize, color=color, &
                          colormap=colormap, vmin=vmin, vmax=vmax, label=label, &
                          show_colorbar=show_colorbar, default_color=default_color)

        if (present(z)) then
            if (.not. allocated(self%plots(self%plot_count)%z)) then
                allocate(self%plots(self%plot_count)%z(size(z)))
            end if
            self%plots(self%plot_count)%z = z
        end if

        if (present(alpha)) then
            ! Alpha is retained for API parity; implementation pending
        end if

        self%state%rendered = .false.
    end subroutine add_scatter_plot_data

end module fortplot_scatter_plots
