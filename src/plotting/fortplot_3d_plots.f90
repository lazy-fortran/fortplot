module fortplot_3d_plots
    !! 3D plot operations module
    !! 
    !! This module handles all 3D plot operations including 3D line plots
    !! and surface plots (rendered as contour representations).

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: PLOT_TYPE_CONTOUR
    use fortplot_2d_plots, only: add_line_plot_data

    implicit none

    private
    public :: add_3d_plot
    public :: add_surface
    public :: add_3d_line_plot_data
    public :: add_surface_plot_data

contains

    subroutine add_3d_plot(self, x, y, z, label, linestyle, markersize, linewidth)
        !! Add 3D line plot to figure (projected to 2D)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: linestyle
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: linewidth
        
        call add_3d_line_plot_data(self, x, y, z, label, linestyle, &
                                  marker='', markersize=markersize, linewidth=linewidth)
    end subroutine add_3d_plot

    subroutine add_surface(self, x, y, z, label)
        !! Add surface plot to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:,:)
        character(len=*), intent(in), optional :: label
        
        call add_surface_plot_data(self, x, y, z, label)
    end subroutine add_surface

    subroutine add_3d_line_plot_data(self, x, y, z, label, linestyle, marker, markersize, linewidth)
        !! Add 3D line plot data by projecting to 2D
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(wp), intent(in), optional :: markersize, linewidth
        real(wp) :: z_dummy, ms_dummy, lw_dummy
        character(len=1) :: m_dummy
        if (present(markersize)) ms_dummy = markersize
        if (present(linewidth)) lw_dummy = linewidth
        if (present(marker)) m_dummy = marker(1:1)
        if (size(z) > 0) z_dummy = z(1)
        
        ! Project 3D to 2D for basic plotting (z-axis ignored for now)
        call add_line_plot_data(self, x, y, label, linestyle)
    end subroutine add_3d_line_plot_data

    subroutine add_surface_plot_data(self, x, y, z, label)
        !! Add surface plot data (simplified as contour representation)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:,:)
        character(len=*), intent(in), optional :: label
        
        integer :: plot_idx
        
        ! Get current plot index
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_CONTOUR
        
        ! Store grid data
        allocate(self%plots(plot_idx)%x_grid(size(x)))
        allocate(self%plots(plot_idx)%y_grid(size(y)))
        allocate(self%plots(plot_idx)%z_grid(size(z, 1), size(z, 2)))
        
        self%plots(plot_idx)%x_grid = x
        self%plots(plot_idx)%y_grid = y
        self%plots(plot_idx)%z_grid = z
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine add_surface_plot_data

end module fortplot_3d_plots
