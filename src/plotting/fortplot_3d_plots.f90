module fortplot_3d_plots
    !! 3D plot operations module
    !! 
    !! This module handles all 3D plot operations including 3D line plots
    !! and surface plots (rendered as contour representations).

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: PLOT_TYPE_CONTOUR
    use fortplot_2d_plots, only: add_line_plot_data
    use fortplot_projection, only: project_3d_to_2d, get_default_view_angles
    use fortplot_logging, only: log_error

    implicit none

    private
    public :: add_3d_plot
    public :: add_surface
    public :: add_3d_line_plot_data
    public :: add_surface_plot_data

contains

    subroutine add_3d_plot(self, x, y, z, label, linestyle, marker, markersize, linewidth)
        !! Add 3D line plot to figure (projected to 2D)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: linestyle
        character(len=*), intent(in), optional :: marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: linewidth
        
        call add_3d_line_plot_data(self, x, y, z, label, linestyle, &
                                  marker=marker, markersize=markersize, linewidth=linewidth)
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
        integer :: n_points, plot_idx, previous_count
        real(wp), allocatable :: x_proj(:), y_proj(:)
        real(wp) :: azim, elev, dist

        n_points = size(x)
        if (size(y) /= n_points .or. size(z) /= n_points) then
            call log_error('add_3d_plot: x, y, and z must have matching sizes')
            return
        end if

        if (n_points == 0) then
            call log_error('add_3d_plot: coordinate arrays must not be empty')
            return
        end if

        allocate(x_proj(n_points))
        allocate(y_proj(n_points))
        call get_default_view_angles(azim, elev, dist)
        call project_3d_to_2d(x, y, z, azim, elev, dist, x_proj, y_proj)

        previous_count = self%plot_count
        call add_line_plot_data(self, x_proj, y_proj, label, linestyle, marker=marker)

        if (self%plot_count <= previous_count) then
            return
        end if

        plot_idx = self%plot_count
        if (.not. allocated(self%plots)) then
            return
        end if
        if (plot_idx < 1 .or. plot_idx > size(self%plots)) then
            return
        end if

        self%plots(plot_idx)%z = z

        if (present(markersize)) then
            associate(unused_ms => markersize); end associate
        end if
        if (present(linewidth)) then
            associate(unused_lw => linewidth); end associate
        end if
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
