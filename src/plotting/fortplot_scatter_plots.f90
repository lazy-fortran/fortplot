module fortplot_scatter_plots
    !! Scatter plot operations module
    !! 
    !! This module handles all scatter plot operations including 2D and 3D
    !! scatter plots with size and color mapping capabilities.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: PLOT_TYPE_SCATTER

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

    subroutine add_scatter_3d_impl(self, x, y, z, s, c, label, marker, markersize, color, &
                                   colormap, vmin, vmax, show_colorbar, alpha)
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
            real(wp) :: alpha_dummy
            alpha_dummy = alpha
        end if
        call add_scatter_plot_data(self, x, y, z, s, c, label, marker, markersize, color, &
                                  colormap, vmin, vmax, show_colorbar, alpha)
    end subroutine add_scatter_3d_impl

    subroutine add_scatter_plot_data(self, x, y, z, s, c, label, marker, markersize, color, &
                                    colormap, vmin, vmax, show_colorbar, alpha)
        !! Add scatter plot data with optional properties
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: z(:), s(:), c(:), markersize, color(3), vmin, vmax, alpha
        character(len=*), intent(in), optional :: label, marker, colormap
        logical, intent(in), optional :: show_colorbar
        
        integer :: plot_idx
        
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_SCATTER
        
        allocate(self%plots(plot_idx)%x(size(x)))
        allocate(self%plots(plot_idx)%y(size(y)))
        
        self%plots(plot_idx)%x = x
        self%plots(plot_idx)%y = y
        
        if (present(z)) then
            allocate(self%plots(plot_idx)%z(size(z)))
            self%plots(plot_idx)%z = z
        end if
        
        if (present(s)) then
            allocate(self%plots(plot_idx)%scatter_sizes(size(s)))
            self%plots(plot_idx)%scatter_sizes = s
        end if
        
        if (present(c)) then
            allocate(self%plots(plot_idx)%scatter_colors(size(c)))
            self%plots(plot_idx)%scatter_colors = c
        end if
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        end if
        
        if (present(marker)) then
            self%plots(plot_idx)%marker = marker
        end if
        
        if (present(markersize)) then
            self%plots(plot_idx)%scatter_size_default = markersize
        end if
        
        if (present(colormap)) then
            self%plots(plot_idx)%scatter_colormap = colormap
        end if
        
        if (present(vmin)) then
            self%plots(plot_idx)%scatter_vmin = vmin
            self%plots(plot_idx)%scatter_vrange_set = .true.
        end if
        
        if (present(vmax)) then
            self%plots(plot_idx)%scatter_vmax = vmax
            self%plots(plot_idx)%scatter_vrange_set = .true.
        end if
        
        if (present(show_colorbar)) then
            self%plots(plot_idx)%scatter_colorbar = show_colorbar
        end if
        
        ! Note: alpha not directly supported in plot_data_t structure
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine add_scatter_plot_data

end module fortplot_scatter_plots
