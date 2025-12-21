module fortplot_plot_contours
    !! Contour and pcolormesh plotting functionality
    !! 
    !! Provides:
    !! - Basic contour plots
    !! - Filled contour plots with colormaps
    !! - Pcolormesh (pseudocolor mesh) plots
    !! - Colormap and colorbar support
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH
    use fortplot_figure_plot_management, only: generate_default_contour_levels
    use fortplot_errors, only: fortplot_error_t, SUCCESS, ERROR_RESOURCE_LIMIT
    implicit none
    
    private
    public :: add_contour_impl, add_contour_filled_impl, add_pcolormesh_impl
    
contains
    
    subroutine add_contour_impl(self, x_grid, y_grid, z_grid, levels, label)
        !! Add basic contour plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        call add_contour_plot_data(self, x_grid, y_grid, z_grid, levels, label)
    end subroutine add_contour_impl
    
    subroutine add_contour_filled_impl(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        !! Add filled contour plot with colors
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap
        logical, intent(in), optional :: show_colorbar
        character(len=*), intent(in), optional :: label
        
        call add_colored_contour_plot_data(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
    end subroutine add_contour_filled_impl
    
    subroutine add_pcolormesh_impl(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        !! Add pseudocolor mesh plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        character(len=*), intent(in), optional :: edgecolors
        real(wp), intent(in), optional :: linewidths
        
        type(fortplot_error_t) :: error
        call add_pcolormesh_plot_data(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths, error)
    end subroutine add_pcolormesh_impl
    
    ! Private helper subroutines
    
    subroutine add_contour_plot_data(self, x_grid, y_grid, z_grid, levels, label)
        !! Add contour plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        integer :: plot_idx
        
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_CONTOUR
        
        allocate(self%plots(plot_idx)%x_grid(size(x_grid)))
        allocate(self%plots(plot_idx)%y_grid(size(y_grid)))
        allocate(self%plots(plot_idx)%z_grid(size(z_grid, 1), size(z_grid, 2)))
        
        self%plots(plot_idx)%x_grid = x_grid
        self%plots(plot_idx)%y_grid = y_grid
        self%plots(plot_idx)%z_grid = z_grid

        if (present(levels)) then
            if (size(levels) > 0) then
                allocate(self%plots(plot_idx)%contour_levels(size(levels)))
                self%plots(plot_idx)%contour_levels = levels
            else
                call generate_default_contour_levels(self%plots(plot_idx))
            end if
        else
            call generate_default_contour_levels(self%plots(plot_idx))
        end if

        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine add_contour_plot_data
    
    subroutine add_colored_contour_plot_data(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        !! Add colored contour plot data (filled contour)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap
        logical, intent(in), optional :: show_colorbar
        character(len=*), intent(in), optional :: label
        
        integer :: plot_idx
        
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_CONTOUR
        
        allocate(self%plots(plot_idx)%x_grid(size(x_grid)))
        allocate(self%plots(plot_idx)%y_grid(size(y_grid)))
        allocate(self%plots(plot_idx)%z_grid(size(z_grid, 1), size(z_grid, 2)))
        
        self%plots(plot_idx)%x_grid = x_grid
        self%plots(plot_idx)%y_grid = y_grid
        self%plots(plot_idx)%z_grid = z_grid

        if (present(levels)) then
            if (size(levels) > 0) then
                allocate(self%plots(plot_idx)%contour_levels(size(levels)))
                self%plots(plot_idx)%contour_levels = levels
            else
                call generate_default_contour_levels(self%plots(plot_idx))
            end if
        else
            call generate_default_contour_levels(self%plots(plot_idx))
        end if

        ! Color properties
        self%plots(plot_idx)%use_color_levels = .true.
        self%plots(plot_idx)%fill_contours = .true.
        
        if (present(colormap)) then
            self%plots(plot_idx)%colormap = colormap
        else
            self%plots(plot_idx)%colormap = 'crest'
        end if
        
        if (present(show_colorbar)) then
            self%plots(plot_idx)%show_colorbar = show_colorbar
        else
            self%plots(plot_idx)%show_colorbar = .true.
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine add_colored_contour_plot_data
    
    subroutine add_pcolormesh_plot_data(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths, error)
        !! Add pcolormesh plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        character(len=*), intent(in), optional :: edgecolors
        real(wp), intent(in), optional :: linewidths
        type(fortplot_error_t), intent(out) :: error
        
        integer :: plot_idx, i, j
        
        error%status = SUCCESS
        
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            error%status = ERROR_RESOURCE_LIMIT
            error%message = "Maximum plot count exceeded"
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_PCOLORMESH
        
        ! Store pcolormesh data
        ! Note: x and y are edge coordinates, so dimensions are (ny+1, nx+1)
        allocate(self%plots(plot_idx)%pcolormesh_data%x_vertices(size(y), size(x)))
        allocate(self%plots(plot_idx)%pcolormesh_data%y_vertices(size(y), size(x)))
        allocate(self%plots(plot_idx)%pcolormesh_data%c_values(size(c, 1), size(c, 2)))
        
        ! Create mesh grid from 1D arrays
        do i = 1, size(y)
            self%plots(plot_idx)%pcolormesh_data%x_vertices(i, :) = x
        end do
        do j = 1, size(x)
            self%plots(plot_idx)%pcolormesh_data%y_vertices(:, j) = y
        end do
        self%plots(plot_idx)%pcolormesh_data%c_values = c
        self%plots(plot_idx)%pcolormesh_data%nx = size(c, 2)
        self%plots(plot_idx)%pcolormesh_data%ny = size(c, 1)
        
        if (present(colormap)) then
            self%plots(plot_idx)%pcolormesh_data%colormap_name = colormap
        else
            self%plots(plot_idx)%pcolormesh_data%colormap_name = 'viridis'
        end if
        
        if (present(vmin)) then
            self%plots(plot_idx)%pcolormesh_data%vmin = vmin
            self%plots(plot_idx)%pcolormesh_data%vmin_set = .true.
        else
            self%plots(plot_idx)%pcolormesh_data%vmin = minval(c)
            self%plots(plot_idx)%pcolormesh_data%vmin_set = .true.
        end if
        
        if (present(vmax)) then
            self%plots(plot_idx)%pcolormesh_data%vmax = vmax
            self%plots(plot_idx)%pcolormesh_data%vmax_set = .true.
        else
            self%plots(plot_idx)%pcolormesh_data%vmax = maxval(c)
            self%plots(plot_idx)%pcolormesh_data%vmax_set = .true.
        end if

        ! Match matplotlib: do not force symmetric normalization; use full data min/max unless user overrides.
        
        if (present(edgecolors)) then
            ! Handle edge colors - if 'none', disable edges
            if (edgecolors == 'none') then
                self%plots(plot_idx)%pcolormesh_data%show_edges = .false.
            else
                self%plots(plot_idx)%pcolormesh_data%show_edges = .true.
                ! Could parse color string here if needed
            end if
        else
            self%plots(plot_idx)%pcolormesh_data%show_edges = .false.
        end if
        
        if (present(linewidths)) then
            self%plots(plot_idx)%pcolormesh_data%edge_width = linewidths
        else
            self%plots(plot_idx)%pcolormesh_data%edge_width = 0.5_wp
        end if
        
    end subroutine add_pcolormesh_plot_data

end module fortplot_plot_contours
