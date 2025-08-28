module fortplot_plotting_advanced
    !! Advanced plotting methods (contour, bar, histogram, boxplot, streamplot)
    !! Extracted from fortplot_plotting to meet QADS size requirements
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot_figure_core, only: figure_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                    PLOT_TYPE_ERRORBAR, PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, &
                                    PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER, &
                                    HALF_WIDTH, IQR_WHISKER_MULTIPLIER
    use fortplot_colors, only: parse_color, color_t
    use fortplot_streamplot_matplotlib
    use fortplot_streamplot_core, only: setup_streamplot_parameters
    use fortplot_logging, only: log_warning, log_error, log_info
    use fortplot_errors, only: fortplot_error_t, SUCCESS, ERROR_RESOURCE_LIMIT
    use fortplot_utilities, only: sort_array
    
    implicit none
    
    private
    public :: add_contour_impl, add_contour_filled_impl, add_pcolormesh_impl
    public :: bar_impl, barh_impl, hist_impl, boxplot_impl
    public :: streamplot_impl
    
    ! Histogram constants
    integer, parameter :: DEFAULT_HISTOGRAM_BINS = 10
    integer, parameter :: MAX_SAFE_BINS = 10000
    real(wp), parameter :: IDENTICAL_VALUE_PADDING = 0.5_wp
    real(wp), parameter :: BIN_EDGE_PADDING_FACTOR = 0.001_wp
    
    ! Box plot constants
    real(wp), parameter :: BOX_PLOT_LINE_WIDTH = 2.0_wp
    
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
    
    subroutine bar_impl(self, x, heights, width, label, color)
        !! Add vertical bar plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), heights(:)
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        call add_bar_plot_data(self, x, heights, width, label, color, horizontal=.false.)
    end subroutine bar_impl
    
    subroutine barh_impl(self, y, widths, height, label, color)
        !! Add horizontal bar plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y(:), widths(:)
        real(wp), intent(in), optional :: height
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        call add_bar_plot_data(self, y, widths, height, label, color, horizontal=.true.)
    end subroutine barh_impl
    
    subroutine hist_impl(self, data, bins, density, label, color)
        !! Add histogram
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        call add_histogram_plot_data(self, data, bins, density, label, color)
    end subroutine hist_impl
    
    subroutine boxplot_impl(self, data, position, width, label, show_outliers, horizontal, color)
        !! Add boxplot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers, horizontal
        real(wp), intent(in), optional :: color(3)
        
        call add_boxplot_data(self, data, position, width, label, show_outliers, horizontal, color)
    end subroutine boxplot_impl
    
    subroutine streamplot_impl(self, x, y, u, v, density, color, linewidth, rtol, atol, max_time, arrowsize, arrowstyle)
        !! Add streamlines for vector field
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density, color(3), linewidth
        real(wp), intent(in), optional :: rtol, atol, max_time, arrowsize
        character(len=*), intent(in), optional :: arrowstyle
        
        call setup_streamplot_parameters(self, x, y, u, v, density, color, linewidth, &
                                        rtol, atol, max_time, arrowsize, arrowstyle)
    end subroutine streamplot_impl
    
    ! Private helper subroutines
    
    subroutine add_contour_plot_data(self, x_grid, y_grid, z_grid, levels, label)
        !! Add contour plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        integer :: subplot_idx, plot_idx
        
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
            allocate(self%plots(plot_idx)%contour_levels(size(levels)))
            self%plots(plot_idx)%contour_levels = levels
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
        
        integer :: subplot_idx, plot_idx
        
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
            allocate(self%plots(plot_idx)%contour_levels(size(levels)))
            self%plots(plot_idx)%contour_levels = levels
        end if
        
        ! Color properties
        self%plots(plot_idx)%use_color_levels = .true.
        
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
        
        integer :: subplot_idx, plot_idx, i, j
        
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
    
    subroutine add_bar_plot_data(self, positions, values, bar_size, label, color, horizontal)
        !! Add bar plot data (handles both vertical and horizontal)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: positions(:), values(:)
        real(wp), intent(in), optional :: bar_size
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        logical, intent(in) :: horizontal
        
        integer :: subplot_idx, plot_idx, color_idx
        
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_BAR
        
        allocate(self%plots(plot_idx)%bar_x(size(positions)))
        allocate(self%plots(plot_idx)%bar_heights(size(values)))
        
        self%plots(plot_idx)%bar_x = positions
        self%plots(plot_idx)%bar_heights = values
        
        if (present(bar_size)) then
            self%plots(plot_idx)%bar_width = bar_size
        else
            ! Default bar width calculation
            if (size(positions) > 1) then
                self%plots(plot_idx)%bar_width = 0.8_wp * minval(positions(2:) - positions(:size(positions)-1))
            else
                self%plots(plot_idx)%bar_width = 0.8_wp
            end if
        end if
        
        self%plots(plot_idx)%bar_horizontal = horizontal
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        else
            ! Default color cycling
            color_idx = mod(plot_idx - 1, 6) + 1
            self%plots(plot_idx)%color = self%state%colors(:, color_idx)
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine add_bar_plot_data
    
    subroutine add_histogram_plot_data(self, data, bins, density, label, color)
        !! Add histogram plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        integer :: n_bins, i, bin_idx, color_idx
        real(wp) :: data_min, data_max, bin_width, total_count
        real(wp), allocatable :: sorted_data(:), bin_edges(:), counts(:)
        integer :: subplot_idx, plot_idx
        
        call compute_histogram_bins(data, bins, bin_edges, counts)
        
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_HISTOGRAM
        
        allocate(self%plots(plot_idx)%hist_bin_edges(size(bin_edges)))
        allocate(self%plots(plot_idx)%hist_counts(size(counts)))
        
        self%plots(plot_idx)%hist_bin_edges = bin_edges
        self%plots(plot_idx)%hist_counts = counts
        
        if (present(density)) then
            self%plots(plot_idx)%hist_density = density
        else
            self%plots(plot_idx)%hist_density = .false.
        end if
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        else
            color_idx = mod(plot_idx - 1, 6) + 1
            self%plots(plot_idx)%color = self%state%colors(:, color_idx)
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine add_histogram_plot_data
    
    subroutine compute_histogram_bins(data, bins, bin_edges, counts)
        !! Compute histogram bins and counts
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        real(wp), allocatable, intent(out) :: bin_edges(:), counts(:)
        
        integer :: n_bins, i, bin_idx
        real(wp) :: data_min, data_max, bin_width
        real(wp), allocatable :: sorted_data(:)
        
        ! Determine number of bins
        if (present(bins)) then
            n_bins = min(max(bins, 1), MAX_SAFE_BINS)
        else
            n_bins = DEFAULT_HISTOGRAM_BINS
        end if
        
        ! Find data range
        data_min = minval(data)
        data_max = maxval(data)
        
        ! Handle edge case: all values identical
        if (abs(data_max - data_min) < epsilon(1.0_wp)) then
            data_min = data_min - IDENTICAL_VALUE_PADDING
            data_max = data_max + IDENTICAL_VALUE_PADDING
        end if
        
        ! Calculate bin width and edges
        bin_width = (data_max - data_min) / real(n_bins, wp)
        
        allocate(bin_edges(n_bins + 1))
        allocate(counts(n_bins))
        
        do i = 1, n_bins + 1
            bin_edges(i) = data_min + (i - 1) * bin_width
        end do
        
        ! Add small padding to last edge to ensure all data falls within bins
        bin_edges(n_bins + 1) = bin_edges(n_bins + 1) + BIN_EDGE_PADDING_FACTOR * bin_width
        
        ! Initialize counts
        counts = 0.0_wp
        
        ! Count data points in each bin
        do i = 1, size(data)
            if (ieee_is_finite(data(i))) then
                bin_idx = min(int((data(i) - data_min) / bin_width) + 1, n_bins)
                bin_idx = max(bin_idx, 1)
                counts(bin_idx) = counts(bin_idx) + 1.0_wp
            end if
        end do
    end subroutine compute_histogram_bins
    
    subroutine add_boxplot_data(self, data, position, width, label, show_outliers, horizontal, color)
        !! Add boxplot data with quartile calculations
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers, horizontal
        real(wp), intent(in), optional :: color(3)
        
        integer :: subplot_idx, plot_idx, color_idx
        
        call compute_boxplot_statistics(data, self%plots, self%plot_count + 1, &
                                       position, width, show_outliers)
        
        self%plot_count = self%plot_count + 1
        plot_idx = self%plot_count
        
        ! Ensure plots array is allocated
        if (.not. allocated(self%plots)) then
            allocate(self%plots(self%state%max_plots))
        else if (plot_idx > size(self%plots)) then
            return
        end if
        
        self%plots(plot_idx)%plot_type = PLOT_TYPE_BOXPLOT
        
        ! Store raw data
        allocate(self%plots(plot_idx)%box_data(size(data)))
        self%plots(plot_idx)%box_data = data
        
        if (present(horizontal)) then
            self%plots(plot_idx)%horizontal = horizontal
        else
            self%plots(plot_idx)%horizontal = .false.
        end if
        
        if (present(color)) then
            self%plots(plot_idx)%color = color
        else
            color_idx = mod(plot_idx - 1, 6) + 1
            self%plots(plot_idx)%color = self%state%colors(:, color_idx)
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine add_boxplot_data
    
    subroutine compute_boxplot_statistics(data, plots, plot_idx, position, width, show_outliers)
        !! Compute quartiles and outliers for boxplot
        real(wp), intent(in) :: data(:)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_idx
        real(wp), intent(in), optional :: position, width
        logical, intent(in), optional :: show_outliers
        
        real(wp), allocatable :: sorted_data(:), outlier_list(:)
        integer :: n, q1_idx, q2_idx, q3_idx, n_outliers, i
        real(wp) :: iqr, lower_fence, upper_fence
        
        n = size(data)
        allocate(sorted_data(n))
        sorted_data = data
        
        ! Sort data
        call sort_array(sorted_data)
        
        ! Calculate quartile indices
        q1_idx = max(1, nint(0.25_wp * n))
        q2_idx = max(1, nint(0.50_wp * n))
        q3_idx = max(1, nint(0.75_wp * n))
        
        ! Store quartiles
        plots(plot_idx)%q1 = sorted_data(q1_idx)
        plots(plot_idx)%q2 = sorted_data(q2_idx)
        plots(plot_idx)%q3 = sorted_data(q3_idx)
        
        ! Calculate IQR and fences
        iqr = plots(plot_idx)%q3 - plots(plot_idx)%q1
        lower_fence = plots(plot_idx)%q1 - IQR_WHISKER_MULTIPLIER * iqr
        upper_fence = plots(plot_idx)%q3 + IQR_WHISKER_MULTIPLIER * iqr
        
        ! Find whisker positions (last data point within fences)
        plots(plot_idx)%whisker_low = sorted_data(1)
        plots(plot_idx)%whisker_high = sorted_data(n)
        
        do i = 1, n
            if (sorted_data(i) >= lower_fence) then
                plots(plot_idx)%whisker_low = sorted_data(i)
                exit
            end if
        end do
        
        do i = n, 1, -1
            if (sorted_data(i) <= upper_fence) then
                plots(plot_idx)%whisker_high = sorted_data(i)
                exit
            end if
        end do
        
        ! Find outliers if requested
        if (present(show_outliers)) then
            plots(plot_idx)%show_outliers = show_outliers
        else
            plots(plot_idx)%show_outliers = .true.
        end if
        
        if (plots(plot_idx)%show_outliers) then
            ! Count outliers
            n_outliers = 0
            do i = 1, n
                if (sorted_data(i) < lower_fence .or. sorted_data(i) > upper_fence) then
                    n_outliers = n_outliers + 1
                end if
            end do
            
            ! Store outliers
            if (n_outliers > 0) then
                allocate(plots(plot_idx)%outliers(n_outliers))
                n_outliers = 0
                do i = 1, n
                    if (sorted_data(i) < lower_fence .or. sorted_data(i) > upper_fence) then
                        n_outliers = n_outliers + 1
                        plots(plot_idx)%outliers(n_outliers) = sorted_data(i)
                    end if
                end do
            end if
        end if
        
        ! Set position and width
        if (present(position)) then
            plots(plot_idx)%position = position
        else
            plots(plot_idx)%position = 1.0_wp
        end if
        
        if (present(width)) then
            plots(plot_idx)%width = width
        else
            plots(plot_idx)%width = 0.6_wp
        end if
    end subroutine compute_boxplot_statistics
    
    ! Note: setup_streamplot_parameters is provided by fortplot_streamplot_core module
    ! No need to duplicate it here
    
end module fortplot_plotting_advanced