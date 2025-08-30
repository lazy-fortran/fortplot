module fortplot_plot_statistics
    !! Statistical plotting functionality
    !! 
    !! Provides:
    !! - Histograms with automatic binning
    !! - Box plots with quartile calculations
    !! - Statistical computations (quantiles, outliers)
    !! - Density normalization for histograms
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    use fortplot_figure_core, only: figure_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_HISTOGRAM, PLOT_TYPE_BOXPLOT, &
        IQR_WHISKER_MULTIPLIER
    use fortplot_utils_sort, only: sort_array
    implicit none
    
    private
    public :: hist_impl, boxplot_impl
    
    ! Histogram constants
    integer, parameter :: DEFAULT_HISTOGRAM_BINS = 10
    integer, parameter :: MAX_SAFE_BINS = 10000
    real(wp), parameter :: IDENTICAL_VALUE_PADDING = 0.5_wp
    real(wp), parameter :: BIN_EDGE_PADDING_FACTOR = 0.001_wp
    
contains
    
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
    
    ! Private helper subroutines
    
    subroutine add_histogram_plot_data(self, data, bins, density, label, color)
        !! Add histogram plot data
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        integer :: color_idx
        real(wp), allocatable :: bin_edges(:), counts(:)
        integer :: plot_idx
        
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
        
        integer :: plot_idx, color_idx
        
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
        
        real(wp), allocatable :: sorted_data(:)
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

end module fortplot_plot_statistics