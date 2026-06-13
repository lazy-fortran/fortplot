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
    
    subroutine hist_impl(self, data, bins, density, label, color, range, &
                         weights, cumulative, orientation, alpha)
        !! Add histogram (matplotlib-compatible).
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: range(2)
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative
        character(len=*), intent(in), optional :: orientation
        real(wp), intent(in), optional :: alpha

        call add_histogram_plot_data(self, data, bins, density, label, color, &
                                     range=range, weights=weights, &
                                     cumulative=cumulative, &
                                     orientation=orientation, alpha=alpha)
    end subroutine hist_impl
    
    subroutine boxplot_impl(self, data, position, width, label, show_outliers, horizontal, color)
        !! Add boxplot
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: data(:)
        real(wp), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers, horizontal
        real(wp), intent(in), optional :: color(3)
        
        call add_boxplot_data(self, data, position, width, label, show_outliers, horizontal, color)
    end subroutine boxplot_impl
    
    ! Private helper subroutines
    
    subroutine add_histogram_plot_data(self, data, bins, density, label, color, &
                                       range, weights, cumulative, orientation, &
                                       alpha)
        !! Add histogram plot data (matplotlib-compatible).
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: range(2)
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative
        character(len=*), intent(in), optional :: orientation
        real(wp), intent(in), optional :: alpha

        integer :: color_idx
        real(wp), allocatable :: bin_edges(:), counts(:)
        integer :: plot_idx

        call compute_histogram_bins(data, bins, bin_edges, counts, range=range, &
                                     weights=weights, cumulative=cumulative)

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

        if (present(cumulative)) then
            self%plots(plot_idx)%hist_cumulative = cumulative
        else
            self%plots(plot_idx)%hist_cumulative = .false.
        end if

        if (present(color)) then
            self%plots(plot_idx)%color = color
        else
            color_idx = mod(plot_idx - 1, size(self%state%colors, 2)) + 1
            self%plots(plot_idx)%color = self%state%colors(:, color_idx)
        end if

        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if

        if (present(alpha)) then
            self%plots(plot_idx)%fill_alpha = max(0.0_wp, min(1.0_wp, alpha))
            self%plots(plot_idx)%marker_face_alpha = self%plots(plot_idx)%fill_alpha
        end if
    end subroutine add_histogram_plot_data
    
    subroutine compute_histogram_bins(data, bins, bin_edges, counts, range, &
                                     weights, cumulative)
        !! Compute histogram bins and counts (matplotlib-compatible).
        !!
        !! Supports optional range clipping, per-sample weights, and
        !! cumulative accumulation in addition to the original binning.
        real(wp), contiguous, intent(in) :: data(:)
        integer, intent(in), optional :: bins
        real(wp), allocatable, intent(out) :: bin_edges(:), counts(:)
        real(wp), intent(in), optional :: range(2)
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative

        integer :: n_bins, i, bin_idx
        real(wp) :: data_min, data_max, bin_width

        ! Determine number of bins
        if (present(bins)) then
            n_bins = min(max(bins, 1), MAX_SAFE_BINS)
        else
            n_bins = DEFAULT_HISTOGRAM_BINS
        end if

        if (present(weights)) then
            if (size(weights) /= size(data)) then
                return
            end if
        end if

        if (present(range)) then
            data_min = range(1)
            data_max = range(2)
        else
            ! Find data range
            data_min = minval(data)
            data_max = maxval(data)

            ! Handle edge case: all values identical
            if (abs(data_max - data_min) < epsilon(1.0_wp)) then
                data_min = data_min - IDENTICAL_VALUE_PADDING
                data_max = data_max + IDENTICAL_VALUE_PADDING
            end if
        end if

        if (data_max <= data_min) then
            ! Empty range — produce zero counts without crashing.
            allocate(bin_edges(n_bins + 1), counts(n_bins))
            bin_edges = 0.0_wp
            counts = 0.0_wp
            return
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
            if (data(i) < data_min .or. data(i) > data_max) cycle
            if (ieee_is_finite(data(i))) then
                bin_idx = min(int((data(i) - data_min) / bin_width) + 1, n_bins)
                bin_idx = max(bin_idx, 1)
                if (present(weights)) then
                    counts(bin_idx) = counts(bin_idx) + weights(i)
                else
                    counts(bin_idx) = counts(bin_idx) + 1.0_wp
                end if
            end if
        end do

        ! Cumulative accumulation
        if (present(cumulative)) then
            if (cumulative) then
                do i = 2, n_bins
                    counts(i) = counts(i) + counts(i - 1)
                end do
            end if
        end if
    end subroutine compute_histogram_bins
    
    subroutine add_boxplot_data(self, data, position, width, label, show_outliers, horizontal, color)
        !! Add boxplot data with quartile calculations
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: data(:)
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
            color_idx = mod(plot_idx - 1, size(self%state%colors, 2)) + 1
            self%plots(plot_idx)%color = self%state%colors(:, color_idx)
        end if
        
        if (present(label) .and. len_trim(label) > 0) then
            self%plots(plot_idx)%label = label
        end if
    end subroutine add_boxplot_data
    
    subroutine compute_boxplot_statistics(data, plots, plot_idx, position, width, show_outliers)
        !! Compute quartiles and outliers for boxplot
        real(wp), contiguous, intent(in) :: data(:)
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_idx
        real(wp), intent(in), optional :: position, width
        logical, intent(in), optional :: show_outliers
        
        real(wp), allocatable :: sorted_data(:)
        integer :: n, n_outliers, i
        real(wp) :: iqr, lower_fence, upper_fence

        n = size(data)
        allocate(sorted_data(n))
        sorted_data = data

        ! Sort data
        call sort_array(sorted_data)

        ! Quartiles via matplotlib's default linear-interpolation percentile
        plots(plot_idx)%q1 = linear_percentile(sorted_data, 25.0_wp)
        plots(plot_idx)%q2 = linear_percentile(sorted_data, 50.0_wp)
        plots(plot_idx)%q3 = linear_percentile(sorted_data, 75.0_wp)
        
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

    pure function linear_percentile(sorted_data, percentile) result(value)
        !! Percentile via linear interpolation between closest ranks.
        !! Matches numpy's default ('linear', type 7) used by matplotlib boxplots:
        !! position = (n - 1) * p / 100, interpolating between bracketing samples.
        real(wp), intent(in) :: sorted_data(:)
        real(wp), intent(in) :: percentile
        real(wp) :: value

        integer :: n, lo
        real(wp) :: rank, frac

        n = size(sorted_data)
        if (n == 1) then
            value = sorted_data(1)
            return
        end if

        rank = (real(n, wp) - 1.0_wp) * percentile / 100.0_wp
        lo = int(rank) + 1
        if (lo >= n) then
            value = sorted_data(n)
            return
        end if
        frac = rank - real(lo - 1, wp)
        value = sorted_data(lo) + frac * (sorted_data(lo + 1) - sorted_data(lo))
    end function linear_percentile

end module fortplot_plot_statistics