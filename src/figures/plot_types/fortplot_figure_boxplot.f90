module fortplot_figure_boxplot
    !! Box plot implementation module
    !! 
    !! Extracted from fortplot_figure_core to meet QADS size limits.
    !! Provides box plot creation and data range updating.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_BOXPLOT
    use fortplot_utils_sort, only: sort_array
    implicit none
    
    private
    public :: add_boxplot
    public :: update_boxplot_ranges
    
    interface
    end interface
    
contains
    
    subroutine add_boxplot(plots, plot_count, data, position, width, label, &
                          show_outliers, horizontal, color, max_plots)
        !! Add a box plot to the plot array
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        character(len=*), intent(in), optional :: color
        integer, intent(in) :: max_plots
        
        type(plot_data_t), allocatable :: new_plots(:)
        integer :: plot_idx
        
        ! Handle empty data
        if (size(data) == 0) return
        
        ! Check plot count
        plot_count = plot_count + 1
        if (plot_count > max_plots) then
            print *, "WARNING: Maximum number of plots exceeded"
            plot_count = max_plots
            return
        end if
        
        ! Resize array if needed
        if (plot_count > size(plots)) then
            allocate(new_plots(size(plots) * 2))
            new_plots(1:plot_count-1) = plots(1:plot_count-1)
            call move_alloc(new_plots, plots)
        end if
        
        plot_idx = plot_count
        
        ! Store box plot data
        if (allocated(plots(plot_idx)%box_data)) then
            deallocate(plots(plot_idx)%box_data)
        end if
        allocate(plots(plot_idx)%box_data(size(data)))
        plots(plot_idx)%box_data = data
        
        ! Set plot type
        plots(plot_idx)%plot_type = PLOT_TYPE_BOXPLOT
        
        ! Store label if provided
        if (present(label)) then
            plots(plot_idx)%label = label
        else
            plots(plot_idx)%label = ''
        end if
        
        ! Store position
        if (present(position)) then
            plots(plot_idx)%position = position
        else
            plots(plot_idx)%position = 1.0_wp
        end if
        
        ! Store width
        if (present(width)) then
            plots(plot_idx)%width = width
        else
            plots(plot_idx)%width = 0.5_wp
        end if
        
        ! Store other parameters
        plots(plot_idx)%show_outliers = .true.
        if (present(show_outliers)) then
            plots(plot_idx)%show_outliers = show_outliers
        end if
        
        plots(plot_idx)%horizontal = .false.
        if (present(horizontal)) then
            plots(plot_idx)%horizontal = horizontal
        end if
        
        ! Compute statistics (quartiles, whiskers, outliers)
        call compute_boxplot_stats_inplace(plots(plot_idx))

        ! Color would need conversion from string to RGB
        ! For now, use default color from plot_data_t initialization
        if (present(color)) then
            ! Reference optional color to keep interface stable without side effects
            associate(unused_color_len => len_trim(color)); end associate
        end if
    end subroutine add_boxplot
    
    subroutine compute_boxplot_stats_inplace(plot)
        !! Compute quartiles, whiskers, and outliers for a box plot in-place
        type(plot_data_t), intent(inout) :: plot
        real(wp), allocatable :: sorted(:)
        integer :: n, i, q1_idx, q2_idx, q3_idx, n_out
        real(wp) :: iqr, lfence, ufence
        
        if (.not. allocated(plot%box_data)) return
        n = size(plot%box_data)
        if (n == 0) return
        
        allocate(sorted(n))
        sorted = plot%box_data
        call sort_array(sorted)
        
        q1_idx = max(1, nint(0.25_wp * n))
        q2_idx = max(1, nint(0.50_wp * n))
        q3_idx = max(1, nint(0.75_wp * n))
        plot%q1 = sorted(q1_idx)
        plot%q2 = sorted(q2_idx)
        plot%q3 = sorted(q3_idx)
        
        iqr = plot%q3 - plot%q1
        lfence = plot%q1 - 1.5_wp * iqr
        ufence = plot%q3 + 1.5_wp * iqr
        
        plot%whisker_low = sorted(1)
        do i = 1, n
            if (sorted(i) >= lfence) then
                plot%whisker_low = sorted(i)
                exit
            end if
        end do
        
        plot%whisker_high = sorted(n)
        do i = n, 1, -1
            if (sorted(i) <= ufence) then
                plot%whisker_high = sorted(i)
                exit
            end if
        end do
        
        if (plot%show_outliers) then
            n_out = 0
            do i = 1, n
                if (sorted(i) < lfence .or. sorted(i) > ufence) n_out = n_out + 1
            end do
            if (n_out > 0) then
                if (allocated(plot%outliers)) deallocate(plot%outliers)
                allocate(plot%outliers(n_out))
                n_out = 0
                do i = 1, n
                    if (sorted(i) < lfence .or. sorted(i) > ufence) then
                        n_out = n_out + 1
                        plot%outliers(n_out) = sorted(i)
                    end if
                end do
            end if
        end if
        
        if (allocated(sorted)) deallocate(sorted)
    end subroutine compute_boxplot_stats_inplace
    
    subroutine update_boxplot_ranges(data, position, x_min, x_max, y_min, y_max, &
                                     x_range_set, y_range_set)
        !! Update data ranges based on boxplot statistics
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        logical, intent(inout) :: x_range_set, y_range_set
        
        real(wp), allocatable :: sorted_data(:)
        real(wp) :: pos, box_half_width
        real(wp) :: q1, q2, q3, iqr
        real(wp) :: whisker_low, whisker_high
        real(wp) :: data_min, data_max
        integer :: n, q1_idx, q2_idx, q3_idx
        
        n = size(data)
        if (n == 0) return
        
        ! Position and width
        pos = 1.0_wp
        if (present(position)) pos = position
        box_half_width = 0.3_wp
        
        ! Sort data for quartile calculation
        allocate(sorted_data(n))
        sorted_data = data
        call sort_array(sorted_data)
        
        ! Calculate quartiles
        q1_idx = max(1, int(0.25_wp * n))
        q2_idx = max(1, int(0.50_wp * n))
        q3_idx = max(1, int(0.75_wp * n))
        
        q1 = sorted_data(q1_idx)
        q2 = sorted_data(q2_idx)
        q3 = sorted_data(q3_idx)
        
        ! IQR and whiskers
        iqr = q3 - q1
        whisker_low = q1 - 1.5_wp * iqr
        whisker_high = q3 + 1.5_wp * iqr
        
        ! Find actual whisker values (within data range)
        data_min = sorted_data(1)
        data_max = sorted_data(n)
        whisker_low = max(whisker_low, data_min)
        whisker_high = min(whisker_high, data_max)
        
        ! Update x-range (position)
        if (.not. x_range_set) then
            x_min = pos - box_half_width - 0.2_wp
            x_max = pos + box_half_width + 0.2_wp
            x_range_set = .true.
        else
            x_min = min(x_min, pos - box_half_width - 0.2_wp)
            x_max = max(x_max, pos + box_half_width + 0.2_wp)
        end if
        
        ! Update y-range (data values)
        if (.not. y_range_set) then
            y_min = data_min - 0.1_wp * abs(data_max - data_min)
            y_max = data_max + 0.1_wp * abs(data_max - data_min)
            y_range_set = .true.
        else
            y_min = min(y_min, data_min - 0.1_wp * abs(data_max - data_min))
            y_max = max(y_max, data_max + 0.1_wp * abs(data_max - data_min))
        end if
    end subroutine update_boxplot_ranges
    
end module fortplot_figure_boxplot
