module fortplot_tick_scales
    !! Specialized tick generation for logarithmic and symmetric logarithmic scales
    !! 
    !! Provides:
    !! - Logarithmic tick location algorithms
    !! - Symmetric logarithmic (symlog) tick algorithms
    !! - Scale-aware tick label generation
    !! - Sub-tick generation for detailed log scales
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_tick_formatting, only: format_tick_value, format_log_tick_value
    implicit none
    
    intrinsic :: floor, log10, ceiling
    
    private
    public :: calculate_tick_labels_log, calculate_tick_labels_symlog
    public :: generate_scale_aware_tick_labels

contains

    subroutine calculate_tick_labels_log(data_min, data_max, num_ticks, labels)
        !! Calculate logarithmic tick labels at powers of 10 with sub-ticks
        real(wp), intent(in) :: data_min, data_max
        integer, intent(in) :: num_ticks
        character(len=20), intent(out) :: labels(:)
        
        call generate_log_tick_locations(data_min, data_max, num_ticks, labels)
    end subroutine calculate_tick_labels_log

    subroutine calculate_tick_labels_symlog(data_min, data_max, linear_threshold, num_ticks, labels)
        !! Calculate symlog tick labels with improved linear and log regions
        real(wp), intent(in) :: data_min, data_max, linear_threshold
        integer, intent(in) :: num_ticks
        character(len=20), intent(out) :: labels(:)
        
        call generate_symlog_tick_locations(data_min, data_max, linear_threshold, &
                                           num_ticks, labels)
    end subroutine calculate_tick_labels_symlog

    subroutine generate_scale_aware_tick_labels(data_min, data_max, num_ticks, labels, scale, threshold)
        !! Generate tick labels based on scale type - common function for all backends
        real(wp), intent(in) :: data_min, data_max
        integer, intent(in) :: num_ticks
        character(len=20), intent(out) :: labels(:)
        character(len=*), intent(in), optional :: scale
        real(wp), intent(in), optional :: threshold
        
        character(len=10) :: scale_type
        real(wp) :: symlog_threshold
        
        ! Set defaults
        scale_type = 'linear'
        if (present(scale)) scale_type = scale
        symlog_threshold = 1.0_wp
        if (present(threshold)) symlog_threshold = threshold
        
        ! Generate labels based on scale type
        select case (trim(scale_type))
        case ('log')
            call calculate_tick_labels_log(data_min, data_max, num_ticks, labels)
        case ('symlog')
            call calculate_tick_labels_symlog(data_min, data_max, symlog_threshold, num_ticks, labels)
        case default  ! 'linear'
            ! For linear scale, we need to import from tick_calculation module
            ! This would require module dependency - handled in the main interface
            labels(1:min(num_ticks, size(labels))) = ''
        end select
    end subroutine generate_scale_aware_tick_labels

    subroutine generate_log_tick_locations(data_min, data_max, num_ticks, labels)
        !! Generate proper logarithmic tick locations with sub-ticks when appropriate
        real(wp), intent(in) :: data_min, data_max
        integer, intent(in) :: num_ticks
        character(len=20), intent(out) :: labels(:)
        
        real(wp) :: decade_range
        logical :: use_subticks
        
        if (num_ticks <= 0 .or. data_min <= 0.0_wp .or. data_max <= 0.0_wp) then
            labels(1:min(num_ticks, size(labels))) = ''
            return
        end if
        
        ! Calculate decade range to determine tick strategy
        decade_range = log10(data_max) - log10(data_min)
        use_subticks = decade_range <= 2.0_wp  ! Use subticks for small ranges
        
        if (use_subticks) then
            call generate_log_subticks(data_min, data_max, num_ticks, labels)
        else
            call generate_log_major_ticks(data_min, data_max, num_ticks, labels)
        end if
    end subroutine generate_log_tick_locations
    
    subroutine generate_log_major_ticks(data_min, data_max, num_ticks, labels)
        !! Generate major ticks at powers of 10 for wide log ranges
        real(wp), intent(in) :: data_min, data_max
        integer, intent(in) :: num_ticks
        character(len=20), intent(out) :: labels(:)
        
        integer :: i, min_power, max_power, actual_num_ticks, power
        real(wp) :: tick_value
        
        min_power = floor(log10(data_min))
        max_power = ceiling(log10(data_max))
        
        actual_num_ticks = 0
        do power = min_power, max_power
            if (actual_num_ticks >= size(labels)) exit
            
            tick_value = 10.0_wp ** power
            
            ! Use tolerance for endpoint inclusion like matplotlib
            if (tick_value >= data_min * 0.999_wp .and. tick_value <= data_max * 1.001_wp) then
                actual_num_ticks = actual_num_ticks + 1
                labels(actual_num_ticks) = format_log_tick_value(tick_value)
            end if
        end do
        
        ! Clear unused labels
        do i = actual_num_ticks + 1, min(num_ticks, size(labels))
            labels(i) = ''
        end do
    end subroutine generate_log_major_ticks
    
    subroutine generate_log_subticks(data_min, data_max, num_ticks, labels)
        !! Generate subticks within decades for narrow log ranges
        real(wp), intent(in) :: data_min, data_max
        integer, intent(in) :: num_ticks
        character(len=20), intent(out) :: labels(:)
        
        integer :: i, actual_num_ticks, subtick_idx, power
        real(wp) :: tick_value, decade_start
        real(wp), parameter :: subtick_multipliers(8) = [1.0_wp, 2.0_wp, 3.0_wp, 5.0_wp, &
                                                         6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp]
        
        actual_num_ticks = 0
        power = floor(log10(data_min))
        
        do while (power <= ceiling(log10(data_max)) .and. actual_num_ticks < size(labels))
            decade_start = 10.0_wp ** power
            
            ! Add major tick (10^power)
            tick_value = decade_start
            if (tick_value >= data_min * 0.999_wp .and. tick_value <= data_max * 1.001_wp) then
                actual_num_ticks = actual_num_ticks + 1
                labels(actual_num_ticks) = format_log_tick_value(tick_value)
            end if
            
            ! Add subticks within this decade
            do subtick_idx = 1, size(subtick_multipliers)
                if (actual_num_ticks >= size(labels)) exit
                
                tick_value = decade_start * subtick_multipliers(subtick_idx)
                
                if (tick_value >= data_min * 0.999_wp .and. tick_value <= data_max * 1.001_wp .and. &
                    tick_value < 10.0_wp ** (power + 1)) then
                    actual_num_ticks = actual_num_ticks + 1
                    labels(actual_num_ticks) = format_log_tick_value(tick_value)
                end if
            end do
            
            power = power + 1
        end do
        
        ! Clear unused labels
        do i = actual_num_ticks + 1, min(num_ticks, size(labels))
            labels(i) = ''
        end do
    end subroutine generate_log_subticks
    
    subroutine generate_symlog_tick_locations(data_min, data_max, linear_threshold, &
                                             num_ticks, labels)
        !! Generate improved symlog tick locations
        real(wp), intent(in) :: data_min, data_max, linear_threshold
        integer, intent(in) :: num_ticks
        character(len=20), intent(out) :: labels(:)
        
        integer :: actual_num_ticks, i
        real(wp) :: tick_locations(20)
        
        if (num_ticks <= 0) then
            labels(1:min(num_ticks, size(labels))) = ''
            return
        end if
        
        call calculate_symlog_ticks(data_min, data_max, linear_threshold, &
                                   tick_locations, actual_num_ticks)
        
        ! Format the tick locations using scale-aware formatting
        do i = 1, min(actual_num_ticks, size(labels))
            if (abs(tick_locations(i)) > linear_threshold) then
                ! Use log formatting for values outside linear region
                labels(i) = format_log_tick_value(tick_locations(i))
            else
                ! Use regular formatting for linear region
                labels(i) = format_tick_value(tick_locations(i), data_max - data_min)
            end if
        end do
        
        ! Clear unused labels
        do i = actual_num_ticks + 1, min(num_ticks, size(labels))
            labels(i) = ''
        end do
    end subroutine generate_symlog_tick_locations
    
    subroutine calculate_symlog_ticks(data_min, data_max, linear_threshold, &
                                     tick_locations, actual_num_ticks)
        !! Calculate symlog tick locations with proper linear/log transitions
        real(wp), intent(in) :: data_min, data_max, linear_threshold
        real(wp), intent(out) :: tick_locations(:)
        integer, intent(out) :: actual_num_ticks
        real(wp) :: candidates(20)
        integer :: num_candidates
        
        actual_num_ticks = 0
        num_candidates = 0
        
        ! Always include zero if it's in range
        if (data_min <= 0.0_wp .and. data_max >= 0.0_wp) then
            num_candidates = num_candidates + 1
            candidates(num_candidates) = 0.0_wp
        end if
        
        ! Add positive log ticks
        if (data_max > linear_threshold) then
            call add_positive_log_candidates(data_max, linear_threshold, candidates, num_candidates)
        end if
        
        ! Add negative log ticks
        if (data_min < -linear_threshold) then
            call add_negative_log_candidates(data_min, linear_threshold, candidates, num_candidates)
        end if
        
        ! Add linear region ticks
        call add_linear_region_candidates(data_min, data_max, linear_threshold, candidates, num_candidates)
        
        ! Sort and filter candidates within range
        call sort_and_filter_candidates(candidates, num_candidates, data_min, data_max, &
                                       tick_locations, actual_num_ticks)
    end subroutine calculate_symlog_ticks
    
    subroutine add_positive_log_candidates(data_max, linear_threshold, candidates, num_candidates)
        !! Add positive logarithmic tick candidates
        real(wp), intent(in) :: data_max, linear_threshold
        real(wp), intent(inout) :: candidates(:)
        integer, intent(inout) :: num_candidates
        
        integer :: power
        real(wp) :: tick_val
        
        power = 1
        do while (power <= 10 .and. num_candidates < size(candidates))
            tick_val = 10.0_wp ** power
            if (tick_val > linear_threshold .and. tick_val <= data_max * 1.001_wp) then
                num_candidates = num_candidates + 1
                candidates(num_candidates) = tick_val
            end if
            power = power + 1
        end do
    end subroutine add_positive_log_candidates
    
    subroutine add_negative_log_candidates(data_min, linear_threshold, candidates, num_candidates)
        !! Add negative logarithmic tick candidates
        real(wp), intent(in) :: data_min, linear_threshold
        real(wp), intent(inout) :: candidates(:)
        integer, intent(inout) :: num_candidates
        
        integer :: power
        real(wp) :: tick_val
        
        power = 1
        do while (power <= 10 .and. num_candidates < size(candidates))
            tick_val = -(10.0_wp ** power)
            if (tick_val < -linear_threshold .and. tick_val >= data_min * 1.001_wp) then
                num_candidates = num_candidates + 1
                candidates(num_candidates) = tick_val
            end if
            power = power + 1
        end do
    end subroutine add_negative_log_candidates
    
    subroutine add_linear_region_candidates(data_min, data_max, linear_threshold, candidates, num_candidates)
        !! Add linear region tick candidates
        real(wp), intent(in) :: data_min, data_max, linear_threshold
        real(wp), intent(inout) :: candidates(:)
        integer, intent(inout) :: num_candidates
        
        real(wp) :: linear_min, linear_max, step, current_tick
        integer :: i, num_linear_ticks
        
        linear_min = max(data_min, -linear_threshold)
        linear_max = min(data_max, linear_threshold)
        
        if (linear_max > linear_min .and. num_candidates < size(candidates)) then
            num_linear_ticks = 3  ! Simple linear ticks in the middle region
            step = (linear_max - linear_min) / real(num_linear_ticks + 1, wp)
            
            do i = 1, num_linear_ticks
                current_tick = linear_min + real(i, wp) * step
                if (abs(current_tick) > 1.0e-10_wp .and. num_candidates < size(candidates)) then
                    num_candidates = num_candidates + 1
                    candidates(num_candidates) = current_tick
                end if
            end do
        end if
    end subroutine add_linear_region_candidates
    
    subroutine sort_and_filter_candidates(candidates, num_candidates, data_min, data_max, &
                                         tick_locations, actual_num_ticks)
        !! Sort candidates and filter to final tick locations
        real(wp), intent(in) :: candidates(:)
        integer, intent(in) :: num_candidates
        real(wp), intent(in) :: data_min, data_max
        real(wp), intent(out) :: tick_locations(:)
        integer, intent(out) :: actual_num_ticks
        
        integer :: i
        real(wp) :: temp_candidates(20)
        
        ! Copy and simple sort (bubble sort for small arrays)
        temp_candidates(1:num_candidates) = candidates(1:num_candidates)
        call simple_sort(temp_candidates, num_candidates)
        
        ! Filter within range
        actual_num_ticks = 0
        do i = 1, num_candidates
            if (temp_candidates(i) >= data_min .and. temp_candidates(i) <= data_max) then
                if (actual_num_ticks < size(tick_locations)) then
                    actual_num_ticks = actual_num_ticks + 1
                    tick_locations(actual_num_ticks) = temp_candidates(i)
                end if
            end if
        end do
    end subroutine sort_and_filter_candidates
    
    subroutine simple_sort(array, n)
        !! Simple bubble sort for small arrays
        real(wp), intent(inout) :: array(:)
        integer, intent(in) :: n
        
        integer :: i, j
        real(wp) :: temp
        
        do i = 1, n - 1
            do j = 1, n - i
                if (array(j) > array(j + 1)) then
                    temp = array(j)
                    array(j) = array(j + 1)
                    array(j + 1) = temp
                end if
            end do
        end do
    end subroutine simple_sort

end module fortplot_tick_scales