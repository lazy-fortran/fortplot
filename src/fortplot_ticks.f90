module fortplot_ticks
    !! Tick generation and formatting for all scale types
    !! 
    !! This module handles all tick-related functionality including
    !! linear, logarithmic, and symmetric logarithmic tick generation,
    !! following Single Responsibility Principle by focusing solely
    !! on tick calculation and formatting.
    !! 
    !! Author: fortplotlib contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    intrinsic :: floor, log10
    
    private
    public :: calculate_tick_labels, calculate_tick_labels_log, calculate_tick_labels_symlog
    public :: format_tick_value, calculate_nice_axis_limits
    public :: generate_scale_aware_tick_labels
    
contains

    subroutine calculate_tick_labels(data_min, data_max, num_ticks, labels)
        !! Calculate appropriate tick labels at nice locations like matplotlib
        !! Ensures all labels have consistent formatting and nice round numbers
        real(wp), intent(in) :: data_min, data_max
        integer, intent(in) :: num_ticks
        character(len=20), intent(out) :: labels(:)
        
        integer :: i, decimal_places, actual_num_ticks
        real(wp) :: nice_min, nice_max, nice_step
        real(wp) :: tick_locations(size(labels))
        
        if (num_ticks <= 1) return
        
        ! Find nice tick locations using matplotlib-style algorithm
        call find_nice_tick_locations(data_min, data_max, num_ticks, &
                                     nice_min, nice_max, nice_step, &
                                     tick_locations, actual_num_ticks)
        
        ! Determine consistent formatting for the nice step size
        decimal_places = determine_decimal_places_from_step(nice_step)
        
        ! Format the nice tick locations
        do i = 1, min(actual_num_ticks, size(labels))
            labels(i) = format_tick_value_consistent(tick_locations(i), decimal_places)
        end do
        
        ! Clear unused labels
        do i = actual_num_ticks + 1, size(labels)
            labels(i) = ''
        end do
    end subroutine calculate_tick_labels

    function format_tick_value(value, range) result(formatted)
        !! Format tick value based on data range like matplotlib
        real(wp), intent(in) :: value, range
        character(len=20) :: formatted
        real(wp) :: abs_value
        
        abs_value = abs(value)
        
        if (abs_value < 1.0e-10_wp) then
            formatted = '0'
        else if (range >= 100.0_wp .or. abs_value >= 1000.0_wp) then
            ! Use integer format for large ranges
            write(formatted, '(I0)') nint(value)
        else if (range >= 10.0_wp .or. abs_value >= 10.0_wp) then
            ! 1 decimal place for medium ranges
            write(formatted, '(F0.1)') value
            call ensure_leading_zero(formatted)
            call remove_trailing_zeros(formatted)
        else if (range >= 1.0_wp .or. abs_value >= 1.0_wp) then
            ! 2 decimal places for small ranges
            write(formatted, '(F0.2)') value
            call ensure_leading_zero(formatted)
            call remove_trailing_zeros(formatted)
        else
            ! 3 decimal places for very small ranges
            write(formatted, '(F0.3)') value
            call ensure_leading_zero(formatted)
            call remove_trailing_zeros(formatted)
        end if
    end function format_tick_value

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

    subroutine calculate_nice_axis_limits(data_min, data_max, target_num_ticks, &
                                         nice_min, nice_max)
        !! Calculate nice axis limits that encompass the data like matplotlib
        !! The axis limits are set to nice round numbers based on tick locations
        real(wp), intent(in) :: data_min, data_max
        integer, intent(in) :: target_num_ticks
        real(wp), intent(out) :: nice_min, nice_max
        
        real(wp) :: nice_step, tick_locations(20)
        integer :: actual_num_ticks
        
        ! Use the same algorithm as tick calculation to find nice boundaries
        call find_nice_tick_locations(data_min, data_max, target_num_ticks, &
                                     nice_min, nice_max, nice_step, &
                                     tick_locations, actual_num_ticks)
        
        ! The nice_min and nice_max from find_nice_tick_locations are already
        ! appropriate axis limits that encompass the data
    end subroutine calculate_nice_axis_limits

    ! === PRIVATE HELPER ROUTINES ===
    
    subroutine remove_trailing_zeros(str)
        !! Remove trailing zeros from decimal representation
        character(len=*), intent(inout) :: str
        integer :: i, decimal_pos
        
        decimal_pos = index(str, '.')
        if (decimal_pos == 0) return  ! No decimal point
        
        ! Remove trailing zeros
        do i = len_trim(str), decimal_pos + 1, -1
            if (str(i:i) == '0') then
                str(i:i) = ' '
            else
                exit
            end if
        end do
        
        ! Remove trailing decimal point if all decimals were zeros
        if (len_trim(str) == decimal_pos) then
            str(decimal_pos:decimal_pos) = ' '
        end if
    end subroutine remove_trailing_zeros

    subroutine ensure_leading_zero(str)
        !! Ensure numbers like .5 become 0.5 for readability
        character(len=*), intent(inout) :: str
        character(len=len(str)) :: temp
        
        str = adjustl(str)  ! Remove leading spaces
        if (len_trim(str) > 0) then
            if (str(1:1) == '.') then
                temp = '0' // trim(str)
                str = temp
            else if (str(1:2) == '-.') then
                temp = '-0' // str(2:len_trim(str))
                str = temp
            end if
        end if
    end subroutine ensure_leading_zero

    function format_tick_value_consistent(value, decimal_places) result(formatted)
        !! Format tick value with consistent number of decimal places
        real(wp), intent(in) :: value
        integer, intent(in) :: decimal_places
        character(len=20) :: formatted
        
        if (abs(value) < 1.0e-10_wp) then
            if (decimal_places == 0) then
                formatted = '0'
            else
                write(formatted, '(F0.' // char(decimal_places + 48) // ')') 0.0_wp
                call ensure_leading_zero(formatted)
            end if
        else if (decimal_places == 0) then
            write(formatted, '(I0)') nint(value)
        else
            write(formatted, '(F0.' // char(decimal_places + 48) // ')') value
            call ensure_leading_zero(formatted)
        end if
    end function format_tick_value_consistent

    subroutine find_nice_tick_locations(data_min, data_max, target_num_ticks, &
                                       nice_min, nice_max, nice_step, &
                                       tick_locations, actual_num_ticks)
        !! Find nice tick locations following matplotlib's MaxNLocator algorithm
        real(wp), intent(in) :: data_min, data_max
        integer, intent(in) :: target_num_ticks
        real(wp), intent(out) :: nice_min, nice_max, nice_step
        real(wp), intent(out) :: tick_locations(:)
        integer, intent(out) :: actual_num_ticks
        
        real(wp) :: range, rough_step, magnitude, normalized_step
        real(wp) :: nice_normalized_step, current_tick
        integer :: i
        
        range = data_max - data_min
        if (range <= 0.0_wp) then
            tick_locations(1) = data_min
            actual_num_ticks = 1
            return
        end if
        
        ! Calculate rough step size
        rough_step = range / real(max(target_num_ticks - 1, 1), wp)
        
        ! Find magnitude and normalize
        magnitude = 10.0_wp ** floor(log10(rough_step))
        normalized_step = rough_step / magnitude
        
        ! Choose nice normalized step (1, 2, 5, 10)
        if (normalized_step <= 1.0_wp) then
            nice_normalized_step = 1.0_wp
        else if (normalized_step <= 2.0_wp) then
            nice_normalized_step = 2.0_wp
        else if (normalized_step <= 5.0_wp) then
            nice_normalized_step = 5.0_wp
        else
            nice_normalized_step = 10.0_wp
        end if
        
        nice_step = nice_normalized_step * magnitude
        
        ! Find nice boundaries (implement ceil manually)
        nice_min = floor(data_min / nice_step) * nice_step
        nice_max = floor(data_max / nice_step + 1.0_wp) * nice_step
        
        ! Generate tick locations
        actual_num_ticks = 0
        current_tick = nice_min
        do i = 1, size(tick_locations)
            if (current_tick > nice_max + 1.0e-10_wp) exit
            actual_num_ticks = actual_num_ticks + 1
            tick_locations(i) = current_tick
            current_tick = current_tick + nice_step
        end do
    end subroutine find_nice_tick_locations

    function determine_decimal_places_from_step(step) result(decimal_places)
        !! Determine decimal places based on step size for nice formatting
        real(wp), intent(in) :: step
        integer :: decimal_places
        
        if (step >= 1.0_wp) then
            decimal_places = 0
        else if (step >= 0.1_wp) then
            decimal_places = 1
        else if (step >= 0.01_wp) then
            decimal_places = 2
        else if (step >= 0.001_wp) then
            decimal_places = 3
        else
            decimal_places = 4
        end if
    end function determine_decimal_places_from_step



    subroutine generate_log_tick_locations(data_min, data_max, num_ticks, labels)
        !! Generate proper logarithmic tick locations with sub-ticks when appropriate
        real(wp), intent(in) :: data_min, data_max
        integer, intent(in) :: num_ticks
        character(len=20), intent(out) :: labels(:)
        
        integer :: i, min_power, max_power, actual_num_ticks, power
        real(wp) :: tick_value, decade_range
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
            
            if (tick_value >= data_min .and. tick_value <= data_max) then
                actual_num_ticks = actual_num_ticks + 1
                labels(actual_num_ticks) = format_tick_value(tick_value, data_max - data_min)
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
            if (tick_value >= data_min .and. tick_value <= data_max) then
                actual_num_ticks = actual_num_ticks + 1
                labels(actual_num_ticks) = format_tick_value(tick_value, data_max - data_min)
            end if
            
            ! Add subticks within this decade
            do subtick_idx = 1, size(subtick_multipliers)
                if (actual_num_ticks >= size(labels)) exit
                
                tick_value = decade_start * subtick_multipliers(subtick_idx)
                
                if (tick_value >= data_min .and. tick_value <= data_max .and. &
                    tick_value < 10.0_wp ** (power + 1)) then
                    actual_num_ticks = actual_num_ticks + 1
                    labels(actual_num_ticks) = format_tick_value(tick_value, data_max - data_min)
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
        
        ! Format the tick locations using consistent formatting
        do i = 1, min(actual_num_ticks, size(labels))
            labels(i) = format_tick_value(tick_locations(i), data_max - data_min)
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
        
        integer :: i
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
            if (tick_val > linear_threshold .and. tick_val <= data_max) then
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
            if (tick_val < -linear_threshold .and. tick_val >= data_min) then
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
        
        integer :: i, j
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
            call calculate_tick_labels(data_min, data_max, num_ticks, labels)
        end select
    end subroutine generate_scale_aware_tick_labels

end module fortplot_ticks