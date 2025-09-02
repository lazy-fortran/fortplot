module fortplot_tick_calculation
    !! Core tick calculation algorithms for linear scales
    !! 
    !! Provides:
    !! - Nice tick location algorithms following matplotlib MaxNLocator
    !! - Axis limit calculation with nice boundaries
    !! - Linear scale tick generation
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    intrinsic :: floor, log10
    
    private
    public :: calculate_tick_labels, calculate_nice_axis_limits
    public :: find_nice_tick_locations, determine_decimal_places_from_step
    public :: determine_decimals_from_ticks
    public :: format_tick_value_consistent

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
        
        ! Format the nice tick locations with consistent decimal places
        do i = 1, min(actual_num_ticks, size(labels))
            labels(i) = format_tick_value_consistent(tick_locations(i), decimal_places)
        end do
        
        ! Clear unused labels
        do i = actual_num_ticks + 1, size(labels)
            labels(i) = ''
        end do
    end subroutine calculate_tick_labels

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

    subroutine find_nice_tick_locations(data_min, data_max, target_num_ticks, &
                                       nice_min, nice_max, nice_step, &
                                       tick_locations, actual_num_ticks)
        !! Find nice tick locations following matplotlib's MaxNLocator algorithm exactly
        real(wp), intent(in) :: data_min, data_max
        integer, intent(in) :: target_num_ticks
        real(wp), intent(out) :: nice_min, nice_max, nice_step
        real(wp), intent(out) :: tick_locations(:)
        integer, intent(out) :: actual_num_ticks
        
        real(wp) :: range, rough_step, magnitude, normalized_step
        real(wp) :: nice_normalized_step, best_vmin
        integer :: low_edge, high_edge, i
        
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
        
        ! Follow matplotlib's exact algorithm:
        ! best_vmin = (data_min // step) * step
        best_vmin = floor(data_min / nice_step) * nice_step
        
        ! Calculate edge indices like matplotlib's _Edge_integer with proper tolerance
        ! low = largest n where n*step <= (data_min - best_vmin)
        low_edge = floor((data_min - best_vmin) / nice_step + 1.0e-10_wp)
        
        ! high = smallest n where n*step >= (data_max - best_vmin)  
        ! Add small epsilon to ensure floating point precision doesn't miss endpoints
        high_edge = floor((data_max - best_vmin) / nice_step + 1.0_wp - 1.0e-10_wp)
        
        ! Generate ticks: np.arange(low, high + 1) * step + best_vmin
        ! Equivalent to matplotlib's high + 1 endpoint inclusion
        actual_num_ticks = 0
        do i = low_edge, high_edge
            if (actual_num_ticks >= size(tick_locations)) exit
            actual_num_ticks = actual_num_ticks + 1
            tick_locations(actual_num_ticks) = real(i, wp) * nice_step + best_vmin
        end do
        
        ! Set nice boundaries for axis limits
        if (actual_num_ticks > 0) then
            nice_min = tick_locations(1)
            nice_max = tick_locations(actual_num_ticks)
        else
            ! No ticks generated - use data bounds as fallback
            nice_min = data_min
            nice_max = data_max
        end if
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

    function determine_decimals_from_ticks(tick_positions, n) result(decimal_places)
        !! Determine decimal places from an array of tick positions.
        !! Uses the smallest non-zero spacing as representative step.
        real(wp), intent(in) :: tick_positions(:)
        integer, intent(in) :: n
        integer :: decimal_places
        real(wp) :: step, d
        integer :: i

        decimal_places = 0
        if (n < 2) return

        step = abs(tick_positions(2) - tick_positions(1))
        do i = 3, n
            d = abs(tick_positions(i) - tick_positions(i-1))
            if (d > 1.0e-12_wp) step = min(step, d)
        end do

        decimal_places = determine_decimal_places_from_step(step)
    end function determine_decimals_from_ticks

    function format_tick_value_consistent(value, decimal_places) result(formatted)
        !! Format tick value with consistent decimal places for uniform appearance
        real(wp), intent(in) :: value
        integer, intent(in) :: decimal_places
        character(len=20) :: formatted
        character(len=10) :: format_str
        
        if (abs(value) < 1.0e-10_wp) then
            if (decimal_places == 0) then
                formatted = '0'
            else
                write(format_str, '(A, I0, A)') '(F0.', decimal_places, ')'
                write(formatted, format_str) 0.0_wp
            end if
        else if (decimal_places == 0) then
            write(formatted, '(I0)') nint(value)
        else
            write(format_str, '(A, I0, A)') '(F0.', decimal_places, ')'
            write(formatted, format_str) value
        end if
        
        call ensure_leading_zero(formatted)
    end function format_tick_value_consistent

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

end module fortplot_tick_calculation
