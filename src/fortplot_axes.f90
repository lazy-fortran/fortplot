module fortplot_axes
    !! Axes and tick generation module
    !! 
    !! This module handles axis drawing, tick computation, and label formatting
    !! for all scale types. Follows Single Responsibility Principle by focusing
    !! solely on axis-related functionality.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_scales
    implicit none
    
    private
    public :: compute_scale_ticks, format_tick_label
    public :: draw_axis_ticks, generate_log_minor_ticks
    public :: format_number_professionally
    
    integer, parameter :: MAX_TICKS = 20
    
contains

    subroutine compute_scale_ticks(scale_type, data_min, data_max, threshold, tick_positions, num_ticks)
        !! Compute tick positions for different scale types
        !! 
        !! @param scale_type: Type of scale ('linear', 'log', 'symlog')
        !! @param data_min: Minimum data value
        !! @param data_max: Maximum data value
        !! @param threshold: Threshold for symlog (ignored for others)
        !! @param tick_positions: Output array of tick positions
        !! @param num_ticks: Number of ticks generated
        
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in) :: data_min, data_max, threshold
        real(wp), intent(out) :: tick_positions(MAX_TICKS)
        integer, intent(out) :: num_ticks
        
        select case (trim(scale_type))
        case ('linear')
            call compute_linear_ticks(data_min, data_max, tick_positions, num_ticks)
        case ('log')
            call compute_log_ticks(data_min, data_max, tick_positions, num_ticks)
        case ('symlog')
            call compute_symlog_ticks(data_min, data_max, threshold, tick_positions, num_ticks)
        case default
            call compute_linear_ticks(data_min, data_max, tick_positions, num_ticks)
        end select
    end subroutine compute_scale_ticks

    subroutine compute_linear_ticks(data_min, data_max, tick_positions, num_ticks)
        !! Compute tick positions for linear scale
        real(wp), intent(in) :: data_min, data_max
        real(wp), intent(out) :: tick_positions(MAX_TICKS)
        integer, intent(out) :: num_ticks
        
        real(wp) :: range, step, nice_step, tick_value
        integer :: i, max_ticks_desired
        
        max_ticks_desired = 8
        range = data_max - data_min
        
        if (range <= 0.0_wp) then
            num_ticks = 0
            return
        end if
        
        step = range / real(max_ticks_desired, wp)
        nice_step = calculate_nice_step(step)
        
        ! Find first tick >= data_min
        tick_value = ceiling(data_min / nice_step) * nice_step
        num_ticks = 0
        
        do while (tick_value <= data_max .and. num_ticks < MAX_TICKS)
            num_ticks = num_ticks + 1
            tick_positions(num_ticks) = tick_value
            tick_value = tick_value + nice_step
        end do
    end subroutine compute_linear_ticks

    subroutine compute_log_ticks(data_min, data_max, tick_positions, num_ticks)
        !! Compute tick positions for logarithmic scale
        real(wp), intent(in) :: data_min, data_max
        real(wp), intent(out) :: tick_positions(MAX_TICKS)
        integer, intent(out) :: num_ticks
        
        real(wp) :: log_min, log_max, current_power
        integer :: start_power, end_power, power
        
        if (data_min <= 0.0_wp .or. data_max <= 0.0_wp) then
            num_ticks = 0
            return
        end if
        
        log_min = log10(data_min)
        log_max = log10(data_max)
        
        start_power = floor(log_min)
        end_power = ceiling(log_max)
        
        num_ticks = 0
        do power = start_power, end_power
            if (num_ticks >= MAX_TICKS) exit
            current_power = 10.0_wp**power
            if (current_power >= data_min .and. current_power <= data_max) then
                num_ticks = num_ticks + 1
                tick_positions(num_ticks) = current_power
            end if
        end do
    end subroutine compute_log_ticks

    subroutine compute_symlog_ticks(data_min, data_max, threshold, tick_positions, num_ticks)
        !! Compute tick positions for symmetric logarithmic scale
        real(wp), intent(in) :: data_min, data_max, threshold
        real(wp), intent(out) :: tick_positions(MAX_TICKS)
        integer, intent(out) :: num_ticks
        
        num_ticks = 0
        
        ! Add negative log region ticks
        if (data_min < -threshold) then
            call add_negative_symlog_ticks(data_min, -threshold, tick_positions, num_ticks)
        end if
        
        ! Add linear region ticks
        if (data_min <= threshold .and. data_max >= -threshold) then
            call add_linear_symlog_ticks(max(data_min, -threshold), min(data_max, threshold), &
                                       tick_positions, num_ticks)
        end if
        
        ! Add positive log region ticks
        if (data_max > threshold) then
            call add_positive_symlog_ticks(threshold, data_max, tick_positions, num_ticks)
        end if
    end subroutine compute_symlog_ticks

    function format_tick_label(value, scale_type) result(label)
        !! Format a tick value as a string label
        !! 
        !! @param value: Tick value to format
        !! @param scale_type: Scale type for context
        !! @return label: Formatted label string
        
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: scale_type
        character(len=20) :: label
        
        if (abs(value) < 1.0e-10_wp) then
            label = '0'
        else if (trim(scale_type) == 'log' .and. is_power_of_ten(value)) then
            label = format_power_of_ten(value)
        else
            label = format_number_professionally(value)
        end if
    end function format_tick_label

    function format_number_professionally(value) result(formatted)
        !! Format a number with appropriate precision and style
        !! 
        !! @param value: Number to format
        !! @return formatted: Professionally formatted string
        
        real(wp), intent(in) :: value
        character(len=20) :: formatted
        real(wp) :: abs_value
        
        abs_value = abs(value)
        
        if (abs_value == 0.0_wp) then
            formatted = '0'
        else if (abs_value >= 1.0e4_wp .or. abs_value < 1.0e-3_wp) then
            ! Use scientific notation for very large or very small numbers
            write(formatted, '(ES12.2)') value
            call clean_scientific_notation(formatted)
        else if (abs_value >= 100.0_wp) then
            ! Integer format for large whole numbers
            write(formatted, '(F0.0)') value
        else if (abs_value >= 1.0_wp) then
            ! 1-2 decimal places for numbers >= 1
            write(formatted, '(F0.2)') value
            call remove_trailing_zeros(formatted)
        else
            ! More decimal places for small numbers
            write(formatted, '(F0.4)') value
            call remove_trailing_zeros(formatted)
        end if
    end function format_number_professionally

    subroutine draw_axis_ticks(backend, tick_positions, num_ticks, is_x_axis, &
                              data_min, data_max, scale_type, plot_area)
        !! Draw axis ticks and labels
        !! 
        !! @param backend: Plotting backend
        !! @param tick_positions: Array of tick positions in data coordinates
        !! @param num_ticks: Number of ticks
        !! @param is_x_axis: True for x-axis, false for y-axis
        !! @param data_min: Minimum data value
        !! @param data_max: Maximum data value
        !! @param scale_type: Scale type
        !! @param plot_area: Plot area bounds [x, y, width, height]
        
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: tick_positions(:)
        integer, intent(in) :: num_ticks
        logical, intent(in) :: is_x_axis
        real(wp), intent(in) :: data_min, data_max
        character(len=*), intent(in) :: scale_type
        integer, intent(in) :: plot_area(4)
        
        integer :: i
        real(wp) :: tick_screen_pos
        character(len=20) :: label
        
        do i = 1, num_ticks
            if (is_x_axis) then
                tick_screen_pos = transform_x_coordinate(tick_positions(i), data_min, data_max, plot_area(3))
                tick_screen_pos = tick_screen_pos + plot_area(1)
            else
                tick_screen_pos = transform_y_coordinate(tick_positions(i), data_min, data_max, plot_area(4), .true.)
                tick_screen_pos = tick_screen_pos + plot_area(2)
            end if
            
            label = format_tick_label(tick_positions(i), scale_type)
            call draw_single_tick(backend, tick_screen_pos, label, is_x_axis, plot_area)
        end do
    end subroutine draw_axis_ticks

    subroutine generate_log_minor_ticks(major_tick, minor_ticks, num_minor)
        !! Generate minor tick positions for log scale
        real(wp), intent(in) :: major_tick
        real(wp), intent(out) :: minor_ticks(8)
        integer, intent(out) :: num_minor
        
        integer :: i
        real(wp) :: decade_start
        
        decade_start = major_tick
        num_minor = 8
        
        do i = 1, 8
            minor_ticks(i) = decade_start * (i + 1)
        end do
    end subroutine generate_log_minor_ticks

    ! Helper subroutines (implementation details)
    
    function calculate_nice_step(raw_step) result(nice_step)
        real(wp), intent(in) :: raw_step
        real(wp) :: nice_step, magnitude, normalized
        
        magnitude = 10.0_wp**floor(log10(raw_step))
        normalized = raw_step / magnitude
        
        if (normalized <= 1.0_wp) then
            nice_step = magnitude
        else if (normalized <= 2.0_wp) then
            nice_step = 2.0_wp * magnitude
        else if (normalized <= 5.0_wp) then
            nice_step = 5.0_wp * magnitude
        else
            nice_step = 10.0_wp * magnitude
        end if
    end function calculate_nice_step
    
    subroutine add_negative_symlog_ticks(data_min, upper_bound, tick_positions, num_ticks)
        real(wp), intent(in) :: data_min, upper_bound
        real(wp), intent(inout) :: tick_positions(MAX_TICKS)
        integer, intent(inout) :: num_ticks
        ! Implementation for negative symlog region
    end subroutine add_negative_symlog_ticks
    
    subroutine add_linear_symlog_ticks(lower_bound, upper_bound, tick_positions, num_ticks)
        real(wp), intent(in) :: lower_bound, upper_bound
        real(wp), intent(inout) :: tick_positions(MAX_TICKS)
        integer, intent(inout) :: num_ticks
        ! Implementation for linear symlog region
    end subroutine add_linear_symlog_ticks
    
    subroutine add_positive_symlog_ticks(lower_bound, data_max, tick_positions, num_ticks)
        real(wp), intent(in) :: lower_bound, data_max
        real(wp), intent(inout) :: tick_positions(MAX_TICKS)
        integer, intent(inout) :: num_ticks
        ! Implementation for positive symlog region
    end subroutine add_positive_symlog_ticks
    
    function is_power_of_ten(value) result(is_power)
        real(wp), intent(in) :: value
        logical :: is_power
        real(wp) :: log_val
        log_val = log10(abs(value))
        is_power = abs(log_val - nint(log_val)) < 1.0e-10_wp
    end function is_power_of_ten
    
    function format_power_of_ten(value) result(formatted)
        real(wp), intent(in) :: value
        character(len=20) :: formatted
        integer :: exponent
        exponent = nint(log10(abs(value)))
        if (value < 0.0_wp) then
            write(formatted, '(A, I0)') '-10^', exponent
        else
            write(formatted, '(A, I0)') '10^', exponent
        end if
    end function format_power_of_ten
    
    subroutine clean_scientific_notation(str)
        character(len=*), intent(inout) :: str
        ! Clean up scientific notation formatting
    end subroutine clean_scientific_notation
    
    subroutine remove_trailing_zeros(str)
        character(len=*), intent(inout) :: str
        integer :: i
        ! Remove trailing zeros after decimal point
        do i = len_trim(str), 1, -1
            if (str(i:i) == '0') then
                str(i:i) = ' '
            else if (str(i:i) == '.') then
                str(i:i) = ' '
                exit
            else
                exit
            end if
        end do
    end subroutine remove_trailing_zeros
    
    subroutine draw_single_tick(backend, screen_pos, label, is_x_axis, plot_area)
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: screen_pos
        character(len=*), intent(in) :: label
        logical, intent(in) :: is_x_axis
        integer, intent(in) :: plot_area(4)
        ! Draw a single tick mark and label
    end subroutine draw_single_tick

end module fortplot_axes