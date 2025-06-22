module fortplot_margins
    !! Common margin and axis functionality for plotting backends
    !! 
    !! This module provides shared functionality for calculating margins,
    !! plot areas, and drawing axes across different backends (PNG, PDF, etc.)
    !! Follows DRY principle by centralizing common plotting layout logic.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    intrinsic :: floor, log10
    
    private
    public :: plot_margins_t, calculate_plot_area, draw_basic_axes_frame, get_axis_tick_positions
    public :: calculate_tick_labels, format_tick_value, calculate_nice_axis_limits
    
    ! Standard matplotlib-style margins
    type :: plot_margins_t
        real(wp) :: left = 0.15_wp     ! 15% left margin
        real(wp) :: right = 0.05_wp    ! 5% right margin  
        real(wp) :: bottom = 0.15_wp   ! 15% bottom margin
        real(wp) :: top = 0.05_wp      ! 5% top margin
    end type plot_margins_t
    
    ! Plot area geometry
    type :: plot_area_t
        integer :: left, bottom, width, height
    end type plot_area_t
    
    public :: plot_area_t
    
contains

    subroutine calculate_plot_area(canvas_width, canvas_height, margins, plot_area)
        !! Calculate plot area based on canvas size and margins
        integer, intent(in) :: canvas_width, canvas_height
        type(plot_margins_t), intent(in) :: margins
        type(plot_area_t), intent(out) :: plot_area
        
        plot_area%left = int(margins%left * real(canvas_width, wp)) + 1
        plot_area%bottom = int(margins%top * real(canvas_height, wp)) + 1  ! For image coords (Y=0 at top)
        plot_area%width = canvas_width - int((margins%left + margins%right) * real(canvas_width, wp))
        plot_area%height = canvas_height - int((margins%bottom + margins%top) * real(canvas_height, wp))
    end subroutine calculate_plot_area

    subroutine get_axis_tick_positions(plot_area, num_ticks_x, num_ticks_y, &
                                      x_positions, y_positions, actual_num_x, actual_num_y)
        !! Generate tick mark positions for basic axes
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: num_ticks_x, num_ticks_y
        real(wp), intent(out) :: x_positions(:), y_positions(:)
        integer, intent(out) :: actual_num_x, actual_num_y
        
        real(wp) :: tick_spacing_x, tick_spacing_y
        integer :: i
        
        actual_num_x = min(num_ticks_x, size(x_positions))
        actual_num_y = min(num_ticks_y, size(y_positions))
        
        if (actual_num_x > 1) then
            tick_spacing_x = real(plot_area%width, wp) / real(actual_num_x - 1, wp)
            do i = 1, actual_num_x
                x_positions(i) = real(plot_area%left, wp) + real(i - 1, wp) * tick_spacing_x
            end do
        end if
        
        if (actual_num_y > 1) then
            tick_spacing_y = real(plot_area%height, wp) / real(actual_num_y - 1, wp)
            do i = 1, actual_num_y
                y_positions(i) = real(plot_area%bottom, wp) + real(i - 1, wp) * tick_spacing_y
            end do
        end if
    end subroutine get_axis_tick_positions

    subroutine draw_basic_axes_frame(plot_area, draw_line_proc)
        !! Draw basic rectangular axes frame
        !! Uses a procedure pointer to be backend-agnostic
        type(plot_area_t), intent(in) :: plot_area
        interface
            subroutine draw_line_proc(x1, y1, x2, y2)
                import :: wp
                real(wp), intent(in) :: x1, y1, x2, y2
            end subroutine
        end interface
        
        real(wp) :: left, right, top, bottom
        
        left = real(plot_area%left, wp)
        right = real(plot_area%left + plot_area%width, wp) 
        top = real(plot_area%bottom, wp)
        bottom = real(plot_area%bottom + plot_area%height, wp)
        
        ! Draw frame (rectangle)
        call draw_line_proc(left, top, right, top)        ! Top edge
        call draw_line_proc(left, bottom, right, bottom)  ! Bottom edge  
        call draw_line_proc(left, top, left, bottom)      ! Left edge
        call draw_line_proc(right, top, right, bottom)    ! Right edge
    end subroutine draw_basic_axes_frame

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

    function determine_decimal_places(range, step) result(decimal_places)
        !! Determine appropriate number of decimal places for consistent formatting
        !! following matplotlib's approach
        real(wp), intent(in) :: range, step
        integer :: decimal_places
        
        if (range >= 100.0_wp) then
            ! Large ranges use integer formatting
            decimal_places = 0
        else if (step >= 1.0_wp) then
            ! Step size >= 1 suggests integer or minimal decimal formatting
            decimal_places = 0
        else if (step >= 0.1_wp) then
            ! Medium step size - use 1 decimal place
            decimal_places = 1
        else if (step >= 0.01_wp) then
            ! Small step size - use 2 decimal places
            decimal_places = 2
        else
            ! Very small step size - use 3 decimal places
            decimal_places = 3
        end if
    end function determine_decimal_places

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

end module fortplot_margins