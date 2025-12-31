module fortplot_axes
    !! Axes and tick generation module
    !!
    !! This module handles axis drawing, tick computation, and label formatting
    !! for all scale types. Follows Single Responsibility Principle by focusing
    !! solely on axis-related functionality.

    use, intrinsic :: iso_fortran_env, only: int64, wp => real64
    use fortplot_context
    use fortplot_scales
    use fortplot_constants, only: SCIENTIFIC_THRESHOLD_HIGH
    use fortplot_tick_formatting, only: format_power_of_ten_label
    use fortplot_datetime, only: datetime_t, datetime_to_unix_seconds, &
                                 unix_seconds_to_datetime, format_unix_seconds, &
                                 unix_seconds_from_julian_day, &
                                 julian_day_from_unix_seconds, SECONDS_PER_DAY
    implicit none

    private
    public :: compute_scale_ticks, format_tick_label, MAX_TICKS

    integer, parameter :: MAX_TICKS = 20

    ! Format threshold constants for tick label formatting
    ! SCIENTIFIC_THRESHOLD_HIGH is imported from fortplot_constants
    real(wp), parameter :: SCIENTIFIC_THRESHOLD_LOW = 0.01_wp
    ! Use scientific for abs(value) < this
    real(wp), parameter :: NO_DECIMAL_THRESHOLD = 100.0_wp
    ! No decimal places for abs(value) >= this
    real(wp), parameter :: ONE_DECIMAL_THRESHOLD = 10.0_wp
    ! One decimal place for abs(value) >= this
    real(wp), parameter :: TWO_DECIMAL_THRESHOLD = 1.0_wp
    ! Two decimal places for abs(value) >= this
    real(wp), parameter :: TICK_EPS = 1.0e-10_wp
    ! Numerical tolerance for tick comparisons

contains

    subroutine compute_scale_ticks(scale_type, data_min, data_max, threshold, &
                                   tick_positions, num_ticks)
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
            call compute_symlog_ticks(data_min, data_max, threshold, &
                                      tick_positions, num_ticks)
        case ('date', 'date_jd')
            call compute_date_ticks(scale_type, data_min, data_max, &
                                    tick_positions, num_ticks)
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
        integer :: max_ticks_desired

        max_ticks_desired = 8
        range = data_max - data_min

        if (range <= 0.0_wp) then
            num_ticks = 0
            return
        end if

        step = range/real(max_ticks_desired, wp)
        nice_step = calculate_nice_step(step)

        ! Find first tick >= data_min
        tick_value = ceiling(data_min/nice_step)*nice_step
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

    subroutine compute_symlog_ticks(data_min, data_max, threshold, &
                                    tick_positions, num_ticks)
        !! Compute tick positions for symmetric logarithmic scale
        real(wp), intent(in) :: data_min, data_max, threshold
        real(wp), intent(out) :: tick_positions(MAX_TICKS)
        integer, intent(out) :: num_ticks

        num_ticks = 0

        ! Add negative log region ticks
        if (data_min < -threshold) then
            call add_negative_symlog_ticks(data_min, min(-threshold, data_max), &
                                           tick_positions, num_ticks)
        end if

        ! Add linear region ticks (only for the region within threshold bounds)
        if (max(data_min, -threshold) <= min(data_max, threshold)) then
            call add_linear_symlog_ticks(max(data_min, -threshold), min(data_max, &
                                                                        threshold), &
                                         tick_positions, num_ticks)
        end if

        ! Add positive log region ticks
        if (data_max > threshold) then
            call add_positive_symlog_ticks(max(threshold, data_min), data_max, &
                                           tick_positions, num_ticks)
        end if

        if (num_ticks > 1) then
            call sort_tick_positions(tick_positions, num_ticks)
        end if
    end subroutine compute_symlog_ticks

    function format_tick_label(value, scale_type, date_format, data_min, data_max) &
        result(label)
        !! Format a tick value as a string label
        !!
        !! @param value: Tick value to format
        !! @param scale_type: Scale type for context
        !! @return label: Formatted label string

        real(wp), intent(in) :: value
        character(len=*), intent(in) :: scale_type
        character(len=*), intent(in), optional :: date_format
        real(wp), intent(in), optional :: data_min, data_max
        character(len=50) :: label
        real(wp) :: abs_value
        logical :: is_log_scale

        abs_value = abs(value)
        if (is_date_scale(scale_type)) then
            label = format_date_tick_label(value, scale_type, date_format, &
                                           data_min, data_max)
            label = adjustl(label)
            return
        end if
        is_log_scale = trim(scale_type) == 'log' .or. trim(scale_type) == 'symlog'

        if (abs_value <= epsilon(1.0_wp)) then
            label = '0'
        else if (is_log_scale .and. is_power_of_ten(value)) then
            ! Unify log and symlog formatting: show powers of ten with superscript
            label = format_power_of_ten_label(value)
        else if (.not. is_log_scale .and. abs_value < TICK_EPS) then
            label = '0'
        else if (abs_value >= SCIENTIFIC_THRESHOLD_HIGH .or. abs_value < &
                 SCIENTIFIC_THRESHOLD_LOW) then
            ! Use scientific notation for very large or very small values
            write (label, '(ES10.2)') value
            label = adjustl(label)
        else if (abs_value >= NO_DECIMAL_THRESHOLD) then
            ! No decimal places for values >= 100
            write (label, '(F0.0)') value
        else if (abs_value >= ONE_DECIMAL_THRESHOLD) then
            ! One decimal place for values >= 10
            write (label, '(F0.1)') value
        else if (abs_value >= TWO_DECIMAL_THRESHOLD) then
            ! Two decimal places for values >= 1
            write (label, '(F0.2)') value
        else
            ! Three decimal places for small values
            write (label, '(F0.3)') value
        end if

        label = adjustl(label)
    end function format_tick_label

    pure logical function is_date_scale(scale_type) result(is_date)
        character(len=*), intent(in) :: scale_type

        is_date = trim(scale_type) == 'date' .or. trim(scale_type) == 'date_jd'
    end function is_date_scale

    function format_date_tick_label(value, scale_type, date_format, data_min, &
                                    data_max) &
        result(label)
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: scale_type
        character(len=*), intent(in), optional :: date_format
        real(wp), intent(in), optional :: data_min, data_max
        character(len=50) :: label

        integer(int64) :: seconds
        character(len=64) :: fmt

        seconds = date_value_to_unix_seconds(value, scale_type)

        if (present(date_format)) then
            if (len_trim(date_format) > 0) then
                fmt = trim(date_format)
            else
                fmt = trim(default_date_format(scale_type, data_min, data_max))
            end if
        else
            fmt = trim(default_date_format(scale_type, data_min, data_max))
        end if

        label = format_unix_seconds(seconds, trim(fmt))
    end function format_date_tick_label

    function default_date_format(scale_type, data_min, data_max) result(fmt)
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in), optional :: data_min, data_max
        character(len=32) :: fmt

        integer(int64) :: smin, smax
        real(wp) :: range_seconds

        fmt = '%Y-%m-%d'

        if (.not. present(data_min) .or. .not. present(data_max)) return

        smin = date_value_to_unix_seconds(data_min, scale_type)
        smax = date_value_to_unix_seconds(data_max, scale_type)
        range_seconds = abs(real(smax - smin, wp))

        if (range_seconds >= 2.0_wp*365.0_wp*SECONDS_PER_DAY) then
            fmt = '%Y'
        else if (range_seconds >= 60.0_wp*SECONDS_PER_DAY) then
            fmt = '%Y-%m'
        else if (range_seconds >= 2.0_wp*SECONDS_PER_DAY) then
            fmt = '%Y-%m-%d'
        else if (range_seconds >= 2.0_wp*3600.0_wp) then
            fmt = '%m-%d %H:%M'
        else
            fmt = '%H:%M:%S'
        end if
    end function default_date_format

    integer(int64) function date_value_to_unix_seconds(value, scale_type) &
        result(seconds)
        real(wp), intent(in) :: value
        character(len=*), intent(in) :: scale_type

        if (trim(scale_type) == 'date_jd') then
            seconds = unix_seconds_from_julian_day(value)
        else
            seconds = nint(value, int64)
        end if
    end function date_value_to_unix_seconds

    real(wp) function date_value_from_unix_seconds(seconds, scale_type) result(value)
        integer(int64), intent(in) :: seconds
        character(len=*), intent(in) :: scale_type

        if (trim(scale_type) == 'date_jd') then
            value = julian_day_from_unix_seconds(seconds)
        else
            value = real(seconds, wp)
        end if
    end function date_value_from_unix_seconds

    subroutine compute_date_ticks(scale_type, data_min, data_max, &
                                  tick_positions, num_ticks)
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in) :: data_min, data_max
        real(wp), intent(out) :: tick_positions(MAX_TICKS)
        integer, intent(out) :: num_ticks

        integer(int64) :: smin, smax, range_s
        integer(int64) :: step_s, tick_s
        integer :: month_step, year_step
        integer :: y, m, d, hh, mm, ss
        integer :: y_tick, m_tick
        logical :: use_months, use_years

        num_ticks = 0
        if (data_max <= data_min) return

        smin = date_value_to_unix_seconds(data_min, scale_type)
        smax = date_value_to_unix_seconds(data_max, scale_type)
        if (smax <= smin) return

        range_s = smax - smin
        use_months = range_s >= int(60.0_wp*SECONDS_PER_DAY, int64) .and. &
                     range_s < int(2.0_wp*365.0_wp*SECONDS_PER_DAY, int64)
        use_years = range_s >= int(2.0_wp*365.0_wp*SECONDS_PER_DAY, int64)

        if (use_years) then
            year_step = pick_year_step(range_s)
            call unix_seconds_to_ymdhms(smin, y, m, d, hh, mm, ss)
            y_tick = y
            if (.not. (m == 1 .and. d == 1 .and. hh == 0 .and. mm == 0 .and. ss &
                       == 0)) then
                y_tick = y_tick + 1
            end if
            tick_s = ymdhms_to_unix_seconds(y_tick, 1, 1, 0, 0, 0)
            do while (tick_s <= smax .and. num_ticks < MAX_TICKS)
                num_ticks = num_ticks + 1
                tick_positions(num_ticks) = date_value_from_unix_seconds(tick_s, &
                                                                         scale_type)
                y_tick = y_tick + year_step
                tick_s = ymdhms_to_unix_seconds(y_tick, 1, 1, 0, 0, 0)
            end do
            return
        end if

        if (use_months) then
            month_step = pick_month_step(range_s)
            call unix_seconds_to_ymdhms(smin, y, m, d, hh, mm, ss)
            y_tick = y
            m_tick = m
            if (.not. (d == 1 .and. hh == 0 .and. mm == 0 .and. ss == 0)) then
                call add_months(y_tick, m_tick, 1)
            end if
            tick_s = ymdhms_to_unix_seconds(y_tick, m_tick, 1, 0, 0, 0)
            do while (tick_s <= smax .and. num_ticks < MAX_TICKS)
                num_ticks = num_ticks + 1
                tick_positions(num_ticks) = date_value_from_unix_seconds(tick_s, &
                                                                         scale_type)
                call add_months(y_tick, m_tick, month_step)
                tick_s = ymdhms_to_unix_seconds(y_tick, m_tick, 1, 0, 0, 0)
            end do
            return
        end if

        step_s = pick_fixed_step_seconds(range_s)
        tick_s = ceiling_div_int64(smin, step_s)*step_s
        do while (tick_s <= smax .and. num_ticks < MAX_TICKS)
            num_ticks = num_ticks + 1
            tick_positions(num_ticks) = date_value_from_unix_seconds(tick_s, scale_type)
            tick_s = tick_s + step_s
        end do
    end subroutine compute_date_ticks

    integer(int64) function pick_fixed_step_seconds(range_s) result(step_s)
        integer(int64), intent(in) :: range_s
        integer(int64) :: target

        target = max(1_int64, range_s/8_int64)

        if (target <= 1_int64) then
            step_s = 1_int64
        else if (target <= 5_int64) then
            step_s = 5_int64
        else if (target <= 10_int64) then
            step_s = 10_int64
        else if (target <= 30_int64) then
            step_s = 30_int64
        else if (target <= 60_int64) then
            step_s = 60_int64
        else if (target <= 5_int64*60_int64) then
            step_s = 5_int64*60_int64
        else if (target <= 15_int64*60_int64) then
            step_s = 15_int64*60_int64
        else if (target <= 30_int64*60_int64) then
            step_s = 30_int64*60_int64
        else if (target <= 3600_int64) then
            step_s = 3600_int64
        else if (target <= 6_int64*3600_int64) then
            step_s = 6_int64*3600_int64
        else if (target <= 12_int64*3600_int64) then
            step_s = 12_int64*3600_int64
        else if (target <= int(SECONDS_PER_DAY, int64)) then
            step_s = int(SECONDS_PER_DAY, int64)
        else if (target <= 7_int64*int(SECONDS_PER_DAY, int64)) then
            step_s = 7_int64*int(SECONDS_PER_DAY, int64)
        else
            step_s = 14_int64*int(SECONDS_PER_DAY, int64)
        end if
    end function pick_fixed_step_seconds

    integer function pick_month_step(range_s) result(step_months)
        integer(int64), intent(in) :: range_s
        real(wp) :: range_days

        range_days = real(range_s, wp)/SECONDS_PER_DAY
        if (range_days <= 120.0_wp) then
            step_months = 1
        else if (range_days <= 365.0_wp) then
            step_months = 3
        else
            step_months = 6
        end if
    end function pick_month_step

    integer function pick_year_step(range_s) result(step_years)
        integer(int64), intent(in) :: range_s
        real(wp) :: range_years

        range_years = real(range_s, wp)/(365.0_wp*SECONDS_PER_DAY)
        if (range_years <= 5.0_wp) then
            step_years = 1
        else if (range_years <= 20.0_wp) then
            step_years = 2
        else
            step_years = 5
        end if
    end function pick_year_step

    integer(int64) function ceiling_div_int64(a, b) result(q)
        integer(int64), intent(in) :: a, b

        q = a/b
        if (mod(a, b) /= 0_int64) then
            if ((a > 0_int64) .eqv. (b > 0_int64)) q = q + 1_int64
        end if
    end function ceiling_div_int64

    subroutine unix_seconds_to_ymdhms(seconds, year, month, day, hour, minute, second)
        integer(int64), intent(in) :: seconds
        integer, intent(out) :: year, month, day, hour, minute, second
        type(datetime_t) :: dt

        dt = unix_seconds_to_datetime(seconds)
        year = dt%year
        month = dt%month
        day = dt%day
        hour = dt%hour
        minute = dt%minute
        second = dt%second
    end subroutine unix_seconds_to_ymdhms

    integer(int64) function ymdhms_to_unix_seconds(year, month, day, hour, &
                                                   minute, second) &
        result(seconds)
        integer, intent(in) :: year, month, day, hour, minute, second
        type(datetime_t) :: dt

        dt%year = year
        dt%month = month
        dt%day = day
        dt%hour = hour
        dt%minute = minute
        dt%second = second
        seconds = datetime_to_unix_seconds(dt)
    end function ymdhms_to_unix_seconds

    subroutine add_months(year, month, delta_months)
        integer, intent(inout) :: year, month
        integer, intent(in) :: delta_months
        integer :: total

        total = (year*12 + (month - 1)) + delta_months
        year = total/12
        month = mod(total, 12) + 1
    end subroutine add_months

    ! Helper subroutines (implementation details)

    function calculate_nice_step(raw_step) result(nice_step)
        real(wp), intent(in) :: raw_step
        real(wp) :: nice_step, magnitude, normalized

        magnitude = 10.0_wp**floor(log10(raw_step))
        normalized = raw_step/magnitude

        if (normalized <= 1.0_wp) then
            nice_step = magnitude
        else if (normalized <= 2.0_wp) then
            nice_step = 2.0_wp*magnitude
        else if (normalized <= 5.0_wp) then
            nice_step = 5.0_wp*magnitude
        else
            nice_step = 10.0_wp*magnitude
        end if
    end function calculate_nice_step

    subroutine add_negative_symlog_ticks(data_min, upper_bound, &
                                         tick_positions, num_ticks)
        !! Add ticks for negative logarithmic region of symlog scale
        real(wp), intent(in) :: data_min, upper_bound
        real(wp), intent(inout) :: tick_positions(MAX_TICKS)
        integer, intent(inout) :: num_ticks

        real(wp) :: log_min, log_max, current_power
        integer :: start_power, end_power, power

        if (data_min >= 0.0_wp .or. upper_bound >= 0.0_wp .or. upper_bound <= &
            data_min) return

        ! Work with positive values for log calculations
        ! For negative range [-500, -1], we want powers that give us ticks in that range
        log_min = log10(-upper_bound)  ! log10(1) = 0 (closer to zero)
        log_max = log10(-data_min)     ! log10(500) = ~2.7 (larger magnitude)

        start_power = floor(log_min)
        end_power = ceiling(log_max)

        do power = start_power, end_power
            if (num_ticks >= MAX_TICKS) exit
            current_power = -(10.0_wp**power)

            if (current_power >= data_min - TICK_EPS .and. &
                current_power <= upper_bound + TICK_EPS) then
                if (.not. tick_exists(current_power, tick_positions, num_ticks)) then
                    num_ticks = num_ticks + 1
                    tick_positions(num_ticks) = current_power
                end if
            end if
        end do
    end subroutine add_negative_symlog_ticks

    subroutine add_linear_symlog_ticks(lower_bound, upper_bound, &
                                       tick_positions, num_ticks)
        !! Add ticks for linear region of symlog scale
        real(wp), intent(in) :: lower_bound, upper_bound
        real(wp), intent(inout) :: tick_positions(MAX_TICKS)
        integer, intent(inout) :: num_ticks

        real(wp) :: range, step, tick_value
        integer :: max_linear_ticks

        if (upper_bound <= lower_bound) return

        range = upper_bound - lower_bound
        max_linear_ticks = 5  ! Reasonable number for linear region

        ! Always include zero if it's in the range
        if (lower_bound <= 0.0_wp .and. upper_bound >= 0.0_wp .and. num_ticks < &
            MAX_TICKS) then
            if (.not. tick_exists(0.0_wp, tick_positions, num_ticks)) then
                num_ticks = num_ticks + 1
                tick_positions(num_ticks) = 0.0_wp
            end if
        end if

        ! Add additional linear ticks
        step = range/real(max_linear_ticks + 1, wp)
        step = calculate_nice_step(step)

        ! Find first tick >= lower_bound
        tick_value = ceiling(lower_bound/step)*step

        do while (tick_value <= upper_bound .and. num_ticks < MAX_TICKS)
            ! Skip zero if already added, avoid duplicates
            if (abs(tick_value) > 1.0e-10_wp) then
                if (.not. tick_exists(tick_value, tick_positions, num_ticks)) then
                    num_ticks = num_ticks + 1
                    tick_positions(num_ticks) = tick_value
                end if
            end if
            tick_value = tick_value + step
        end do
    end subroutine add_linear_symlog_ticks

    subroutine add_positive_symlog_ticks(lower_bound, data_max, &
                                         tick_positions, num_ticks)
        !! Add ticks for positive logarithmic region of symlog scale
        real(wp), intent(in) :: lower_bound, data_max
        real(wp), intent(inout) :: tick_positions(MAX_TICKS)
        integer, intent(inout) :: num_ticks

        real(wp) :: log_min, log_max, current_power
        integer :: start_power, end_power, power

        if (lower_bound <= 0.0_wp .or. data_max <= 0.0_wp) return

        log_min = log10(lower_bound)
        log_max = log10(data_max)

        start_power = floor(log_min)
        end_power = ceiling(log_max)

        do power = start_power, end_power
            if (num_ticks >= MAX_TICKS) exit
            current_power = 10.0_wp**power

            if (current_power >= lower_bound - TICK_EPS .and. &
                current_power <= data_max + TICK_EPS) then
                if (.not. tick_exists(current_power, tick_positions, num_ticks)) then
                    num_ticks = num_ticks + 1
                    tick_positions(num_ticks) = current_power
                end if
            end if
        end do
    end subroutine add_positive_symlog_ticks

    subroutine sort_tick_positions(values, count)
        real(wp), intent(inout) :: values(MAX_TICKS)
        integer, intent(in) :: count
        integer :: i, j
        real(wp) :: key

        if (count <= 1) return

        do i = 2, count
            key = values(i)
            j = i - 1
            do
                if (j < 1) exit
                if (values(j) <= key) exit
                values(j + 1) = values(j)
                j = j - 1
            end do
            values(j + 1) = key
        end do
    end subroutine sort_tick_positions

    function is_power_of_ten(value) result(is_power)
        real(wp), intent(in) :: value
        logical :: is_power
        real(wp) :: log_val
        log_val = log10(abs(value))
        is_power = abs(log_val - nint(log_val)) < 1.0e-10_wp
    end function is_power_of_ten

    ! format_power_of_ten moved to fortplot_tick_formatting to reduce duplication

    logical function tick_exists(value, tick_positions, num_ticks)
        real(wp), intent(in) :: value
        real(wp), intent(in) :: tick_positions(MAX_TICKS)
        integer, intent(in) :: num_ticks
        integer :: i
        real(wp) :: tolerance

        tolerance = TICK_EPS*max(1.0_wp, abs(value))
        tick_exists = .false.

        do i = 1, num_ticks
            if (abs(tick_positions(i) - value) <= tolerance) then
                tick_exists = .true.
                return
            end if
        end do
    end function tick_exists

end module fortplot_axes
