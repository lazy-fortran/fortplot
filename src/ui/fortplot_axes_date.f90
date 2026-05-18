module fortplot_axes_date
    !! Date axis handling module
    !!
    !! This module handles date-specific tick computation, format selection,
    !! and conversion between date values and Unix seconds.

    use, intrinsic :: iso_fortran_env, only: int64, wp => real64
    use fortplot_datetime, only: datetime_t, datetime_to_unix_seconds, &
                                 unix_seconds_to_datetime, format_unix_seconds, &
                                 unix_seconds_from_julian_day, &
                                 julian_day_from_unix_seconds, SECONDS_PER_DAY
    implicit none

    private
    public :: compute_date_ticks, pick_fixed_step_seconds, &
              is_date_scale, format_date_tick_label, default_date_format, &
              date_value_to_unix_seconds, date_value_from_unix_seconds

contains

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
        real(wp), intent(out) :: tick_positions(20)
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
            do while (tick_s <= smax .and. num_ticks < 20)
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
            do while (tick_s <= smax .and. num_ticks < 20)
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
        do while (tick_s <= smax .and. num_ticks < 20)
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
        else if (target <= 2_int64*int(SECONDS_PER_DAY, int64)) then
            step_s = 2_int64*int(SECONDS_PER_DAY, int64)
        else if (target <= 3_int64*int(SECONDS_PER_DAY, int64)) then
            step_s = 3_int64*int(SECONDS_PER_DAY, int64)
        else if (target <= 5_int64*int(SECONDS_PER_DAY, int64)) then
            step_s = 5_int64*int(SECONDS_PER_DAY, int64)
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

end module fortplot_axes_date
