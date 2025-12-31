module fortplot_datetime
    !! Minimal datetime utilities for date/time axes.
    !!
    !! Provides:
    !! - datetime_t derived type
    !! - Unix seconds <-> datetime conversion (UTC, proleptic Gregorian)
    !! - Julian day <-> Unix seconds conversion
    !! - Small strftime-like formatter (limited tokens)

    use, intrinsic :: iso_fortran_env, only: int64, wp => real64
    implicit none

    private
    public :: datetime_t
    public :: datetime_to_unix_seconds, unix_seconds_to_datetime
    public :: format_unix_seconds
    public :: unix_seconds_from_julian_day, julian_day_from_unix_seconds
    public :: SECONDS_PER_DAY, JD_UNIX_EPOCH

    real(wp), parameter :: SECONDS_PER_DAY = 86400.0_wp
    real(wp), parameter :: JD_UNIX_EPOCH = 2440587.5_wp

    type :: datetime_t
        integer :: year = 1970
        integer :: month = 1
        integer :: day = 1
        integer :: hour = 0
        integer :: minute = 0
        integer :: second = 0
    end type datetime_t

contains

    pure elemental function datetime_to_unix_seconds(dt) result(seconds)
        type(datetime_t), intent(in) :: dt
        integer(int64) :: seconds
        integer(int64) :: days

        days = days_from_civil(int(dt%year, int64), int(dt%month, int64), &
                               int(dt%day, int64))
        seconds = days*int(SECONDS_PER_DAY, int64) + &
                  int(dt%hour, int64)*3600_int64 + &
                  int(dt%minute, int64)*60_int64 + &
                  int(dt%second, int64)
    end function datetime_to_unix_seconds

    pure elemental function unix_seconds_to_datetime(seconds) result(dt)
        integer(int64), intent(in) :: seconds
        type(datetime_t) :: dt
        integer(int64) :: days, secs_of_day
        integer(int64) :: year_i, month_i, day_i

        days = floor_div_int64(seconds, int(SECONDS_PER_DAY, int64))
        secs_of_day = seconds - days*int(SECONDS_PER_DAY, int64)
        if (secs_of_day < 0_int64) then
            secs_of_day = secs_of_day + int(SECONDS_PER_DAY, int64)
            days = days - 1_int64
        end if

        call civil_from_days(days, year_i, month_i, day_i)

        dt%year = int(year_i)
        dt%month = int(month_i)
        dt%day = int(day_i)
        dt%hour = int(secs_of_day/3600_int64)
        dt%minute = int(mod(secs_of_day, 3600_int64)/60_int64)
        dt%second = int(mod(secs_of_day, 60_int64))
    end function unix_seconds_to_datetime

    pure function unix_seconds_from_julian_day(jd) result(seconds)
        real(wp), intent(in) :: jd
        integer(int64) :: seconds

        seconds = nint((jd - JD_UNIX_EPOCH)*SECONDS_PER_DAY, int64)
    end function unix_seconds_from_julian_day

    pure function julian_day_from_unix_seconds(seconds) result(jd)
        integer(int64), intent(in) :: seconds
        real(wp) :: jd

        jd = JD_UNIX_EPOCH + real(seconds, wp)/SECONDS_PER_DAY
    end function julian_day_from_unix_seconds

    pure function format_unix_seconds(seconds, format) result(text)
        integer(int64), intent(in) :: seconds
        character(len=*), intent(in) :: format
        character(len=64) :: text

        type(datetime_t) :: dt

        dt = unix_seconds_to_datetime(seconds)
        text = format_datetime(dt, format)
        text = adjustl(text)
    end function format_unix_seconds

    pure function format_datetime(dt, format) result(text)
        type(datetime_t), intent(in) :: dt
        character(len=*), intent(in) :: format
        character(len=64) :: text

        integer :: i, out_pos
        character :: c
        character(len=16) :: temp

        text = ''
        out_pos = 1

        i = 1
        do while (i <= len_trim(format))
            c = format(i:i)
            if (c == '%' .and. i < len_trim(format)) then
                i = i + 1
                select case (format(i:i))
                case ('Y')
                    write (temp, '(I4.4)') dt%year
                    call append_chunk(text, out_pos, trim(temp))
                case ('m')
                    write (temp, '(I2.2)') dt%month
                    call append_chunk(text, out_pos, trim(temp))
                case ('d')
                    write (temp, '(I2.2)') dt%day
                    call append_chunk(text, out_pos, trim(temp))
                case ('H')
                    write (temp, '(I2.2)') dt%hour
                    call append_chunk(text, out_pos, trim(temp))
                case ('M')
                    write (temp, '(I2.2)') dt%minute
                    call append_chunk(text, out_pos, trim(temp))
                case ('S')
                    write (temp, '(I2.2)') dt%second
                    call append_chunk(text, out_pos, trim(temp))
                case ('%')
                    call append_char(text, out_pos, '%')
                case default
                    call append_char(text, out_pos, '%')
                    call append_char(text, out_pos, format(i:i))
                end select
            else
                call append_char(text, out_pos, c)
            end if
            i = i + 1
        end do
    end function format_datetime

    pure subroutine append_char(buffer, pos, c)
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: pos
        character, intent(in) :: c

        if (pos > len(buffer)) return
        buffer(pos:pos) = c
        pos = pos + 1
    end subroutine append_char

    pure subroutine append_chunk(buffer, pos, chunk)
        character(len=*), intent(inout) :: buffer
        integer, intent(inout) :: pos
        character(len=*), intent(in) :: chunk

        integer :: j, n

        n = len_trim(chunk)
        do j = 1, n
            call append_char(buffer, pos, chunk(j:j))
        end do
    end subroutine append_chunk

    pure function floor_div_int64(a, b) result(q)
        integer(int64), intent(in) :: a, b
        integer(int64) :: q
        integer(int64) :: r

        q = a/b
        r = mod(a, b)
        if (r /= 0_int64 .and. ((r > 0_int64) .neqv. (b > 0_int64))) then
            q = q - 1_int64
        end if
    end function floor_div_int64

    pure function days_from_civil(year, month, day) result(days)
        integer(int64), intent(in) :: year, month, day
        integer(int64) :: days
        integer(int64) :: y, m, era, yoe, doy, doe

        y = year
        m = month
        if (m <= 2_int64) y = y - 1_int64

        era = floor_div_int64(y, 400_int64)
        yoe = y - era*400_int64

        doy = (153_int64*(m + merge(-3_int64, 9_int64, m > 2_int64)) + 2_int64)/ &
              5_int64 + (day - 1_int64)
        doe = yoe*365_int64 + yoe/4_int64 - yoe/100_int64 + doy

        days = era*146097_int64 + doe - 719468_int64
    end function days_from_civil

    pure subroutine civil_from_days(days, year, month, day)
        integer(int64), intent(in) :: days
        integer(int64), intent(out) :: year, month, day

        integer(int64) :: z, era, doe, yoe, doy, mp

        z = days + 719468_int64
        era = floor_div_int64(z, 146097_int64)
        doe = z - era*146097_int64
        yoe = (doe - doe/1460_int64 + doe/36524_int64 - doe/146096_int64)/ &
              365_int64
        year = yoe + era*400_int64
        doy = doe - (365_int64*yoe + yoe/4_int64 - yoe/100_int64)
        mp = (5_int64*doy + 2_int64)/153_int64
        day = doy - (153_int64*mp + 2_int64)/5_int64 + 1_int64
        month = mp + merge(3_int64, -9_int64, mp < 10_int64)
        if (month <= 2_int64) year = year + 1_int64
    end subroutine civil_from_days

end module fortplot_datetime

