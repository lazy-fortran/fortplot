program test_datetime_axis
    !! Verify date/time axis formatting and end-to-end ASCII tick labeling.

    use, intrinsic :: iso_fortran_env, only: int64, wp => real64
    use fortplot, only: figure_t, datetime_t
    use fortplot_axes, only: format_tick_label
    use fortplot_datetime, only: datetime_to_unix_seconds, JD_UNIX_EPOCH
    use fortplot_test_helpers, only: test_get_temp_path, test_initialize_environment
    implicit none

    logical :: all_passed
    character(len=50) :: label
    type(datetime_t) :: dt
    integer(int64) :: seconds

    type(figure_t) :: fig
    integer, parameter :: n = 5
    type(datetime_t) :: times(n)
    real(wp) :: y(n)
    integer :: i, unit
    character(len=256) :: line
    logical :: found_date
    character(len=:), allocatable :: output_path

    all_passed = .true.

    call test_initialize_environment('datetime_axis')

    dt%year = 1970
    dt%month = 1
    dt%day = 1
    dt%hour = 0
    dt%minute = 0
    dt%second = 0
    seconds = datetime_to_unix_seconds(dt)

    label = format_tick_label(real(seconds, wp), 'date', date_format='%Y-%m-%d', &
                              data_min=real(seconds, wp), data_max=real(seconds, wp))
    if (trim(label) /= '1970-01-01') then
        print *, 'FAIL: Expected 1970-01-01, got:', trim(label)
        all_passed = .false.
    end if

    label = format_tick_label(JD_UNIX_EPOCH, 'date_jd', date_format='%Y-%m-%d', &
                              data_min=JD_UNIX_EPOCH, data_max=JD_UNIX_EPOCH)
    if (trim(label) /= '1970-01-01') then
        print *, 'FAIL: Expected 1970-01-01 for date_jd, got:', trim(label)
        all_passed = .false.
    end if

    do i = 1, n
        times(i)%year = 2025
        times(i)%month = 12
        times(i)%day = 25 + i
        times(i)%hour = 0
        times(i)%minute = 0
        times(i)%second = 0
        y(i) = real(i, wp)
    end do

    call fig%initialize(80, 24, backend='ascii')
    call fig%add_plot(times, y)
    call fig%set_xaxis_date_format('%Y-%m-%d')
    output_path = test_get_temp_path('test_datetime_axis.txt')
    call fig%savefig(output_path)

    found_date = .false.
    open (newunit=unit, file=output_path, status='old', &
          action='read')
    do
        read (unit, '(A)', end=100) line
        if (index(line, '2025-12-') > 0) then
            found_date = .true.
            exit
        end if
    end do
100 continue
    close (unit)

    if (.not. found_date) then
        print *, 'FAIL: Expected date tick label in ASCII output'
        all_passed = .false.
    end if

    if (all_passed) then
        print *, 'PASS: datetime axis formatting'
    else
        stop 1
    end if
end program test_datetime_axis
