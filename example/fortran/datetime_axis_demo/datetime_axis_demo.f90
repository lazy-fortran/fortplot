program datetime_axis_demo
    !! Demonstrate date/time axis support using datetime_t values.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t, datetime_t
    use fortplot_errors, only: SUCCESS
    implicit none

    integer, parameter :: n = 10
    type(figure_t) :: fig
    type(datetime_t) :: times(n)
    real(wp) :: y(n)
    integer :: i, save_status
    logical :: all_success
    character(len=*), parameter :: out_dir = &
                                   'output/example/fortran/datetime_axis_demo/'

    do i = 1, n
        times(i)%year = 2025
        times(i)%month = 12
        times(i)%day = 20 + i
        times(i)%hour = 0
        times(i)%minute = 0
        times(i)%second = 0
        y(i) = 0.5_wp*real(i, wp)
    end do

    call fig%initialize(800, 600, backend='png')
    call fig%add_plot(times, y, label='measurements')
    call fig%set_xaxis_date_format('%Y-%m-%d')
    call fig%set_xlabel('Date')
    call fig%set_ylabel('Value')
    call fig%set_title('Datetime Axis Demo')
    call fig%legend()

    all_success = .true.
    call fig%savefig_with_status(out_dir//'datetime_axis_demo.png', save_status)
    if (save_status /= SUCCESS) all_success = .false.
    call fig%savefig_with_status(out_dir//'datetime_axis_demo.pdf', save_status)
    if (save_status /= SUCCESS) all_success = .false.

    if (all_success) then
        print *, 'Created: datetime_axis_demo.png/pdf'
    else
        print *, 'ERROR: Failed to create some datetime_axis_demo files'
        error stop 1
    end if
end program datetime_axis_demo
