program twin_axes_demo
    !! Demonstrates twin x/y axis support with independent scaling and labels
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure, plot, twinx, twiny, use_axis, xlabel, ylabel, title
    use fortplot, only: legend, set_yscale, set_xscale, savefig_with_status
    use fortplot_errors, only: SUCCESS
    implicit none

    integer, parameter :: n = 120
    real(wp) :: time_hours(n)
    real(wp) :: primary_signal(n)
    real(wp) :: secondary_signal(n)
    real(wp) :: cumulative_index(n)
    integer :: i, status
    logical :: ok

    do i = 1, n
        time_hours(i) = real(i - 1, wp) / 4.0_wp
        primary_signal(i) = 15.0_wp + 8.0_wp * sin( &
            2.0_wp * acos(-1.0_wp) * time_hours(i) / 24.0_wp)
        secondary_signal(i) = 40.0_wp + 10.0_wp * exp(-0.05_wp * time_hours(i))
        cumulative_index(i) = log(1.0_wp + real(i, wp))
    end do

    call figure()
    call plot(time_hours, primary_signal, label='Primary axis')
    call ylabel('Temperature (relative)')

    call twinx()
    call plot(time_hours, secondary_signal, label='Secondary axis')
    call set_yscale('log')
    call ylabel('Humidity (log scale)')

    call twiny()
    call plot(cumulative_index, primary_signal, label='Top axis')
    call set_xscale('log')
    call xlabel('Cumulative index (log scale)')

    call use_axis('primary')
    call xlabel('Time (hours)')
    call title('Twin axis demo')
    call legend()

    ok = .true.
    call savefig_with_status('output/example/fortran/twin_axes_demo/' // &
                             'twin_axes_demo.png', status)
    if (status /= SUCCESS) ok = .false.
    call savefig_with_status('output/example/fortran/twin_axes_demo/' // &
                             'twin_axes_demo.txt', status)
    if (status /= SUCCESS) ok = .false.
    if (.not. ok) then
        print *, 'WARNING: Failed to write one or more twin axis demo outputs'
    end if

end program twin_axes_demo
