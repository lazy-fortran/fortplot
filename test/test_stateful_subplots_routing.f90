program test_stateful_subplots_routing
    !! Verify that matplotlib-style stateful API routes plots and labels
    !! to the selected subplot when a grid is active.

    use fortplot, only: wp, figure_t, plot, title, subplot, subplots, &
                        get_global_figure
    implicit none

    real(wp) :: x(5), y1(5), y2(5)
    integer :: i
    type(figure_t), pointer :: f

    print *, 'Running stateful subplot routing tests...'

    do i = 1, 5
        x(i) = real(i, wp)
        y1(i) = x(i)
        y2(i) = 2.0_wp * x(i)
    end do

    call subplots(2, 2)

    ! Select (1,1) -> index 1 and route plot + title
    call subplot(2, 2, 1)
    call plot(x, y1, label='a1')
    call title('A1')

    ! Select (2,2) -> index 4 and route plot + title
    call subplot(2, 2, 4)
    call plot(x, y2, label='a4')
    call title('A4')

    ! Inspect global figure for subplot-local state
    f => get_global_figure()

    if (f%subplot_plot_count(1, 1) /= 1) then
        print *, '  FAIL: subplot (1,1) plot count mismatch'
        stop 1
    end if

    if (f%subplot_plot_count(2, 2) /= 1) then
        print *, '  FAIL: subplot (2,2) plot count mismatch'
        stop 1
    end if

    if (f%subplot_plot_count(1, 2) /= 0 .or. f%subplot_plot_count(2, 1) /= 0) then
        print *, '  FAIL: unexpected plots in other subplots'
        stop 1
    end if

    if (f%subplot_title(1, 1) /= 'A1') then
        print *, '  FAIL: title routing for (1,1)'
        stop 1
    end if

    if (f%subplot_title(2, 2) /= 'A4') then
        print *, '  FAIL: title routing for (2,2)'
        stop 1
    end if

    print *, 'All stateful subplot routing tests PASSED!'

end program test_stateful_subplots_routing
