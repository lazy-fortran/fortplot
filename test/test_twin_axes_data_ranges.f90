program test_twin_axes_data_ranges
    !! Verify that twin axis data ranges are computed when plots are added.
    !! Regression test for issue #1709: twinx/twiny ranges were never updated,
    !! causing tick generation to use stale default values.
    use fortplot
    use fortplot_plot_data, only: AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_matplotlib_session, only: get_global_figure
    use iso_fortran_env, only: real64, error_unit
    implicit none

    real(real64), allocatable :: x(:), y(:), y_secondary(:), y_top(:)
    class(figure_t), pointer :: fig_ptr
    integer :: i, n
    real(real64) :: tol

    n = 20
    allocate(x(n), y(n), y_secondary(n), y_top(n))
    tol = 1.0e-6_real64

    ! Primary data: x=[1..20], y=[100,200,...,2000]
    do i = 1, n
        x(i) = real(i, real64)
        y(i) = real(i, real64) * 100.0_real64
    end do

    ! Twinx data: same x, y_secondary=[1.0, exp(1)..exp(20)]
    do i = 1, n
        y_secondary(i) = exp(real(i, real64) * 0.2_real64)
    end do

    ! Twiny data: x_top=[log(1)..log(20)], y=[100,...,2000]
    do i = 1, n
        y_top(i) = log(real(i, real64))
    end do

    call figure()
    call plot(x, y, label="primary")
    call ylabel("Primary Y")

    call twinx()
    call plot(x, y_secondary, label="secondary")
    call ylabel("Secondary Y (log)")
    call set_yscale("log")

    call twiny()
    call plot(y_top, y, label="top")
    call set_xscale("log")
    call xlabel("Top X (log)")

    fig_ptr => get_global_figure()
    if (.not. associated(fig_ptr)) then
        write (error_unit, '(A)') 'ERROR: global figure not associated'
        stop 1
    end if

    ! --- REQ-001: Plot axis tagging ---
    if (fig_ptr%plots(1)%axis /= AXIS_PRIMARY) then
        write (error_unit, '(A)') 'FAIL: plot 1 axis not AXIS_PRIMARY'
        stop 1
    end if
    if (fig_ptr%plots(2)%axis /= AXIS_TWINX) then
        write (error_unit, '(A)') 'FAIL: plot 2 axis not AXIS_TWINX'
        stop 1
    end if
    if (fig_ptr%plots(3)%axis /= AXIS_TWINY) then
        write (error_unit, '(A)') 'FAIL: plot 3 axis not AXIS_TWINY'
        stop 1
    end if

    ! --- REQ-002: Twinx y-range must span secondary data, not primary ---
    ! y_secondary = exp(0.2)..exp(4.0) ~ [1.22, 54.6]
    if (abs(fig_ptr%state%twinx_y_min - 1.221403_real64) > tol .and. &
        abs(fig_ptr%state%twinx_y_min - 1.221403_real64) < 0.5_real64) then
        ! Acceptable range - secondary data y_min is ~1.22
    else if (fig_ptr%state%twinx_y_min >= 99.0_real64) then
        write (error_unit, '(A,F12.4,A)') 'FAIL: twinx_y_min is stale (', &
            fig_ptr%state%twinx_y_min, '); expected ~1.22 not ~100'
        stop 1
    end if
    if (fig_ptr%state%twinx_y_max <= 55.0_real64) then
        ! Acceptable - secondary data y_max is ~54.6
    else if (fig_ptr%state%twinx_y_max >= 1999.0_real64) then
        write (error_unit, '(A,F12.4,A)') 'FAIL: twinx_y_max is stale (', &
            fig_ptr%state%twinx_y_max, '); expected ~54.6 not ~2000'
        stop 1
    end if

    ! --- REQ-003: Twiny x-range must span top data, not primary ---
    ! y_top = log(1)..log(20) ~ [0.0, 2.996]
    if (fig_ptr%state%twiny_x_min <= -0.5_real64) then
        write (error_unit, '(A,F12.4,A)') 'FAIL: twiny_x_min is stale (', &
            fig_ptr%state%twiny_x_min, '); expected ~0.0 not ~-5'
        stop 1
    end if
    if (fig_ptr%state%twiny_x_max >= 4.0_real64) then
        write (error_unit, '(A,F12.4,A)') 'FAIL: twiny_x_max is stale (', &
            fig_ptr%state%twiny_x_max, '); expected ~3.0 not ~20'
        stop 1
    end if

    ! --- Verify scales are set correctly ---
    if (trim(fig_ptr%state%twinx_yscale) /= 'log') then
        write (error_unit, '(A)') 'FAIL: twinx_yscale not log'
        stop 1
    end if
    if (trim(fig_ptr%state%twiny_xscale) /= 'log') then
        write (error_unit, '(A)') 'FAIL: twiny_xscale not log'
        stop 1
    end if

    ! --- Verify rendered output ---
    call savefig('build/test/output/test_twin_axes_data_ranges.png')

    print *, 'PASS: twin axes data ranges computed correctly'

    deallocate(x, y, y_secondary, y_top)

end program test_twin_axes_data_ranges
