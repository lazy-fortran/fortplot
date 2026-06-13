program test_boxplot_autoscale_margin
    !! Boxplot value axis must receive a single matplotlib-style 5% autoscale
    !! margin, not a stacked margin that drags the axis past the data baseline.
    !! Data spans 1..12, so the value axis must be 0.45..12.55 (5% of the span
    !! on each side) and must not reach 0.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure, only: figure_t
    use fortplot_test_helpers, only: test_get_temp_path
    implicit none

    type(figure_t) :: fig
    real(wp) :: group_a(10), group_b(10), group_c(10)
    real(wp) :: y_lo, y_hi, span, expected_lo, expected_hi
    real(wp), parameter :: tol = 1.0e-6_wp
    integer :: i
    logical :: ok

    do i = 1, 10
        group_a(i) = real(i, wp)
        group_b(i) = real(i + 1, wp)
        group_c(i) = real(i + 2, wp)
    end do

    call fig%initialize()
    call fig%boxplot(group_a, position=1.0_wp, width=0.6_wp)
    call fig%boxplot(group_b, position=2.0_wp, width=0.6_wp)
    call fig%boxplot(group_c, position=3.0_wp, width=0.6_wp)
    call fig%savefig(test_get_temp_path('boxplot_autoscale_margin.png'))

    y_lo = fig%get_y_min()
    y_hi = fig%get_y_max()

    span = 12.0_wp - 1.0_wp
    expected_lo = 1.0_wp - 0.05_wp * span
    expected_hi = 12.0_wp + 0.05_wp * span

    ok = .true.
    if (abs(y_lo - expected_lo) > tol) then
        print *, 'FAIL: y_min', y_lo, 'expected', expected_lo
        ok = .false.
    end if
    if (abs(y_hi - expected_hi) > tol) then
        print *, 'FAIL: y_max', y_hi, 'expected', expected_hi
        ok = .false.
    end if
    if (y_lo <= 0.0_wp) then
        print *, 'FAIL: value axis dragged to or past zero', y_lo
        ok = .false.
    end if

    if (.not. ok) then
        error stop 'boxplot autoscale margin regression'
    end if
    print *, 'PASS: boxplot value axis has single 5% margin'
end program test_boxplot_autoscale_margin
