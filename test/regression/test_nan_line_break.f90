program test_nan_line_break
    !! Regression: polylines must break at NaN/Inf separators instead of
    !! drawing through them, and no non-finite endpoint may reach a backend.
    !! Matplotlib treats NaN as a path break. Drawing a segment that touches
    !! a NaN point corrupts vector backends (e.g. "NaN ... c" in PDF streams).
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan
    use fortplot_spy_backend, only: spy_context_t
    use fortplot_line_rendering, only: render_solid_line
    implicit none

    type(spy_context_t) :: spy
    real(wp) :: x(7), y(7), nan
    logical :: all_passed

    all_passed = .true.
    nan = ieee_value(nan, ieee_quiet_nan)

    ! Two segments of three points each, separated by one NaN point.
    ! Finite-finite adjacent pairs: (1,2),(2,3) and (5,6),(6,7) => 4 segments.
    x = [0.0_wp, 1.0_wp, 2.0_wp, nan, 4.0_wp, 5.0_wp, 6.0_wp]
    y = [0.0_wp, 1.0_wp, 0.0_wp, nan, 0.0_wp, 1.0_wp, 0.0_wp]

    call spy%reset()
    call render_solid_line(spy, x, y)

    if (spy%line_calls /= 4) then
        print *, "FAIL: expected 4 finite segments, got", spy%line_calls
        all_passed = .false.
    end if

    if (spy%saw_nonfinite_line) then
        print *, "FAIL: a non-finite endpoint was forwarded to the backend"
        all_passed = .false.
    end if

    if (all_passed) then
        print *, "PASS: NaN-bounded segments are skipped and never forwarded"
    else
        error stop 1
    end if
end program test_nan_line_break
