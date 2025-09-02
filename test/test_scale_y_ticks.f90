! Verify Y-axis tick spacing for log and symlog scales
program test_scale_y_ticks
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster_axes, only: map_value_to_plot_y
    use fortplot_margins, only: plot_area_t
    implicit none

    type(plot_area_t) :: area
    real(wp) :: py1, py10, py100, py1000
    real(wp) :: d1, d2, d3
    real(wp) :: p100, p10, p5, p0, dl1, dl2
    integer :: failures

    failures = 0

    ! Define a simple plot area
    area%left = 100
    area%bottom = 200
    area%width = 500
    area%height = 400

    ! Test 1: Log scale spacing (y in [1, 1000]) should be equal per decade
    py1    = map_value_to_plot_y(1.0_wp,    1.0_wp, 1000.0_wp, area, 'log', 1.0_wp)
    py10   = map_value_to_plot_y(10.0_wp,   1.0_wp, 1000.0_wp, area, 'log', 1.0_wp)
    py100  = map_value_to_plot_y(100.0_wp,  1.0_wp, 1000.0_wp, area, 'log', 1.0_wp)
    py1000 = map_value_to_plot_y(1000.0_wp, 1.0_wp, 1000.0_wp, area, 'log', 1.0_wp)

    d1 = abs(py10 - py1)
    d2 = abs(py100 - py10)
    d3 = abs(py1000 - py100)

    if (.not. approx_equal(d1, d2) .or. .not. approx_equal(d2, d3)) then
        print *, 'FAIL: log-scale Y tick spacing not equal per decade:', d1, d2, d3
        failures = failures + 1
    else
        print *, 'PASS: log-scale Y tick spacing equal per decade'
    end if

    ! Test 2: Symlog spacing with threshold=10
    ! Expect: distance(10->5) == distance(5->0) (linear region),
    ! and distance(100->10) is ~1/5 of distance(10->5)
    p100 = map_value_to_plot_y(100.0_wp, -1000.0_wp, 1000.0_wp, area, 'symlog', 10.0_wp)
    p10  = map_value_to_plot_y(10.0_wp,  -1000.0_wp, 1000.0_wp, area, 'symlog', 10.0_wp)
    p5   = map_value_to_plot_y(5.0_wp,   -1000.0_wp, 1000.0_wp, area, 'symlog', 10.0_wp)
    p0   = map_value_to_plot_y(0.0_wp,   -1000.0_wp, 1000.0_wp, area, 'symlog', 10.0_wp)

    dl1 = abs(p10 - p5)
    dl2 = abs(p5 - p0)

    if (.not. approx_equal(dl1, dl2)) then
        print *, 'FAIL: symlog Y spacing near zero not linear equal:', dl1, dl2
        failures = failures + 1
    else
        print *, 'PASS: symlog Y spacing near zero equal as expected'
    end if

    ! Check ratio for decade segment vs linear segment (~0.2)
    if (.not. approx_equal_ratio(abs(p100 - p10), dl1, 0.2_wp)) then
        print *, 'FAIL: symlog decade-to-linear spacing ratio off:', abs(p100 - p10), dl1
        failures = failures + 1
    else
        print *, 'PASS: symlog decade vs linear spacing ratio ~0.2'
    end if

    if (failures == 0) then
        print *, 'All scale Y tick spacing tests passed.'
    else
        stop 1
    end if

contains
    logical function approx_equal(a, b)
        real(wp), intent(in) :: a, b
        real(wp) :: tol
        tol = 1.0e-6_wp * max(1.0_wp, max(abs(a), abs(b)))
        approx_equal = abs(a - b) <= tol
    end function approx_equal

    logical function approx_equal_ratio(num, den, target)
        real(wp), intent(in) :: num, den, target
        real(wp) :: ratio, tol
        if (den <= 0.0_wp) then
            approx_equal_ratio = .false.
            return
        end if
        ratio = num / den
        tol = 5.0e-2_wp  ! 5% tolerance
        approx_equal_ratio = abs(ratio - target) <= tol
    end function approx_equal_ratio

end program test_scale_y_ticks

