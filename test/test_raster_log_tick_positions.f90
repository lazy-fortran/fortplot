program test_raster_log_tick_positions
    !! Verify raster tick mapping respects log and symlog scales
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_axes, only: map_value_to_plot_y, map_value_to_plot_x

    implicit none

    call test_log_y_spacing_equal()
    call test_symlog_y_symmetry()
    call test_log_x_spacing_equal()

    print *, 'Raster log/symlog tick mapping tests passed'
contains

    subroutine test_log_y_spacing_equal()
        type(plot_area_t) :: area
        real(wp) :: y_min, y_max, thr
        character(len=*), parameter :: scale = 'log'
        real(wp) :: p(4), d1, d2, d3
        real(wp), parameter :: tol = 1.0e-9_wp

        area%left = 80; area%bottom = 50; area%width = 400; area%height = 300
        y_min = 1.0_wp; y_max = 1000.0_wp; thr = 1.0_wp

        p(1) = map_value_to_plot_y(1.0_wp,    y_min, y_max, area, scale, thr)
        p(2) = map_value_to_plot_y(10.0_wp,   y_min, y_max, area, scale, thr)
        p(3) = map_value_to_plot_y(100.0_wp,  y_min, y_max, area, scale, thr)
        p(4) = map_value_to_plot_y(1000.0_wp, y_min, y_max, area, scale, thr)

        ! On log scale, decades should be evenly spaced in pixels
        d1 = abs(p(1) - p(2))
        d2 = abs(p(2) - p(3))
        d3 = abs(p(3) - p(4))

        if (abs(d1 - d2) > tol .or. abs(d2 - d3) > tol) then
            print *, 'FAIL: Y log tick spacing not equal:', d1, d2, d3
            stop 1
        end if
    end subroutine test_log_y_spacing_equal

    subroutine test_symlog_y_symmetry()
        type(plot_area_t) :: area
        real(wp) :: y_min, y_max, thr
        character(len=*), parameter :: scale = 'symlog'
        real(wp) :: p_neg10, p_pos10, p_neg100, p_pos100, p_zero
        real(wp), parameter :: tol = 1.0e-9_wp

        area%left = 80; area%bottom = 50; area%width = 400; area%height = 300
        y_min = -100.0_wp; y_max = 100.0_wp; thr = 1.0_wp

        p_zero   = map_value_to_plot_y(0.0_wp,    y_min, y_max, area, scale, thr)
        p_neg10  = map_value_to_plot_y(-10.0_wp,  y_min, y_max, area, scale, thr)
        p_pos10  = map_value_to_plot_y( 10.0_wp,  y_min, y_max, area, scale, thr)
        p_neg100 = map_value_to_plot_y(-100.0_wp, y_min, y_max, area, scale, thr)
        p_pos100 = map_value_to_plot_y( 100.0_wp, y_min, y_max, area, scale, thr)

        ! Symmetry around zero for equal magnitudes
        if (abs(abs(p_neg10 - p_zero) - abs(p_pos10 - p_zero)) > tol) then
            print *, 'FAIL: Symlog symmetry broken at 10'
            stop 1
        end if
        if (abs(abs(p_neg100 - p_zero) - abs(p_pos100 - p_zero)) > tol) then
            print *, 'FAIL: Symlog symmetry broken at 100'
            stop 1
        end if
    end subroutine test_symlog_y_symmetry

    subroutine test_log_x_spacing_equal()
        type(plot_area_t) :: area
        real(wp) :: x_min, x_max, thr
        character(len=*), parameter :: scale = 'log'
        real(wp) :: p(4), d1, d2, d3
        real(wp), parameter :: tol = 1.0e-9_wp

        area%left = 80; area%bottom = 50; area%width = 400; area%height = 300
        x_min = 1.0_wp; x_max = 1000.0_wp; thr = 1.0_wp

        p(1) = map_value_to_plot_x(1.0_wp,    x_min, x_max, area, scale, thr)
        p(2) = map_value_to_plot_x(10.0_wp,   x_min, x_max, area, scale, thr)
        p(3) = map_value_to_plot_x(100.0_wp,  x_min, x_max, area, scale, thr)
        p(4) = map_value_to_plot_x(1000.0_wp, x_min, x_max, area, scale, thr)

        d1 = abs(p(2) - p(1))
        d2 = abs(p(3) - p(2))
        d3 = abs(p(4) - p(3))

        if (abs(d1 - d2) > tol .or. abs(d2 - d3) > tol) then
            print *, 'FAIL: X log tick spacing not equal:', d1, d2, d3
            stop 1
        end if
    end subroutine test_log_x_spacing_equal

end program test_raster_log_tick_positions

