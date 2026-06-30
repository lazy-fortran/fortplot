program test_alpha_rendering
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_bar_rendering, only: render_bar_plot
    use fortplot_context, only: setup_canvas
    use fortplot_line_rendering, only: render_line_plot
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_LINE, PLOT_TYPE_BAR
    use fortplot_spy_backend, only: spy_context_t
    implicit none

    logical :: failed

    failed = .false.
    call test_line_alpha_renders_blended_color(failed)
    call test_bar_alpha_renders_blended_colors(failed)

    if (failed) stop 1
    print *, "alpha rendering tests passed"

contains

    subroutine assert_true(name, condition, failed)
        character(len=*), intent(in) :: name
        logical, intent(in) :: condition
        logical, intent(inout) :: failed

        if (.not. condition) then
            failed = .true.
            print *, "FAIL: ", trim(name)
        end if
    end subroutine assert_true

    pure function blend_color(color, alpha) result(blended)
        real(wp), intent(in) :: color(3)
        real(wp), intent(in) :: alpha
        real(wp) :: blended(3)

        blended = max(0.0_wp, min(1.0_wp, alpha))*color + &
                  (1.0_wp - max(0.0_wp, min(1.0_wp, alpha)))
    end function blend_color

    subroutine test_line_alpha_renders_blended_color(failed)
        logical, intent(inout) :: failed
        type(spy_context_t) :: backend
        type(plot_data_t) :: plot
        real(wp) :: x(3), y(3), color(3), expected(3)

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [2.0_wp, 1.0_wp, 4.0_wp]
        color = [0.2_wp, 0.4_wp, 0.6_wp]
        expected = blend_color(color, 0.25_wp)

        plot%plot_type = PLOT_TYPE_LINE
        allocate(plot%x(size(x)))
        allocate(plot%y(size(y)))
        plot%x = x
        plot%y = y
        plot%color = color
        plot%fill_alpha = 0.25_wp

        backend%expected_edge = expected
        call setup_canvas(backend, 200, 200)
        call render_line_plot(backend, plot, 'linear', 'linear', &
                              0.0_wp)

        call assert_true("line alpha: one segment rendered", backend%line_calls == 2, &
                         failed)
        call assert_true("line alpha: blended color used", backend%line_color_ok, failed)
        call assert_true("line alpha: no unexpected backend calls", &
                         backend%unexpected_calls == 0, failed)
    end subroutine test_line_alpha_renders_blended_color

    subroutine test_bar_alpha_renders_blended_colors(failed)
        logical, intent(inout) :: failed
        type(spy_context_t) :: backend
        type(plot_data_t) :: plot
        real(wp) :: x(2), h(2), fill(3), expected_fill(3)

        x = [1.0_wp, 2.0_wp]
        h = [1.0_wp, 2.0_wp]
        fill = [0.1_wp, 0.2_wp, 0.3_wp]
        expected_fill = blend_color(fill, 0.4_wp)

        plot%plot_type = PLOT_TYPE_BAR
        allocate(plot%bar_x(size(x)))
        allocate(plot%bar_heights(size(h)))
        allocate(plot%bar_bottom(size(x)))
        plot%bar_x = x
        plot%bar_heights = h
        plot%bar_bottom = 0.0_wp
        plot%bar_width = 0.8_wp
        plot%bar_horizontal = .false.
        plot%color = fill
        plot%bar_color_per_bar_set = .false.
        plot%fill_alpha = 0.4_wp

        backend%expected_fill = expected_fill
        backend%expected_edge = expected_fill
        call setup_canvas(backend, 200, 200)
        call render_bar_plot(backend, plot, 'linear', 'linear', &
                             0.0_wp)

        call assert_true("bar alpha: fills rendered", backend%fill_calls == 2, failed)
        call assert_true("bar alpha: edges rendered", backend%line_calls == 8, failed)
        call assert_true("bar alpha: blended fill color used", backend%fill_color_ok, &
                         failed)
        call assert_true("bar alpha: blended edge color used", backend%line_color_ok, &
                         failed)
        call assert_true("bar alpha: no unexpected backend calls", &
                         backend%unexpected_calls == 0, failed)
    end subroutine test_bar_alpha_renders_blended_colors

end program test_alpha_rendering
