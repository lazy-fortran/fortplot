program test_histogram_rendering
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_bar_rendering, only: render_histogram_plot
    use fortplot_colors, only: clear_color_cache, parse_color
    use fortplot_context, only: setup_canvas
    use fortplot_matplotlib, only: figure, hist, get_global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: PLOT_TYPE_HISTOGRAM
    use fortplot_spy_backend, only: spy_context_t
    implicit none

    logical :: failed

    failed = .false.
    call clear_color_cache()
    call test_hist_renders_filled_quads(failed)
    call test_horizontal_hist_renders_filled_quads(failed)
    call test_histtype_rejects_garbage(failed)
    call test_histtype_accepts_step(failed)

    if (failed) stop 1
    print *, "histogram rendering tests passed"

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

    subroutine resolve_expected_color(name, rgb)
        character(len=*), intent(in) :: name
        real(wp), intent(out) :: rgb(3)
        logical :: ok

        call parse_color(name, rgb, ok)
        if (.not. ok) then
            print *, "FAIL: expected color did not parse: ", trim(name)
            stop 1
        end if
    end subroutine resolve_expected_color

    subroutine test_hist_renders_filled_quads(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        type(spy_context_t) :: backend
        real(wp) :: data(6), expected(3)

        data = [1.0_wp, 1.0_wp, 2.0_wp, 2.0_wp, 3.0_wp, 3.0_wp]
        call resolve_expected_color('steelblue', expected)

        call figure()
        call hist(data, bins=3, color='steelblue')
        fig => get_global_figure()

        backend%expected_fill = expected
        call setup_canvas(backend, 200, 200)
        call render_histogram_plot(backend, fig%plots(fig%plot_count), 'linear', 'linear', &
                                   0.0_wp)

        call assert_true("hist stores histogram plot type", &
                         fig%plots(fig%plot_count)%plot_type == PLOT_TYPE_HISTOGRAM, failed)
        call assert_true("hist render fills one quad per bin", backend%fill_calls == 3, failed)
        call assert_true("hist render avoids outline seams", backend%line_calls == 0, failed)
        call assert_true("hist render uses requested fill color", backend%fill_color_ok, failed)
        call assert_true("hist render has no unexpected backend calls", &
                         backend%unexpected_calls == 0, failed)
    end subroutine test_hist_renders_filled_quads

    subroutine test_horizontal_hist_renders_filled_quads(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        type(spy_context_t) :: backend
        real(wp) :: data(4), expected(3)

        data = [0.0_wp, 0.5_wp, 1.0_wp, 1.5_wp]
        call resolve_expected_color('tab:blue', expected)

        call figure()
        call hist(data, bins=2, orientation='horizontal', color='tab:blue')
        fig => get_global_figure()

        backend%expected_fill = expected
        call setup_canvas(backend, 200, 200)
        call render_histogram_plot(backend, fig%plots(fig%plot_count), 'linear', 'linear', &
                                   0.0_wp)

        call assert_true("horizontal hist keeps histogram plot type", &
                         fig%plots(fig%plot_count)%plot_type == PLOT_TYPE_HISTOGRAM, failed)
        call assert_true("horizontal hist marks horizontal storage", &
                         fig%plots(fig%plot_count)%bar_horizontal, failed)
        call assert_true("horizontal hist render fills one quad per bin", &
                         backend%fill_calls == 2, failed)
        call assert_true("horizontal hist render avoids outline seams", &
                         backend%line_calls == 0, failed)
        call assert_true("horizontal hist render uses requested fill color", &
                         backend%fill_color_ok, failed)
    end subroutine test_horizontal_hist_renders_filled_quads

    subroutine test_histtype_rejects_garbage(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        real(wp) :: data(4)

        data = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]

        call figure()
        call hist(data, bins=2, histtype='akjsnsdijdknjb', color='steelblue')
        fig => get_global_figure()

        call assert_true("invalid histtype must not create a plot", &
                         fig%plot_count == 0, failed)
    end subroutine test_histtype_rejects_garbage

    subroutine test_histtype_accepts_step(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        real(wp) :: data(4)

        data = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]

        call figure()
        call hist(data, bins=2, histtype='step', color='steelblue')
        fig => get_global_figure()

        call assert_true("valid histtype must still create a plot", &
                         fig%plot_count == 1, failed)
    end subroutine test_histtype_accepts_step

end program test_histogram_rendering
