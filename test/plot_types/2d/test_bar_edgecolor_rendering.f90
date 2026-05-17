program test_bar_edgecolor_rendering
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_bar_rendering, only: render_bar_plot
    use fortplot_context, only: setup_canvas
    use fortplot_matplotlib, only: bar, barh, figure, get_global_figure
    use fortplot_figure_core, only: figure_t
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_BAR
    use fortplot_spy_backend, only: spy_context_t
    implicit none

    logical :: failed

    failed = .false.
    call test_render_bar_plot_draws_quads(failed)
    call test_bar_respects_edgecolor(failed)
    call test_bar_defaults_edgecolor_to_facecolor(failed)
    call test_barh_respects_edgecolor(failed)

    if (failed) stop 1
    print *, "bar/barh edgecolor rendering tests passed"

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

    subroutine test_bar_respects_edgecolor(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        type(spy_context_t) :: backend

        real(wp) :: x(3), heights(3)
        real(wp) :: fill(3), edge(3)

        x = [1.0_wp, 2.0_wp, 3.0_wp]
        heights = [1.0_wp, 2.0_wp, 3.0_wp]
        fill = [1.0_wp, 0.0_wp, 0.0_wp]
        edge = [0.0_wp, 0.0_wp, 1.0_wp]

        call figure()
        call bar(x, heights, color=fill, edgecolor=edge)
        fig => get_global_figure()

        backend%expected_fill = fill
        backend%expected_edge = edge
        call setup_canvas(backend, 200, 200)

        call render_bar_plot(backend, fig%plots(fig%plot_count), 'linear', 'linear', &
                             0.0_wp)

        call assert_true("bar edgecolor: fills present", backend%fill_calls == 3, &
                         failed)
        call assert_true("bar edgecolor: edges present", backend%line_calls == 12, &
                         failed)
        call assert_true("bar edgecolor: fill color used", backend%fill_color_ok, &
                         failed)
        call assert_true("bar edgecolor: edge color used", backend%line_color_ok, &
                         failed)
        call assert_true("bar edgecolor: no extra backend calls", &
                         backend%unexpected_calls == 0, failed)
    end subroutine test_bar_respects_edgecolor

    subroutine test_render_bar_plot_draws_quads(failed)
        logical, intent(inout) :: failed
        type(spy_context_t) :: backend
        type(plot_data_t) :: plot
        real(wp) :: fill(3), edge(3)

        fill = [0.1_wp, 0.2_wp, 0.3_wp]
        edge = [0.9_wp, 0.8_wp, 0.7_wp]

        plot%plot_type = PLOT_TYPE_BAR
        plot%color = fill
        plot%bar_width = 1.0_wp
        plot%bar_horizontal = .false.
        plot%bar_edgecolor = edge
        plot%bar_edgecolor_set = .true.
        allocate (plot%bar_x(1))
        allocate (plot%bar_heights(1))
        allocate (plot%bar_bottom(1))
        plot%bar_x = [1.0_wp]
        plot%bar_heights = [2.0_wp]
        plot%bar_bottom = [0.0_wp]

        backend%expected_fill = fill
        backend%expected_edge = edge
        call setup_canvas(backend, 200, 200)

        call render_bar_plot(backend, plot, 'linear', 'linear', 0.0_wp)

        call assert_true("direct render draws one quad", backend%fill_calls == 1, &
                         failed)
        call assert_true("direct render draws quad edges", backend%line_calls == 4, &
                         failed)
        call assert_true("direct render uses fill color", backend%fill_color_ok, failed)
        call assert_true("direct render uses edge color", backend%line_color_ok, failed)
        call assert_true("direct render: no extra backend calls", &
                         backend%unexpected_calls == 0, failed)
    end subroutine test_render_bar_plot_draws_quads

    subroutine test_bar_defaults_edgecolor_to_facecolor(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        type(spy_context_t) :: backend

        real(wp) :: x(2), heights(2)
        real(wp) :: fill(3)

        x = [1.0_wp, 2.0_wp]
        heights = [2.0_wp, 4.0_wp]
        fill = [0.25_wp, 0.5_wp, 0.75_wp]

        call figure()
        call bar(x, heights, color=fill)
        fig => get_global_figure()

        backend%expected_fill = fill
        backend%expected_edge = fill
        call setup_canvas(backend, 200, 200)

        call render_bar_plot(backend, fig%plots(fig%plot_count), 'linear', 'linear', &
                             0.0_wp)

        call assert_true("bar default edgecolor: fills present", &
                         backend%fill_calls == 2, failed)
        call assert_true("bar default edgecolor: edges present", &
                         backend%line_calls == 8, failed)
        call assert_true("bar default edgecolor: fill color used", &
                         backend%fill_color_ok, failed)
        call assert_true("bar default edgecolor: edge color used", &
                         backend%line_color_ok, failed)
        call assert_true("bar default edgecolor: no extra backend calls", &
                         backend%unexpected_calls == 0, failed)
    end subroutine test_bar_defaults_edgecolor_to_facecolor

    subroutine test_barh_respects_edgecolor(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        type(spy_context_t) :: backend

        real(wp) :: y(3), widths(3)
        real(wp) :: fill(3), edge(3)

        y = [1.0_wp, 2.0_wp, 3.0_wp]
        widths = [1.5_wp, 0.5_wp, 2.25_wp]
        fill = [0.0_wp, 1.0_wp, 0.0_wp]
        edge = [1.0_wp, 0.0_wp, 1.0_wp]

        call figure()
        call barh(y, widths, color=fill, edgecolor=edge)
        fig => get_global_figure()

        backend%expected_fill = fill
        backend%expected_edge = edge
        call setup_canvas(backend, 200, 200)

        call render_bar_plot(backend, fig%plots(fig%plot_count), 'linear', 'linear', &
                             0.0_wp)

        call assert_true("barh edgecolor: fills present", backend%fill_calls == 3, &
                         failed)
        call assert_true("barh edgecolor: edges present", backend%line_calls == 12, &
                         failed)
        call assert_true("barh edgecolor: fill color used", backend%fill_color_ok, &
                         failed)
        call assert_true("barh edgecolor: edge color used", backend%line_color_ok, &
                         failed)
        call assert_true("barh edgecolor: no extra backend calls", &
                         backend%unexpected_calls == 0, failed)
    end subroutine test_barh_respects_edgecolor

end program test_bar_edgecolor_rendering
