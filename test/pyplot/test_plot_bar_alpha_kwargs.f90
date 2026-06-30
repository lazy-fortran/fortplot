program test_plot_bar_alpha_kwargs
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_matplotlib, only: figure, plot, add_plot, bar, barh, &
                                   bar_rgb_array, barh_rgb_array
    use fortplot_matplotlib_session, only: get_global_figure
    use fortplot_figure_core, only: figure_t

    implicit none

    call test_plot_alpha()
    call test_add_plot_alpha()
    call test_bar_alpha()
    call test_barh_alpha()
    call test_bar_rgb_array_alpha()
    call test_barh_rgb_array_alpha()

    print *, 'PASS: alpha kwargs accepted by plot/bar/barh variants'

contains

    subroutine test_plot_alpha()
        real(wp) :: x(3), y(3)
        class(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [2.0_wp, 1.0_wp, 2.5_wp]
        call plot(x, y, marker='o', alpha=0.25_wp)

        fig => get_global_figure()
        call assert_plot_alpha(fig, 1, 0.25_wp)
        call assert_marker_alpha(fig, 1, 0.25_wp)
    end subroutine test_plot_alpha

    subroutine test_add_plot_alpha()
        real(wp) :: x(3), y(3)
        class(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 3.0_wp, 2.0_wp]
        call add_plot(x, y, alpha=1.4_wp)

        fig => get_global_figure()
        call assert_plot_alpha(fig, 1, 1.0_wp)
    end subroutine test_add_plot_alpha

    subroutine test_bar_alpha()
        real(wp) :: x(3), h(3)
        class(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        h = [2.0_wp, 1.0_wp, 3.0_wp]
        call bar(x, h, color='red', alpha=0.6_wp)

        fig => get_global_figure()
        call assert_plot_alpha(fig, 1, 0.6_wp)
    end subroutine test_bar_alpha

    subroutine test_barh_alpha()
        real(wp) :: y(3), w(3)
        class(figure_t), pointer :: fig

        call figure()
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        w = [2.0_wp, 1.0_wp, 3.0_wp]
        call barh(y, w, color=[0.0_wp, 0.447_wp, 0.698_wp], alpha=-0.2_wp)

        fig => get_global_figure()
        call assert_plot_alpha(fig, 1, 0.0_wp)
    end subroutine test_barh_alpha

    subroutine test_bar_rgb_array_alpha()
        real(wp) :: x(3), h(3)
        real(wp) :: colors(3, 3)
        class(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        h = [2.0_wp, 1.0_wp, 3.0_wp]
        colors(:, 1) = [1.0_wp, 0.0_wp, 0.0_wp]
        colors(:, 2) = [0.0_wp, 1.0_wp, 0.0_wp]
        colors(:, 3) = [0.0_wp, 0.0_wp, 1.0_wp]
        call bar_rgb_array(x, h, color_per_bar=colors, alpha=0.5_wp)

        fig => get_global_figure()
        call assert_plot_alpha(fig, 1, 0.5_wp)
    end subroutine test_bar_rgb_array_alpha

    subroutine test_barh_rgb_array_alpha()
        real(wp) :: y(3), w(3)
        real(wp) :: colors(3, 3)
        class(figure_t), pointer :: fig

        call figure()
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        w = [2.0_wp, 1.0_wp, 3.0_wp]
        colors(:, 1) = [1.0_wp, 0.0_wp, 0.0_wp]
        colors(:, 2) = [0.0_wp, 1.0_wp, 0.0_wp]
        colors(:, 3) = [0.0_wp, 0.0_wp, 1.0_wp]
        call barh_rgb_array(y, w, color_per_bar=colors, alpha=1.2_wp)

        fig => get_global_figure()
        call assert_plot_alpha(fig, 1, 1.0_wp)
    end subroutine test_barh_rgb_array_alpha

    subroutine assert_plot_alpha(fig, idx, expected)
        class(figure_t), pointer, intent(in) :: fig
        integer, intent(in) :: idx
        real(wp), intent(in) :: expected

        if (.not. associated(fig)) error stop 'assert_plot_alpha: figure missing'
        if (fig%plot_count < idx) error stop 'assert_plot_alpha: plot missing'
        if (abs(fig%plots(idx)%fill_alpha - expected) > 1.0e-12_wp) &
            error stop 'assert_plot_alpha: alpha mismatch'
    end subroutine assert_plot_alpha

    subroutine assert_marker_alpha(fig, idx, expected)
        class(figure_t), pointer, intent(in) :: fig
        integer, intent(in) :: idx
        real(wp), intent(in) :: expected

        if (.not. associated(fig)) error stop 'assert_marker_alpha: figure missing'
        if (fig%plot_count < idx) error stop 'assert_marker_alpha: plot missing'
        if (abs(fig%plots(idx)%marker_face_alpha - expected) > 1.0e-12_wp) &
            error stop 'assert_marker_alpha: face alpha mismatch'
        if (abs(fig%plots(idx)%marker_edge_alpha - expected) > 1.0e-12_wp) &
            error stop 'assert_marker_alpha: edge alpha mismatch'
    end subroutine assert_marker_alpha

end program test_plot_bar_alpha_kwargs
