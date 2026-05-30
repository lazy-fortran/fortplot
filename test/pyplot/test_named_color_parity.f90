program test_named_color_parity
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_colors, only: clear_color_cache, parse_color
    use fortplot_matplotlib, only: bar, figure, get_global_figure, hist, scatter
    use fortplot_figure_core, only: figure_t
    implicit none

    logical :: failed

    failed = .false.
    call clear_color_cache()
    call test_parse_color_aliases(failed)
    call test_bar_accepts_css4_name(failed)
    call test_scatter_accepts_tab_name(failed)
    call test_hist_accepts_tab_name(failed)

    if (failed) stop 1
    print *, "named color parity tests passed"

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

    subroutine assert_color(name, actual, expected, failed)
        character(len=*), intent(in) :: name
        real(wp), intent(in) :: actual(3), expected(3)
        logical, intent(inout) :: failed

        call assert_true(name, all(abs(actual - expected) < 1.0e-6_wp), failed)
    end subroutine assert_color

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

    subroutine test_parse_color_aliases(failed)
        logical, intent(inout) :: failed
        real(wp) :: rgb(3), expected(3)
        logical :: ok

        call parse_color('LightGoldenRodYellow', rgb, ok)
        call assert_true("parse_color accepts CSS4 names case-insensitively", ok, failed)
        call resolve_expected_color('lightgoldenrodyellow', expected)
        call assert_color("parse_color returns the CSS4 RGB value", rgb, expected, failed)

        call parse_color('tab:blue', rgb, ok)
        call assert_true("parse_color accepts tab cycle names", ok, failed)
        call resolve_expected_color('tab:blue', expected)
        call assert_color("parse_color returns the tab cycle RGB value", rgb, expected, failed)
    end subroutine test_parse_color_aliases

    subroutine test_bar_accepts_css4_name(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        real(wp) :: expected(3)

        call figure()
        call bar([1.0_wp, 2.0_wp], [2.0_wp, 3.0_wp], color='steelblue')
        fig => get_global_figure()
        call resolve_expected_color('steelblue', expected)
        call assert_color("bar stores CSS4 color strings", &
                          fig%plots(fig%plot_count)%color, expected, failed)
    end subroutine test_bar_accepts_css4_name

    subroutine test_scatter_accepts_tab_name(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        real(wp) :: expected(3)

        call figure()
        call scatter([1.0_wp, 2.0_wp], [3.0_wp, 4.0_wp], color='tab:blue')
        fig => get_global_figure()
        call resolve_expected_color('tab:blue', expected)
        call assert_color("scatter stores tab cycle color strings", &
                          fig%plots(fig%plot_count)%color, expected, failed)
    end subroutine test_scatter_accepts_tab_name

    subroutine test_hist_accepts_tab_name(failed)
        logical, intent(inout) :: failed
        type(figure_t), pointer :: fig
        real(wp) :: expected(3)

        call figure()
        call hist([0.0_wp, 0.5_wp, 1.0_wp, 1.5_wp], bins=2, color='tab:blue')
        fig => get_global_figure()
        call resolve_expected_color('tab:blue', expected)
        call assert_color("hist stores tab cycle color strings", &
                          fig%plots(fig%plot_count)%color, expected, failed)
    end subroutine test_hist_accepts_tab_name

end program test_named_color_parity
