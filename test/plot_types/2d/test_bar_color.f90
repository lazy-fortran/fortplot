program test_bar_color
    !! Test bar/barh color/edgecolor keyword flexibility
    !! Covers issue #1661: bar() and barh() must accept string colors
    !! and string edgecolors in all combinations.
    !!
    !! REQ-001: bar(x, h, color='red') - string color
    !! REQ-002: bar(x, h, color=[1,0,0], edgecolor='red') - RGB color + string edgecolor
    !! REQ-003: barh(y, w, color='red') - string color
    !! REQ-004: barh(y, w, color=[1,0,0], edgecolor='red') - RGB color + string edgecolor
    !! REQ-005: bar(x, h) - no color (backward compat)
    !! REQ-006: bar(x, h, color=[1,0,0]) - RGB color only (backward compat)
    !! REQ-007: bar(x, h, color='red', edgecolor='blue') - both string colors
    !! REQ-008: barh(y, w) - no color (backward compat)
    !! REQ-009: barh(y, w, color=[1,0,0]) - RGB color only (backward compat)
    !! REQ-010: barh(y, w, color='red', edgecolor='blue') - both string colors

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_matplotlib, only: bar, barh, bar_rgb_array, barh_rgb_array, figure, get_global_figure
    use fortplot_figure_core, only: figure_t

    implicit none

    call test_bar_string_color()
    call test_bar_rgb_color_string_edgecolor()
    call test_barh_string_color()
    call test_barh_rgb_color_string_edgecolor()
    call test_bar_no_color()
    call test_bar_rgb_color_only()
    call test_bar_both_string_colors()
    call test_barh_no_color()
    call test_barh_rgb_color_only()
    call test_barh_both_string_colors()
    call test_bar_per_bar_color()
    call test_bar_per_bar_color_and_edgecolor()
    call test_barh_per_bar_color()
    call test_barh_per_bar_color_and_edgecolor()

    print *, "All bar color/edgecolor tests passed"

contains

    subroutine test_bar_string_color()
        !! REQ-001: bar(x, h, color='red')
        real(wp) :: x(3), h(3)
        type(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        h = [1.0_wp, 2.0_wp, 3.0_wp]
        call bar(x, h, color='red')

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: REQ-001 bar with string color did not add plot"
            stop 1
        end if
    end subroutine test_bar_string_color

    subroutine test_bar_rgb_color_string_edgecolor()
        !! REQ-002: bar(x, h, color=[1,0,0], edgecolor='red')
        real(wp) :: x(3), h(3)
        type(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        h = [1.0_wp, 2.0_wp, 3.0_wp]
        call bar(x, h, color=[1.0_wp, 0.0_wp, 0.0_wp], edgecolor='red')

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: REQ-002 bar with RGB color + string edgecolor did not add plot"
            stop 1
        end if
    end subroutine test_bar_rgb_color_string_edgecolor

    subroutine test_barh_string_color()
        !! REQ-003: barh(y, w, color='red')
        real(wp) :: y(3), w(3)
        type(figure_t), pointer :: fig

        call figure()
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        w = [1.0_wp, 2.0_wp, 3.0_wp]
        call barh(y, w, color='red')

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: REQ-003 barh with string color did not add plot"
            stop 1
        end if
    end subroutine test_barh_string_color

    subroutine test_barh_rgb_color_string_edgecolor()
        !! REQ-004: barh(y, w, color=[1,0,0], edgecolor='red')
        real(wp) :: y(3), w(3)
        type(figure_t), pointer :: fig

        call figure()
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        w = [1.0_wp, 2.0_wp, 3.0_wp]
        call barh(y, w, color=[1.0_wp, 0.0_wp, 0.0_wp], edgecolor='red')

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: REQ-004 barh with RGB color + string edgecolor did not add plot"
            stop 1
        end if
    end subroutine test_barh_rgb_color_string_edgecolor

    subroutine test_bar_no_color()
        !! REQ-005: bar(x, h) - no color (backward compat)
        real(wp) :: x(3), h(3)
        type(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        h = [1.0_wp, 2.0_wp, 3.0_wp]
        call bar(x, h)

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: REQ-005 bar with no color did not add plot"
            stop 1
        end if
    end subroutine test_bar_no_color

    subroutine test_bar_rgb_color_only()
        !! REQ-006: bar(x, h, color=[1,0,0]) - RGB color only
        real(wp) :: x(3), h(3)
        type(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        h = [1.0_wp, 2.0_wp, 3.0_wp]
        call bar(x, h, color=[1.0_wp, 0.0_wp, 0.0_wp])

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: REQ-006 bar with RGB color only did not add plot"
            stop 1
        end if
    end subroutine test_bar_rgb_color_only

    subroutine test_bar_both_string_colors()
        !! REQ-007: bar(x, h, color='red', edgecolor='blue') - both string colors
        real(wp) :: x(3), h(3)
        type(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        h = [1.0_wp, 2.0_wp, 3.0_wp]
        call bar(x, h, color='red', edgecolor='blue')

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: REQ-007 bar with both string colors did not add plot"
            stop 1
        end if
    end subroutine test_bar_both_string_colors

    subroutine test_barh_no_color()
        !! REQ-008: barh(y, w) - no color (backward compat)
        real(wp) :: y(3), w(3)
        type(figure_t), pointer :: fig

        call figure()
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        w = [1.0_wp, 2.0_wp, 3.0_wp]
        call barh(y, w)

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: REQ-008 barh with no color did not add plot"
            stop 1
        end if
    end subroutine test_barh_no_color

    subroutine test_barh_rgb_color_only()
        !! REQ-009: barh(y, w, color=[1,0,0]) - RGB color only
        real(wp) :: y(3), w(3)
        type(figure_t), pointer :: fig

        call figure()
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        w = [1.0_wp, 2.0_wp, 3.0_wp]
        call barh(y, w, color=[1.0_wp, 0.0_wp, 0.0_wp])

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: REQ-009 barh with RGB color only did not add plot"
            stop 1
        end if
    end subroutine test_barh_rgb_color_only

    subroutine test_barh_both_string_colors()
        !! REQ-010: barh(y, w, color='red', edgecolor='blue') - both string colors
        real(wp) :: y(3), w(3)
        type(figure_t), pointer :: fig

        call figure()
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        w = [1.0_wp, 2.0_wp, 3.0_wp]
        call barh(y, w, color='red', edgecolor='blue')

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: REQ-010 barh with both string colors did not add plot"
            stop 1
        end if
    end subroutine test_barh_both_string_colors

   subroutine test_bar_per_bar_color()
        !! Per-bar RGB color array: bar_rgb_array(x, h, color_per_bar=[..., ..., ...])
        real(wp) :: x(4), h(4)
        real(wp) :: colors(3, 4)
        type(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        h = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        colors = reshape([1.0_wp, 0.0_wp, 0.0_wp, &
                          0.0_wp, 1.0_wp, 0.0_wp, &
                          0.0_wp, 0.0_wp, 1.0_wp, &
                          1.0_wp, 1.0_wp, 0.0_wp], [3, 4])
        call bar_rgb_array(x, h, color_per_bar=colors)

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: bar with per-bar color array did not add plot"
            stop 1
        end if
        if (.not. fig%plots(fig%plot_count)%bar_color_per_bar_set) then
            print *, "FAIL: bar per-bar color not stored"
            stop 1
        end if
    end subroutine test_bar_per_bar_color

  subroutine test_bar_per_bar_color_and_edgecolor()
        !! Per-bar RGB color + edgecolor arrays
        real(wp) :: x(3), h(3)
        real(wp) :: colors(3, 3), edges(3, 3)
        type(figure_t), pointer :: fig

        call figure()
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        h = [1.0_wp, 2.0_wp, 3.0_wp]
        colors = reshape([1.0_wp, 0.0_wp, 0.0_wp, &
                          0.0_wp, 1.0_wp, 0.0_wp, &
                          0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
        edges = reshape([0.0_wp, 0.0_wp, 0.0_wp, &
                         0.5_wp, 0.5_wp, 0.5_wp, &
                         1.0_wp, 1.0_wp, 1.0_wp], [3, 3])
        call bar_rgb_array(x, h, color_per_bar=colors, edgecolor_per_bar=edges)

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: bar with per-bar color+edge did not add plot"
            stop 1
        end if
        if (.not. fig%plots(fig%plot_count)%bar_color_per_bar_set) then
            print *, "FAIL: bar per-bar color not stored"
            stop 1
        end if
        if (.not. fig%plots(fig%plot_count)%bar_edgecolor_per_bar_set) then
            print *, "FAIL: bar per-bar edgecolor not stored"
            stop 1
        end if
    end subroutine test_bar_per_bar_color_and_edgecolor

    subroutine test_barh_per_bar_color()
        !! Per-bar RGB color array for barh
        real(wp) :: y(4), w(4)
        real(wp) :: colors(3, 4)
        type(figure_t), pointer :: fig

        call figure()
        y = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        w = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        colors = reshape([1.0_wp, 0.0_wp, 0.0_wp, &
                          0.0_wp, 1.0_wp, 0.0_wp, &
                          0.0_wp, 0.0_wp, 1.0_wp, &
                          1.0_wp, 1.0_wp, 0.0_wp], [3, 4])
        call barh_rgb_array(y, w, color_per_bar=colors)

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: barh with per-bar color array did not add plot"
            stop 1
        end if
        if (.not. fig%plots(fig%plot_count)%bar_color_per_bar_set) then
            print *, "FAIL: barh per-bar color not stored"
            stop 1
        end if
    end subroutine test_barh_per_bar_color

    subroutine test_barh_per_bar_color_and_edgecolor()
        !! Per-bar RGB color + edgecolor arrays for barh
        real(wp) :: y(3), w(3)
        real(wp) :: colors(3, 3), edges(3, 3)
        type(figure_t), pointer :: fig

        call figure()
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        w = [1.0_wp, 2.0_wp, 3.0_wp]
        colors = reshape([1.0_wp, 0.0_wp, 0.0_wp, &
                          0.0_wp, 1.0_wp, 0.0_wp, &
                          0.0_wp, 0.0_wp, 1.0_wp], [3, 3])
        edges = reshape([0.0_wp, 0.0_wp, 0.0_wp, &
                         0.5_wp, 0.5_wp, 0.5_wp, &
                         1.0_wp, 1.0_wp, 1.0_wp], [3, 3])
        call barh_rgb_array(y, w, color_per_bar=colors, edgecolor_per_bar=edges)

        fig => get_global_figure()
        if (fig%plot_count < 1) then
            print *, "FAIL: barh with per-bar color+edge did not add plot"
            stop 1
        end if
        if (.not. fig%plots(fig%plot_count)%bar_color_per_bar_set) then
            print *, "FAIL: barh per-bar color not stored"
            stop 1
        end if
        if (.not. fig%plots(fig%plot_count)%bar_edgecolor_per_bar_set) then
            print *, "FAIL: barh per-bar edgecolor not stored"
            stop 1
        end if
    end subroutine test_barh_per_bar_color_and_edgecolor

end program test_bar_color
