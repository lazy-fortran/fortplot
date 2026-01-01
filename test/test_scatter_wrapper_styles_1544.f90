program test_scatter_wrapper_styles_1544
    use fortplot, only: figure_t, wp
    use fortplot_scatter_plots, only: add_scatter_2d, add_scatter_3d
    implicit none

    type(figure_t) :: fig
    real(wp) :: x(3), y(3), z(3)
    real(wp) :: edge_color(3), face_color(3)
    real(wp) :: tol
    integer :: failures

    tol = 1.0e-12_wp
    failures = 0

    x = [1.0_wp, 2.0_wp, 3.0_wp]
    y = [4.0_wp, 5.0_wp, 6.0_wp]
    z = [7.0_wp, 8.0_wp, 9.0_wp]

    edge_color = [0.1_wp, 0.2_wp, 0.3_wp]
    face_color = [0.9_wp, 0.8_wp, 0.7_wp]

    call fig%initialize()
    call add_scatter_2d(fig, x, y, edgecolor=edge_color, facecolor=face_color, &
                        linewidth=2.5_wp)

    if (.not. fig%plots(fig%plot_count)%marker_edgecolor_set) failures = failures + 1
    if (.not. fig%plots(fig%plot_count)%marker_facecolor_set) failures = failures + 1

    if (any(abs(fig%plots(fig%plot_count)%marker_edgecolor - edge_color) > tol)) then
        failures = failures + 1
    end if
    if (any(abs(fig%plots(fig%plot_count)%marker_facecolor - face_color) > tol)) then
        failures = failures + 1
    end if
    if (abs(fig%plots(fig%plot_count)%marker_linewidth - 2.5_wp) > tol) then
        failures = failures + 1
    end if

    edge_color = [0.4_wp, 0.5_wp, 0.6_wp]
    face_color = [0.6_wp, 0.5_wp, 0.4_wp]

    call add_scatter_3d(fig, x, y, z, edgecolor=edge_color, facecolor=face_color, &
                        linewidth=1.25_wp)

    if (.not. fig%plots(fig%plot_count)%marker_edgecolor_set) failures = failures + 1
    if (.not. fig%plots(fig%plot_count)%marker_facecolor_set) failures = failures + 1

    if (any(abs(fig%plots(fig%plot_count)%marker_edgecolor - edge_color) > tol)) then
        failures = failures + 1
    end if
    if (any(abs(fig%plots(fig%plot_count)%marker_facecolor - face_color) > tol)) then
        failures = failures + 1
    end if
    if (abs(fig%plots(fig%plot_count)%marker_linewidth - 1.25_wp) > tol) then
        failures = failures + 1
    end if

    if (failures == 0) then
        print *, "PASS: test_scatter_wrapper_styles_1544"
        stop 0
    end if

    print *, "FAIL: test_scatter_wrapper_styles_1544 failures=", failures
    stop 1
end program test_scatter_wrapper_styles_1544
