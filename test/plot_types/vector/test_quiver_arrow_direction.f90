program test_quiver_arrow_direction
    !! Matplotlib scale=5 sends a unit vector across one fifth of the axes width.
    !! The UV angle stays 45 degrees even on rectangular axes; pivot translates
    !! the arrow without rotating or changing its length.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    use fortplot_quiver_geometry, only: prepare_quiver_geometry, quiver_arrow_vertices
    implicit none
    type(plot_data_t) :: plot
    real(wp) :: lengths(1), angles(1), width, x(8), y(8)

    plot%x = [0.25_wp]
    plot%y = [0.5_wp]
    plot%quiver_u = [1.0_wp]
    plot%quiver_v = [1.0_wp]
    plot%quiver_scale = 5.0_wp
    plot%quiver_width = 0.01_wp
    call prepare_quiver_geometry(plot, [0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp], &
                           [400.0_wp, 300.0_wp], 100.0_wp, 'linear', 'linear', 1.0_wp, &
                                 lengths, angles, width)
    call quiver_arrow_vertices(lengths(1), angles(1), width, 3.0_wp, 5.0_wp, &
                               'tail', x, y)
    if (abs(x(4) - 80.0_wp) > 1.0e-12_wp) &
        error stop 'tail-pivot x tip must follow inverse scale'
    if (abs(y(4) - 80.0_wp) > 1.0e-12_wp) &
        error stop 'UV y tip must use display-space direction'
    call quiver_arrow_vertices(lengths(1), angles(1), width, 3.0_wp, 5.0_wp, &
                               'middle', x, y)
    if (abs(x(4) - 40.0_wp) > 1.0e-12_wp) &
        error stop 'middle pivot must put half the vector ahead of the origin'
    call quiver_arrow_vertices(lengths(1), angles(1), width, 3.0_wp, 5.0_wp, &
                               'tip', x, y)
    if (abs(x(4)) + abs(y(4)) > 1.0e-12_wp) &
        error stop 'tip pivot must anchor the tip at the supplied origin'
end program test_quiver_arrow_direction
