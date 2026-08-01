program test_quiver_arrow_direction
    !! Quiver vectors must pass their endpoint to the backend arrow renderer.
    !!
    !! The spy backend records the renderer call, so this oracle is independent
    !! of SVG layout and checks the documented tail-pivot geometry directly.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_plot_renderers, only: render_quiver_plot
    use fortplot_plot_data, only: PLOT_TYPE_QUIVER, plot_data_t
    use fortplot_spy_backend, only: spy_context_t
    implicit none

    type(plot_data_t) :: plot
    type(spy_context_t) :: context
    real(wp), parameter :: origin_x = 0.25_wp, origin_y = 0.5_wp
    real(wp), parameter :: vector_x = 1.0_wp, vector_y = 0.0_wp
    real(wp), parameter :: expected_scale = 0.095_wp

    allocate(plot%x(1), plot%y(1), plot%quiver_u(1), plot%quiver_v(1))
    plot%plot_type = PLOT_TYPE_QUIVER
    plot%x = origin_x
    plot%y = origin_y
    plot%quiver_u = vector_x
    plot%quiver_v = vector_y
    plot%quiver_scale = 1.0_wp
    plot%quiver_pivot = 'tail'

    call context%reset()
    context%width = 400
    context%height = 300
    call render_quiver_plot(context, plot, 0.0_wp, 1.0_wp, 0.0_wp, 1.0_wp, &
        'linear', 'linear', 1.0_wp)

    if (context%arrow_calls /= 1) then
        error stop 'quiver renderer did not emit exactly one arrow'
    end if
    if (abs(context%last_arrow(1) - (origin_x + expected_scale)) > 1.0e-12_wp) then
        error stop 'quiver arrow tip does not follow the declared x direction'
    end if
    if (abs(context%last_arrow(2) - origin_y) > 1.0e-12_wp) then
        error stop 'quiver arrow tip changed the declared y coordinate'
    end if
    if (abs(context%last_arrow(3) - vector_x*expected_scale) > 1.0e-12_wp) then
        error stop 'quiver arrow x component was rescaled unexpectedly'
    end if

end program test_quiver_arrow_direction
