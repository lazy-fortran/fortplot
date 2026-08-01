program test_scatter_colormap_3d
    !! Rendering oracle for per-point colours on projected 3-D scatter plots.
    use, intrinsic :: iso_fortran_env, only: real64
    use fortplot_3d_data_rendering, only: render_3d_markers
    use fortplot_colormap, only: colormap_value_to_color
    use fortplot_plot_data, only: PLOT_TYPE_SCATTER, plot_data_t
    use fortplot_spy_backend, only: spy_context_t
    implicit none

    type(plot_data_t) :: plot
    type(spy_context_t) :: backend
    real(real64), parameter :: x(2) = [0.0_real64, 1.0_real64]
    real(real64), parameter :: y(2) = [0.0_real64, 1.0_real64]
    real(real64), parameter :: z(2) = [0.0_real64, 1.0_real64]
    real(real64), parameter :: values(2) = [0.0_real64, 1.0_real64]
    real(real64) :: expected_low(3), expected_high(3)

    plot%plot_type = PLOT_TYPE_SCATTER
    plot%x = x
    plot%y = y
    plot%z = z
    plot%marker = 'o'
    plot%scatter_colors = values
    plot%scatter_colormap = 'viridis'
    plot%scatter_vmin = 0.0_real64
    plot%scatter_vmax = 1.0_real64
    plot%scatter_vrange_set = .true.

    call backend%reset()
    call render_3d_markers(backend, plot, 0.0_real64, 1.0_real64, &
        0.0_real64, 1.0_real64, 0.0_real64, 1.0_real64)
    call colormap_value_to_color(0.0_real64, 0.0_real64, 1.0_real64, &
        'viridis', expected_low)
    call colormap_value_to_color(1.0_real64, 0.0_real64, 1.0_real64, &
        'viridis', expected_high)
    if (backend%marker_color_calls /= 2) then
        error stop '3-D scatter did not render both markers'
    end if
    if (maxval(abs(backend%marker_face_history(:, 1) - expected_low)) > &
        1.0e-12_real64) then
        error stop '3-D scatter low colour was not mapped through viridis'
    end if
    if (maxval(abs(backend%marker_face_history(:, 2) - expected_high)) > &
        1.0e-12_real64) then
        error stop '3-D scatter high colour was not mapped through viridis'
    end if

    print *, 'PASS: 3-D scatter per-point colormap rendering'
end program test_scatter_colormap_3d
