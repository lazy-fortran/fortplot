program test_fortarray_adapter
    use, intrinsic :: iso_fortran_env, only: real64
    use fortarray_core, only: data_array_t, data_array
    use fortplot_figure_core, only: figure_t
    use fortplot_fortarray, only: plot, contourf
    implicit none

    type(data_array_t) :: field
    type(figure_t) :: figure
    type(data_array_t) :: surface
    type(figure_t) :: contour_figure
    real(real64) :: radius(3), angle(2), expected_surface(3, 2)
    integer :: stat

    radius = [0.1_real64, 0.4_real64, 0.9_real64]
    field = data_array([2.0_real64, 3.0_real64, 5.0_real64], ["radius"], &
        name="density")
    call field%set_coord("radius", radius)
    call figure%initialize()
    call plot(figure, field, stat)

    if (stat /= 0) error stop "adapter rejected a rank-one DataArray"
    if (figure%plot_count /= 1) error stop "adapter did not create one plot"
    if (any(abs(figure%plots(1)%x - radius) > 1.0e-12_real64)) then
        error stop "adapter did not use the dimension coordinate"
    end if
    if (any(abs(figure%plots(1)%y - field%values) > 1.0e-12_real64)) then
        error stop "adapter changed the field values"
    end if

    angle = [0.0_real64, 1.5_real64]
    expected_surface = reshape([1.0_real64, 2.0_real64, 3.0_real64, &
        4.0_real64, 5.0_real64, 6.0_real64], [3, 2])
    surface = data_array(expected_surface, ["radius", "angle "], name="potential")
    call surface%set_coord("radius", radius)
    call surface%set_coord("angle", angle)
    call contour_figure%initialize()
    call contourf(contour_figure, surface, stat)

    if (stat /= 0) error stop "adapter rejected a rank-two DataArray"
    if (contour_figure%plot_count /= 1) error stop "adapter did not create one contour"
    if (any(abs(contour_figure%plots(1)%x_grid - radius) > 1.0e-12_real64)) then
        error stop "contour adapter did not use the first dimension coordinate"
    end if
    if (any(abs(contour_figure%plots(1)%y_grid - angle) > 1.0e-12_real64)) then
        error stop "contour adapter did not use the second dimension coordinate"
    end if
    if (any(abs(contour_figure%plots(1)%z_grid - expected_surface) > 1.0e-12_real64)) then
        error stop "contour adapter changed the field layout"
    end if
end program test_fortarray_adapter
