program test_fortarray_adapter
    use, intrinsic :: iso_fortran_env, only: real64
    use fortarray_core, only: data_array_t, data_array
    use fortplot_figure_core, only: figure_t
    use fortplot_fortarray, only: plot, contourf
    use fortplot_contour_algorithms, only: calculate_marching_squares_config, &
                                          get_contour_lines
    implicit none

    type(data_array_t) :: field
    type(figure_t) :: figure
    type(data_array_t) :: surface
    type(figure_t) :: contour_figure
    real(real64) :: radius(3), angle(2), expected_surface(3, 2)
    integer :: stat, i, j

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
    if (any(shape(contour_figure%plots(1)%z_grid) /= [size(angle), size(radius)])) then
        error stop "contour field does not cover every coordinate pair"
    end if
    do j = 1, size(angle)
        do i = 1, size(radius)
            if (abs(contour_figure%plots(1)%z_grid(j, i) - &
                    expected_surface(i, j)) > 1.0e-12_real64) then
                error stop "contour adapter changed the value at a coordinate pair"
            end if
        end do
    end do
    call check_plane_contour(2)
    call check_plane_contour(3)

contains

    subroutine check_plane_contour(nx)
        !! Independent physical oracle: every contour point of z=x+10y at
        !! level 8 must satisfy x+10y=8, for both square and rectangular grids.
        integer, intent(in) :: nx
        type(data_array_t) :: plane
        type(figure_t) :: plane_figure
        real(real64) :: x(nx), y(2), z(nx, 2), points(8), corners(4)
        real(real64), parameter :: level = 8.0_real64
        integer :: ix, iy, config, nlines, point, ierr, total_lines

        x = [(real(ix - 1, real64), ix=1, nx)]
        y = [0.0_real64, 1.5_real64]
        do iy = 1, 2
            z(:, iy) = x + 10.0_real64*y(iy)
        end do
        plane = data_array(z, ['x', 'y'], name='plane')
        call plane%set_coord('x', x)
        call plane%set_coord('y', y)
        call plane_figure%initialize()
        call contourf(plane_figure, plane, ierr)
        if (ierr /= 0) error stop 'plane adapter failed'
        total_lines = 0
        do ix = 1, nx - 1
            corners = [plane_figure%plots(1)%z_grid(1, ix), &
                       plane_figure%plots(1)%z_grid(1, ix + 1), &
                       plane_figure%plots(1)%z_grid(2, ix + 1), &
                       plane_figure%plots(1)%z_grid(2, ix)]
            call calculate_marching_squares_config(corners(1), corners(2), &
                                                   corners(3), corners(4), level, config)
            call get_contour_lines(config, x(ix), y(1), x(ix + 1), y(1), &
                                   x(ix + 1), y(2), x(ix), y(2), corners(1), &
                                   corners(2), corners(3), corners(4), level, points, nlines)
            total_lines = total_lines + nlines
            do point = 1, nlines*4, 2
                if (abs(points(point) + 10.0_real64*points(point + 1) - level) > &
                    1.0e-10_real64) error stop 'adapter rotated physical contour'
            end do
        end do
        if (total_lines /= nx - 1) error stop 'physical contour is incomplete'
    end subroutine check_plane_contour

end program test_fortarray_adapter
