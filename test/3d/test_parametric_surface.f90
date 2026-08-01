program test_parametric_surface
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot, only: add_parametric_surface, figure, figure_t, get_global_figure, &
                        savefig, title
    use fortplot_system_runtime, only: create_directory_runtime
    use fortplot_validation, only: validate_file_size, validate_png_format, &
                                   validation_result_t
    implicit none

    integer, parameter :: meridian_count = 17, azimuth_count = 33
    real(wp), parameter :: pi = acos(-1.0_wp)
    real(wp) :: theta(meridian_count), phi(azimuth_count)
    real(wp) :: x(meridian_count, azimuth_count)
    real(wp) :: y(meridian_count, azimuth_count)
    real(wp) :: z(meridian_count, azimuth_count)
    real(wp) :: major_radius, minor_radius
    integer :: i, j
    class(figure_t), pointer :: figure_handle
    logical :: directory_ok
    type(validation_result_t) :: validation
    character(len=*), parameter :: output_file = &
        'build/test/output/test_parametric_surface.png'

    call create_directory_runtime('build/test/output', directory_ok)
    if (.not. directory_ok) stop 1

    major_radius = 2.0_wp
    minor_radius = 0.6_wp
    do i = 1, meridian_count
        theta(i) = 2.0_wp*pi*real(i - 1, wp)/real(meridian_count - 1, wp)
    end do
    do j = 1, azimuth_count
        phi(j) = 2.0_wp*pi*real(j - 1, wp)/real(azimuth_count - 1, wp)
    end do
    do j = 1, azimuth_count
        do i = 1, meridian_count
            x(i, j) = (major_radius + minor_radius*cos(theta(i)))*cos(phi(j))
            y(i, j) = (major_radius + minor_radius*cos(theta(i)))*sin(phi(j))
            z(i, j) = minor_radius*sin(theta(i))
        end do
    end do

    call figure(figsize=[7.0_wp, 6.0_wp])
    call add_parametric_surface(x, y, z, cmap='viridis', alpha=0.95_wp, &
                                linewidth=0.0_wp, filled=.true., &
                                label='parametric torus')
    figure_handle => get_global_figure()
    if (figure_handle%state%plot_count /= 1) stop 1
    if (.not. allocated(figure_handle%plots(1)%parametric_x_grid)) stop 1
    if (.not. figure_handle%plots(1)%surface_filled) stop 1
    if (size(figure_handle%plots(1)%parametric_x_grid) /= &
        meridian_count*azimuth_count) stop 1
    call title('Parametric 3D surface')
    call savefig(output_file)

    validation = validate_file_size(output_file, 200)
    if (.not. validation%passed) stop 1
    validation = validate_png_format(output_file)
    if (.not. validation%passed) stop 1
end program test_parametric_surface
