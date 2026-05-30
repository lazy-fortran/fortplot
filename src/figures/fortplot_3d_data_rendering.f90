module fortplot_3d_data_rendering
    !! Render 3D line and scatter samples against the same projected cube as
    !! the 3D frame.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_projection, only: project_3d_to_2d
    use fortplot_rendering, only: render_line_plot, render_markers
    implicit none

    private
    public :: render_3d_line_plot, render_3d_markers
    public :: project_3d_samples_to_axes

    real(wp), parameter :: EPSILON = 1.0e-12_wp

contains

    subroutine render_3d_line_plot(backend, plot, x_min, x_max, y_min, y_max, &
                                   z_min, z_max)
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max

        type(plot_data_t) :: projected
        real(wp), allocatable :: x_out(:), y_out(:)

        if (.not. valid_3d_samples(plot)) return

        call project_3d_samples_to_axes(plot%x, plot%y, plot%z, x_min, x_max, &
                                        y_min, y_max, z_min, z_max, &
                                        backend%view_azim, backend%view_elev, &
                                        backend%view_dist, x_out, y_out)
        projected = plot
        call replace_xy(projected, x_out, y_out)
        call render_line_plot(backend, projected, 'linear', 'linear', 1.0_wp)
    end subroutine render_3d_line_plot

    subroutine render_3d_markers(backend, plot, x_min, x_max, y_min, y_max, &
                                 z_min, z_max)
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max

        type(plot_data_t) :: projected
        real(wp), allocatable :: x_out(:), y_out(:)

        if (.not. valid_3d_samples(plot)) return

        call project_3d_samples_to_axes(plot%x, plot%y, plot%z, x_min, x_max, &
                                        y_min, y_max, z_min, z_max, &
                                        backend%view_azim, backend%view_elev, &
                                        backend%view_dist, x_out, y_out)
        projected = plot
        call replace_xy(projected, x_out, y_out)
        call render_markers(backend, projected, x_min, x_max, y_min, y_max, &
                            'linear', 'linear', 1.0_wp)
    end subroutine render_3d_markers

    subroutine project_3d_samples_to_axes(x, y, z, x_min, x_max, y_min, y_max, &
                                          z_min, z_max, azim, elev, dist, &
                                          x_out, y_out)
        real(wp), contiguous, intent(in) :: x(:), y(:), z(:)
        real(wp), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        real(wp), intent(in) :: azim, elev, dist
        real(wp), allocatable, intent(out) :: x_out(:), y_out(:)

        real(wp), allocatable :: x_norm(:), y_norm(:), z_norm(:)
        real(wp), allocatable :: x_proj(:), y_proj(:)
        real(wp) :: range_x, range_y, range_z
        real(wp) :: proj_x_min, proj_x_max, proj_y_min, proj_y_max
        real(wp) :: denom_x, denom_y
        integer :: i, n

        n = min(size(x), min(size(y), size(z)))
        allocate (x_out(n), y_out(n))
        if (n <= 0) return

        range_x = max(EPSILON, x_max - x_min)
        range_y = max(EPSILON, y_max - y_min)
        range_z = max(EPSILON, z_max - z_min)

        allocate (x_norm(n), y_norm(n), z_norm(n), x_proj(n), y_proj(n))
        do i = 1, n
            x_norm(i) = (x(i) - x_min)/range_x
            y_norm(i) = (y(i) - y_min)/range_y
            z_norm(i) = (z(i) - z_min)/range_z
        end do

        call projected_unit_cube_bounds(azim, elev, dist, proj_x_min, proj_x_max, &
                                        proj_y_min, proj_y_max)
        denom_x = max(EPSILON, proj_x_max - proj_x_min)
        denom_y = max(EPSILON, proj_y_max - proj_y_min)

        call project_3d_to_2d(x_norm, y_norm, z_norm, azim, elev, dist, &
                              x_proj, y_proj)

        do i = 1, n
            x_out(i) = x_min + (x_proj(i) - proj_x_min)/denom_x*range_x
            y_out(i) = y_min + (y_proj(i) - proj_y_min)/denom_y*range_y
        end do
    end subroutine project_3d_samples_to_axes

    subroutine projected_unit_cube_bounds(azim, elev, dist, x_min, x_max, &
                                          y_min, y_max)
        real(wp), intent(in) :: azim, elev, dist
        real(wp), intent(out) :: x_min, x_max, y_min, y_max

        real(wp) :: x_cube(8), y_cube(8), z_cube(8)
        real(wp) :: x_proj(8), y_proj(8)

        x_cube = [0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, &
                  0.0_wp]
        y_cube = [0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, &
                  1.0_wp]
        z_cube = [0.0_wp, 0.0_wp, 0.0_wp, 0.0_wp, 1.0_wp, 1.0_wp, 1.0_wp, &
                  1.0_wp]

        call project_3d_to_2d(x_cube, y_cube, z_cube, azim, elev, dist, &
                              x_proj, y_proj)
        x_min = minval(x_proj)
        x_max = maxval(x_proj)
        y_min = minval(y_proj)
        y_max = maxval(y_proj)
    end subroutine projected_unit_cube_bounds

    logical function valid_3d_samples(plot)
        type(plot_data_t), intent(in) :: plot

        valid_3d_samples = .false.
        if (.not. allocated(plot%x)) return
        if (.not. allocated(plot%y)) return
        if (.not. allocated(plot%z)) return
        valid_3d_samples = size(plot%x) > 0 .and. size(plot%x) == size(plot%y) &
                           .and. size(plot%x) == size(plot%z)
    end function valid_3d_samples

    subroutine replace_xy(plot, x_new, y_new)
        type(plot_data_t), intent(inout) :: plot
        real(wp), contiguous, intent(in) :: x_new(:), y_new(:)

        if (allocated(plot%x)) deallocate (plot%x)
        if (allocated(plot%y)) deallocate (plot%y)
        allocate (plot%x(size(x_new)), plot%y(size(y_new)))
        plot%x = x_new
        plot%y = y_new
    end subroutine replace_xy

end module fortplot_3d_data_rendering
