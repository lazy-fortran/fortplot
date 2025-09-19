program test_3d_projection
    !! Validate that 3D plotting utilities project data to 2D as expected

    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_3d_plots, only: add_3d_plot
    use fortplot_scatter_plots, only: add_scatter_plot_data
    use fortplot_projection, only: get_default_view_angles, project_3d_to_2d
    use fortplot_windows_test_helper, only: get_windows_safe_tolerance
    implicit none

    call test_line_projection()
    call test_scatter_projection()

    print *, 'All 3D projection tests passed!'

contains

    subroutine test_line_projection()
        type(figure_t) :: fig
        real(wp) :: x(4), y(4), z(4)
        real(wp) :: x_expected(4), y_expected(4)
        real(wp) :: tol, azim, elev, dist
        integer :: plot_idx

        call fig%initialize()

        x = [0.0_wp, 1.0_wp, -1.0_wp, 0.5_wp]
        y = [0.0_wp, 0.5_wp, 0.5_wp, -0.5_wp]
        z = [0.0_wp, 0.5_wp, 0.5_wp, 1.0_wp]

        call get_default_view_angles(azim, elev, dist)
        call project_3d_to_2d(x, y, z, azim, elev, dist, x_expected, y_expected)

        call add_3d_plot(fig, x, y, z)

        plot_idx = fig%plot_count
        tol = get_windows_safe_tolerance(1.0e-12_wp)

        if (plot_idx /= 1) then
            error stop 'Expected one plot in figure after add_3d_plot'
        end if

        if (.not. allocated(fig%plots(plot_idx)%z)) then
            error stop '3D line plot should retain original z samples'
        end if

        if (any(abs(fig%plots(plot_idx)%x - x_expected) > tol)) then
            error stop 'Projected x coordinates mismatch for 3D line plot'
        end if

        if (any(abs(fig%plots(plot_idx)%y - y_expected) > tol)) then
            error stop 'Projected y coordinates mismatch for 3D line plot'
        end if

        if (any(abs(fig%plots(plot_idx)%z - z) > tol)) then
            error stop 'Stored z coordinates mismatch for 3D line plot'
        end if
    end subroutine test_line_projection

    subroutine test_scatter_projection()
        type(figure_t) :: fig
        real(wp) :: x(3), y(3), z(3)
        real(wp) :: x_expected(3), y_expected(3)
        real(wp) :: tol, azim, elev, dist
        integer :: plot_idx

        call fig%initialize()

        x = [0.0_wp, 0.5_wp, -0.5_wp]
        y = [0.0_wp, -0.25_wp, 0.75_wp]
        z = [0.0_wp, 0.8_wp, 0.4_wp]

        call get_default_view_angles(azim, elev, dist)
        call project_3d_to_2d(x, y, z, azim, elev, dist, x_expected, y_expected)

        call add_scatter_plot_data(fig, x, y, z=z)

        plot_idx = fig%plot_count
        tol = get_windows_safe_tolerance(1.0e-12_wp)

        if (plot_idx /= 1) then
            error stop 'Expected one scatter plot in figure after add_scatter_plot_data'
        end if

        if (.not. allocated(fig%plots(plot_idx)%z)) then
            error stop '3D scatter plot should retain original z samples'
        end if

        if (any(abs(fig%plots(plot_idx)%x - x_expected) > tol)) then
            error stop 'Projected x coordinates mismatch for 3D scatter plot'
        end if

        if (any(abs(fig%plots(plot_idx)%y - y_expected) > tol)) then
            error stop 'Projected y coordinates mismatch for 3D scatter plot'
        end if

        if (any(abs(fig%plots(plot_idx)%z - z) > tol)) then
            error stop 'Stored z coordinates mismatch for 3D scatter plot'
        end if
    end subroutine test_scatter_projection

end program test_3d_projection
