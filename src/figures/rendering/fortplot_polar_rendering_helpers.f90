module fortplot_polar_rendering_helpers
    !! Polar rendering helper procedures
    !!
    !! Extracted from fortplot_figure_rendering_pipeline to reduce module size
    !! Single Responsibility: Polar axes and plot rendering helpers

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t
    use fortplot_polar_rendering, only: render_polar_data, render_polar_boundary, &
                                        render_polar_radial_gridlines, &
                                        render_polar_angular_gridlines, &
                                        render_polar_angular_ticks
    implicit none

    private
    public :: render_polar_axes, render_polar_plot_internal

contains

    subroutine render_polar_axes(backend, x_min, x_max, y_min, y_max, state)
        !! Render polar axes: circular boundary, radial spokes, angular circles, tick labels
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(figure_state_t), intent(in) :: state

        real(wp) :: center_x, center_y, radius
        real(wp) :: theta_offset
        logical :: clockwise
        integer :: n_spokes, n_circles

        if (.not. state%polar_projection) return

        center_x = (x_min + x_max)*0.5_wp
        center_y = (y_min + y_max)*0.5_wp
        radius = min(x_max - x_min, y_max - y_min)*0.45_wp

        theta_offset = state%polar_theta_offset
        clockwise = state%polar_theta_direction_cw
        n_spokes = state%polar_theta_gridlines
        n_circles = state%polar_r_gridlines

        ! Render concentric circles (angular gridlines)
        call render_polar_angular_gridlines(backend, center_x, center_y, radius, &
                                            n_circles)

        ! Render radial spokes
        call render_polar_radial_gridlines(backend, center_x, center_y, radius, &
                                           n_spokes, theta_offset, clockwise)

        ! Render circular boundary
        call render_polar_boundary(backend, center_x, center_y, radius)

        ! Render angular tick labels
        call render_polar_angular_ticks(backend, center_x, center_y, radius, &
                                        n_spokes, theta_offset, clockwise)
    end subroutine render_polar_axes

    subroutine render_polar_plot_internal(backend, plot, x_min, x_max, y_min, y_max, &
                                          state)
        !! Render polar plot data within the coordinate system
        !! The plot stores pre-converted Cartesian coordinates in x/y arrays
        !! but we use polar_theta/polar_r for proper polar rendering
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        type(figure_state_t), intent(in), optional :: state

        real(wp) :: center_x, center_y, radius, r_scale
        real(wp) :: theta_offset
        logical :: clockwise
        integer :: n

        ! Compute center and radius in data coordinates
        center_x = (x_min + x_max)*0.5_wp
        center_y = (y_min + y_max)*0.5_wp
        radius = min(x_max - x_min, y_max - y_min)*0.45_wp

        ! Get polar configuration from state
        theta_offset = 0.0_wp  ! 0 deg at east (matplotlib)
        clockwise = .false.
        if (present(state)) then
            theta_offset = state%polar_theta_offset
            clockwise = state%polar_theta_direction_cw
        end if

        ! Compute r_scale based on polar data range
        r_scale = 1.0_wp
        if (allocated(plot%polar_r)) then
            if (size(plot%polar_r) > 0) then
                r_scale = radius/maxval(abs(plot%polar_r))
            end if
        end if

        ! Render polar data if available
        if (allocated(plot%polar_theta) .and. allocated(plot%polar_r)) then
            n = min(size(plot%polar_theta), size(plot%polar_r))
            if (n > 0) then
                call render_polar_data(backend, plot%polar_theta, plot%polar_r, n, &
                                       center_x, center_y, r_scale, &
                                       theta_offset, clockwise, plot%color)
            end if
        end if
    end subroutine render_polar_plot_internal

end module fortplot_polar_rendering_helpers
