module fortplot_polar_rendering
    !! Polar projection rendering for polygon backends
    !!
    !! Renders circular plot boundary, radial gridlines (spokes),
    !! angular gridlines (concentric circles), and angular tick labels.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_polar, only: polar_to_cartesian, compute_angular_ticks, &
                              compute_radial_ticks, PI, TWO_PI, RAD_TO_DEG
    implicit none

    private
    public :: render_polar_boundary
    public :: render_polar_radial_gridlines
    public :: render_polar_angular_gridlines
    public :: render_polar_angular_ticks
    public :: render_polar_data

    integer, parameter :: CIRCLE_SEGMENTS = 72  ! 5-degree resolution

contains

    subroutine render_polar_boundary(backend, center_x, center_y, radius, &
                                     line_width, color)
        !! Render circular boundary for polar plot
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: center_x, center_y, radius
        real(wp), intent(in), optional :: line_width
        real(wp), intent(in), optional :: color(3)

        real(wp) :: angle, x1, y1, x2, y2
        real(wp) :: lw, c(3)
        integer :: i

        lw = 1.0_wp
        if (present(line_width)) lw = line_width
        c = [0.0_wp, 0.0_wp, 0.0_wp]  ! Black default
        if (present(color)) c = color

        call backend%color(c(1), c(2), c(3))
        call backend%set_line_width(lw)
        call backend%set_line_style('-')

        do i = 1, CIRCLE_SEGMENTS
            angle = TWO_PI*real(i - 1, wp)/real(CIRCLE_SEGMENTS, wp)
            x1 = center_x + radius*cos(angle)
            y1 = center_y + radius*sin(angle)

            angle = TWO_PI*real(i, wp)/real(CIRCLE_SEGMENTS, wp)
            x2 = center_x + radius*cos(angle)
            y2 = center_y + radius*sin(angle)

            call backend%line(x1, y1, x2, y2)
        end do
    end subroutine render_polar_boundary

    subroutine render_polar_radial_gridlines(backend, center_x, center_y, &
                                             radius, n_spokes, &
                                             theta_offset, clockwise, &
                                             color, alpha)
        !! Render radial gridlines (spokes) from center to boundary
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: center_x, center_y, radius
        integer, intent(in) :: n_spokes
        real(wp), intent(in) :: theta_offset
        logical, intent(in) :: clockwise
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: alpha

        real(wp) :: theta, x_end, y_end
        real(wp) :: c(3), step
        integer :: i

        if (n_spokes <= 0) return

        c = [0.7_wp, 0.7_wp, 0.7_wp]  ! Gray default
        if (present(color)) c = color

        call backend%color(c(1), c(2), c(3))
        call backend%set_line_style(':')
        call backend%set_line_width(0.5_wp)

        step = TWO_PI/real(n_spokes, wp)
        do i = 1, n_spokes
            theta = real(i - 1, wp)*step
            call polar_to_cartesian(theta, radius, x_end, y_end, &
                                    theta_offset, clockwise)
            x_end = center_x + x_end
            y_end = center_y + y_end
            call backend%line(center_x, center_y, x_end, y_end)
        end do

        call backend%set_line_style('-')
    end subroutine render_polar_radial_gridlines

    subroutine render_polar_angular_gridlines(backend, center_x, center_y, &
                                              r_max, n_circles, &
                                              color, alpha)
        !! Render angular gridlines (concentric circles)
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: center_x, center_y, r_max
        integer, intent(in) :: n_circles
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: alpha

        real(wp) :: r_step, r_current
        real(wp) :: c(3)
        integer :: i

        if (n_circles <= 0) return

        c = [0.7_wp, 0.7_wp, 0.7_wp]  ! Gray default
        if (present(color)) c = color

        call backend%color(c(1), c(2), c(3))
        call backend%set_line_style(':')
        call backend%set_line_width(0.5_wp)

        r_step = r_max/real(n_circles + 1, wp)
        do i = 1, n_circles
            r_current = r_step*real(i, wp)
            call render_circle(backend, center_x, center_y, r_current)
        end do

        call backend%set_line_style('-')
    end subroutine render_polar_angular_gridlines

    subroutine render_circle(backend, cx, cy, radius)
        !! Helper: render a circle as line segments
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: cx, cy, radius

        real(wp) :: angle, x1, y1, x2, y2
        integer :: i

        do i = 1, CIRCLE_SEGMENTS
            angle = TWO_PI*real(i - 1, wp)/real(CIRCLE_SEGMENTS, wp)
            x1 = cx + radius*cos(angle)
            y1 = cy + radius*sin(angle)

            angle = TWO_PI*real(i, wp)/real(CIRCLE_SEGMENTS, wp)
            x2 = cx + radius*cos(angle)
            y2 = cy + radius*sin(angle)

            call backend%line(x1, y1, x2, y2)
        end do
    end subroutine render_circle

    subroutine render_polar_angular_ticks(backend, center_x, center_y, radius, &
                                          n_ticks, theta_offset, clockwise, &
                                          label_offset)
        !! Render angular tick labels around the polar plot boundary
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: center_x, center_y, radius
        integer, intent(in) :: n_ticks
        real(wp), intent(in) :: theta_offset
        logical, intent(in) :: clockwise
        real(wp), intent(in), optional :: label_offset

        real(wp) :: theta, x_label, y_label, label_r
        real(wp) :: tick_angles(36)
        character(len=8) :: tick_labels(36)
        integer :: i, n

        if (n_ticks <= 0) return

        n = min(n_ticks, 36)
        call compute_angular_ticks(n, tick_angles(1:n), tick_labels(1:n))

        label_r = radius*1.12_wp
        if (present(label_offset)) label_r = radius + label_offset

        call backend%color(0.0_wp, 0.0_wp, 0.0_wp)

        do i = 1, n
            call polar_to_cartesian(tick_angles(i), label_r, x_label, y_label, &
                                    theta_offset, clockwise)
            x_label = center_x + x_label
            y_label = center_y + y_label
            call backend%text(x_label, y_label, trim(tick_labels(i)))
        end do
    end subroutine render_polar_angular_ticks

    subroutine render_polar_data(backend, theta, r, n, center_x, center_y, &
                                 r_scale, theta_offset, clockwise, color)
        !! Render polar data as connected line segments
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: theta(:), r(:)
        integer, intent(in) :: n
        real(wp), intent(in) :: center_x, center_y, r_scale
        real(wp), intent(in) :: theta_offset
        logical, intent(in) :: clockwise
        real(wp), intent(in), optional :: color(3)

        real(wp) :: x1, y1, x2, y2, r_scaled
        real(wp) :: c(3)
        integer :: i

        if (n < 2) return

        c = [0.0_wp, 0.447_wp, 0.698_wp]  ! Default blue
        if (present(color)) c = color

        call backend%color(c(1), c(2), c(3))
        call backend%set_line_width(1.5_wp)
        call backend%set_line_style('-')

        r_scaled = r(1)*r_scale
        call polar_to_cartesian(theta(1), r_scaled, x1, y1, theta_offset, &
                                clockwise)
        x1 = center_x + x1
        y1 = center_y + y1

        do i = 2, n
            r_scaled = r(i)*r_scale
            call polar_to_cartesian(theta(i), r_scaled, x2, y2, theta_offset, &
                                    clockwise)
            x2 = center_x + x2
            y2 = center_y + y2

            call backend%line(x1, y1, x2, y2)

            x1 = x2
            y1 = y2
        end do
    end subroutine render_polar_data

end module fortplot_polar_rendering
