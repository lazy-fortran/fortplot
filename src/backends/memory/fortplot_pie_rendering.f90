module fortplot_pie_rendering
    !! Pie chart rendering helpers for polygon backends

    use iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_scales, only: apply_scale_transform
    implicit none

    private
    public :: render_pie_plot

contains

    subroutine render_pie_plot(backend, plot_data, xscale, yscale, symlog_threshold)
        !! Render pie slices using triangle fans per wedge
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: slice_count, seg_count, i, j
        real(wp) :: angle_start, angle_end, angle_span, mid_angle
        real(wp) :: step_angle, angle_a, angle_b
        real(wp) :: center_x, center_y, radius, offset_value
        real(wp) :: center_x_t, center_y_t
        real(wp) :: x_quad(4), y_quad(4)
        real(wp) :: x_a, y_a, x_b, y_b
        real(wp) :: x_a_t, y_a_t, x_b_t, y_b_t
        real(wp), parameter :: PI = acos(-1.0_wp)
        real(wp), parameter :: MIN_SEGMENT_ANGLE = 5.0_wp * PI / 180.0_wp

        slice_count = plot_data%pie_slice_count
        if (slice_count <= 0) return
        if (.not. allocated(plot_data%pie_start)) return
        if (.not. allocated(plot_data%pie_end)) return
        if (.not. allocated(plot_data%pie_colors)) return

        radius = plot_data%pie_radius

        do i = 1, slice_count
            angle_start = plot_data%pie_start(i)
            angle_end = plot_data%pie_end(i)
            angle_span = angle_end - angle_start
            if (abs(angle_span) < 1.0e-9_wp) cycle

            mid_angle = angle_start + 0.5_wp * angle_span
            offset_value = 0.0_wp
            if (allocated(plot_data%pie_offsets)) then
                if (size(plot_data%pie_offsets) >= i) then
                    offset_value = plot_data%pie_offsets(i)
                end if
            end if

            center_x = plot_data%pie_center(1) + offset_value * cos(mid_angle)
            center_y = plot_data%pie_center(2) + offset_value * sin(mid_angle)
            center_x_t = apply_scale_transform(center_x, xscale, symlog_threshold)
            center_y_t = apply_scale_transform(center_y, yscale, symlog_threshold)

            seg_count = max(8, int(abs(angle_span) / MIN_SEGMENT_ANGLE) + 1)

            call backend%color(plot_data%pie_colors(1, i), plot_data%pie_colors(2, i), &
                               plot_data%pie_colors(3, i))

            step_angle = angle_span / real(seg_count, wp)
            do j = 0, seg_count - 1
                angle_a = angle_start + real(j, wp) * step_angle
                angle_b = angle_a + step_angle

                x_a = center_x + radius * cos(angle_a)
                y_a = center_y + radius * sin(angle_a)
                x_b = center_x + radius * cos(angle_b)
                y_b = center_y + radius * sin(angle_b)

                x_a_t = apply_scale_transform(x_a, xscale, symlog_threshold)
                y_a_t = apply_scale_transform(y_a, yscale, symlog_threshold)
                x_b_t = apply_scale_transform(x_b, xscale, symlog_threshold)
                y_b_t = apply_scale_transform(y_b, yscale, symlog_threshold)

                x_quad(1) = center_x_t
                y_quad(1) = center_y_t
                x_quad(2) = x_a_t
                y_quad(2) = y_a_t
                x_quad(3) = x_b_t
                y_quad(3) = y_b_t
                x_quad(4) = center_x_t
                y_quad(4) = center_y_t

                call backend%fill_quad(x_quad, y_quad)
            end do

            ! matplotlib default pie wedges use edgecolor 'none': fill only,
            ! no outline stroke around the slices.
        end do
    end subroutine render_pie_plot

end module fortplot_pie_rendering
