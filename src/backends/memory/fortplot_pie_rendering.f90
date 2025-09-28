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

        integer :: slice_count, seg_count, i, j, required_size
        real(wp) :: angle_start, angle_end, angle_span, mid_angle
        real(wp) :: step_angle, angle_a, angle_b
        real(wp) :: center_x, center_y, radius, offset_value
        real(wp) :: center_x_t, center_y_t
        real(wp) :: x_quad(4), y_quad(4)
        real(wp) :: x_a, y_a, x_b, y_b
        real(wp) :: x_a_t, y_a_t, x_b_t, y_b_t
        real(wp), allocatable :: edge_x(:), edge_y(:)
        real(wp), parameter :: PI = acos(-1.0_wp)
        real(wp), parameter :: MIN_SEGMENT_ANGLE = 5.0_wp * PI / 180.0_wp
        real(wp) :: edge_color(3)

        slice_count = plot_data%pie_slice_count
        if (slice_count <= 0) return
        if (.not. allocated(plot_data%pie_start)) return
        if (.not. allocated(plot_data%pie_end)) return
        if (.not. allocated(plot_data%pie_colors)) return

        radius = plot_data%pie_radius
        edge_color = [0.1_wp, 0.1_wp, 0.1_wp]

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
            required_size = seg_count + 1
            call ensure_edge_capacity(edge_x, required_size)
            call ensure_edge_capacity(edge_y, required_size)

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

                edge_x(j + 1) = x_a_t
                edge_y(j + 1) = y_a_t

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

            x_b = center_x + radius * cos(angle_end)
            y_b = center_y + radius * sin(angle_end)
            edge_x(seg_count + 1) = apply_scale_transform(x_b, xscale, symlog_threshold)
            edge_y(seg_count + 1) = apply_scale_transform(y_b, yscale, symlog_threshold)

            ! Draw only the outer arc edge, not the radial lines
            call backend%color(edge_color(1), edge_color(2), edge_color(3))
            call backend%set_line_width(1.0_wp)
            ! Skip radial lines to prevent hairline artifacts in PDF
            ! Only draw the arc outline
            do j = 1, seg_count
                call backend%line(edge_x(j), edge_y(j), edge_x(j + 1), edge_y(j + 1))
            end do
        end do
    end subroutine render_pie_plot

    subroutine ensure_edge_capacity(buffer, required)
        !! Ensure that the working edge buffer has at least the required size
        real(wp), allocatable, intent(inout) :: buffer(:)
        integer, intent(in) :: required
        real(wp), allocatable :: tmp(:)

        if (required <= 0) then
            if (allocated(buffer)) call move_alloc(buffer, tmp)
            return
        end if

        if (.not. allocated(buffer)) then
            allocate(buffer(required))
        else if (size(buffer) < required) then
            allocate(tmp(required))
            call move_alloc(tmp, buffer)
        end if
    end subroutine ensure_edge_capacity

end module fortplot_pie_rendering
