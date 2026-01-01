module fortplot_marker_rendering
    !! Marker rendering module
    !!
    !! This module handles all marker-based rendering operations including
    !! scatter plot markers and single point markers.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_plot_data
    implicit none

    private
    public :: render_markers
    public :: draw_single_point_marker

contains

    subroutine render_markers(backend, plot_data, x_min_t, x_max_t, y_min_t, y_max_t, &
                              xscale, yscale, symlog_threshold)
        !! Render markers for a plot
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        real(wp) :: x_scaled, y_scaled
        real(wp) :: marker_rgb(3)
        integer :: i

        if (.not. allocated(plot_data%marker)) return
        if (len_trim(plot_data%marker) == 0) return
        if (trim(plot_data%marker) == 'None' .or. trim(plot_data%marker) == &
            'none') return

        ! Validate input data
        if (.not. allocated(plot_data%x) .or. .not. allocated(plot_data%y)) return
        if (size(plot_data%x) == 0 .or. size(plot_data%y) == 0) return
        if (size(plot_data%x) /= size(plot_data%y)) return

        marker_rgb = plot_data%color
        if (plot_data%marker_color_set) marker_rgb = plot_data%marker_color

        call backend%set_marker_colors(marker_rgb(1), marker_rgb(2), marker_rgb(3), &
                                       marker_rgb(1), marker_rgb(2), marker_rgb(3))
        call backend%color(marker_rgb(1), marker_rgb(2), marker_rgb(3))

        do i = 1, size(plot_data%x)
            x_scaled = apply_scale_transform(plot_data%x(i), xscale, symlog_threshold)
            y_scaled = apply_scale_transform(plot_data%y(i), yscale, symlog_threshold)
            call backend%draw_marker(x_scaled, y_scaled, plot_data%marker)
        end do
    end subroutine render_markers

    subroutine draw_single_point_marker(backend, x, y)
        !! Draw a visible marker for a single point
        !! This ensures single points are visible even without explicit markers
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x, y

        real(wp) :: marker_size
        real(wp) :: data_range_x, data_range_y

        ! Calculate marker size based on the current data range to ensure visibility
        ! This makes the marker a reasonable fraction of the plot area
        data_range_x = abs(backend%x_max - backend%x_min)
        data_range_y = abs(backend%y_max - backend%y_min)

        ! Use 1% of the smaller data range as marker size
        marker_size = 0.01_wp*min(data_range_x, data_range_y)

        ! Ensure minimum visibility (in case data range is very small)
        if (marker_size < epsilon(1.0_wp)*1000) then
            marker_size = 0.1_wp*max(data_range_x, data_range_y)
        end if

        ! Draw a small cross or plus sign centered at the point
        ! This works across all backends (PNG, PDF, ASCII)

        ! Horizontal line of the cross
        call backend%line(x - marker_size, y, x + marker_size, y)

        ! Vertical line of the cross
        call backend%line(x, y - marker_size, x, y + marker_size)

    end subroutine draw_single_point_marker

end module fortplot_marker_rendering
