module fortplot_raster_axes_secondary
    !! Secondary axis and helper functions for raster backend
    !! Extracted from fortplot_raster_axes for size compliance (refs #1694)
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_core, only: raster_image_t
    use fortplot_raster_ticks_secondary, only: &
        raster_draw_y_axis_ticks_right, &
        raster_draw_x_axis_ticks_top
    use fortplot_raster_labels, only: raster_render_ylabel_right, &
                                       raster_draw_top_xlabel
    use fortplot_scales, only: apply_scale_transform
    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
                                         format_tick_value_consistent
    use fortplot_raster_labels, only: &
        compute_ylabel_x_pos_impl => compute_ylabel_x_pos, &
        y_tick_label_right_edge_at_axis_impl => y_tick_label_right_edge_at_axis
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_draw_secondary_y_axis
    public :: raster_draw_secondary_x_axis_top
    public :: compute_non_overlapping_mask_simple
    public :: compute_ylabel_x_pos, compute_ylabel_x_pos_old
    public :: y_tick_label_right_edge_at_axis, y_tick_label_right_edge_at_axis_old

contains

    subroutine raster_draw_secondary_y_axis(raster, width, height, plot_area, yscale, &
                                            symlog_threshold, &
                                            y_min, y_max, ylabel, date_format)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: y_min, y_max
        character(len=:), allocatable, intent(in), optional :: ylabel
        character(len=*), intent(in), optional :: date_format
        character(len=:), allocatable :: ylabel_local
        real(wp) :: y_tick_positions(MAX_TICKS)
        character(len=50) :: tick_labels(MAX_TICKS)
        integer :: tick_colors(3, MAX_TICKS)
        integer :: num_y_ticks, decimals, i

        ylabel_local = ""
        if (present(ylabel)) then
            if (allocated(ylabel)) ylabel_local = ylabel
        end if

        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, &
                                 y_tick_positions, num_y_ticks)

        if (num_y_ticks > 0) then
            decimals = 0
            if (trim(yscale) == 'linear' .and. num_y_ticks >= 2) then
                decimals = determine_decimals_from_ticks(y_tick_positions, num_y_ticks)
            end if

            do i = 1, num_y_ticks
                if (trim(yscale) == 'linear') then
                    tick_labels(i) = &
                        format_tick_value_consistent(y_tick_positions(i), decimals)
                else
                    tick_labels(i) = format_tick_label(y_tick_positions(i), yscale, &
                                                       date_format=date_format, &
                                                       data_min=y_min, data_max=y_max)
                end if
                tick_colors(:, i) = (/0, 0, 0/)
            end do

            call raster_draw_y_axis_ticks_right(raster, width, height, plot_area, &
                                                yscale, symlog_threshold, &
                                                y_tick_positions(1:num_y_ticks), &
                                                tick_labels(1:num_y_ticks), &
                                                tick_colors(:, 1:num_y_ticks), &
                                                y_min, y_max)
        end if

        if (len_trim(ylabel_local) > 0) then
            call raster_render_ylabel_right(raster, width, height, plot_area, &
                                            ylabel_local)
        end if
    end subroutine raster_draw_secondary_y_axis

    subroutine raster_draw_secondary_x_axis_top(raster, width, height, plot_area, &
                                                xscale, symlog_threshold, &
                                                x_min, x_max, xlabel, date_format)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max
        character(len=:), allocatable, intent(in), optional :: xlabel
        character(len=*), intent(in), optional :: date_format
        real(wp) :: x_tick_positions(MAX_TICKS)
        character(len=50) :: tick_labels(MAX_TICKS)
        integer :: tick_colors(3, MAX_TICKS)
        integer :: num_x_ticks, decimals, i

        character(len=:), allocatable :: xlabel_local

        xlabel_local = ""
        if (present(xlabel)) then
            if (allocated(xlabel)) xlabel_local = xlabel
        end if

        call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, &
                                 x_tick_positions, num_x_ticks)

        if (num_x_ticks > 0) then
            decimals = 0
            if (trim(xscale) == 'linear' .and. num_x_ticks >= 2) then
                decimals = determine_decimals_from_ticks(x_tick_positions, num_x_ticks)
            end if

            do i = 1, num_x_ticks
                if (trim(xscale) == 'linear') then
                    tick_labels(i) = &
                        format_tick_value_consistent(x_tick_positions(i), decimals)
                else
                    tick_labels(i) = format_tick_label(x_tick_positions(i), xscale, &
                                                       date_format=date_format, &
                                                       data_min=x_min, data_max=x_max)
                end if
                tick_colors(:, i) = (/0, 0, 0/)
            end do

            call raster_draw_x_axis_ticks_top(raster, width, height, plot_area, &
                                              xscale, symlog_threshold, &
                                              x_tick_positions(1:num_x_ticks), &
                                              tick_labels(1:num_x_ticks), &
                                              tick_colors(:, 1:num_x_ticks), &
                                              x_min, x_max)
        end if
        if (len_trim(xlabel_local) > 0) then
            call raster_draw_top_xlabel(raster, width, height, plot_area, xlabel_local)
        end if
    end subroutine raster_draw_secondary_x_axis_top

    subroutine compute_non_overlapping_mask_simple(centers, widths, min_gap, keep)
        real(wp), contiguous, intent(in) :: centers(:)
        integer, intent(in) :: widths(:)
        real(wp), intent(in) :: min_gap
        logical, intent(out) :: keep(size(centers))
        integer :: n, i
        real(wp) :: last_right, left_i, right_i, gap

        n = size(centers)
        if (size(widths) /= n) then
            keep = .false.
            return
        end if

        keep = .false.
        last_right = -1.0e30_wp
        gap = max(0.0_wp, min_gap)
        do i = 1, n
            left_i = centers(i) - 0.5_wp*real(widths(i), wp)
            right_i = centers(i) + 0.5_wp*real(widths(i), wp)
            if (left_i >= last_right + gap) then
                keep(i) = .true.
                last_right = right_i
            end if
        end do
    end subroutine compute_non_overlapping_mask_simple

    integer function compute_ylabel_x_pos(y_tick_label_edge, rotated_width) &
        result(x_pos)
        integer, intent(in) :: y_tick_label_edge
        integer, intent(in) :: rotated_width
        x_pos = compute_ylabel_x_pos_impl(y_tick_label_edge, rotated_width)
    end function compute_ylabel_x_pos

    integer function compute_ylabel_x_pos_old(plot_area, rotated_width, &
                                              y_tick_max_width) result(x_pos)
        use fortplot_constants, only: TICK_MARK_LENGTH, YLABEL_EXTRA_GAP
        use fortplot_raster_ticks, only: Y_TICK_LABEL_RIGHT_PAD
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: rotated_width, y_tick_max_width
        integer :: clearance, min_left_margin

        clearance = TICK_MARK_LENGTH + Y_TICK_LABEL_RIGHT_PAD + &
                    max(0, y_tick_max_width) + YLABEL_EXTRA_GAP
        x_pos = plot_area%left - clearance - rotated_width

        min_left_margin = max(15, rotated_width/4)
        if (x_pos < min_left_margin) then
            x_pos = min_left_margin
        end if
    end function compute_ylabel_x_pos_old

    integer function y_tick_label_right_edge_at_axis(plot_area, max_width_measured) &
        result(edge)
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: max_width_measured
        edge = y_tick_label_right_edge_at_axis_impl(plot_area, max_width_measured)
    end function y_tick_label_right_edge_at_axis

    integer function y_tick_label_right_edge_at_axis_old(plot_area) result(edge)
        type(plot_area_t), intent(in) :: plot_area
        edge = y_tick_label_right_edge_at_axis_impl(plot_area, 0)
    end function y_tick_label_right_edge_at_axis_old

end module fortplot_raster_axes_secondary
