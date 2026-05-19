module fortplot_figure_data_ranges_specialized
    !! Specialized plot-type range processors
    !! Extracted from fortplot_figure_data_ranges for size compliance (refs #1694)

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: process_line_plot_ranges, process_fill_between_ranges
    public :: process_pie_ranges, process_contour_plot_ranges
    public :: process_pcolormesh_ranges, process_boxplot_ranges
    public :: process_errorbar_ranges, process_bar_plot_ranges

contains

    subroutine process_line_plot_ranges(plot, first_plot, has_valid_data, &
                                        x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process line plot data to calculate ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data

        if (allocated(plot%x) .and. allocated(plot%y)) then
            if (size(plot%x) > 0 .and. size(plot%y) > 0) then
                if (first_plot) then
                    x_min_data = minval(plot%x)
                    x_max_data = maxval(plot%x)
                    y_min_data = minval(plot%y)
                    y_max_data = maxval(plot%y)
                    first_plot = .false.
                else
                    x_min_data = min(x_min_data, minval(plot%x))
                    x_max_data = max(x_max_data, maxval(plot%x))
                    y_min_data = min(y_min_data, minval(plot%y))
                    y_max_data = max(y_max_data, maxval(plot%y))
                end if
                has_valid_data = .true.
            end if
        end if
    end subroutine process_line_plot_ranges

    subroutine process_fill_between_ranges(plot, first_plot, has_valid_data, &
                                           x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process fill_between data to calculate ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data

        integer :: n, idx
        real(wp) :: x_val, y_top, y_bottom
        logical :: considered

        if (.not. allocated(plot%fill_between_data%x)) return
        n = size(plot%fill_between_data%x)
        if (n == 0) return

        considered = .false.
        do idx = 1, n
            if (plot%fill_between_data%has_mask) then
                if (.not. plot%fill_between_data%mask(idx)) cycle
            end if

            x_val = plot%fill_between_data%x(idx)
            y_top = plot%fill_between_data%upper(idx)
            y_bottom = plot%fill_between_data%lower(idx)

            if (first_plot .and. .not. considered) then
                x_min_data = x_val
                x_max_data = x_val
                y_min_data = min(y_top, y_bottom)
                y_max_data = max(y_top, y_bottom)
                first_plot = .false.
            else
                x_min_data = min(x_min_data, x_val)
                x_max_data = max(x_max_data, x_val)
                y_min_data = min(y_min_data, min(y_top, y_bottom))
                y_max_data = max(y_max_data, max(y_top, y_bottom))
            end if
            considered = .true.
        end do

        if (considered) has_valid_data = .true.
    end subroutine process_fill_between_ranges

    subroutine process_pie_ranges(plot, first_plot, has_valid_data, x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process pie chart slices to compute axis ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data

        real(wp) :: radius_extent, offset_max
        real(wp) :: cx, cy

        if (plot%pie_slice_count <= 0) return

        radius_extent = plot%pie_radius
        offset_max = 0.0_wp
        if (allocated(plot%pie_offsets)) then
            if (size(plot%pie_offsets) >= plot%pie_slice_count) then
                offset_max = maxval(plot%pie_offsets(1:plot%pie_slice_count))
                offset_max = max(offset_max, 0.0_wp)
            end if
        end if

        radius_extent = radius_extent + offset_max
        if (allocated(plot%pie_labels)) then
            radius_extent = radius_extent + 0.25_wp * plot%pie_radius
        end if

        cx = plot%pie_center(1)
        cy = plot%pie_center(2)

        if (first_plot) then
            x_min_data = cx - radius_extent
            x_max_data = cx + radius_extent
            y_min_data = cy - radius_extent
            y_max_data = cy + radius_extent
            first_plot = .false.
        else
            x_min_data = min(x_min_data, cx - radius_extent)
            x_max_data = max(x_max_data, cx + radius_extent)
            y_min_data = min(y_min_data, cy - radius_extent)
            y_max_data = max(y_max_data, cy + radius_extent)
        end if

        has_valid_data = .true.
    end subroutine process_pie_ranges

    subroutine process_contour_plot_ranges(plot, first_plot, has_valid_data, &
                                           x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process contour plot data to calculate ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data

        if (allocated(plot%x_grid) .and. allocated(plot%y_grid)) then
            if (size(plot%x_grid) > 0 .and. size(plot%y_grid) > 0) then
                if (first_plot) then
                    x_min_data = minval(plot%x_grid)
                    x_max_data = maxval(plot%x_grid)
                    y_min_data = minval(plot%y_grid)
                    y_max_data = maxval(plot%y_grid)
                    first_plot = .false.
                else
                    x_min_data = min(x_min_data, minval(plot%x_grid))
                    x_max_data = max(x_max_data, maxval(plot%x_grid))
                    y_min_data = min(y_min_data, minval(plot%y_grid))
                    y_max_data = max(y_max_data, maxval(plot%y_grid))
                end if
                has_valid_data = .true.
            end if
        end if
    end subroutine process_contour_plot_ranges

    subroutine process_pcolormesh_ranges(plot, first_plot, has_valid_data, &
                                         x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process pcolormesh plot data to calculate ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data

        if (allocated(plot%pcolormesh_data%x_vertices) .and. &
            allocated(plot%pcolormesh_data%y_vertices)) then
            if (size(plot%pcolormesh_data%x_vertices) > 0 .and. &
                size(plot%pcolormesh_data%y_vertices) > 0) then
                if (first_plot) then
                    x_min_data = minval(plot%pcolormesh_data%x_vertices)
                    x_max_data = maxval(plot%pcolormesh_data%x_vertices)
                    y_min_data = minval(plot%pcolormesh_data%y_vertices)
                    y_max_data = maxval(plot%pcolormesh_data%y_vertices)
                    first_plot = .false.
                else
                    x_min_data = min(x_min_data, minval(plot%pcolormesh_data%x_vertices))
                    x_max_data = max(x_max_data, maxval(plot%pcolormesh_data%x_vertices))
                    y_min_data = min(y_min_data, minval(plot%pcolormesh_data%y_vertices))
                    y_max_data = max(y_max_data, maxval(plot%pcolormesh_data%y_vertices))
                end if
                has_valid_data = .true.
            end if
        end if
    end subroutine process_pcolormesh_ranges

    subroutine process_boxplot_ranges(plot, first_plot, has_valid_data, &
                                      x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process box plot data to calculate ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data

        real(wp) :: data_min, data_max
        real(wp) :: pos, halfw
        logical :: horiz

        if (.not. allocated(plot%box_data)) return
        if (size(plot%box_data) == 0) return

        data_min = minval(plot%box_data)
        data_max = maxval(plot%box_data)
        pos = plot%position
        halfw = 0.5_wp * plot%width
        horiz = plot%horizontal

        if (.not. horiz) then
            if (first_plot) then
                x_min_data = pos - halfw - 0.2_wp
                x_max_data = pos + halfw + 0.2_wp
                y_min_data = data_min - 0.1_wp * abs(data_max - data_min)
                y_max_data = data_max + 0.1_wp * abs(data_max - data_min)
                first_plot = .false.
            else
                x_min_data = min(x_min_data, pos - halfw - 0.2_wp)
                x_max_data = max(x_max_data, pos + halfw + 0.2_wp)
                y_min_data = min(y_min_data, data_min - 0.1_wp * abs(data_max - data_min))
                y_max_data = max(y_max_data, data_max + 0.1_wp * abs(data_max - data_min))
            end if
        else
            if (first_plot) then
                x_min_data = data_min - 0.1_wp * abs(data_max - data_min)
                x_max_data = data_max + 0.1_wp * abs(data_max - data_min)
                y_min_data = pos - halfw - 0.2_wp
                y_max_data = pos + halfw + 0.2_wp
                first_plot = .false.
            else
                x_min_data = min(x_min_data, data_min - 0.1_wp * abs(data_max - data_min))
                x_max_data = max(x_max_data, data_max + 0.1_wp * abs(data_max - data_min))
                y_min_data = min(y_min_data, pos - halfw - 0.2_wp)
                y_max_data = max(y_max_data, pos + halfw + 0.2_wp)
            end if
        end if
        has_valid_data = .true.
    end subroutine process_boxplot_ranges

    subroutine process_errorbar_ranges(plot, first_plot, has_valid_data, &
                                       x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process errorbar plot data to calculate ranges including error extents
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data

        real(wp) :: xmin, xmax, ymin, ymax
        integer :: n

        if (.not. allocated(plot%x) .or. .not. allocated(plot%y)) return
        if (size(plot%x) == 0 .or. size(plot%y) == 0) return
        n = min(size(plot%x), size(plot%y))

        xmin = minval(plot%x(1:n))
        xmax = maxval(plot%x(1:n))
        ymin = minval(plot%y(1:n))
        ymax = maxval(plot%y(1:n))

        if (plot%has_xerr) then
            if (plot%asymmetric_xerr .and. allocated(plot%xerr_lower) &
                .and. allocated(plot%xerr_upper)) then
                xmin = min(xmin, minval(plot%x(1:n) - plot%xerr_lower(1:n)))
                xmax = max(xmax, maxval(plot%x(1:n) + plot%xerr_upper(1:n)))
            else if (allocated(plot%xerr)) then
                xmin = min(xmin, minval(plot%x(1:n) - plot%xerr(1:n)))
                xmax = max(xmax, maxval(plot%x(1:n) + plot%xerr(1:n)))
            end if
        end if

        if (plot%has_yerr) then
            if (plot%asymmetric_yerr .and. allocated(plot%yerr_lower) &
                .and. allocated(plot%yerr_upper)) then
                ymin = min(ymin, minval(plot%y(1:n) - plot%yerr_lower(1:n)))
                ymax = max(ymax, maxval(plot%y(1:n) + plot%yerr_upper(1:n)))
            else if (allocated(plot%yerr)) then
                ymin = min(ymin, minval(plot%y(1:n) - plot%yerr(1:n)))
                ymax = max(ymax, maxval(plot%y(1:n) + plot%yerr(1:n)))
            end if
        end if

        if (first_plot) then
            x_min_data = xmin; x_max_data = xmax
            y_min_data = ymin; y_max_data = ymax
            first_plot = .false.
        else
            x_min_data = min(x_min_data, xmin)
            x_max_data = max(x_max_data, xmax)
            y_min_data = min(y_min_data, ymin)
            y_max_data = max(y_max_data, ymax)
        end if
        has_valid_data = .true.
    end subroutine process_errorbar_ranges

    subroutine process_bar_plot_ranges(plot, first_plot, has_valid_data, &
                                       x_min_data, x_max_data, &
                                       y_min_data, y_max_data)
        !! Process bar plot data to calculate axis ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data
        real(wp), intent(inout) :: y_min_data, y_max_data

        integer :: n, i
        real(wp), parameter :: DEFAULT_BAR_WIDTH = 0.8_wp
        real(wp), parameter :: BAR_MARGIN = 0.05_wp
        real(wp) :: half_width, effective_width
        real(wp) :: x_min_bar, x_max_bar
        real(wp) :: y_min_bar, y_max_bar
        real(wp) :: left_edge, right_edge
        real(wp) :: lower_edge, upper_edge
        real(wp) :: base_val, top_val
        real(wp) :: range_x, range_y

        if (.not. allocated(plot%bar_x)) return
        if (.not. allocated(plot%bar_heights)) return

        n = min(size(plot%bar_x), size(plot%bar_heights))
        if (n <= 0) return

        effective_width = abs(plot%bar_width)
        if (effective_width <= 0.0_wp) effective_width = DEFAULT_BAR_WIDTH
        half_width = 0.5_wp * effective_width

        x_min_bar = huge(0.0_wp)
        x_max_bar = -huge(0.0_wp)
        y_min_bar = huge(0.0_wp)
        y_max_bar = -huge(0.0_wp)

        do i = 1, n
            if (allocated(plot%bar_bottom) .and. i <= size(plot%bar_bottom)) then
                base_val = plot%bar_bottom(i)
            else
                base_val = 0.0_wp
            end if
            top_val = base_val + plot%bar_heights(i)

            if (plot%bar_horizontal) then
                left_edge = min(base_val, top_val)
                right_edge = max(base_val, top_val)
                lower_edge = plot%bar_x(i) - half_width
                upper_edge = plot%bar_x(i) + half_width
            else
                left_edge = plot%bar_x(i) - half_width
                right_edge = plot%bar_x(i) + half_width
                lower_edge = min(base_val, top_val)
                upper_edge = max(base_val, top_val)
            end if

            x_min_bar = min(x_min_bar, left_edge)
            x_max_bar = max(x_max_bar, right_edge)
            y_min_bar = min(y_min_bar, lower_edge)
            y_max_bar = max(y_max_bar, upper_edge)
        end do

        range_x = x_max_bar - x_min_bar
        range_y = y_max_bar - y_min_bar
        if (range_x > 0.0_wp) then
            x_min_bar = x_min_bar - BAR_MARGIN * range_x
            x_max_bar = x_max_bar + BAR_MARGIN * range_x
        end if
        if (range_y > 0.0_wp) then
            y_min_bar = y_min_bar - BAR_MARGIN * range_y
            y_max_bar = y_max_bar + BAR_MARGIN * range_y
        end if

        if (first_plot) then
            x_min_data = x_min_bar
            x_max_data = x_max_bar
            y_min_data = y_min_bar
            y_max_data = y_max_bar
            first_plot = .false.
        else
            x_min_data = min(x_min_data, x_min_bar)
            x_max_data = max(x_max_data, x_max_bar)
            y_min_data = min(y_min_data, y_min_bar)
            y_max_data = max(y_max_data, y_max_bar)
        end if

        has_valid_data = .true.
    end subroutine process_bar_plot_ranges

end module fortplot_figure_data_ranges_specialized
