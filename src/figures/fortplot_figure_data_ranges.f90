module fortplot_figure_data_ranges
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_scales, only: apply_scale_transform, clamp_extreme_log_range
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_LINE, &
                                  PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                  PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
                                  PLOT_TYPE_BOXPLOT, PLOT_TYPE_ERRORBAR, &
                                  PLOT_TYPE_SURFACE, PLOT_TYPE_PIE, &
                                  PLOT_TYPE_BAR
    implicit none

    private
    public :: calculate_figure_data_ranges

contains

    subroutine calculate_figure_data_ranges(plots, plot_count, xlim_set, ylim_set, &
                                          x_min, x_max, y_min, y_max, &
                                          x_min_transformed, x_max_transformed, &
                                          y_min_transformed, y_max_transformed, &
                                          xscale, yscale, symlog_threshold, axis_filter)
        !! Calculate overall data ranges for the figure with robust edge case handling
        !! Fixed Issue #432: Handles zero-size arrays and single points properly
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer, intent(in), optional :: axis_filter

        real(wp) :: x_min_data, x_max_data, y_min_data, y_max_data
        logical :: first_plot, has_valid_data
        integer :: i
        integer :: filtered_axis
        logical :: use_filter

        use_filter = present(axis_filter)
        if (use_filter) filtered_axis = axis_filter
        
        ! Initialize data ranges and check for early return
        call initialize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                   x_min_transformed, x_max_transformed, &
                                   y_min_transformed, y_max_transformed, &
                                   xscale, yscale, symlog_threshold, &
                                   x_min_data, x_max_data, y_min_data, y_max_data, &
                                   first_plot, has_valid_data)
        if (xlim_set .and. ylim_set) return
        
        ! Process all plots to calculate data ranges
        do i = 1, plot_count
            if (use_filter) then
                if (plots(i)%axis /= filtered_axis) cycle
            end if
            select case (plots(i)%plot_type)
            case (PLOT_TYPE_LINE)
                call process_line_plot_ranges(plots(i), first_plot, has_valid_data, &
                                             x_min_data, x_max_data, &
                                             y_min_data, y_max_data)
                
            case (PLOT_TYPE_SCATTER)
                ! Scatter uses same x/y range computation as line plots
                call process_line_plot_ranges(plots(i), first_plot, has_valid_data, &
                                             x_min_data, x_max_data, &
                                             y_min_data, y_max_data)

            case (PLOT_TYPE_ERRORBAR)
                call process_errorbar_ranges(plots(i), first_plot, has_valid_data, &
                                             x_min_data, x_max_data, &
                                             y_min_data, y_max_data)

            case (PLOT_TYPE_BAR)
                call process_bar_plot_ranges(plots(i), first_plot, has_valid_data, &
                                             x_min_data, x_max_data, &
                                             y_min_data, y_max_data)

            case (PLOT_TYPE_FILL)
                call process_fill_between_ranges(plots(i), first_plot, has_valid_data, &
                                                x_min_data, x_max_data, &
                                                y_min_data, y_max_data)

            case (PLOT_TYPE_PIE)
                call process_pie_ranges(plots(i), first_plot, has_valid_data, &
                                        x_min_data, x_max_data, y_min_data, y_max_data)

            case (PLOT_TYPE_CONTOUR)
                call process_contour_plot_ranges(plots(i), first_plot, has_valid_data, &
                                                x_min_data, x_max_data, &
                                                y_min_data, y_max_data)

            case (PLOT_TYPE_SURFACE)
                call process_contour_plot_ranges(plots(i), first_plot, has_valid_data, &
                                                x_min_data, x_max_data, &
                                                y_min_data, y_max_data)
                
            case (PLOT_TYPE_PCOLORMESH)
                call process_pcolormesh_ranges(plots(i), first_plot, has_valid_data, &
                                              x_min_data, x_max_data, &
                                              y_min_data, y_max_data)
            
            case (PLOT_TYPE_BOXPLOT)
                call process_boxplot_ranges(plots(i), first_plot, has_valid_data, &
                                            x_min_data, x_max_data, &
                                            y_min_data, y_max_data)
                                              
            end select
        end do
        
        ! Apply single point margins if needed
        call apply_single_point_margins(has_valid_data, x_min_data, x_max_data, &
                                       y_min_data, y_max_data)
        
        ! Finalize data ranges with user limits and transformations
        call finalize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                 x_min_data, x_max_data, y_min_data, y_max_data, &
                                 x_min_transformed, x_max_transformed, &
                                 y_min_transformed, y_max_transformed, &
                                 xscale, yscale, symlog_threshold)
    end subroutine calculate_figure_data_ranges
    
    subroutine initialize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                     x_min_transformed, x_max_transformed, &
                                     y_min_transformed, y_max_transformed, &
                                     xscale, yscale, symlog_threshold, &
                                     x_min_data, x_max_data, y_min_data, y_max_data, &
                                     first_plot, has_valid_data)
        !! Initialize data ranges and handle early return case
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(out) :: x_min_data, x_max_data, y_min_data, y_max_data
        logical, intent(out) :: first_plot, has_valid_data
        
        if (xlim_set .and. ylim_set) then
            x_min_transformed = apply_scale_transform(x_min, xscale, symlog_threshold)
            x_max_transformed = apply_scale_transform(x_max, xscale, symlog_threshold)
            y_min_transformed = apply_scale_transform(y_min, yscale, symlog_threshold)
            y_max_transformed = apply_scale_transform(y_max, yscale, symlog_threshold)
            return
        end if
        
        first_plot = .true.
        has_valid_data = .false.
        
        ! Initialize with safe default ranges for empty data case
        x_min_data = 0.0_wp
        x_max_data = 1.0_wp
        y_min_data = 0.0_wp
        y_max_data = 1.0_wp
    end subroutine initialize_data_ranges
    
    subroutine process_line_plot_ranges(plot, first_plot, has_valid_data, &
                                       x_min_data, x_max_data, y_min_data, y_max_data)
        !! Process line plot data to calculate ranges
        type(plot_data_t), intent(in) :: plot
        logical, intent(inout) :: first_plot, has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data
        
        if (allocated(plot%x) .and. allocated(plot%y)) then
            ! CRITICAL FIX: Check for non-empty arrays before minval/maxval
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
            ! Vertical box: x-range around position, y-range from data
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
            ! Horizontal box: swap axes
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
        real(wp) :: half_width, effective_width
        real(wp) :: x_min_bar, x_max_bar
        real(wp) :: y_min_bar, y_max_bar
        real(wp) :: left_edge, right_edge
        real(wp) :: lower_edge, upper_edge
        real(wp) :: base_val, top_val

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
            ! Get base offset (bottom for vertical, left for horizontal)
            if (allocated(plot%bar_bottom) .and. i <= size(plot%bar_bottom)) then
                base_val = plot%bar_bottom(i)
            else
                base_val = 0.0_wp
            end if
            top_val = base_val + plot%bar_heights(i)

            if (plot%bar_horizontal) then
                ! Horizontal bars: base_val is left edge, top_val is right edge
                left_edge = min(base_val, top_val)
                right_edge = max(base_val, top_val)
                lower_edge = plot%bar_x(i) - half_width
                upper_edge = plot%bar_x(i) + half_width
            else
                ! Vertical bars: base_val is bottom, top_val is top
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

    subroutine apply_single_point_margins(has_valid_data, x_min_data, x_max_data, &
                                         y_min_data, y_max_data)
        !! Apply margins for single point case and machine precision ranges (Issue #435)
        !! 
        !! Enhanced to handle machine precision coordinate boundaries where ranges
        !! are extremely small but not exactly zero, preventing coordinate
        !! transformation failures during normalization.
        logical, intent(in) :: has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data
        
        real(wp) :: range_x, range_y, margin_factor
        real(wp) :: machine_precision_threshold
        
        ! Default margin for single points and empty data (10% of unit range)
        margin_factor = 0.1_wp
        
        ! Machine precision threshold: 100x epsilon for robust detection
        ! This catches ranges that are effectively at machine precision limits
        machine_precision_threshold = 100.0_wp * epsilon(1.0_wp)
        
        ! CRITICAL FIX: Handle both zero range and machine precision range cases
        if (has_valid_data) then
            range_x = x_max_data - x_min_data
            range_y = y_max_data - y_min_data
            
            ! Enhanced range detection: catch both zero and machine precision ranges
            if (abs(range_x) < 1.0e-10_wp .or. &
                abs(range_x) < machine_precision_threshold) then
                
                call expand_precision_range(x_min_data, x_max_data, range_x, &
                                          margin_factor, machine_precision_threshold)
            end if
            
            if (abs(range_y) < 1.0e-10_wp .or. &
                abs(range_y) < machine_precision_threshold) then
                
                call expand_precision_range(y_min_data, y_max_data, range_y, &
                                          margin_factor, machine_precision_threshold)
            end if
        end if
    end subroutine apply_single_point_margins
    
    subroutine expand_precision_range(coord_min, coord_max, current_range, &
                                    margin_factor, precision_threshold)
        !! Expand coordinate range for machine precision boundaries (Issue #435)
        !! 
        !! Intelligently expands coordinate ranges that are at machine precision
        !! scale to ensure proper coordinate transformation and visualization
        real(wp), intent(inout) :: coord_min, coord_max
        real(wp), intent(in) :: current_range, margin_factor, precision_threshold
        
        real(wp) :: range_center, expanded_range, absolute_scale
        real(wp) :: minimum_visible_range
        
        ! Calculate range properties
        range_center = (coord_min + coord_max) * 0.5_wp
        absolute_scale = max(abs(coord_min), abs(coord_max))
        
        ! Determine minimum visible range based on coordinate scale
        if (absolute_scale < precision_threshold) then
            ! Near-zero coordinates: use absolute minimum range
            minimum_visible_range = margin_factor
        else
            ! Non-zero coordinates: use relative minimum range
            minimum_visible_range = absolute_scale * margin_factor
        end if
        
        ! Ensure the range is at least the minimum visible range
        if (abs(current_range) < minimum_visible_range) then
            expanded_range = minimum_visible_range
            coord_min = range_center - expanded_range * 0.5_wp
            coord_max = range_center + expanded_range * 0.5_wp
        end if
    end subroutine expand_precision_range
    
    subroutine finalize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                   x_min_data, x_max_data, y_min_data, y_max_data, &
                                   x_min_transformed, x_max_transformed, &
                                   y_min_transformed, y_max_transformed, &
                                   xscale, yscale, symlog_threshold)
        !! Apply user limits and scale transformations with extreme value protection
        !! Fixed Issue #433: Added range clamping for extreme numeric values
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: x_min_data, x_max_data, y_min_data, y_max_data
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        real(wp) :: x_clamped_min, x_clamped_max, y_clamped_min, y_clamped_max
        
        ! Apply user-specified limits or use calculated data ranges
        if (.not. xlim_set) then
            x_min = x_min_data
            x_max = x_max_data
        end if
        
        if (.not. ylim_set) then
            y_min = y_min_data
            y_max = y_max_data
        end if
        
        ! Apply extreme value clamping for log scales to prevent precision loss
        if (trim(xscale) == 'log') then
            call clamp_extreme_log_range(x_min, x_max, x_clamped_min, x_clamped_max)
            if (abs(x_clamped_min - x_min) > 1.0e-10_wp .or. &
                abs(x_clamped_max - x_max) > 1.0e-10_wp) then
                print *, "Info: X-axis range clamped for log scale visualization"
                print *, "      Original:", x_min, "to", x_max
                print *, "      Clamped: ", x_clamped_min, "to", x_clamped_max
            end if
            x_min = x_clamped_min
            x_max = x_clamped_max
        end if
        
        if (trim(yscale) == 'log') then
            call clamp_extreme_log_range(y_min, y_max, y_clamped_min, y_clamped_max)
            if (abs(y_clamped_min - y_min) > 1.0e-10_wp .or. &
                abs(y_clamped_max - y_max) > 1.0e-10_wp) then
                print *, "Info: Y-axis range clamped for log scale visualization"
                print *, "      Original:", y_min, "to", y_max
                print *, "      Clamped: ", y_clamped_min, "to", y_clamped_max
            end if
            y_min = y_clamped_min
            y_max = y_clamped_max
        end if
        
        ! Apply scale transformations
        x_min_transformed = apply_scale_transform(x_min, xscale, symlog_threshold)
        x_max_transformed = apply_scale_transform(x_max, xscale, symlog_threshold)
        y_min_transformed = apply_scale_transform(y_min, yscale, symlog_threshold)
        y_max_transformed = apply_scale_transform(y_max, yscale, symlog_threshold)
    end subroutine finalize_data_ranges
end module fortplot_figure_data_ranges
