module fortplot_figure_data_ranges
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_scales, only: apply_scale_transform, apply_inverse_scale_transform, &
                               clamp_extreme_log_range
    use fortplot_logging, only: log_debug
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_LINE, &
                                    PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                    PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
                                    PLOT_TYPE_BOXPLOT, PLOT_TYPE_ERRORBAR, &
                                    PLOT_TYPE_SURFACE, PLOT_TYPE_PIE, &
                                    PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM, &
                                    PLOT_TYPE_QUIVER
    use fortplot_figure_data_ranges_specialized, only: &
        process_line_plot_ranges, process_fill_between_ranges, &
        process_pie_ranges, process_contour_plot_ranges, &
        process_pcolormesh_ranges, process_boxplot_ranges, &
        process_errorbar_ranges, process_bar_plot_ranges
    implicit none

    private
    public :: calculate_figure_data_ranges
    public :: determine_sticky_edges

contains

    subroutine determine_sticky_edges(plots, plot_count, axis_filter, &
                                      sticky_x_min, sticky_x_max, &
                                      sticky_y_min, sticky_y_max)
        !! Flag axis sides where the generic 5% data-range margin must not be
        !! applied, matching matplotlib's sticky edges. Bars carry their own
        !! margin (with a sticky baseline) in process_bar_plot_ranges; a
        !! pcolormesh and contour grids fill their data extent exactly. In each
        !! case the data range is authoritative and adding margin would float
        !! the artist off the axis (bars) or leave a gap around it (mesh/contour).
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        integer, intent(in), optional :: axis_filter
        logical, intent(out) :: sticky_x_min, sticky_x_max
        logical, intent(out) :: sticky_y_min, sticky_y_max
        integer :: i

        sticky_x_min = .false.
        sticky_x_max = .false.
        sticky_y_min = .false.
        sticky_y_max = .false.

        do i = 1, min(plot_count, size(plots))
            if (present(axis_filter)) then
                if (plots(i)%axis /= axis_filter) cycle
            end if
            if (plots(i)%plot_type == PLOT_TYPE_BAR .or. &
                plots(i)%plot_type == PLOT_TYPE_HISTOGRAM .or. &
                plots(i)%plot_type == PLOT_TYPE_PCOLORMESH .or. &
                plots(i)%plot_type == PLOT_TYPE_CONTOUR) then
                sticky_x_min = .true.
                sticky_x_max = .true.
                sticky_y_min = .true.
                sticky_y_max = .true.
                return
            end if
        end do
    end subroutine determine_sticky_edges

    subroutine calculate_figure_data_ranges(plots, plot_count, xlim_set, ylim_set, &
                                           x_min, x_max, y_min, y_max, &
                                           x_min_transformed, x_max_transformed, &
                                           y_min_transformed, y_max_transformed, &
                                           xscale, yscale, symlog_threshold, &
                                           symlog_base, symlog_linscale, axis_filter)
        !! Calculate overall data ranges for the figure with robust edge case handling
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in), optional :: symlog_base, symlog_linscale
        integer, intent(in), optional :: axis_filter

        real(wp) :: x_min_data, x_max_data, y_min_data, y_max_data
        logical :: first_plot, has_valid_data
        integer :: i
        integer :: filtered_axis
        logical :: use_filter
        logical :: sticky_x_min, sticky_x_max
        logical :: sticky_y_min, sticky_y_max

        use_filter = present(axis_filter)
        if (use_filter) filtered_axis = axis_filter

        call initialize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                    x_min_transformed, x_max_transformed, &
                                    y_min_transformed, y_max_transformed, &
                                    xscale, yscale, symlog_threshold, &
                                    symlog_base, symlog_linscale, &
                                    x_min_data, x_max_data, y_min_data, y_max_data, &
                                    first_plot, has_valid_data)
        if (xlim_set .and. ylim_set) return

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
                call process_line_plot_ranges(plots(i), first_plot, has_valid_data, &
                                              x_min_data, x_max_data, &
                                              y_min_data, y_max_data)

            case (PLOT_TYPE_ERRORBAR)
                call process_errorbar_ranges(plots(i), first_plot, has_valid_data, &
                                              x_min_data, x_max_data, &
                                              y_min_data, y_max_data)

            case (PLOT_TYPE_BAR, PLOT_TYPE_HISTOGRAM)
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

            case (PLOT_TYPE_QUIVER)
                call process_line_plot_ranges(plots(i), first_plot, has_valid_data, &
                                              x_min_data, x_max_data, &
                                              y_min_data, y_max_data)

            end select
        end do

        call apply_single_point_margins(has_valid_data, x_min_data, x_max_data, &
                                        y_min_data, y_max_data)

        if (use_filter) then
            call determine_sticky_edges(plots, plot_count, filtered_axis, &
                                       sticky_x_min, sticky_x_max, &
                                       sticky_y_min, sticky_y_max)
        else
            call determine_sticky_edges(plots, plot_count, &
                                       sticky_x_min=sticky_x_min, &
                                       sticky_x_max=sticky_x_max, &
                                       sticky_y_min=sticky_y_min, &
                                       sticky_y_max=sticky_y_max)
        end if

        call finalize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                  x_min_data, x_max_data, y_min_data, y_max_data, &
                                  x_min_transformed, x_max_transformed, &
                                  y_min_transformed, y_max_transformed, &
                                  xscale, yscale, symlog_threshold, &
                                  symlog_base, symlog_linscale, &
                                  sticky_x_min, sticky_x_max, &
                                  sticky_y_min, sticky_y_max)
    end subroutine calculate_figure_data_ranges

    subroutine initialize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                      x_min_transformed, x_max_transformed, &
                                      y_min_transformed, y_max_transformed, &
                                      xscale, yscale, symlog_threshold, &
                                      symlog_base, symlog_linscale, &
                                      x_min_data, x_max_data, y_min_data, y_max_data, &
                                      first_plot, has_valid_data)
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in), optional :: symlog_base, symlog_linscale
        real(wp), intent(out) :: x_min_data, x_max_data, y_min_data, y_max_data
        logical, intent(out) :: first_plot, has_valid_data

        if (xlim_set .and. ylim_set) then
            x_min_transformed = apply_scale_transform(x_min, xscale, symlog_threshold, &
                                                      base=symlog_base, linscale=symlog_linscale)
            x_max_transformed = apply_scale_transform(x_max, xscale, symlog_threshold, &
                                                      base=symlog_base, linscale=symlog_linscale)
            y_min_transformed = apply_scale_transform(y_min, yscale, symlog_threshold, &
                                                      base=symlog_base, linscale=symlog_linscale)
            y_max_transformed = apply_scale_transform(y_max, yscale, symlog_threshold, &
                                                      base=symlog_base, linscale=symlog_linscale)
            return
        end if

        first_plot = .true.
        has_valid_data = .false.
        x_min_data = 0.0_wp
        x_max_data = 1.0_wp
        y_min_data = 0.0_wp
        y_max_data = 1.0_wp
    end subroutine initialize_data_ranges

    subroutine apply_single_point_margins(has_valid_data, x_min_data, x_max_data, &
                                          y_min_data, y_max_data)
        logical, intent(in) :: has_valid_data
        real(wp), intent(inout) :: x_min_data, x_max_data, y_min_data, y_max_data

        real(wp) :: range_x, range_y, margin_factor
        real(wp) :: machine_precision_threshold

        margin_factor = 0.1_wp
        machine_precision_threshold = 100.0_wp * epsilon(1.0_wp)

        if (has_valid_data) then
            range_x = x_max_data - x_min_data
            range_y = y_max_data - y_min_data

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
        real(wp), intent(inout) :: coord_min, coord_max
        real(wp), intent(in) :: current_range, margin_factor, precision_threshold

        real(wp) :: range_center, expanded_range, absolute_scale
        real(wp) :: minimum_visible_range

        range_center = (coord_min + coord_max) * 0.5_wp
        absolute_scale = max(abs(coord_min), abs(coord_max))

        if (absolute_scale < precision_threshold) then
            minimum_visible_range = margin_factor
        else
            minimum_visible_range = absolute_scale * margin_factor
        end if

        if (abs(current_range) < minimum_visible_range) then
            expanded_range = minimum_visible_range
            coord_min = range_center - expanded_range * 0.5_wp
            coord_max = range_center + expanded_range * 0.5_wp
        end if
    end subroutine expand_precision_range

    subroutine apply_default_axis_margin(coord_min, coord_max, scale_type, &
                                         symlog_threshold, base, linscale, &
                                         sticky_min, sticky_max)
        real(wp), intent(inout) :: coord_min, coord_max
        character(len=*), intent(in) :: scale_type
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in), optional :: base, linscale
        logical, intent(in), optional :: sticky_min, sticky_max

        real(wp) :: transformed_min, transformed_max
        real(wp) :: expanded_min, expanded_max
        real(wp) :: clamped_min, clamped_max
        logical :: pin_min, pin_max

        pin_min = .false.
        pin_max = .false.
        if (present(sticky_min)) pin_min = sticky_min
        if (present(sticky_max)) pin_max = sticky_max
        if (coord_max <= coord_min) return

        transformed_min = apply_scale_transform(coord_min, scale_type, symlog_threshold, &
                                                base=base, linscale=linscale)
        transformed_max = apply_scale_transform(coord_max, scale_type, symlog_threshold, &
                                                base=base, linscale=linscale)
        call expand_transformed_range(transformed_min, transformed_max, &
                                      expanded_min, expanded_max, pin_min, pin_max)

        coord_min = apply_inverse_scale_transform(expanded_min, scale_type, symlog_threshold, &
                                                  base=base, linscale=linscale)
        coord_max = apply_inverse_scale_transform(expanded_max, scale_type, symlog_threshold, &
                                                  base=base, linscale=linscale)

        if (trim(scale_type) == 'log') then
            call clamp_extreme_log_range(coord_min, coord_max, clamped_min, clamped_max)
            coord_min = clamped_min
            coord_max = clamped_max
        end if
    end subroutine apply_default_axis_margin

    subroutine expand_transformed_range(data_min, data_max, expanded_min, expanded_max, &
                                        sticky_min, sticky_max)
        real(wp), intent(in) :: data_min, data_max
        real(wp), intent(out) :: expanded_min, expanded_max
        logical, intent(in) :: sticky_min, sticky_max

        real(wp), parameter :: DATA_RANGE_MARGIN = 0.05_wp
        real(wp) :: span

        if (data_max <= data_min) then
            expanded_min = data_min
            expanded_max = data_max
            return
        end if

        span = data_max - data_min
        if (sticky_min) then
            expanded_min = data_min
        else
            expanded_min = data_min - DATA_RANGE_MARGIN * span
        end if
        if (sticky_max) then
            expanded_max = data_max
        else
            expanded_max = data_max + DATA_RANGE_MARGIN * span
        end if
    end subroutine expand_transformed_range

    subroutine finalize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                     x_min_data, x_max_data, y_min_data, y_max_data, &
                                     x_min_transformed, x_max_transformed, &
                                     y_min_transformed, y_max_transformed, &
                                     xscale, yscale, symlog_threshold, &
                                     symlog_base, symlog_linscale, &
                                     sticky_x_min, sticky_x_max, &
                                     sticky_y_min, sticky_y_max)
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: x_min_data, x_max_data, y_min_data, y_max_data
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in), optional :: symlog_base, symlog_linscale
        logical, intent(in), optional :: sticky_x_min, sticky_x_max
        logical, intent(in), optional :: sticky_y_min, sticky_y_max

        real(wp) :: x_clamped_min, x_clamped_max, y_clamped_min, y_clamped_max
        character(len=256) :: msg

        if (.not. xlim_set) then
            x_min = x_min_data
            x_max = x_max_data
        end if

        if (.not. ylim_set) then
            y_min = y_min_data
            y_max = y_max_data
        end if

        if (trim(xscale) == 'log') then
            call clamp_extreme_log_range(x_min, x_max, x_clamped_min, x_clamped_max)
            if (abs(x_clamped_min - x_min) > 1.0e-10_wp .or. &
                abs(x_clamped_max - x_max) > 1.0e-10_wp) then
                write(msg, '(A,A,F12.4,A,F12.4,A,F12.4,A,F12.4)') &
                    'X-axis range clamped for log scale visualization; ', &
                    'original=', x_min, ' to ', x_max, &
                    '; clamped=', x_clamped_min, ' to ', x_clamped_max
                call log_debug(trim(adjustl(msg)))
            end if
            x_min = x_clamped_min
            x_max = x_clamped_max
        end if

        if (trim(yscale) == 'log') then
            call clamp_extreme_log_range(y_min, y_max, y_clamped_min, y_clamped_max)
            if (abs(y_clamped_min - y_min) > 1.0e-10_wp .or. &
                abs(y_clamped_max - y_max) > 1.0e-10_wp) then
                write(msg, '(A,A,F12.4,A,F12.4,A,F12.4,A,F12.4)') &
                    'Y-axis range clamped for log scale visualization; ', &
                    'original=', y_min, ' to ', y_max, &
                    '; clamped=', y_clamped_min, ' to ', y_clamped_max
                call log_debug(trim(adjustl(msg)))
            end if
            y_min = y_clamped_min
            y_max = y_clamped_max
        end if

        if (.not. xlim_set) then
            call apply_default_axis_margin(x_min, x_max, xscale, symlog_threshold, &
                                           symlog_base, symlog_linscale, &
                                           sticky_min=sticky_x_min, &
                                           sticky_max=sticky_x_max)
        end if
        if (.not. ylim_set) then
            call apply_default_axis_margin(y_min, y_max, yscale, symlog_threshold, &
                                           symlog_base, symlog_linscale, &
                                           sticky_min=sticky_y_min, &
                                           sticky_max=sticky_y_max)
        end if

        x_min_transformed = apply_scale_transform(x_min, xscale, symlog_threshold, &
                                                  base=symlog_base, linscale=symlog_linscale)
        x_max_transformed = apply_scale_transform(x_max, xscale, symlog_threshold, &
                                                  base=symlog_base, linscale=symlog_linscale)
        y_min_transformed = apply_scale_transform(y_min, yscale, symlog_threshold, &
                                                  base=symlog_base, linscale=symlog_linscale)
        y_max_transformed = apply_scale_transform(y_max, yscale, symlog_threshold, &
                                                  base=symlog_base, linscale=symlog_linscale)
    end subroutine finalize_data_ranges
end module fortplot_figure_data_ranges
