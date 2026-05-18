module fortplot_figure_data_ranges
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_scales, only: apply_scale_transform, clamp_extreme_log_range
    use fortplot_logging, only: log_debug
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_LINE, &
                                    PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
                                    PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
                                    PLOT_TYPE_BOXPLOT, PLOT_TYPE_ERRORBAR, &
                                    PLOT_TYPE_SURFACE, PLOT_TYPE_PIE, &
                                    PLOT_TYPE_BAR, PLOT_TYPE_QUIVER
    use fortplot_figure_data_ranges_specialized, only: &
        process_line_plot_ranges, process_fill_between_ranges, &
        process_pie_ranges, process_contour_plot_ranges, &
        process_pcolormesh_ranges, process_boxplot_ranges, &
        process_errorbar_ranges, process_bar_plot_ranges
    implicit none

    private
    public :: calculate_figure_data_ranges

contains

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

            case (PLOT_TYPE_QUIVER)
                call process_line_plot_ranges(plots(i), first_plot, has_valid_data, &
                                              x_min_data, x_max_data, &
                                              y_min_data, y_max_data)

            end select
        end do

        call apply_single_point_margins(has_valid_data, x_min_data, x_max_data, &
                                        y_min_data, y_max_data)

      call finalize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                   x_min_data, x_max_data, y_min_data, y_max_data, &
                                   x_min_transformed, x_max_transformed, &
                                   y_min_transformed, y_max_transformed, &
                                   xscale, yscale, symlog_threshold, &
                                   symlog_base, symlog_linscale)
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

    subroutine finalize_data_ranges(xlim_set, ylim_set, x_min, x_max, y_min, y_max, &
                                     x_min_data, x_max_data, y_min_data, y_max_data, &
                                     x_min_transformed, x_max_transformed, &
                                     y_min_transformed, y_max_transformed, &
                                     xscale, yscale, symlog_threshold, &
                                     symlog_base, symlog_linscale)
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: x_min_data, x_max_data, y_min_data, y_max_data
        real(wp), intent(out) :: x_min_transformed, x_max_transformed
        real(wp), intent(out) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in), optional :: symlog_base, symlog_linscale

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
