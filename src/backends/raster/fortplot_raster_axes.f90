module fortplot_raster_axes
    !! Raster axes coordination module (refactored for size compliance)
    !! Orchestrates tick and label rendering through specialized modules
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_line_styles, only: draw_styled_line
    use fortplot_raster_core, only: raster_image_t
    use fortplot_raster_ticks
    use fortplot_raster_labels, only: raster_draw_axis_labels, raster_render_ylabel, &
                                      raster_render_ylabel_right, &
                                      raster_draw_top_xlabel, &
                                      render_title_centered, compute_title_position, &
                                      compute_ylabel_x_pos_impl => &
                                      compute_ylabel_x_pos, &
                                      y_tick_label_right_edge_at_axis_impl => &
                                      y_tick_label_right_edge_at_axis
    use fortplot_scales, only: apply_scale_transform
    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
                                         format_tick_value_consistent
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_unicode, only: escape_unicode_for_raster
    use fortplot_text, only: calculate_text_width, render_text_to_image
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private

    ! Primary public interfaces
    public :: raster_draw_axes_and_labels
    public :: raster_draw_axes_lines_and_ticks
    public :: raster_draw_axis_labels_only
    public :: map_value_to_plot_x, map_value_to_plot_y

    ! Re-exports for backward compatibility
    public :: raster_render_ylabel
    public :: compute_title_position
    public :: compute_non_overlapping_mask, compute_non_overlapping_mask_simple
    public :: compute_ylabel_x_pos, compute_ylabel_x_pos_old
    public :: y_tick_label_right_edge_at_axis, y_tick_label_right_edge_at_axis_old
    public :: raster_draw_x_axis_ticks
    public :: raster_draw_y_axis_ticks
    public :: raster_draw_x_axis_tick_marks_only
    public :: raster_draw_y_axis_tick_marks_only
    public :: raster_draw_x_axis_tick_labels_only
    public :: raster_draw_y_axis_tick_labels_only
    public :: raster_draw_secondary_y_axis
    public :: raster_draw_secondary_x_axis_top
    public :: raster_draw_x_minor_ticks
    public :: raster_draw_y_minor_ticks

contains

    real(wp) function map_value_to_plot_x(value, data_min, data_max, plot_area, scale, &
                                          symlog_threshold) result(px)
        !! Map a data value to pixel X coordinate using axis scale
        real(wp), intent(in) :: value, data_min, data_max
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: scale
        real(wp), intent(in) :: symlog_threshold
        real(wp) :: v_t, min_t, max_t

        min_t = apply_scale_transform(data_min, scale, symlog_threshold)
        max_t = apply_scale_transform(data_max, scale, symlog_threshold)
        v_t = apply_scale_transform(value, scale, symlog_threshold)

        if (max_t > min_t) then
            px = real(plot_area%left, wp) + &
                 (v_t - min_t)/(max_t - min_t)*real(plot_area%width, wp)
        else
            px = real(plot_area%left, wp) + 0.5_wp*real(plot_area%width, wp)
        end if
    end function map_value_to_plot_x

    real(wp) function map_value_to_plot_y(value, data_min, data_max, plot_area, scale, &
                                          symlog_threshold) result(py)
        !! Map a data value to pixel Y coordinate using axis scale
        !! Raster coordinates have Y increasing downward; account for that here.
        real(wp), intent(in) :: value, data_min, data_max
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: scale
        real(wp), intent(in) :: symlog_threshold
        real(wp) :: v_t, min_t, max_t

        min_t = apply_scale_transform(data_min, scale, symlog_threshold)
        max_t = apply_scale_transform(data_max, scale, symlog_threshold)
        v_t = apply_scale_transform(value, scale, symlog_threshold)

        if (max_t > min_t) then
            py = real(plot_area%bottom + plot_area%height, wp) - &
                 (v_t - min_t)/(max_t - min_t)*real(plot_area%height, wp)
        else
            py = real(plot_area%bottom, wp) + 0.5_wp*real(plot_area%height, wp)
        end if
    end function map_value_to_plot_y

    subroutine raster_draw_axes_and_labels(raster, width, height, plot_area, &
                                           xscale, yscale, symlog_threshold, &
                                           x_min, x_max, y_min, y_max, &
                                           title, xlabel, ylabel)
        !! Draw axes and labels for raster backends
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel

        ! Draw main axes lines
        call draw_raster_axes_lines(raster, width, height, plot_area, x_min, x_max, &
                                    y_min, y_max)

        ! Draw tick marks and labels
        call raster_draw_x_axis_ticks_wrapper(raster, width, height, plot_area, &
                                              xscale, symlog_threshold, &
                                              x_min, x_max, y_min, y_max)
        call raster_draw_y_axis_ticks_wrapper(raster, width, height, plot_area, &
                                              yscale, symlog_threshold, &
                                              x_min, x_max, y_min, y_max)

        ! Draw labels and title
        call raster_draw_axis_labels_wrapper(raster, width, height, plot_area, title, &
                                             xlabel, ylabel)
    end subroutine raster_draw_axes_and_labels

    subroutine raster_draw_axes_lines_and_ticks(raster, width, height, plot_area, &
                                                xscale, yscale, symlog_threshold, &
                                                x_min, x_max, y_min, y_max)
        !! Draw axes lines and tick marks WITHOUT labels (for proper drawing order)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        ! Draw main axes lines
        call draw_raster_axes_lines(raster, width, height, plot_area, x_min, x_max, &
                                    y_min, y_max)

        ! Draw tick marks WITHOUT labels (delegate to ticks module via wrapper)
        call raster_draw_x_axis_tick_marks_only_wrapper(raster, width, height, &
                                                        plot_area, xscale, &
                                                        symlog_threshold, &
                                                        x_min, x_max, y_min, y_max)
        call raster_draw_y_axis_tick_marks_only_wrapper(raster, width, height, &
                                                        plot_area, yscale, &
                                                        symlog_threshold, &
                                                        x_min, x_max, y_min, y_max)
    end subroutine raster_draw_axes_lines_and_ticks

    subroutine raster_draw_axis_labels_only(raster, width, height, plot_area, &
                                            xscale, yscale, symlog_threshold, &
                                            x_min, x_max, y_min, y_max, &
                                            title, xlabel, ylabel, &
                                            custom_xticks, custom_xtick_labels, &
                                            custom_yticks, custom_ytick_labels)
        !! Draw ONLY axis labels and tick labels (for proper drawing order)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)

        ! Draw tick labels (delegate to ticks module via wrapper)
        if (present(custom_xticks) .and. present(custom_xtick_labels)) then
            call raster_draw_x_axis_tick_labels_only_custom(raster, width, height, &
                                                             plot_area, xscale, &
                                                             symlog_threshold, &
                                                             x_min, x_max, &
                                                             custom_xticks, &
                                                             custom_xtick_labels)
        else
            call raster_draw_x_axis_tick_labels_only_wrapper(raster, width, height, &
                                                             plot_area, xscale, &
                                                             symlog_threshold, &
                                                             x_min, x_max, y_min, y_max)
        end if

        if (present(custom_yticks) .and. present(custom_ytick_labels)) then
            call raster_draw_y_axis_tick_labels_only_custom(raster, width, height, &
                                                             plot_area, yscale, &
                                                             symlog_threshold, &
                                                             y_min, y_max, &
                                                             custom_yticks, &
                                                             custom_ytick_labels)
        else
            call raster_draw_y_axis_tick_labels_only_wrapper(raster, width, height, &
                                                             plot_area, yscale, &
                                                             symlog_threshold, &
                                                             x_min, x_max, y_min, y_max)
        end if

        ! Draw axis labels and title
        call raster_draw_axis_labels_wrapper(raster, width, height, plot_area, title, &
                                             xlabel, ylabel)
    end subroutine raster_draw_axis_labels_only

    subroutine draw_raster_axes_lines(raster, width, height, plot_area, x_min, x_max, &
                                      y_min, y_max)
        !! Draw main axes lines (box around plot area)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        real(wp) :: line_r, line_g, line_b
        real(wp) :: dummy_pattern(1), pattern_dist
        real(wp) :: x_bottom_left, y_bottom_left, x_bottom_right, y_bottom_right
        real(wp) :: x_top_left, y_top_left
        associate (dxmin => x_min, dxmax => x_max, dymin => y_min, dymax => y_max); end associate

        line_r = 0.0_wp; line_g = 0.0_wp; line_b = 0.0_wp  ! Black axes
        dummy_pattern = 0.0_wp
        pattern_dist = 0.0_wp

        ! Calculate axes positions in pixel coordinates
        x_bottom_left = real(plot_area%left, wp)
        y_bottom_left = real(plot_area%bottom + plot_area%height, wp)
        x_bottom_right = real(plot_area%left + plot_area%width, wp)
        y_bottom_right = y_bottom_left
        x_top_left = x_bottom_left
        y_top_left = real(plot_area%bottom, wp)

        ! Draw bottom axis (X axis)
        call draw_styled_line(raster%image_data, width, height, &
                              x_bottom_left, y_bottom_left, x_bottom_right, &
                              y_bottom_right, &
                              line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, &
                              0, 0.0_wp, pattern_dist)

        ! Draw left axis (Y axis)
        call draw_styled_line(raster%image_data, width, height, &
                              x_bottom_left, y_bottom_left, x_top_left, y_top_left, &
                              line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, &
                              0, 0.0_wp, pattern_dist)

        ! Draw top axis (optional box)
        call draw_styled_line(raster%image_data, width, height, &
                              x_top_left, y_top_left, x_bottom_right, y_top_left, &
                              line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, &
                              0, 0.0_wp, pattern_dist)

        ! Draw right axis (optional box)
        call draw_styled_line(raster%image_data, width, height, &
                              x_bottom_right, y_top_left, x_bottom_right, &
                              y_bottom_right, &
                              line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, &
                              0, 0.0_wp, pattern_dist)
    end subroutine draw_raster_axes_lines

    ! Wrapper subroutines to handle the old interface while using new modules

    subroutine raster_draw_x_axis_ticks_wrapper(raster, width, height, plot_area, &
                                                xscale, symlog_threshold, &
                                                x_min, x_max, &
                                                y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp) :: x_tick_positions(MAX_TICKS)
        character(len=50) :: tick_labels(MAX_TICKS)
        integer :: tick_colors(3, MAX_TICKS)
        integer :: num_x_ticks, i, decimals

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
                    tick_labels(i) = format_tick_label(x_tick_positions(i), xscale)
                end if
                tick_colors(:, i) = (/0, 0, 0/)
            end do

            call raster_draw_x_axis_ticks(raster, width, height, plot_area, xscale, &
                                          symlog_threshold, &
                                          x_tick_positions(1:num_x_ticks), &
                                          tick_labels(1:num_x_ticks), &
                                          tick_colors(:, 1:num_x_ticks), x_min, x_max)
        end if
    end subroutine raster_draw_x_axis_ticks_wrapper

    subroutine raster_draw_y_axis_ticks_wrapper(raster, width, height, plot_area, &
                                                yscale, symlog_threshold, &
                                                x_min, x_max, &
                                                y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp) :: y_tick_positions(MAX_TICKS)
        character(len=50) :: tick_labels(MAX_TICKS)
        integer :: tick_colors(3, MAX_TICKS)
        integer :: num_y_ticks, i, decimals

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
                    tick_labels(i) = format_tick_label(y_tick_positions(i), yscale)
                end if
                tick_colors(:, i) = (/0, 0, 0/)
            end do

            call raster_draw_y_axis_ticks(raster, width, height, plot_area, yscale, &
                                          symlog_threshold, &
                                          y_tick_positions(1:num_y_ticks), &
                                          tick_labels(1:num_y_ticks), &
                                          tick_colors(:, 1:num_y_ticks), y_min, y_max)
        end if
    end subroutine raster_draw_y_axis_ticks_wrapper

    subroutine raster_draw_x_axis_tick_marks_only_wrapper(raster, width, height, &
                                                          plot_area, &
                                                          xscale, &
                                                          symlog_threshold, &
                                                          x_min, &
                                                          x_max, y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp) :: x_tick_positions(MAX_TICKS)
        integer :: tick_colors(3, MAX_TICKS)
        integer :: num_x_ticks, i

        call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, &
                                 x_tick_positions, num_x_ticks)

        if (num_x_ticks > 0) then
            do i = 1, num_x_ticks
                tick_colors(:, i) = (/0, 0, 0/)
            end do
            call raster_draw_x_axis_tick_marks_only(raster, width, height, plot_area, &
                                                    xscale, symlog_threshold, &
                                                    x_tick_positions(1:num_x_ticks), &
                                                    tick_colors(:, &
                                                                1:num_x_ticks), &
                                                    x_min, x_max)
        end if
    end subroutine raster_draw_x_axis_tick_marks_only_wrapper

    subroutine raster_draw_y_axis_tick_marks_only_wrapper(raster, width, height, &
                                                          plot_area, &
                                                          yscale, &
                                                          symlog_threshold, &
                                                          x_min, &
                                                          x_max, y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp) :: y_tick_positions(MAX_TICKS)
        integer :: tick_colors(3, MAX_TICKS)
        integer :: num_y_ticks, i

        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, &
                                 y_tick_positions, num_y_ticks)

        if (num_y_ticks > 0) then
            do i = 1, num_y_ticks
                tick_colors(:, i) = (/0, 0, 0/)
            end do
            call raster_draw_y_axis_tick_marks_only(raster, width, height, plot_area, &
                                                    yscale, symlog_threshold, &
                                                    y_tick_positions(1:num_y_ticks), &
                                                    tick_colors(:, &
                                                                1:num_y_ticks), &
                                                    y_min, y_max)
        end if
    end subroutine raster_draw_y_axis_tick_marks_only_wrapper

    subroutine raster_draw_x_axis_tick_labels_only_wrapper(raster, width, height, &
                                                           plot_area, &
                                                           xscale, &
                                                           symlog_threshold, &
                                                           x_min, &
                                                           x_max, y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp) :: x_tick_positions(MAX_TICKS)
        character(len=50) :: tick_labels(MAX_TICKS)
        integer :: num_x_ticks, i, decimals

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
                    tick_labels(i) = format_tick_label(x_tick_positions(i), xscale)
                end if
            end do

            call raster_draw_x_axis_tick_labels_only(raster, width, height, plot_area, &
                                                     xscale, symlog_threshold, &
                                                     x_tick_positions(1:num_x_ticks), &
                                                     tick_labels(1:num_x_ticks), &
                                                     x_min, x_max)
        end if
    end subroutine raster_draw_x_axis_tick_labels_only_wrapper

    subroutine raster_draw_y_axis_tick_labels_only_wrapper(raster, width, height, &
                                                           plot_area, &
                                                           yscale, &
                                                           symlog_threshold, &
                                                           x_min, &
                                                           x_max, y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp) :: y_tick_positions(MAX_TICKS)
        character(len=50) :: tick_labels(MAX_TICKS)
        integer :: num_y_ticks, i, decimals

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
                    tick_labels(i) = format_tick_label(y_tick_positions(i), yscale)
                end if
            end do

            call raster_draw_y_axis_tick_labels_only(raster, width, height, plot_area, &
                                                     yscale, symlog_threshold, &
                                                     y_tick_positions(1:num_y_ticks), &
                                                     tick_labels(1:num_y_ticks), &
                                                     y_min, y_max)
        end if
    end subroutine raster_draw_y_axis_tick_labels_only_wrapper

    subroutine raster_draw_x_axis_tick_labels_only_custom(raster, width, height, &
                                                           plot_area, xscale, &
                                                           symlog_threshold, &
                                                           x_min, x_max, &
                                                           positions, labels)
        !! Draw x-axis tick labels at custom positions with custom labels
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max
        real(wp), intent(in) :: positions(:)
        character(len=*), intent(in) :: labels(:)

        character(len=50) :: tick_labels(size(positions))
        integer :: i, n

        n = size(positions)
        if (n == 0) return
        if (size(labels) /= n) return

        do i = 1, n
            tick_labels(i) = trim(labels(i))
        end do

        call raster_draw_x_axis_tick_labels_only(raster, width, height, plot_area, &
                                                 xscale, symlog_threshold, &
                                                 positions, tick_labels, &
                                                 x_min, x_max)
    end subroutine raster_draw_x_axis_tick_labels_only_custom

    subroutine raster_draw_y_axis_tick_labels_only_custom(raster, width, height, &
                                                           plot_area, yscale, &
                                                           symlog_threshold, &
                                                           y_min, y_max, &
                                                           positions, labels)
        !! Draw y-axis tick labels at custom positions with custom labels
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: y_min, y_max
        real(wp), intent(in) :: positions(:)
        character(len=*), intent(in) :: labels(:)

        character(len=50) :: tick_labels(size(positions))
        integer :: i, n

        n = size(positions)
        if (n == 0) return
        if (size(labels) /= n) return

        do i = 1, n
            tick_labels(i) = trim(labels(i))
        end do

        call raster_draw_y_axis_tick_labels_only(raster, width, height, plot_area, &
                                                 yscale, symlog_threshold, &
                                                 positions, tick_labels, &
                                                 y_min, y_max)
    end subroutine raster_draw_y_axis_tick_labels_only_custom

    subroutine raster_draw_axis_labels_wrapper(raster, width, height, plot_area, &
                                               title, xlabel, ylabel)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=:), allocatable :: title_str, xlabel_str, ylabel_str

        title_str = ""
        xlabel_str = ""
        ylabel_str = ""

        if (present(title)) then
            if (allocated(title)) title_str = title
        end if

        if (present(xlabel)) then
            if (allocated(xlabel)) xlabel_str = xlabel
        end if

        if (present(ylabel)) then
            if (allocated(ylabel)) ylabel_str = ylabel
        end if

        call raster_draw_axis_labels(raster, width, height, plot_area, title_str, &
                                     xlabel_str, ylabel_str)
    end subroutine raster_draw_axis_labels_wrapper

    subroutine raster_draw_secondary_y_axis(raster, width, height, plot_area, yscale, &
                                            symlog_threshold, &
                                            y_min, y_max, ylabel)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: y_min, y_max
        character(len=:), allocatable, intent(in), optional :: ylabel
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
                    tick_labels(i) = format_tick_label(y_tick_positions(i), yscale)
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
                                                x_min, x_max, xlabel)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max
        character(len=:), allocatable, intent(in), optional :: xlabel
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
                    tick_labels(i) = format_tick_label(x_tick_positions(i), xscale)
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

    ! Simple wrapper for test compatibility
    subroutine compute_non_overlapping_mask_simple(centers, widths, min_gap, keep)
        real(wp), intent(in) :: centers(:)
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

    ! Re-export wrappers for test compatibility
    integer function compute_ylabel_x_pos(y_tick_label_edge, rotated_width, plot_area) &
        result(x_pos)
        integer, intent(in) :: y_tick_label_edge
        integer, intent(in) :: rotated_width
        type(plot_area_t), intent(in) :: plot_area
        x_pos = compute_ylabel_x_pos_impl(y_tick_label_edge, rotated_width, &
                                          plot_area)
    end function compute_ylabel_x_pos

    ! Old interface for backward compatibility
    integer function compute_ylabel_x_pos_old(plot_area, rotated_width, &
                                              y_tick_max_width) result(x_pos)
        use fortplot_constants, only: TICK_MARK_LENGTH
        use fortplot_constants, only: YLABEL_EXTRA_GAP
        use fortplot_raster_ticks, only: Y_TICK_LABEL_RIGHT_PAD
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: rotated_width, y_tick_max_width
        integer :: clearance, min_left_margin

        ! Original implementation logic for backward compatibility
        clearance = TICK_MARK_LENGTH + Y_TICK_LABEL_RIGHT_PAD + &
                    max(0, y_tick_max_width) + YLABEL_EXTRA_GAP
        x_pos = plot_area%left - clearance - rotated_width

        ! Ensure ylabel doesn't go off the left edge
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

    ! Old interface for backward compatibility
    integer function y_tick_label_right_edge_at_axis_old(plot_area) result(edge)
        type(plot_area_t), intent(in) :: plot_area
        edge = y_tick_label_right_edge_at_axis_impl(plot_area, 0)
    end function y_tick_label_right_edge_at_axis_old

end module fortplot_raster_axes
