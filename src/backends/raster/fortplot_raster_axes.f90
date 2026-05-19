module fortplot_raster_axes
    !! Raster axes coordination module (refactored for size compliance)
    !! Orchestrates tick and label rendering through specialized modules
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_line_styles, only: draw_styled_line
    use fortplot_raster_core, only: raster_image_t
    use fortplot_raster_ticks
    use fortplot_raster_ticks_secondary
    use fortplot_raster_axes_secondary
  use fortplot_raster_labels, only: raster_render_ylabel, &
                                        render_title_centered, compute_title_position, &
                                        compute_ylabel_x_pos_impl => &
                                        compute_ylabel_x_pos, &
                                        y_tick_label_right_edge_at_axis_impl => &
                                        y_tick_label_right_edge_at_axis
    use fortplot_raster_axes_custom, only: raster_draw_x_axis_tick_labels_only_custom, &
                                            raster_draw_y_axis_tick_labels_only_custom, &
                                            raster_draw_axis_labels_wrapper, &
                                            raster_draw_x_axis_ticks_wrapper, &
                                            raster_draw_y_axis_ticks_wrapper, &
                                            raster_draw_x_axis_tick_marks_only_wrapper, &
                                            raster_draw_y_axis_tick_marks_only_wrapper, &
                                            raster_draw_x_axis_tick_labels_only_wrapper, &
                                            raster_draw_y_axis_tick_labels_only_wrapper
    use fortplot_scales, only: apply_scale_transform
    use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
    use fortplot_tick_calculation, only: determine_decimals_from_ticks, &
                                         format_tick_value_consistent
    use fortplot_text, only: calculate_text_width
    use fortplot_text_rendering, only: render_text_to_image
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private

    ! Primary public interfaces
    public :: raster_draw_axes_and_labels
    public :: raster_draw_axes_lines_and_ticks
    public :: raster_draw_axis_labels_only
    public :: map_value_to_plot_x, map_value_to_plot_y

    ! Re-exports from fortplot_raster_ticks
    public :: raster_render_ylabel
    public :: compute_title_position
    public :: compute_non_overlapping_mask
    public :: raster_draw_x_axis_ticks
    public :: raster_draw_y_axis_ticks
    public :: raster_draw_x_axis_tick_marks_only
    public :: raster_draw_y_axis_tick_marks_only
    public :: raster_draw_x_axis_tick_labels_only
    public :: raster_draw_y_axis_tick_labels_only

    ! Re-exports from fortplot_raster_ticks_secondary
    public :: raster_draw_x_minor_ticks
    public :: raster_draw_y_minor_ticks

    ! Re-exports from fortplot_raster_axes_secondary
    public :: compute_non_overlapping_mask_simple
    public :: compute_ylabel_x_pos, compute_ylabel_x_pos_old
    public :: y_tick_label_right_edge_at_axis, y_tick_label_right_edge_at_axis_old
    public :: raster_draw_secondary_y_axis
    public :: raster_draw_secondary_x_axis_top

    ! Re-exports from fortplot_raster_axes_custom
    public :: raster_draw_x_axis_tick_labels_only_custom
    public :: raster_draw_y_axis_tick_labels_only_custom
    public :: raster_draw_axis_labels_wrapper
    public :: raster_draw_x_axis_ticks_wrapper
    public :: raster_draw_y_axis_ticks_wrapper
    public :: raster_draw_x_axis_tick_marks_only_wrapper
    public :: raster_draw_y_axis_tick_marks_only_wrapper
    public :: raster_draw_x_axis_tick_labels_only_wrapper
    public :: raster_draw_y_axis_tick_labels_only_wrapper

contains

    real(wp) function map_value_to_plot_x(value, data_min, data_max, plot_area, scale, &
                                          symlog_threshold) result(px)
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
                                           title, xlabel, ylabel, &
                                           x_date_format, y_date_format)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format

        call draw_raster_axes_lines(raster, width, height, plot_area, x_min, x_max, &
                                    y_min, y_max)

        call raster_draw_x_axis_ticks_wrapper(raster, width, height, plot_area, &
                                              xscale, symlog_threshold, &
                                              x_min, x_max, y_min, y_max, &
                                              date_format=x_date_format)
        call raster_draw_y_axis_ticks_wrapper(raster, width, height, plot_area, &
                                              yscale, symlog_threshold, &
                                              x_min, x_max, y_min, y_max, &
                                              date_format=y_date_format)

        call raster_draw_axis_labels_wrapper(raster, width, height, plot_area, title, &
                                             xlabel, ylabel)
    end subroutine raster_draw_axes_and_labels

    subroutine raster_draw_axes_lines_and_ticks(raster, width, height, plot_area, &
                                                xscale, yscale, symlog_threshold, &
                                                x_min, x_max, y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        call draw_raster_axes_lines(raster, width, height, plot_area, x_min, x_max, &
                                    y_min, y_max)

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
                                            custom_yticks, custom_ytick_labels, &
                                            x_date_format, y_date_format)
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
        character(len=*), intent(in), optional :: x_date_format, y_date_format

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
                                                             x_min, x_max, &
                                                             y_min, y_max, &
                                                             date_format=x_date_format)
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
                                                             x_min, x_max, &
                                                             y_min, y_max, &
                                                             date_format=y_date_format)
        end if

        call raster_draw_axis_labels_wrapper(raster, width, height, plot_area, title, &
                                             xlabel, ylabel)
    end subroutine raster_draw_axis_labels_only

    subroutine draw_raster_axes_lines(raster, width, height, plot_area, x_min, x_max, &
                                      y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        real(wp) :: line_r, line_g, line_b
        real(wp) :: dummy_pattern(1), pattern_dist
        real(wp) :: x_bottom_left, y_bottom_left, x_bottom_right, y_bottom_right
        real(wp) :: x_top_left, y_top_left

        line_r = 0.0_wp; line_g = 0.0_wp; line_b = 0.0_wp
        dummy_pattern = 0.0_wp
        pattern_dist = 0.0_wp

        x_bottom_left = real(plot_area%left, wp)
        y_bottom_left = real(plot_area%bottom + plot_area%height, wp)
        x_bottom_right = real(plot_area%left + plot_area%width, wp)
        y_bottom_right = y_bottom_left
        x_top_left = x_bottom_left
        y_top_left = real(plot_area%bottom, wp)

        call draw_styled_line(raster%image_data, width, height, &
                              x_bottom_left, y_bottom_left, x_bottom_right, &
                              y_bottom_right, &
                              line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, &
                              0, 0.0_wp, pattern_dist)

        call draw_styled_line(raster%image_data, width, height, &
                              x_bottom_left, y_bottom_left, x_top_left, y_top_left, &
                              line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, &
                              0, 0.0_wp, pattern_dist)

        call draw_styled_line(raster%image_data, width, height, &
                              x_top_left, y_top_left, x_bottom_right, y_top_left, &
                              line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, &
                              0, 0.0_wp, pattern_dist)

        call draw_styled_line(raster%image_data, width, height, &
                              x_bottom_right, y_top_left, x_bottom_right, &
                              y_bottom_right, &
                              line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, &
                              0, 0.0_wp, pattern_dist)
    end subroutine draw_raster_axes_lines

end module fortplot_raster_axes
