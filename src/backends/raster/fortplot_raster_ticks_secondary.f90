module fortplot_raster_ticks_secondary
    !! Secondary axis ticks (right/top) and minor ticks for raster backend
    !! Extracted from fortplot_raster_ticks for size compliance (refs #1694)
    use fortplot_constants, only: TICK_MARK_LENGTH, X_TICK_LABEL_PAD, &
                                  Y_TICK_LABEL_RIGHT_PAD, Y_TICK_LABEL_LEFT_PAD, &
                                  X_TICK_LABEL_TOP_PAD
    use fortplot_text_rendering, only: render_text_with_size, &
                                       calculate_text_width, &
                                       calculate_text_width_with_size, &
                                       calculate_text_height, &
                                       calculate_text_height_with_size, &
                                       DEFAULT_FONT_SIZE
    use fortplot_text_helpers, only: prepare_text_for_raster
    use fortplot_constants, only: REFERENCE_DPI, FALLBACK_LABEL_HEIGHT_PX, &
                                  MIN_TICK_LABEL_GAP_PX
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_line_styles, only: draw_styled_line
    use fortplot_raster_core, only: raster_image_t, scale_px, pt2px
    use fortplot_scales, only: apply_scale_transform
    use fortplot_raster_ticks, only: resolve_tick_font_px
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_draw_y_axis_ticks_right
    public :: raster_draw_y_axis_tick_marks_only_right
    public :: raster_draw_y_axis_tick_labels_only_right
    public :: raster_draw_x_axis_ticks_top
    public :: raster_draw_x_axis_tick_marks_only_top
    public :: raster_draw_x_axis_tick_labels_only_top
    public :: raster_draw_x_minor_ticks
    public :: raster_draw_y_minor_ticks

    integer, parameter :: MINOR_TICK_LENGTH = 4

    real(wp), parameter :: MAJOR_TICK_WIDTH_PT = 0.8_wp
        !! matplotlib rcParams xtick.major.width / ytick.major.width.
    real(wp), parameter :: MINOR_TICK_WIDTH_PT = 0.6_wp
        !! matplotlib rcParams xtick.minor.width / ytick.minor.width.

contains

    subroutine raster_draw_y_axis_ticks_right(raster, width, height, plot_area, &
                                              yscale, symlog_threshold, &
                                              yticks, ytick_labels, ytick_colors, &
                                              y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), contiguous, intent(in) :: yticks(:)
        character(len=*), intent(in) :: ytick_labels(:)
        integer, intent(in) :: ytick_colors(:, :)
        real(wp), intent(in) :: y_min, y_max
        integer :: j
        integer :: label_width

        raster%last_y_tick_max_width_right = 0
        do j = 1, size(yticks)
            label_width = calculate_text_width(trim(ytick_labels(j)))
            raster%last_y_tick_max_width_right = max(raster%last_y_tick_max_width_right, label_width)
        end do

        call raster_draw_y_axis_tick_marks_only_right(raster, width, height, &
                                                      plot_area, yscale, &
                                                      symlog_threshold, &
                                                      yticks, ytick_colors, &
                                                      y_min, y_max)
        call raster_draw_y_axis_tick_labels_only_right(raster, width, height, &
                                                       plot_area, yscale, &
                                                       symlog_threshold, &
                                                       yticks, ytick_labels, &
                                                       y_min, y_max)
    end subroutine raster_draw_y_axis_ticks_right

    subroutine raster_draw_y_axis_tick_marks_only_right(raster, width, height, &
                                                        plot_area, yscale, &
                                                        symlog_threshold, &
                                                        yticks, ytick_colors, &
                                                        y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), contiguous, intent(in) :: yticks(:)
        integer, intent(in) :: ytick_colors(:, :)
        real(wp), intent(in) :: y_min, y_max
        integer :: tick_y, tick_left, tick_right, j
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: dummy_pattern(1), pattern_dist
        real(wp) :: tick_w

        min_t = apply_scale_transform(y_min, yscale, symlog_threshold)
        max_t = apply_scale_transform(y_max, yscale, symlog_threshold)

        dummy_pattern = 0.0_wp
        pattern_dist = 0.0_wp
        tick_w = pt2px(MAJOR_TICK_WIDTH_PT, raster%dpi)

        do j = 1, size(yticks)
            tick_t = apply_scale_transform(yticks(j), yscale, symlog_threshold)
            if (max_t > min_t) then
                tick_y = plot_area%bottom + plot_area%height - int((tick_t - &
                                                                    min_t)/(max_t &
                                                                            - min_t)* &
                                                                   plot_area%height)
            else
                tick_y = plot_area%bottom
            end if
            tick_left = plot_area%left + plot_area%width
            tick_right = min(width, tick_left + &
                             scale_px(TICK_MARK_LENGTH, raster%dpi))
            call draw_styled_line(raster%image_data, width, height, &
                                  real(tick_left, wp), real(tick_y, wp), &
                                  real(tick_right, wp), &
                                  real(tick_y, wp), &
                                  real(ytick_colors(1, j), wp)/255.0_wp, &
                                  real(ytick_colors(2, j), &
                                       wp)/255.0_wp, &
                                  real(ytick_colors(3, j), wp)/255.0_wp, tick_w, &
                                  'solid', dummy_pattern, &
                                  0, 0.0_wp, pattern_dist)
        end do
    end subroutine raster_draw_y_axis_tick_marks_only_right

    subroutine raster_draw_y_axis_tick_labels_only_right(raster, width, height, &
                                                         plot_area, yscale, &
                                                         symlog_threshold, &
                                                         yticks, ytick_labels, &
                                                         y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), contiguous, intent(in) :: yticks(:)
        character(len=*), intent(in) :: ytick_labels(:)
        real(wp), intent(in) :: y_min, y_max
        integer :: tick_y, label_x, label_y, j
        integer :: label_width, label_height
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: font_px
        character(len=600) :: escaped_text
        font_px = resolve_tick_font_px(raster)

        min_t = apply_scale_transform(y_min, yscale, symlog_threshold)
        max_t = apply_scale_transform(y_max, yscale, symlog_threshold)

        do j = 1, size(yticks)
            tick_t = apply_scale_transform(yticks(j), yscale, symlog_threshold)
            if (max_t > min_t) then
                tick_y = plot_area%bottom + plot_area%height - int((tick_t - &
                                                                    min_t)/(max_t &
                                                                            - min_t)* &
                                                                   plot_area%height)
            else
                tick_y = plot_area%bottom
            end if

            call prepare_text_for_raster(ytick_labels(j), escaped_text)

            label_width = calculate_text_width_with_size(trim(escaped_text), font_px)
            label_height = calculate_text_height_with_size(font_px)
            if (label_height <= 0) label_height = scale_px(FALLBACK_LABEL_HEIGHT_PX, raster%dpi)

            label_x = plot_area%left + plot_area%width + &
                      scale_px(Y_TICK_LABEL_LEFT_PAD, raster%dpi)
            label_y = tick_y + label_height/4

            call render_text_with_size(raster%image_data, width, height, &
                                       label_x, label_y, trim(escaped_text), &
                                       0_1, 0_1, 0_1, font_px)
        end do
    end subroutine raster_draw_y_axis_tick_labels_only_right

    subroutine raster_draw_x_axis_ticks_top(raster, width, height, plot_area, xscale, &
                                            symlog_threshold, &
                                            xticks, xtick_labels, xtick_colors, &
                                            x_min, x_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), contiguous, intent(in) :: xticks(:)
        character(len=*), intent(in) :: xtick_labels(:)
        integer, intent(in) :: xtick_colors(:, :)
        real(wp), intent(in) :: x_min, x_max
        integer :: j
        integer :: label_height

        raster%last_x_tick_max_height_top = 0
        do j = 1, size(xticks)
            label_height = calculate_text_height(trim(xtick_labels(j)))
            raster%last_x_tick_max_height_top = max(raster%last_x_tick_max_height_top, label_height)
        end do

        call raster_draw_x_axis_tick_marks_only_top(raster, width, height, plot_area, &
                                                    xscale, symlog_threshold, &
                                                    xticks, xtick_colors, x_min, x_max)
        call raster_draw_x_axis_tick_labels_only_top(raster, width, height, plot_area, &
                                                     xscale, symlog_threshold, &
                                                     xticks, xtick_labels, x_min, x_max)
    end subroutine raster_draw_x_axis_ticks_top

    subroutine raster_draw_x_axis_tick_marks_only_top(raster, width, height, &
                                                      plot_area, &
                                                      xscale, symlog_threshold, &
                                                      xticks, xtick_colors, &
                                                      x_min, x_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), contiguous, intent(in) :: xticks(:)
        integer, intent(in) :: xtick_colors(:, :)
        real(wp), intent(in) :: x_min, x_max
        integer :: tick_x, tick_top, tick_bottom, j
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: dummy_pattern(1), pattern_dist
        real(wp) :: tick_w

        min_t = apply_scale_transform(x_min, xscale, symlog_threshold)
        max_t = apply_scale_transform(x_max, xscale, symlog_threshold)

        dummy_pattern = 0.0_wp
        pattern_dist = 0.0_wp
        tick_w = pt2px(MAJOR_TICK_WIDTH_PT, raster%dpi)

        do j = 1, size(xticks)
            tick_t = apply_scale_transform(xticks(j), xscale, symlog_threshold)
            if (max_t > min_t) then
                tick_x = plot_area%left + int((tick_t - min_t)/(max_t - &
                                                                min_t)*plot_area%width)
            else
                tick_x = plot_area%left
            end if
            tick_top = max(1, plot_area%bottom - &
                          scale_px(TICK_MARK_LENGTH, raster%dpi))
            tick_bottom = plot_area%bottom
            call draw_styled_line(raster%image_data, width, height, &
                                  real(tick_x, wp), real(tick_top, wp), &
                                  real(tick_x, wp), &
                                  real(tick_bottom, wp), &
                                  real(xtick_colors(1, j), wp)/255.0_wp, &
                                  real(xtick_colors(2, j), &
                                       wp)/255.0_wp, &
                                  real(xtick_colors(3, j), wp)/255.0_wp, tick_w, &
                                  'solid', dummy_pattern, &
                                  0, 0.0_wp, pattern_dist)
        end do
    end subroutine raster_draw_x_axis_tick_marks_only_top

    subroutine raster_draw_x_axis_tick_labels_only_top(raster, width, height, &
                                                       plot_area, xscale, &
                                                       symlog_threshold, &
                                                       xticks, xtick_labels, &
                                                       x_min, x_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), contiguous, intent(in) :: xticks(:)
        character(len=*), intent(in) :: xtick_labels(:)
        real(wp), intent(in) :: x_min, x_max
        integer :: tick_x, label_x, label_y, j
        integer :: label_width, label_height
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: font_px
        character(len=600) :: escaped_text
        font_px = resolve_tick_font_px(raster)

        min_t = apply_scale_transform(x_min, xscale, symlog_threshold)
        max_t = apply_scale_transform(x_max, xscale, symlog_threshold)

        do j = 1, size(xticks)
            tick_t = apply_scale_transform(xticks(j), xscale, symlog_threshold)
            if (max_t > min_t) then
                tick_x = plot_area%left + int((tick_t - min_t)/(max_t - &
                                                                min_t)*plot_area%width)
            else
                tick_x = plot_area%left
            end if

            call prepare_text_for_raster(xtick_labels(j), escaped_text)

            label_width = calculate_text_width_with_size(trim(escaped_text), font_px)
            label_height = calculate_text_height_with_size(font_px)
            if (label_height <= 0) label_height = scale_px(FALLBACK_LABEL_HEIGHT_PX, raster%dpi)

            label_x = tick_x - label_width/2
            label_y = max(1, plot_area%bottom - &
                          scale_px(X_TICK_LABEL_TOP_PAD, raster%dpi) - label_height)

            call render_text_with_size(raster%image_data, width, height, &
                                       label_x, label_y, trim(escaped_text), &
                                       0_1, 0_1, 0_1, font_px)
        end do
    end subroutine raster_draw_x_axis_tick_labels_only_top

    subroutine raster_draw_x_minor_ticks(raster, width, height, plot_area, &
                                          xscale, symlog_threshold, &
                                          minor_ticks, x_min, x_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), contiguous, intent(in) :: minor_ticks(:)
        real(wp), intent(in) :: x_min, x_max

        integer :: tick_x, tick_top, tick_bottom, j
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: dummy_pattern(1), pattern_dist

        min_t = apply_scale_transform(x_min, xscale, symlog_threshold)
        max_t = apply_scale_transform(x_max, xscale, symlog_threshold)

        dummy_pattern = 0.0_wp
        pattern_dist = 0.0_wp

        do j = 1, size(minor_ticks)
            tick_t = apply_scale_transform(minor_ticks(j), xscale, symlog_threshold)
            if (max_t > min_t) then
                tick_x = plot_area%left + int((tick_t - min_t)/(max_t - min_t)* &
                                              plot_area%width)
            else
                tick_x = plot_area%left
            end if
            tick_top = plot_area%bottom + plot_area%height
            tick_bottom = min(height, tick_top + &
                              scale_px(MINOR_TICK_LENGTH, raster%dpi))
            call draw_styled_line(raster%image_data, width, height, &
                                  real(tick_x, wp), real(tick_top, wp), &
                                  real(tick_x, wp), real(tick_bottom, wp), &
                                  0.0_wp, 0.0_wp, 0.0_wp, &
                                  pt2px(MINOR_TICK_WIDTH_PT, raster%dpi), &
                                  'solid', dummy_pattern, 0, 0.0_wp, &
                                  pattern_dist)
        end do
    end subroutine raster_draw_x_minor_ticks

    subroutine raster_draw_y_minor_ticks(raster, width, height, plot_area, &
                                          yscale, symlog_threshold, &
                                          minor_ticks, y_min, y_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), contiguous, intent(in) :: minor_ticks(:)
        real(wp), intent(in) :: y_min, y_max

        integer :: tick_y, tick_left, tick_right, j
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: dummy_pattern(1), pattern_dist

        min_t = apply_scale_transform(y_min, yscale, symlog_threshold)
        max_t = apply_scale_transform(y_max, yscale, symlog_threshold)

        dummy_pattern = 0.0_wp
        pattern_dist = 0.0_wp

        do j = 1, size(minor_ticks)
            tick_t = apply_scale_transform(minor_ticks(j), yscale, symlog_threshold)
            if (max_t > min_t) then
                tick_y = plot_area%bottom + plot_area%height - &
                         int((tick_t - min_t)/(max_t - min_t)*plot_area%height)
            else
                tick_y = plot_area%bottom
            end if
            tick_left = max(1, plot_area%left - &
                           scale_px(MINOR_TICK_LENGTH, raster%dpi))
            tick_right = plot_area%left
            call draw_styled_line(raster%image_data, width, height, &
                                  real(tick_left, wp), real(tick_y, wp), &
                                  real(tick_right, wp), real(tick_y, wp), &
                                  0.0_wp, 0.0_wp, 0.0_wp, &
                                  pt2px(MINOR_TICK_WIDTH_PT, raster%dpi), &
                                  'solid', dummy_pattern, 0, 0.0_wp, &
                                  pattern_dist)
        end do
    end subroutine raster_draw_y_minor_ticks

end module fortplot_raster_ticks_secondary
