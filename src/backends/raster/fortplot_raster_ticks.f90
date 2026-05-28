module fortplot_raster_ticks
    !! Raster tick marks and tick labels rendering functionality (primary axes)
    !! Extracted from fortplot_raster_axes.f90 for single responsibility principle
    use fortplot_constants, only: TICK_MARK_LENGTH, X_TICK_LABEL_PAD, &
                                  Y_TICK_LABEL_RIGHT_PAD, Y_TICK_LABEL_LEFT_PAD, &
                                  X_TICK_LABEL_TOP_PAD
    use fortplot_text_rendering, only: render_text_to_image, render_text_with_size, &
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
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_draw_x_axis_ticks
    public :: raster_draw_y_axis_ticks
    public :: raster_draw_x_axis_tick_marks_only
    public :: raster_draw_y_axis_tick_marks_only
    public :: raster_draw_x_axis_tick_labels_only
    public :: raster_draw_y_axis_tick_labels_only
    public :: X_TICK_LABEL_PAD, Y_TICK_LABEL_RIGHT_PAD
    public :: Y_TICK_LABEL_LEFT_PAD, X_TICK_LABEL_TOP_PAD
    public :: compute_non_overlapping_mask
    public :: resolve_tick_font_px
    public :: MAJOR_TICK_WIDTH_PT, MINOR_TICK_WIDTH_PT

    real(wp), parameter :: MAJOR_TICK_WIDTH_PT = 0.8_wp
        !! matplotlib's rcParams xtick.major.width / ytick.major.width.
    real(wp), parameter :: MINOR_TICK_WIDTH_PT = 0.6_wp
        !! matplotlib's rcParams xtick.minor.width / ytick.minor.width.

contains

    pure function resolve_tick_font_px(raster) result(px)
        type(raster_image_t), intent(in) :: raster
        real(wp) :: px
        if (raster%config_tick_font_size > 0.0_wp) then
            ! Configured tick size is in points; scale to pixels by DPI so an
            ! explicit value renders consistently across resolutions.
            px = raster%config_tick_font_size * raster%dpi / REFERENCE_DPI
        else
            px = real(DEFAULT_FONT_SIZE, wp) * raster%dpi / REFERENCE_DPI
        end if
    end function resolve_tick_font_px

    subroutine raster_draw_x_axis_ticks(raster, width, height, plot_area, &
                                        xscale, symlog_threshold, xticks, &
                                        xtick_labels, xtick_colors, x_min, x_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), contiguous, intent(in) :: xticks(:)
        character(len=*), intent(in) :: xtick_labels(:)
        integer, intent(in) :: xtick_colors(:, :)
        real(wp), intent(in) :: x_min, x_max

        call raster_draw_x_axis_tick_marks_only(raster, width, height, plot_area, &
                                                xscale, symlog_threshold, xticks, &
                                                xtick_colors, x_min, x_max)
        call raster_draw_x_axis_tick_labels_only(raster, width, height, plot_area, &
                                                 xscale, symlog_threshold, xticks, &
                                                 xtick_labels, x_min, x_max)
    end subroutine raster_draw_x_axis_ticks

    subroutine raster_draw_y_axis_ticks(raster, width, height, plot_area, yscale, &
                                        symlog_threshold, &
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
        integer :: label_width, label_height
        real(wp) :: font_px
        font_px = resolve_tick_font_px(raster)

        raster%last_y_tick_max_width = 0
        do j = 1, size(yticks)
            label_width = calculate_text_width_with_size(trim(ytick_labels(j)), font_px)
            raster%last_y_tick_max_width = max(raster%last_y_tick_max_width, label_width)
        end do

        call raster_draw_y_axis_tick_marks_only(raster, width, height, plot_area, &
                                                yscale, symlog_threshold, &
                                                yticks, ytick_colors, y_min, y_max)
        call raster_draw_y_axis_tick_labels_only(raster, width, height, plot_area, &
                                                 yscale, symlog_threshold, &
                                                 yticks, ytick_labels, y_min, y_max)
    end subroutine raster_draw_y_axis_ticks

    subroutine raster_draw_x_axis_tick_marks_only(raster, width, height, plot_area, &
                                                  xscale, symlog_threshold, xticks, &
                                                  xtick_colors, x_min, x_max)
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
                tick_x = plot_area%left + int((tick_t - min_t)/(max_t - min_t)* &
                                              plot_area%width)
            else
                tick_x = plot_area%left
            end if
            tick_top = plot_area%bottom + plot_area%height
            tick_bottom = min(height, tick_top + &
                              scale_px(TICK_MARK_LENGTH, raster%dpi))
            call draw_styled_line(raster%image_data, width, height, &
                                  real(tick_x, wp), real(tick_top, wp), &
                                  real(tick_x, wp), real(tick_bottom, wp), &
                                  real(xtick_colors(1, j), wp)/255.0_wp, &
                                  real(xtick_colors(2, j), wp)/255.0_wp, &
                                  real(xtick_colors(3, j), wp)/255.0_wp, &
                                  tick_w, 'solid', dummy_pattern, 0, 0.0_wp, &
                                  pattern_dist)
        end do
    end subroutine raster_draw_x_axis_tick_marks_only

    subroutine raster_draw_y_axis_tick_marks_only(raster, width, height, plot_area, &
                                                  yscale, symlog_threshold, &
                                                  yticks, ytick_colors, y_min, y_max)
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
                tick_y = plot_area%bottom + plot_area%height - &
                         int((tick_t - min_t)/(max_t - min_t)*plot_area%height)
            else
                tick_y = plot_area%bottom
            end if
            tick_left = max(1, plot_area%left - &
                           scale_px(TICK_MARK_LENGTH, raster%dpi))
            tick_right = plot_area%left
            call draw_styled_line(raster%image_data, width, height, &
                                  real(tick_left, wp), real(tick_y, wp), &
                                  real(tick_right, wp), real(tick_y, wp), &
                                  real(ytick_colors(1, j), wp)/255.0_wp, &
                                  real(ytick_colors(2, j), wp)/255.0_wp, &
                                  real(ytick_colors(3, j), wp)/255.0_wp, &
                                  tick_w, 'solid', dummy_pattern, 0, 0.0_wp, &
                                  pattern_dist)
        end do
    end subroutine raster_draw_y_axis_tick_marks_only

    subroutine raster_draw_x_axis_tick_labels_only(raster, width, height, plot_area, &
                                                   xscale, symlog_threshold, xticks, &
                                                   xtick_labels, x_min, x_max)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), contiguous, intent(in) :: xticks(:)
        character(len=*), intent(in) :: xtick_labels(:)
        real(wp), intent(in) :: x_min, x_max
        logical, dimension(size(xticks)) :: visibility_mask
        integer :: tick_x, label_x, label_y, j
        integer :: label_width, label_height
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: font_px
        character(len=600) :: escaped_text
        font_px = resolve_tick_font_px(raster)

        raster%last_x_tick_max_height_bottom = 0

        min_t = apply_scale_transform(x_min, xscale, symlog_threshold)
        max_t = apply_scale_transform(x_max, xscale, symlog_threshold)

        call compute_non_overlapping_mask(xticks, xtick_labels, x_min, x_max, xscale, &
                                          symlog_threshold, plot_area, visibility_mask)

        do j = 1, size(xticks)
            if (.not. visibility_mask(j)) cycle

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
            raster%last_x_tick_max_height_bottom = max(raster%last_x_tick_max_height_bottom, &
                                                label_height)

            label_x = tick_x - label_width/2
            label_y = plot_area%bottom + plot_area%height + &
                      scale_px(X_TICK_LABEL_PAD, raster%dpi)

            call render_text_with_size(raster%image_data, width, height, &
                                       label_x, label_y, trim(escaped_text), &
                                       0_1, 0_1, 0_1, font_px)
        end do
    end subroutine raster_draw_x_axis_tick_labels_only

    subroutine raster_draw_y_axis_tick_labels_only(raster, width, height, plot_area, &
                                                   yscale, symlog_threshold, &
                                                   yticks, ytick_labels, y_min, y_max)
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

        raster%last_y_tick_max_width = 0

        min_t = apply_scale_transform(y_min, yscale, symlog_threshold)
        max_t = apply_scale_transform(y_max, yscale, symlog_threshold)

        do j = 1, size(yticks)
            tick_t = apply_scale_transform(yticks(j), yscale, symlog_threshold)
            if (max_t > min_t) then
                tick_y = plot_area%bottom + plot_area%height - &
                         int((tick_t - min_t)/(max_t - min_t)*plot_area%height)
            else
                tick_y = plot_area%bottom
            end if

            call prepare_text_for_raster(ytick_labels(j), escaped_text)

            label_width = calculate_text_width_with_size(trim(escaped_text), font_px)
            label_height = calculate_text_height_with_size(font_px)
            if (label_height <= 0) label_height = scale_px(FALLBACK_LABEL_HEIGHT_PX, raster%dpi)

            raster%last_y_tick_max_width = max(raster%last_y_tick_max_width, label_width)

            label_x = plot_area%left - &
                      scale_px(Y_TICK_LABEL_RIGHT_PAD, raster%dpi) - label_width
            label_y = tick_y + label_height/4

            call render_text_with_size(raster%image_data, width, height, &
                                       label_x, label_y, trim(escaped_text), &
                                       0_1, 0_1, 0_1, font_px)
        end do
    end subroutine raster_draw_y_axis_tick_labels_only

    subroutine compute_non_overlapping_mask(xticks, xtick_labels, x_min, &
                                            x_max, xscale, &
                                            symlog_threshold, plot_area, &
                                            visibility_mask)
        real(wp), contiguous, intent(in) :: xticks(:)
        character(len=*), intent(in) :: xtick_labels(:)
        real(wp), intent(in) :: x_min, x_max
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        type(plot_area_t), intent(in) :: plot_area
        logical, intent(out) :: visibility_mask(size(xticks))

        integer :: j, n
        integer, allocatable :: label_lefts(:), label_rights(:)
        real(wp) :: min_t, max_t, tick_t
        integer :: tick_x, label_width
        character(len=600) :: escaped_text
        integer :: min_gap
        integer :: last_visible_right

        n = size(xticks)
        if (n == 0) then
            return
        end if

        min_gap = MIN_TICK_LABEL_GAP_PX

        allocate (label_lefts(n), label_rights(n))

        min_t = apply_scale_transform(x_min, xscale, symlog_threshold)
        max_t = apply_scale_transform(x_max, xscale, symlog_threshold)

        do j = 1, n
            tick_t = apply_scale_transform(xticks(j), xscale, symlog_threshold)
            if (max_t > min_t) then
                tick_x = plot_area%left + int((tick_t - min_t)/(max_t - &
                                                                min_t)*plot_area%width)
            else
                tick_x = plot_area%left
            end if

            call prepare_text_for_raster(xtick_labels(j), escaped_text)
            label_width = calculate_text_width(trim(escaped_text))

            label_lefts(j) = tick_x - label_width/2
            label_rights(j) = tick_x + label_width/2
        end do

        visibility_mask = .false.

        if (n >= 1) then
            visibility_mask(1) = .true.
            last_visible_right = label_rights(1)
        end if

        do j = 2, n - 1
            if (label_lefts(j) >= last_visible_right + min_gap) then
                visibility_mask(j) = .true.
                last_visible_right = label_rights(j)
            end if
        end do

        if (n >= 2) then
            if (.not. visibility_mask(n)) then
                if (label_lefts(n) >= last_visible_right + min_gap) then
                    visibility_mask(n) = .true.
                end if
            end if
        end if

    end subroutine compute_non_overlapping_mask

end module fortplot_raster_ticks
