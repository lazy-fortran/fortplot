module fortplot_raster_ticks
    !! Raster tick marks and tick labels rendering functionality
    !! Extracted from fortplot_raster_axes.f90 for single responsibility principle
    use fortplot_constants, only: TICK_MARK_LENGTH
    use fortplot_text_rendering, only: render_text_to_image, calculate_text_width, calculate_text_height
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_unicode, only: escape_unicode_for_raster
    use fortplot_text_helpers, only: prepare_mathtext_if_needed
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_line_styles, only: draw_styled_line
    use fortplot_raster_core, only: raster_image_t
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
    public :: raster_draw_y_axis_ticks_right
    public :: raster_draw_y_axis_tick_marks_only_right
    public :: raster_draw_y_axis_tick_labels_only_right
    public :: raster_draw_x_axis_ticks_top
    public :: raster_draw_x_axis_tick_marks_only_top
    public :: raster_draw_x_axis_tick_labels_only_top
    public :: last_y_tick_max_width
    public :: last_y_tick_max_width_right
    public :: last_x_tick_max_height_top
    public :: X_TICK_LABEL_PAD, Y_TICK_LABEL_RIGHT_PAD
    public :: Y_TICK_LABEL_LEFT_PAD, X_TICK_LABEL_TOP_PAD
    public :: compute_non_overlapping_mask

    ! Local spacing parameters for raster tick labels (pixels)
    ! X tick labels are positioned X_TICK_LABEL_PAD pixels below the tick end
    ! Y tick labels are right-aligned with a gap of Y_TICK_LABEL_RIGHT_PAD from the tick end
    integer, parameter :: X_TICK_LABEL_PAD = 20
    integer, parameter :: Y_TICK_LABEL_RIGHT_PAD = 10
    integer, parameter :: Y_TICK_LABEL_LEFT_PAD = 10
    integer, parameter :: X_TICK_LABEL_TOP_PAD = 20

    ! Cache the maximum Y-tick label width measured during the last
    ! raster_draw_y_axis_ticks() call so ylabel placement can avoid overlap
    integer :: last_y_tick_max_width = 0
    integer :: last_y_tick_max_width_right = 0
    integer :: last_x_tick_max_height_top = 0

contains

    subroutine raster_draw_x_axis_ticks(raster, width, height, plot_area, &
            xscale, symlog_threshold, xticks, xtick_labels, xtick_colors, x_min, x_max)
        !! Draw x-axis tick marks and labels
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: xticks(:)
        character(len=*), intent(in) :: xtick_labels(:)
        integer, intent(in) :: xtick_colors(:, :)
        real(wp), intent(in) :: x_min, x_max

        call raster_draw_x_axis_tick_marks_only(raster, width, height, plot_area, &
            xscale, symlog_threshold, xticks, xtick_colors, x_min, x_max)
        call raster_draw_x_axis_tick_labels_only(raster, width, height, plot_area, &
            xscale, symlog_threshold, xticks, xtick_labels, x_min, x_max)
    end subroutine raster_draw_x_axis_ticks

    subroutine raster_draw_y_axis_ticks(raster, width, height, plot_area, yscale, symlog_threshold, &
            yticks, ytick_labels, ytick_colors, y_min, y_max)
        !! Draw y-axis tick marks and labels
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: yticks(:)
        character(len=*), intent(in) :: ytick_labels(:)
        integer, intent(in) :: ytick_colors(:, :)
        real(wp), intent(in) :: y_min, y_max
        integer :: j
        integer :: label_width, label_height

        ! Track maximum label width for ylabel positioning
        last_y_tick_max_width = 0
        do j = 1, size(yticks)
            label_width = calculate_text_width(trim(ytick_labels(j)))
            last_y_tick_max_width = max(last_y_tick_max_width, label_width)
        end do

        call raster_draw_y_axis_tick_marks_only(raster, width, height, plot_area, yscale, symlog_threshold, &
            yticks, ytick_colors, y_min, y_max)
        call raster_draw_y_axis_tick_labels_only(raster, width, height, plot_area, yscale, symlog_threshold, &
            yticks, ytick_labels, y_min, y_max)
    end subroutine raster_draw_y_axis_ticks

    subroutine raster_draw_x_axis_tick_marks_only(raster, width, height, plot_area, &
            xscale, symlog_threshold, xticks, xtick_colors, x_min, x_max)
        !! Draw only x-axis tick marks (no labels)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: xticks(:)
        integer, intent(in) :: xtick_colors(:, :)
        real(wp), intent(in) :: x_min, x_max
        integer :: tick_x, tick_top, tick_bottom, j
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: dummy_pattern(1), pattern_dist

        ! Draw x-axis tick marks
        min_t = apply_scale_transform(x_min, xscale, symlog_threshold)
        max_t = apply_scale_transform(x_max, xscale, symlog_threshold)

        dummy_pattern = 0.0_wp
        pattern_dist = 0.0_wp

        do j = 1, size(xticks)
            tick_t = apply_scale_transform(xticks(j), xscale, symlog_threshold)
            if (max_t > min_t) then
                tick_x = plot_area%left + int((tick_t - min_t) / (max_t - min_t) * plot_area%width)
            else
                tick_x = plot_area%left
            end if
            tick_top = plot_area%bottom + plot_area%height
            tick_bottom = min(height, tick_top + TICK_MARK_LENGTH)
            call draw_styled_line(raster%image_data, width, height, &
                real(tick_x, wp), real(tick_top, wp), real(tick_x, wp), real(tick_bottom, wp), &
                real(xtick_colors(1,j),wp)/255.0_wp, real(xtick_colors(2,j),wp)/255.0_wp, &
                real(xtick_colors(3,j),wp)/255.0_wp, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
        end do
    end subroutine raster_draw_x_axis_tick_marks_only

    subroutine raster_draw_y_axis_tick_marks_only(raster, width, height, plot_area, yscale, symlog_threshold, &
            yticks, ytick_colors, y_min, y_max)
        !! Draw only y-axis tick marks (no labels)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: yticks(:)
        integer, intent(in) :: ytick_colors(:, :)
        real(wp), intent(in) :: y_min, y_max
        integer :: tick_y, tick_left, tick_right, j
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: dummy_pattern(1), pattern_dist

        ! Draw y-axis tick marks
        min_t = apply_scale_transform(y_min, yscale, symlog_threshold)
        max_t = apply_scale_transform(y_max, yscale, symlog_threshold)

        dummy_pattern = 0.0_wp
        pattern_dist = 0.0_wp

        do j = 1, size(yticks)
            tick_t = apply_scale_transform(yticks(j), yscale, symlog_threshold)
            if (max_t > min_t) then
                tick_y = plot_area%bottom + plot_area%height - int((tick_t - min_t) / (max_t - min_t) * &
                    plot_area%height)
            else
                tick_y = plot_area%bottom
            end if
            tick_left = max(1, plot_area%left - TICK_MARK_LENGTH)
            tick_right = plot_area%left
            call draw_styled_line(raster%image_data, width, height, &
                real(tick_left, wp), real(tick_y, wp), real(tick_right, wp), real(tick_y, wp), &
                real(ytick_colors(1,j),wp)/255.0_wp, real(ytick_colors(2,j),wp)/255.0_wp, &
                real(ytick_colors(3,j),wp)/255.0_wp, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
        end do
    end subroutine raster_draw_y_axis_tick_marks_only

    subroutine raster_draw_x_axis_tick_labels_only(raster, width, height, plot_area, &
            xscale, symlog_threshold, xticks, xtick_labels, x_min, x_max)
        !! Draw only x-axis tick labels (no marks)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: xticks(:)
        character(len=*), intent(in) :: xtick_labels(:)
        real(wp), intent(in) :: x_min, x_max
        logical, dimension(size(xticks)) :: visibility_mask
        integer :: tick_x, label_x, label_y, j
        integer :: label_width, label_height
        real(wp) :: min_t, max_t, tick_t
        character(len=500) :: processed_text
        character(len=600) :: math_ready
        character(len=600) :: escaped_text
        integer :: processed_len, math_len

        ! Draw x-axis tick labels with overlap prevention
        min_t = apply_scale_transform(x_min, xscale, symlog_threshold)
        max_t = apply_scale_transform(x_max, xscale, symlog_threshold)

        ! Compute which labels can be drawn without overlapping
        call compute_non_overlapping_mask(xticks, xtick_labels, x_min, x_max, xscale, &
            symlog_threshold, plot_area, visibility_mask)

        do j = 1, size(xticks)
            if (.not. visibility_mask(j)) cycle  ! Skip overlapping labels

            tick_t = apply_scale_transform(xticks(j), xscale, symlog_threshold)
            if (max_t > min_t) then
                tick_x = plot_area%left + int((tick_t - min_t) / (max_t - min_t) * plot_area%width)
            else
                tick_x = plot_area%left
            end if

            ! Process LaTeX (allocation handled internally)
            call process_latex_in_text(trim(xtick_labels(j)), processed_text, processed_len)
            call prepare_mathtext_if_needed(processed_text(1:processed_len), math_ready, math_len)
            call escape_unicode_for_raster(math_ready(1:math_len), escaped_text)

            label_width = calculate_text_width(trim(escaped_text))
            label_height = calculate_text_height(trim(escaped_text))

            label_x = tick_x - label_width / 2  ! Center horizontally at tick
            label_y = plot_area%bottom + plot_area%height + X_TICK_LABEL_PAD

            call render_text_to_image(raster%image_data, width, height, &
                label_x, label_y, trim(escaped_text), 0_1, 0_1, 0_1)
        end do
    end subroutine raster_draw_x_axis_tick_labels_only

    subroutine raster_draw_y_axis_tick_labels_only(raster, width, height, plot_area, yscale, symlog_threshold, &
            yticks, ytick_labels, y_min, y_max)
        !! Draw only y-axis tick labels (no marks)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: yticks(:)
        character(len=*), intent(in) :: ytick_labels(:)
        real(wp), intent(in) :: y_min, y_max
        integer :: tick_y, label_x, label_y, j
        integer :: label_width, label_height
        real(wp) :: min_t, max_t, tick_t
        character(len=500) :: processed_text
        character(len=600) :: math_ready
        character(len=600) :: escaped_text
        integer :: processed_len, math_len

        ! Draw y-axis tick labels
        min_t = apply_scale_transform(y_min, yscale, symlog_threshold)
        max_t = apply_scale_transform(y_max, yscale, symlog_threshold)

        do j = 1, size(yticks)
            tick_t = apply_scale_transform(yticks(j), yscale, symlog_threshold)
            if (max_t > min_t) then
                tick_y = plot_area%bottom + plot_area%height - int((tick_t - min_t) / (max_t - min_t) * &
                    plot_area%height)
            else
                tick_y = plot_area%bottom
            end if

            ! Process LaTeX (allocation handled internally)
            call process_latex_in_text(trim(ytick_labels(j)), processed_text, processed_len)
            call prepare_mathtext_if_needed(processed_text(1:processed_len), math_ready, math_len)
            call escape_unicode_for_raster(math_ready(1:math_len), escaped_text)

            label_width = calculate_text_width(trim(escaped_text))
            label_height = calculate_text_height(trim(escaped_text))
            ! If height calculation fails, use a default
            if (label_height <= 0) label_height = 12

            ! Right-align with a small gap from the tick end
            label_x = plot_area%left - Y_TICK_LABEL_RIGHT_PAD - label_width
            ! Center vertically at tick position - move DOWN for better alignment
            label_y = tick_y + label_height / 4  ! Move down from tick for better visual alignment

            call render_text_to_image(raster%image_data, width, height, &
                label_x, label_y, trim(escaped_text), 0_1, 0_1, 0_1)
        end do
    end subroutine raster_draw_y_axis_tick_labels_only

    subroutine raster_draw_y_axis_ticks_right(raster, width, height, plot_area, yscale, symlog_threshold, &
            yticks, ytick_labels, ytick_colors, y_min, y_max)
        !! Draw y-axis tick marks and labels along the right side
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: yticks(:)
        character(len=*), intent(in) :: ytick_labels(:)
        integer, intent(in) :: ytick_colors(:, :)
        real(wp), intent(in) :: y_min, y_max
        integer :: j
        integer :: label_width

        last_y_tick_max_width_right = 0
        do j = 1, size(yticks)
            label_width = calculate_text_width(trim(ytick_labels(j)))
            last_y_tick_max_width_right = max(last_y_tick_max_width_right, label_width)
        end do

        call raster_draw_y_axis_tick_marks_only_right(raster, width, height, plot_area, yscale, symlog_threshold, &
            yticks, ytick_colors, y_min, y_max)
        call raster_draw_y_axis_tick_labels_only_right(raster, width, height, plot_area, yscale, symlog_threshold, &
            yticks, ytick_labels, y_min, y_max)
    end subroutine raster_draw_y_axis_ticks_right

    subroutine raster_draw_y_axis_tick_marks_only_right(raster, width, height, plot_area, yscale, symlog_threshold, &
            yticks, ytick_colors, y_min, y_max)
        !! Draw y-axis tick marks on the right side (no labels)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: yticks(:)
        integer, intent(in) :: ytick_colors(:, :)
        real(wp), intent(in) :: y_min, y_max
        integer :: tick_y, tick_left, tick_right, j
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: dummy_pattern(1), pattern_dist

        min_t = apply_scale_transform(y_min, yscale, symlog_threshold)
        max_t = apply_scale_transform(y_max, yscale, symlog_threshold)

        dummy_pattern = 0.0_wp
        pattern_dist = 0.0_wp

        do j = 1, size(yticks)
            tick_t = apply_scale_transform(yticks(j), yscale, symlog_threshold)
            if (max_t > min_t) then
                tick_y = plot_area%bottom + plot_area%height - int((tick_t - min_t) / (max_t - min_t) * &
                    plot_area%height)
            else
                tick_y = plot_area%bottom
            end if
            tick_left = plot_area%left + plot_area%width
            tick_right = min(width, tick_left + TICK_MARK_LENGTH)
            call draw_styled_line(raster%image_data, width, height, &
                real(tick_left, wp), real(tick_y, wp), real(tick_right, wp), real(tick_y, wp), &
                real(ytick_colors(1,j),wp)/255.0_wp, real(ytick_colors(2,j),wp)/255.0_wp, &
                real(ytick_colors(3,j),wp)/255.0_wp, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
        end do
    end subroutine raster_draw_y_axis_tick_marks_only_right

    subroutine raster_draw_y_axis_tick_labels_only_right(raster, width, height, plot_area, yscale, symlog_threshold, &
            yticks, ytick_labels, y_min, y_max)
        !! Draw y-axis tick labels on the right side (no marks)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: yticks(:)
        character(len=*), intent(in) :: ytick_labels(:)
        real(wp), intent(in) :: y_min, y_max
        integer :: tick_y, label_x, label_y, j
        integer :: label_width, label_height
        real(wp) :: min_t, max_t, tick_t
        character(len=500) :: processed_text
        character(len=600) :: math_ready
        character(len=600) :: escaped_text
        integer :: processed_len, math_len

        min_t = apply_scale_transform(y_min, yscale, symlog_threshold)
        max_t = apply_scale_transform(y_max, yscale, symlog_threshold)

        do j = 1, size(yticks)
            tick_t = apply_scale_transform(yticks(j), yscale, symlog_threshold)
            if (max_t > min_t) then
                tick_y = plot_area%bottom + plot_area%height - int((tick_t - min_t) / (max_t - min_t) * &
                    plot_area%height)
            else
                tick_y = plot_area%bottom
            end if

            call process_latex_in_text(trim(ytick_labels(j)), processed_text, processed_len)
            call prepare_mathtext_if_needed(processed_text(1:processed_len), math_ready, math_len)
            call escape_unicode_for_raster(math_ready(1:math_len), escaped_text)

            label_width = calculate_text_width(trim(escaped_text))
            label_height = calculate_text_height(trim(escaped_text))
            if (label_height <= 0) label_height = 12

            label_x = plot_area%left + plot_area%width + Y_TICK_LABEL_LEFT_PAD
            label_y = tick_y + label_height / 4

            call render_text_to_image(raster%image_data, width, height, &
                label_x, label_y, trim(escaped_text), 0_1, 0_1, 0_1)
        end do
    end subroutine raster_draw_y_axis_tick_labels_only_right

    subroutine raster_draw_x_axis_ticks_top(raster, width, height, plot_area, xscale, symlog_threshold, &
            xticks, xtick_labels, xtick_colors, x_min, x_max)
        !! Draw x-axis tick marks and labels along the top side
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: xticks(:)
        character(len=*), intent(in) :: xtick_labels(:)
        integer, intent(in) :: xtick_colors(:, :)
        real(wp), intent(in) :: x_min, x_max
        integer :: j
        integer :: label_height

        last_x_tick_max_height_top = 0
        do j = 1, size(xticks)
            label_height = calculate_text_height(trim(xtick_labels(j)))
            last_x_tick_max_height_top = max(last_x_tick_max_height_top, label_height)
        end do

        call raster_draw_x_axis_tick_marks_only_top(raster, width, height, plot_area, xscale, symlog_threshold, &
            xticks, xtick_colors, x_min, x_max)
        call raster_draw_x_axis_tick_labels_only_top(raster, width, height, plot_area, xscale, symlog_threshold, &
            xticks, xtick_labels, x_min, x_max)
    end subroutine raster_draw_x_axis_ticks_top

    subroutine raster_draw_x_axis_tick_marks_only_top(raster, width, height, plot_area, &
            xscale, symlog_threshold, xticks, xtick_colors, x_min, x_max)
        !! Draw x-axis tick marks on the top side (no labels)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: xticks(:)
        integer, intent(in) :: xtick_colors(:, :)
        real(wp), intent(in) :: x_min, x_max
        integer :: tick_x, tick_top, tick_bottom, j
        real(wp) :: min_t, max_t, tick_t
        real(wp) :: dummy_pattern(1), pattern_dist

        min_t = apply_scale_transform(x_min, xscale, symlog_threshold)
        max_t = apply_scale_transform(x_max, xscale, symlog_threshold)

        dummy_pattern = 0.0_wp
        pattern_dist = 0.0_wp

        do j = 1, size(xticks)
            tick_t = apply_scale_transform(xticks(j), xscale, symlog_threshold)
            if (max_t > min_t) then
                tick_x = plot_area%left + int((tick_t - min_t) / (max_t - min_t) * plot_area%width)
            else
                tick_x = plot_area%left
            end if
            tick_top = max(1, plot_area%bottom - TICK_MARK_LENGTH)
            tick_bottom = plot_area%bottom
            call draw_styled_line(raster%image_data, width, height, &
                real(tick_x, wp), real(tick_top, wp), real(tick_x, wp), real(tick_bottom, wp), &
                real(xtick_colors(1,j),wp)/255.0_wp, real(xtick_colors(2,j),wp)/255.0_wp, &
                real(xtick_colors(3,j),wp)/255.0_wp, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
        end do
    end subroutine raster_draw_x_axis_tick_marks_only_top

    subroutine raster_draw_x_axis_tick_labels_only_top(raster, width, height, plot_area, xscale, symlog_threshold, &
            xticks, xtick_labels, x_min, x_max)
        !! Draw x-axis tick labels on the top side (no marks)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: xticks(:)
        character(len=*), intent(in) :: xtick_labels(:)
        real(wp), intent(in) :: x_min, x_max
        integer :: tick_x, label_x, label_y, j
        integer :: label_width, label_height
        real(wp) :: min_t, max_t, tick_t
        character(len=500) :: processed_text
        character(len=600) :: math_ready
        character(len=600) :: escaped_text
        integer :: processed_len, math_len

        min_t = apply_scale_transform(x_min, xscale, symlog_threshold)
        max_t = apply_scale_transform(x_max, xscale, symlog_threshold)

        do j = 1, size(xticks)
            tick_t = apply_scale_transform(xticks(j), xscale, symlog_threshold)
            if (max_t > min_t) then
                tick_x = plot_area%left + int((tick_t - min_t) / (max_t - min_t) * plot_area%width)
            else
                tick_x = plot_area%left
            end if

            call process_latex_in_text(trim(xtick_labels(j)), processed_text, processed_len)
            call prepare_mathtext_if_needed(processed_text(1:processed_len), math_ready, math_len)
            call escape_unicode_for_raster(math_ready(1:math_len), escaped_text)

            label_width = calculate_text_width(trim(escaped_text))
            label_height = calculate_text_height(trim(escaped_text))
            if (label_height <= 0) label_height = 12

            label_x = tick_x - label_width / 2
            label_y = max(1, plot_area%bottom - X_TICK_LABEL_TOP_PAD - label_height)

            call render_text_to_image(raster%image_data, width, height, &
                label_x, label_y, trim(escaped_text), 0_1, 0_1, 0_1)
        end do
    end subroutine raster_draw_x_axis_tick_labels_only_top

    subroutine compute_non_overlapping_mask(xticks, xtick_labels, x_min, x_max, xscale, &
            symlog_threshold, plot_area, visibility_mask)
        !! Compute which x-axis tick labels can be drawn without overlapping
        !! Always shows first and last labels, hides overlapping ones in between
        real(wp), intent(in) :: xticks(:)
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
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        integer :: min_gap
        integer :: last_visible_right
        
        n = size(xticks)
        if (n == 0) then
            return
        end if
        
        ! Minimum gap between labels (pixels) - increased for better readability
        min_gap = 8
        
        allocate(label_lefts(n), label_rights(n))
        
        ! Compute transformed bounds
        min_t = apply_scale_transform(x_min, xscale, symlog_threshold)
        max_t = apply_scale_transform(x_max, xscale, symlog_threshold)
        
        ! Calculate left and right edges of each label
        do j = 1, n
            tick_t = apply_scale_transform(xticks(j), xscale, symlog_threshold)
            if (max_t > min_t) then
                tick_x = plot_area%left + int((tick_t - min_t) / (max_t - min_t) * plot_area%width)
            else
                tick_x = plot_area%left
            end if
            
            ! Process LaTeX and calculate width
            call process_latex_in_text(trim(xtick_labels(j)), processed_text, processed_len)
            call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
            label_width = calculate_text_width(trim(escaped_text))
            
            ! Store label bounds (centered at tick)
            label_lefts(j) = tick_x - label_width / 2
            label_rights(j) = tick_x + label_width / 2
        end do
        
        ! Initialize all labels as invisible
        visibility_mask = .false.
        
        ! Always show first label
        if (n >= 1) then
            visibility_mask(1) = .true.
            last_visible_right = label_rights(1)
        end if
        
        ! Check middle labels
        do j = 2, n-1
            if (label_lefts(j) >= last_visible_right + min_gap) then
                ! This label doesn't overlap, show it
                visibility_mask(j) = .true.
                last_visible_right = label_rights(j)
            end if
        end do
        
        ! Always try to show last label if it doesn't overlap
        if (n >= 2) then
            if (.not. visibility_mask(n)) then
                if (label_lefts(n) >= last_visible_right + min_gap) then
                    visibility_mask(n) = .true.
                end if
            end if
        end if
        
        deallocate(label_lefts, label_rights)
    end subroutine compute_non_overlapping_mask

end module fortplot_raster_ticks
