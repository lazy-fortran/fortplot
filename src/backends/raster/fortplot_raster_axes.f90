module fortplot_raster_axes
    !! Raster axes and labels rendering functionality
    !! Extracted from fortplot_raster.f90 for size reduction (SRP compliance)
    use fortplot_constants, only: TICK_MARK_LENGTH, XLABEL_VERTICAL_OFFSET, TITLE_VERTICAL_OFFSET
    use fortplot_text, only: render_text_to_image, calculate_text_width, calculate_text_height
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_unicode, only: escape_unicode_for_raster
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_line_styles, only: draw_styled_line
    use fortplot_raster_core, only: raster_image_t
    use fortplot_bitmap, only: render_text_to_bitmap, rotate_bitmap_90_ccw, composite_bitmap_to_raster
    use fortplot_scales, only: apply_scale_transform
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_draw_axes_and_labels, raster_render_ylabel
    public :: compute_title_position
    public :: compute_non_overlapping_mask  ! Exposed for focused testing
    public :: compute_ylabel_x_pos, y_tick_label_right_edge_at_axis
    public :: map_value_to_plot_x, map_value_to_plot_y

    ! Local spacing parameters for raster tick labels (pixels)
    ! X tick labels are positioned X_TICK_LABEL_PAD pixels below the tick end
    ! Y tick labels are right-aligned with a gap of Y_TICK_LABEL_RIGHT_PAD from the tick end
    integer, parameter :: X_TICK_LABEL_PAD = 14
    integer, parameter :: Y_TICK_LABEL_RIGHT_PAD = 16
    integer, parameter :: YLABEL_EXTRA_GAP = 10

    ! Cache the maximum Y-tick label width measured during the last
    ! raster_draw_y_axis_ticks() call so ylabel placement can avoid overlap
    integer :: last_y_tick_max_width = 0

contains

    real(wp) function map_value_to_plot_x(value, data_min, data_max, plot_area, scale, symlog_threshold) result(px)
        !! Map a data value to pixel X coordinate using axis scale
        real(wp), intent(in) :: value, data_min, data_max
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: scale
        real(wp), intent(in) :: symlog_threshold
        real(wp) :: v_t, min_t, max_t

        min_t = apply_scale_transform(data_min, scale, symlog_threshold)
        max_t = apply_scale_transform(data_max, scale, symlog_threshold)
        v_t   = apply_scale_transform(value,    scale, symlog_threshold)

        if (max_t > min_t) then
            px = real(plot_area%left, wp) + (v_t - min_t) / (max_t - min_t) * real(plot_area%width, wp)
        else
            px = real(plot_area%left, wp) + 0.5_wp * real(plot_area%width, wp)
        end if
    end function map_value_to_plot_x

    real(wp) function map_value_to_plot_y(value, data_min, data_max, plot_area, scale, symlog_threshold) result(py)
        !! Map a data value to pixel Y coordinate using axis scale
        !! Raster coordinates have Y increasing downward; account for that here.
        real(wp), intent(in) :: value, data_min, data_max
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: scale
        real(wp), intent(in) :: symlog_threshold
        real(wp) :: v_t, min_t, max_t

        min_t = apply_scale_transform(data_min, scale, symlog_threshold)
        max_t = apply_scale_transform(data_max, scale, symlog_threshold)
        v_t   = apply_scale_transform(value,    scale, symlog_threshold)

        if (max_t > min_t) then
            py = real(plot_area%bottom + plot_area%height, wp) - &
                 (v_t - min_t) / (max_t - min_t) * real(plot_area%height, wp)
        else
            py = real(plot_area%bottom, wp) + 0.5_wp * real(plot_area%height, wp)
        end if
    end function map_value_to_plot_y

    pure subroutine compute_non_overlapping_mask(centers, widths, min_gap, keep)
        !! Greedy selection to avoid horizontal label overlap
        !! centers: label center positions (pixels)
        !! widths: label widths (pixels)
        !! min_gap: minimum gap between labels (pixels)
        !! keep: output mask of which labels to draw
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
            left_i  = centers(i) - 0.5_wp * real(widths(i), wp)
            right_i = centers(i) + 0.5_wp * real(widths(i), wp)
            if (left_i >= last_right + gap) then
                keep(i) = .true.
                last_right = right_i
            end if
        end do
    end subroutine compute_non_overlapping_mask

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
        call draw_raster_axes_lines(raster, width, height, plot_area, x_min, x_max, y_min, y_max)
        
        ! Draw tick marks and labels
        call raster_draw_x_axis_ticks(raster, width, height, plot_area, xscale, symlog_threshold, &
                                     x_min, x_max, y_min, y_max)
        call raster_draw_y_axis_ticks(raster, width, height, plot_area, yscale, symlog_threshold, &
                                     x_min, x_max, y_min, y_max)
        
        ! Draw labels and title
        call raster_draw_axis_labels(raster, width, height, plot_area, title, xlabel, ylabel)
    end subroutine raster_draw_axes_and_labels

    subroutine draw_raster_axes_lines(raster, width, height, plot_area, x_min, x_max, y_min, y_max)
        !! Draw main axes lines
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        real(wp) :: line_r, line_g, line_b
        real(wp) :: dummy_pattern(1), pattern_dist
        real(wp) :: x_bottom_left, y_bottom_left, x_bottom_right, y_bottom_right
        real(wp) :: x_top_left, y_top_left
        
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
                             x_bottom_left, y_bottom_left, x_bottom_right, y_bottom_right, &
                             line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
        
        ! Draw left axis (Y axis)
        call draw_styled_line(raster%image_data, width, height, &
                             x_bottom_left, y_bottom_left, x_top_left, y_top_left, &
                             line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
    end subroutine draw_raster_axes_lines
    
    subroutine raster_draw_x_axis_ticks(raster, width, height, plot_area, xscale, symlog_threshold, &
                                       x_min, x_max, y_min, y_max)
        !! Draw X-axis tick marks and labels
        use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
        use fortplot_tick_calculation, only: determine_decimal_places_from_step, &
            format_tick_value_consistent
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        real(wp) :: x_tick_positions(MAX_TICKS)
        integer :: num_x_ticks, i
        character(len=50) :: tick_label
        real(wp) :: tick_x
        integer :: tick_length, px, py, text_width
        real(wp) :: line_r, line_g, line_b
        integer(1) :: text_r, text_g, text_b
        real(wp) :: dummy_pattern(1), pattern_dist
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        ! For overlap guard
        real(wp), allocatable :: centers(:)
        integer, allocatable :: widths(:)
        logical, allocatable :: keep(:)
        ! Use generous buffer to avoid rare truncation of escaped labels
        character(len=500), allocatable :: labels(:)
        integer :: decimals
        real(wp) :: step
        
        line_r = 0.0_wp; line_g = 0.0_wp; line_b = 0.0_wp  ! Black color
        text_r = 0; text_g = 0; text_b = 0
        tick_length = TICK_MARK_LENGTH
        
        call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, x_tick_positions, num_x_ticks)

        if (num_x_ticks > 0) then
            allocate(centers(num_x_ticks), widths(num_x_ticks), keep(num_x_ticks), labels(num_x_ticks))

            ! Determine decimals for linear scale based on tick spacing
            decimals = 0
            if (trim(xscale) == 'linear' .and. num_x_ticks >= 2) then
                step = abs(x_tick_positions(2) - x_tick_positions(1))
                do i = 3, num_x_ticks
                    if (abs(x_tick_positions(i) - x_tick_positions(i-1)) > 1.0e-12_wp) then
                        step = min(step, abs(x_tick_positions(i) - x_tick_positions(i-1)))
                    end if
                end do
                decimals = determine_decimal_places_from_step(step)
            end if

            do i = 1, num_x_ticks
                tick_x = x_tick_positions(i)
                centers(i) = map_value_to_plot_x(tick_x, x_min, x_max, plot_area, xscale, symlog_threshold)
                if (trim(xscale) == 'linear') then
                    tick_label = format_tick_value_consistent(tick_x, decimals)
                else
                    tick_label = format_tick_label(tick_x, xscale)
                end if
                call process_latex_in_text(trim(tick_label), processed_text, processed_len)
                call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
                labels(i) = trim(escaped_text)
                widths(i) = max(0, calculate_text_width(labels(i)))
            end do

            call compute_non_overlapping_mask(centers, widths, 2.0_wp, keep)

            do i = 1, num_x_ticks
                px = int(centers(i))
                py = plot_area%bottom + plot_area%height

                ! Draw tick mark always
                dummy_pattern = 0.0_wp
                pattern_dist = 0.0_wp
                call draw_styled_line(raster%image_data, width, height, &
                                     real(px, wp), real(py, wp), real(px, wp), real(py + tick_length, wp), &
                                     line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)

                if (keep(i)) then
                    text_width = widths(i)
                    call render_text_to_image(raster%image_data, width, height, &
                                             px - text_width/2, &
                                             py + tick_length + X_TICK_LABEL_PAD, &
                                             labels(i), text_r, text_g, text_b)
                end if
            end do

            if (allocated(centers)) deallocate(centers)
            if (allocated(widths)) deallocate(widths)
            if (allocated(keep)) deallocate(keep)
            if (allocated(labels)) deallocate(labels)
        end if
    end subroutine raster_draw_x_axis_ticks
    
    subroutine raster_draw_y_axis_ticks(raster, width, height, plot_area, yscale, symlog_threshold, &
                                       x_min, x_max, y_min, y_max)
        !! Draw Y-axis tick marks and labels
        use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
        use fortplot_tick_calculation, only: determine_decimal_places_from_step, &
            format_tick_value_consistent
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        real(wp) :: y_tick_positions(MAX_TICKS)
        integer :: num_y_ticks, i
        character(len=50) :: tick_label
        real(wp) :: tick_y
        integer :: tick_length, px, py, text_width, text_height
        integer :: max_label_width
        real(wp) :: line_r, line_g, line_b
        integer(1) :: text_r, text_g, text_b
        real(wp) :: dummy_pattern(1), pattern_dist
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        integer :: decimals
        real(wp) :: step
        
        line_r = 0.0_wp; line_g = 0.0_wp; line_b = 0.0_wp  ! Black color
        text_r = 0; text_g = 0; text_b = 0
        tick_length = TICK_MARK_LENGTH
        
        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, y_tick_positions, num_y_ticks)
        ! Determine decimals for linear scale based on tick spacing
        decimals = 0
        if (trim(yscale) == 'linear' .and. num_y_ticks >= 2) then
            step = abs(y_tick_positions(2) - y_tick_positions(1))
            do i = 3, num_y_ticks
                if (abs(y_tick_positions(i) - y_tick_positions(i-1)) > 1.0e-12_wp) then
                    step = min(step, abs(y_tick_positions(i) - y_tick_positions(i-1)))
                end if
            end do
            decimals = determine_decimal_places_from_step(step)
        end if
        max_label_width = 0
        do i = 1, num_y_ticks
            tick_y = y_tick_positions(i)
            px = plot_area%left
            py = int(map_value_to_plot_y(tick_y, y_min, y_max, plot_area, yscale, symlog_threshold))
            
            ! Draw tick mark
            dummy_pattern = 0.0_wp
            pattern_dist = 0.0_wp
            call draw_styled_line(raster%image_data, width, height, &
                                 real(px - tick_length, wp), real(py, wp), real(px, wp), real(py, wp), &
                                 line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
            
            ! Draw tick label
            if (trim(yscale) == 'linear') then
                tick_label = format_tick_value_consistent(tick_y, decimals)
            else
                tick_label = format_tick_label(tick_y, yscale)
            end if
            call process_latex_in_text(trim(tick_label), processed_text, processed_len)
            call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
            text_width = calculate_text_width(trim(escaped_text))
            text_height = calculate_text_height(trim(escaped_text))
            if (text_width > max_label_width) max_label_width = text_width
            ! Right-align y-tick label so its right edge sits (TICK_MARK_LENGTH + Y_TICK_LABEL_RIGHT_PAD)
            ! pixels left of the plot area
            call render_text_to_image(raster%image_data, width, height, &
                                     px - tick_length - text_width - Y_TICK_LABEL_RIGHT_PAD, &
                                     py - text_height/2, trim(escaped_text), &
                                     text_r, text_g, text_b)
        end do
        ! Persist the widest y-tick label width for ylabel placement
        last_y_tick_max_width = max_label_width
    end subroutine raster_draw_y_axis_ticks
    
    subroutine raster_draw_axis_labels(raster, width, height, plot_area, title, xlabel, ylabel)
        !! Draw title, xlabel, and ylabel
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        
        integer :: px, py, text_width, text_height
        integer(1) :: text_r, text_g, text_b
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        
        text_r = 0; text_g = 0; text_b = 0  ! Black text
        
        ! Draw title
        if (present(title)) then
            if (allocated(title)) then
                call render_title_centered(raster, width, height, plot_area, title)
            end if
        end if
        
        ! Draw xlabel
        if (present(xlabel)) then
            if (allocated(xlabel)) then
                call process_latex_in_text(xlabel, processed_text, processed_len)
                call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
                text_width = calculate_text_width(trim(escaped_text))
                px = plot_area%left + plot_area%width / 2 - text_width / 2
                py = plot_area%bottom + plot_area%height + XLABEL_VERTICAL_OFFSET
                call render_text_to_image(raster%image_data, width, height, &
                                        px, py, trim(escaped_text), text_r, text_g, text_b)
            end if
        end if
        
        ! Draw ylabel (rotated) - delegated to specialized routine
        if (present(ylabel)) then
            if (allocated(ylabel)) then
                call raster_render_ylabel(raster, width, height, plot_area, ylabel)
            end if
        end if
    end subroutine raster_draw_axis_labels

    subroutine raster_render_ylabel(raster, width, height, plot_area, ylabel)
        !! Render rotated Y-axis label for raster backend
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: ylabel
        
        integer :: text_width, text_height
        integer :: rotated_width, rotated_height
        integer :: x_pos, y_pos
        integer(1), allocatable :: text_bitmap(:,:,:), rotated_bitmap(:,:,:)
        
        ! Calculate text dimensions
        text_width = calculate_text_width(ylabel)
        text_height = calculate_text_height(ylabel)
        
        ! Allocate bitmap for horizontal text
        allocate(text_bitmap(text_width, text_height, 3))
        text_bitmap = -1_1  ! Initialize to white
        
        ! Render text horizontally to bitmap. The text renderer expects
        ! the y coordinate to be the text baseline. Use `text_height` as
        ! the baseline so the full glyphs are drawn inside the bitmap
        ! before rotation (prevents top-row clipping that caused the
        ! ylabel to appear partially erased/white in PNG outputs).
        call render_text_to_bitmap(text_bitmap, text_width, text_height, 1, text_height, ylabel)
        
        ! Allocate rotated bitmap (dimensions swapped for 90° rotation)
        rotated_width = text_height
        rotated_height = text_width
        allocate(rotated_bitmap(rotated_width, rotated_height, 3))
        
        ! Rotate the text 90 degrees counter-clockwise
        call rotate_bitmap_90_ccw(text_bitmap, rotated_bitmap, text_width, text_height)
        
        ! Calculate position for rotated text (left of plot area, centered vertically)
        ! Place ylabel to the left of the widest y-tick label plus a small gap,
        ! also accounting for the tick mark length and label padding. This mimics
        ! matplotlib by ensuring no overlap between ylabel and tick labels.
        x_pos = compute_ylabel_x_pos(plot_area, rotated_width, last_y_tick_max_width)
        y_pos = plot_area%bottom + plot_area%height / 2 - rotated_height / 2
        
        ! Composite the rotated text onto the main raster
        call composite_bitmap_to_raster(raster%image_data, width, height, &
                                       rotated_bitmap, rotated_width, rotated_height, &
                                       x_pos, y_pos)
        
        ! Clean up
        if (allocated(text_bitmap)) deallocate(text_bitmap)
        if (allocated(rotated_bitmap)) deallocate(rotated_bitmap)
    end subroutine raster_render_ylabel

    pure function compute_ylabel_x_pos(plot_area, rotated_text_width, y_tick_max_width) result(x_pos)
        !! Compute x position for ylabel such that it clears tick labels
        !! plot_area: geometry of plotting area
        !! rotated_text_width: width of the rotated ylabel bitmap (pixels)
        !! y_tick_max_width: maximum width among y-tick labels (pixels)
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: rotated_text_width
        integer, intent(in) :: y_tick_max_width
        integer :: x_pos

        integer :: clearance

        ! Place the RIGHT edge of the rotated ylabel at a fixed clearance
        ! from the y-tick label right edge. Since composite uses top-left
        ! anchoring, subtract the full rotated_text_width here.
        clearance = TICK_MARK_LENGTH + Y_TICK_LABEL_RIGHT_PAD + max(0, y_tick_max_width) + YLABEL_EXTRA_GAP
        x_pos = plot_area%left - clearance - rotated_text_width
    end function compute_ylabel_x_pos

    pure function y_tick_label_right_edge_at_axis(plot_area) result(r_edge)
        !! Right-most x coordinate of a y-tick label placed at the left axis tick
        !! This matches raster_draw_y_axis_ticks alignment for the label whose
        !! tick is at px = plot_area%left.
        type(plot_area_t), intent(in) :: plot_area
        integer :: r_edge

        r_edge = plot_area%left - TICK_MARK_LENGTH - Y_TICK_LABEL_RIGHT_PAD
    end function y_tick_label_right_edge_at_axis

    subroutine render_title_centered(raster, width, height, plot_area, title_text)
        !! Render title centered horizontally over plot area (matplotlib-style positioning)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: title_text
        
        real(wp) :: title_px, title_py
        integer(1) :: r, g, b
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        
        ! Compute title position (centered like matplotlib)
        call compute_title_position(plot_area, title_text, processed_text, processed_len, escaped_text, title_px, title_py)
        
        ! Get current color and render title directly in pixel coordinates
        call raster%get_color_bytes(r, g, b)
        call render_text_to_image(raster%image_data, width, height, &
                                 int(title_px), int(title_py), &
                                 trim(escaped_text), r, g, b)
    end subroutine render_title_centered

    subroutine compute_title_position(plot_area, title_text, processed_text, processed_len, escaped_text, title_px, title_py)
        !! Compute centered title position and return processed/escaped text
        !! Exposed for reuse and testing; keeps render routine small and focused
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: title_text
        character(len=*), intent(out) :: processed_text
        integer, intent(out) :: processed_len
        character(len=*), intent(out) :: escaped_text
        real(wp), intent(out) :: title_px, title_py

        integer :: text_width

        call process_latex_in_text(title_text, processed_text, processed_len)
        call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)

        text_width = calculate_text_width(trim(escaped_text))
        title_px = real(plot_area%left + plot_area%width / 2 - text_width / 2, wp)
        title_py = real(plot_area%bottom - TITLE_VERTICAL_OFFSET, wp)
    end subroutine compute_title_position

end module fortplot_raster_axes
