module fortplot_raster_labels
    !! Raster axis labels (title, xlabel, ylabel) rendering functionality
    !! Extracted from fortplot_raster_axes.f90 for single responsibility principle
    use fortplot_constants, only: XLABEL_VERTICAL_OFFSET, TICK_MARK_LENGTH, &
                                  YLABEL_EXTRA_GAP, TITLE_VERTICAL_OFFSET, &
                                  REFERENCE_DPI, FALLBACK_LABEL_HEIGHT_PX, &
                                  MIN_LABEL_MARGIN_PX, CANVAS_EDGE_PADDING_PX
    use fortplot_text_rendering, only: render_text_to_image, calculate_text_width, &
                                       calculate_text_height, &
                                       calculate_text_descent, &
                                       calculate_text_width_with_size, &
                                       render_text_with_size, TITLE_FONT_SIZE
    use fortplot_text_fonts, only: get_font_ascent_ratio
    use fortplot_text_helpers, only: prepare_text_for_raster
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_core, only: raster_image_t, scale_px
    use fortplot_bitmap, only: render_text_to_bitmap, rotate_bitmap_90_ccw, &
                               rotate_bitmap_90_cw, composite_bitmap_to_raster
    use fortplot_raster_ticks, only: &
                                     Y_TICK_LABEL_RIGHT_PAD, &
                                     Y_TICK_LABEL_LEFT_PAD, X_TICK_LABEL_TOP_PAD, &
                                     X_TICK_LABEL_PAD
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_draw_axis_labels
    public :: raster_render_ylabel
    public :: raster_render_ylabel_right
    public :: raster_draw_top_xlabel
    public :: render_title_centered
    public :: render_title_centered_with_size
    public :: compute_title_position
    public :: compute_ylabel_x_pos
    public :: y_tick_label_right_edge_at_axis

contains

    subroutine raster_draw_axis_labels(raster, width, height, plot_area, title, &
                                       xlabel, ylabel)
        !! Draw all axis labels (title, xlabel, ylabel)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: title, xlabel, ylabel
        character(len=600) :: escaped_text
        integer :: label_x, label_y
        integer :: label_width, label_height

        ! Title at top
        if (len_trim(title) > 0) then
            call render_title_centered(raster, width, height, &
                plot_area, title, raster%config_title_font_size)
        end if

        ! X label at bottom
        if (len_trim(xlabel) > 0) then
            call prepare_text_for_raster(xlabel, escaped_text)
            label_width = calculate_text_width(trim(escaped_text))
            label_height = calculate_text_height(trim(escaped_text))
            label_x = plot_area%left + plot_area%width/2 - label_width/2
            ! Position xlabel below x-tick labels with measured clearance
            label_y = plot_area%bottom + plot_area%height + &
                      scale_px(X_TICK_LABEL_PAD, raster%dpi) + &
                      max(raster%last_x_tick_max_height_bottom, FALLBACK_LABEL_HEIGHT_PX) + &
                      scale_px(XLABEL_VERTICAL_OFFSET, raster%dpi)/3
            label_y = min(label_y, height - label_height - CANVAS_EDGE_PADDING_PX)
            call render_text_to_image(raster%image_data, width, height, &
                                      label_x, label_y, &
                                      trim(escaped_text), 0_1, 0_1, 0_1)
        end if

        ! Y label at left
        if (len_trim(ylabel) > 0) then
            call raster_render_ylabel(raster, width, height, plot_area, ylabel)
        end if
    end subroutine raster_draw_axis_labels

    subroutine raster_render_ylabel(raster, width, height, plot_area, ylabel)
        !! Render rotated ylabel to the left of y-axis
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: ylabel
        character(len=600) :: escaped_text
        integer(1), allocatable :: text_bitmap(:, :, :), rotated_bitmap(:, :, :)
        integer :: text_width, text_height, text_descent
        integer :: rotated_width, rotated_height
        integer :: target_x, target_y
        integer :: y_tick_label_edge

        if (len_trim(ylabel) == 0) return

        call prepare_text_for_raster(ylabel, escaped_text)

        text_width = calculate_text_width(trim(escaped_text))
        text_height = calculate_text_height(trim(escaped_text))
        text_descent = calculate_text_descent(trim(escaped_text))

        ! Allocate text bitmap
        allocate (text_bitmap(text_width, text_height, 3))
        text_bitmap = -1_1  ! Initialize to white

        ! Render text to bitmap (upright).
        ! Position baseline to leave room for descenders.
        call render_text_to_bitmap(text_bitmap, text_width, text_height, 0, &
                                   text_height - text_descent, &
                                   trim(escaped_text))

        ! Rotate 90 degrees counter-clockwise
        rotated_width = text_height
        rotated_height = text_width
        allocate (rotated_bitmap(rotated_width, rotated_height, 3))
        call rotate_bitmap_90_ccw(text_bitmap, rotated_bitmap, text_width, text_height)

        ! Compute the rightmost edge of y-tick labels
        y_tick_label_edge = y_tick_label_right_edge_at_axis(plot_area, &
                                                            raster%last_y_tick_max_width, &
                                                            raster%dpi)

        ! Compute ylabel position with dynamic gap
        target_x = compute_ylabel_x_pos(y_tick_label_edge, rotated_width, raster%dpi)

        ! Center vertically in plot area
        target_y = plot_area%bottom + plot_area%height/2 - rotated_height/2

        ! Composite to raster
        call composite_bitmap_to_raster(raster%image_data, width, height, &
                                        rotated_bitmap, &
                                        rotated_width, rotated_height, &
                                        target_x, target_y)

        deallocate (text_bitmap, rotated_bitmap)
    end subroutine raster_render_ylabel

    integer function y_tick_label_left_edge_at_axis(plot_area, max_width_measured, dpi)
        !! Compute the leftmost edge of right-side y-tick labels relative to the axis
        use, intrinsic :: iso_fortran_env, only: wp => real64
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: max_width_measured
        real(wp), intent(in), optional :: dpi
        real(wp) :: dpi_val
        dpi_val = REFERENCE_DPI
        if (present(dpi)) dpi_val = dpi

        y_tick_label_left_edge_at_axis = plot_area%left + plot_area%width + &
                                         scale_px(TICK_MARK_LENGTH, dpi_val) + &
                                         scale_px(Y_TICK_LABEL_LEFT_PAD, dpi_val)
    end function y_tick_label_left_edge_at_axis

    integer function compute_ylabel_right_x_pos(y_tick_label_edge, rotated_width, &
                                                plot_area, canvas_width, dpi)
        !! Compute x-position for right-side ylabel avoiding overlap with tick labels
        use, intrinsic :: iso_fortran_env, only: wp => real64
        integer, intent(in) :: y_tick_label_edge
        integer, intent(in) :: rotated_width
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: canvas_width
        real(wp), intent(in), optional :: dpi
        real(wp) :: dpi_val
        dpi_val = REFERENCE_DPI
        if (present(dpi)) dpi_val = dpi

        compute_ylabel_right_x_pos = y_tick_label_edge + &
                                     scale_px(YLABEL_EXTRA_GAP, dpi_val)
        if (compute_ylabel_right_x_pos + rotated_width > canvas_width - MIN_LABEL_MARGIN_PX) then
            compute_ylabel_right_x_pos = max(plot_area%left + plot_area%width + CANVAS_EDGE_PADDING_PX, &
                                             canvas_width - rotated_width - MIN_LABEL_MARGIN_PX)
        end if
    end function compute_ylabel_right_x_pos

    subroutine raster_render_ylabel_right(raster, width, height, plot_area, ylabel)
        !! Render rotated ylabel along the right side of the axis
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: ylabel
        character(len=600) :: escaped_text
        integer(1), allocatable :: text_bitmap(:, :, :), rotated_bitmap(:, :, :)
        integer :: text_width, text_height, text_descent
        integer :: rotated_width, rotated_height
        integer :: target_x, target_y
        integer :: y_tick_label_edge

        if (len_trim(ylabel) == 0) return

        call prepare_text_for_raster(ylabel, escaped_text)

        text_width = calculate_text_width(trim(escaped_text))
        text_height = calculate_text_height(trim(escaped_text))
        text_descent = calculate_text_descent(trim(escaped_text))

        allocate (text_bitmap(text_width, text_height, 3))
        text_bitmap = -1_1

        call render_text_to_bitmap(text_bitmap, text_width, text_height, 0, &
                                   text_height - text_descent, &
                                   trim(escaped_text))

        rotated_width = text_height
        rotated_height = text_width
        allocate (rotated_bitmap(rotated_width, rotated_height, 3))
        call rotate_bitmap_90_cw(text_bitmap, rotated_bitmap, text_width, text_height)

        y_tick_label_edge = y_tick_label_left_edge_at_axis(plot_area, &
                                                           raster%last_y_tick_max_width_right, &
                                                           raster%dpi)
        target_x = compute_ylabel_right_x_pos(y_tick_label_edge, rotated_width, &
                                              plot_area, width, raster%dpi)
        target_y = plot_area%bottom + plot_area%height/2 - rotated_height/2

        call composite_bitmap_to_raster(raster%image_data, width, height, &
                                        rotated_bitmap, &
                                        rotated_width, rotated_height, &
                                        target_x, target_y)

        deallocate (text_bitmap, rotated_bitmap)
    end subroutine raster_render_ylabel_right

    integer function y_tick_label_right_edge_at_axis(plot_area, max_width_measured, &
                                                    dpi)
        !! Compute the rightmost edge of y-tick labels relative to the y-axis
        use, intrinsic :: iso_fortran_env, only: wp => real64
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: max_width_measured
        real(wp), intent(in), optional :: dpi
        real(wp) :: dpi_val
        dpi_val = REFERENCE_DPI
        if (present(dpi)) dpi_val = dpi

        y_tick_label_right_edge_at_axis = plot_area%left - &
                                          scale_px(TICK_MARK_LENGTH, dpi_val) - &
                                          scale_px(Y_TICK_LABEL_RIGHT_PAD, dpi_val) - &
                                          max(0, max_width_measured)
    end function y_tick_label_right_edge_at_axis

    integer function compute_ylabel_x_pos(y_tick_label_edge, rotated_width, dpi)
        !! Compute x-position for ylabel to avoid overlapping with y-tick labels
        use, intrinsic :: iso_fortran_env, only: wp => real64
        integer, intent(in) :: y_tick_label_edge
        integer, intent(in) :: rotated_width
        real(wp), intent(in), optional :: dpi

        integer :: min_left_margin
        integer :: ideal_x
        integer :: safe_x
        real(wp) :: dpi_val
        dpi_val = REFERENCE_DPI
        if (present(dpi)) dpi_val = dpi

        min_left_margin = max(MIN_LABEL_MARGIN_PX, rotated_width/4)

        ideal_x = y_tick_label_edge - scale_px(YLABEL_EXTRA_GAP, dpi_val) - &
                  rotated_width

        ! If tick labels are already off-canvas, favor keeping the label visible.
        if (y_tick_label_edge <= 0) then
            compute_ylabel_x_pos = max(min_left_margin, ideal_x)
            return
        end if

        if (ideal_x >= min_left_margin) then
            compute_ylabel_x_pos = ideal_x
            return
        end if

        ! If there is not enough room to keep both a minimum margin and the
        ! extra gap, drop the extra gap first; if still no room, allow the
        ! ylabel to extend off-canvas to avoid overlapping tick labels.
        safe_x = y_tick_label_edge - rotated_width - 1
        if (safe_x >= min_left_margin) then
            compute_ylabel_x_pos = min_left_margin
        else
            compute_ylabel_x_pos = safe_x
        end if
    end function compute_ylabel_x_pos

    integer function compute_top_xlabel_y_pos(raster, plot_area, label_height, dpi)
        !! Compute y-position for an x-label rendered above the axis
        use, intrinsic :: iso_fortran_env, only: wp => real64
        type(raster_image_t), intent(in) :: raster
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: label_height
        real(wp), intent(in), optional :: dpi
        real(wp) :: dpi_val
        dpi_val = REFERENCE_DPI
        if (present(dpi)) dpi_val = dpi

        compute_top_xlabel_y_pos = max(1, plot_area%bottom - &
                                       scale_px(X_TICK_LABEL_TOP_PAD, dpi_val) - &
                                       raster%last_x_tick_max_height_top - label_height - CANVAS_EDGE_PADDING_PX)
    end function compute_top_xlabel_y_pos

    subroutine raster_draw_top_xlabel(raster, width, height, plot_area, xlabel)
        !! Render an xlabel centered above the plot area
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: xlabel
        character(len=600) :: escaped_text
        integer :: label_width, label_height
        integer :: label_x, label_y

        if (len_trim(xlabel) == 0) return

        call prepare_text_for_raster(xlabel, escaped_text)

        label_width = calculate_text_width(trim(escaped_text))
        label_height = calculate_text_height(trim(escaped_text))
        if (label_height <= 0) label_height = FALLBACK_LABEL_HEIGHT_PX

        label_x = plot_area%left + plot_area%width/2 - label_width/2
        label_y = compute_top_xlabel_y_pos(raster, plot_area, label_height, raster%dpi)

        call render_text_to_image(raster%image_data, width, height, label_x, label_y, &
                                  trim(escaped_text), 0_1, 0_1, 0_1)
    end subroutine raster_draw_top_xlabel

    subroutine render_title_centered(raster, width, height, plot_area, &
                                     title_text, custom_font_size)
        !! Render title centered above the plot area
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: title_text
        real(wp), intent(in), optional :: custom_font_size
        character(len=600) :: escaped_text
        integer :: title_px, title_py
        real(wp) :: title_px_real, title_py_real, fsize

        if (len_trim(title_text) == 0) return

        fsize = real(TITLE_FONT_SIZE, wp)
        if (present(custom_font_size)) then
            if (custom_font_size > 0.0_wp) fsize = custom_font_size
        end if

        call compute_title_position_sized(plot_area, title_text, &
            escaped_text, title_px_real, title_py_real, &
            fsize, raster%dpi)

        title_px = int(title_px_real)
        title_py = int(title_py_real)

        call render_text_with_size(raster%image_data, width, height, &
                                   title_px, title_py, &
                                   trim(escaped_text), 0_1, 0_1, 0_1, &
                                   fsize)
    end subroutine render_title_centered

    subroutine compute_title_position(plot_area, title_text, escaped_text, &
                                      title_px, title_py, dpi)
        !! Compute the position for centered title above plot area
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: title_text
        character(len=*), intent(out) :: escaped_text
        real(wp), intent(out) :: title_px, title_py
        real(wp), intent(in), optional :: dpi

        call compute_title_position_sized(plot_area, title_text, &
            escaped_text, title_px, title_py, real(TITLE_FONT_SIZE, wp), dpi)
    end subroutine compute_title_position

    subroutine compute_title_position_sized(plot_area, title_text, &
                                            escaped_text, title_px, &
                                            title_py, fsize, dpi)
        !! Compute centered title position with explicit font size.
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: title_text
        character(len=*), intent(out) :: escaped_text
        real(wp), intent(out) :: title_px, title_py
        real(wp), intent(in) :: fsize
        real(wp), intent(in), optional :: dpi
        integer :: title_width

        call prepare_text_for_raster(title_text, escaped_text)

        title_width = calculate_text_width_with_size( &
            trim(escaped_text), fsize)

        title_px = real(plot_area%left + plot_area%width / 2 &
                        - title_width / 2, wp)
        title_py = real(plot_area%bottom - TITLE_VERTICAL_OFFSET, wp)
        title_py = max(1.0_wp, title_py)
    end subroutine compute_title_position_sized

    subroutine render_title_centered_with_size(raster, width, height, center_x, &
                                               title_y, title_text, font_scale)
        !! Render title centered at specified position with custom font scale
        !! Used for suptitle rendering above subplots
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        integer, intent(in) :: center_x, title_y
        character(len=*), intent(in) :: title_text
        real(wp), intent(in) :: font_scale
        character(len=600) :: escaped_text
        integer :: title_width, title_px
        real(wp) :: scaled_font_size

        if (len_trim(title_text) == 0) return

        call prepare_text_for_raster(title_text, escaped_text)

        scaled_font_size = real(TITLE_FONT_SIZE, wp) * font_scale
        title_width = calculate_text_width_with_size(trim(escaped_text), &
                                                     scaled_font_size)
        title_px = center_x - title_width / 2

        call render_text_with_size(raster%image_data, width, height, title_px, &
                                   title_y, trim(escaped_text), 0_1, 0_1, 0_1, &
                                   scaled_font_size)
    end subroutine render_title_centered_with_size

end module fortplot_raster_labels
