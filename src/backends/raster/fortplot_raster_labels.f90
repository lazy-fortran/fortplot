module fortplot_raster_labels
    !! Raster axis labels (title, xlabel, ylabel) rendering functionality
    !! Extracted from fortplot_raster_axes.f90 for single responsibility principle
    use fortplot_constants, only: XLABEL_VERTICAL_OFFSET, TITLE_VERTICAL_OFFSET, TICK_MARK_LENGTH
    use fortplot_text_rendering, only: render_text_to_image, calculate_text_width, calculate_text_height, calculate_text_descent
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_unicode, only: escape_unicode_for_raster
    use fortplot_margins, only: plot_area_t
    use fortplot_raster_core, only: raster_image_t
    use fortplot_bitmap, only: render_text_to_bitmap, rotate_bitmap_90_ccw, composite_bitmap_to_raster
    use fortplot_raster_ticks, only: last_y_tick_max_width, Y_TICK_LABEL_RIGHT_PAD
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_draw_axis_labels
    public :: raster_render_ylabel
    public :: render_title_centered
    public :: compute_title_position
    public :: compute_ylabel_x_pos
    public :: y_tick_label_right_edge_at_axis

    ! Increased gap to better match matplotlib's labelpad (approx 6-8 pixels at 100dpi)
    integer, parameter :: YLABEL_EXTRA_GAP = 25

contains

    subroutine raster_draw_axis_labels(raster, width, height, plot_area, title, xlabel, ylabel)
        !! Draw all axis labels (title, xlabel, ylabel)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: title, xlabel, ylabel
        character(len=500) :: processed_text, escaped_text
        integer :: label_x, label_y, processed_len
        integer :: label_width, label_height

        ! Title at top
        if (len_trim(title) > 0) then
            call render_title_centered(raster, width, height, plot_area, title)
        end if

        ! X label at bottom
        if (len_trim(xlabel) > 0) then
            call process_latex_in_text(trim(xlabel), processed_text, processed_len)
            call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
            label_width = calculate_text_width(trim(escaped_text))
            label_height = calculate_text_height(trim(escaped_text))
            label_x = plot_area%left + plot_area%width/2 - label_width/2
            label_y = min(height - label_height - 5, plot_area%bottom + plot_area%height + XLABEL_VERTICAL_OFFSET)
            call render_text_to_image(raster%image_data, width, height, label_x, label_y, &
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
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        integer(1), allocatable :: text_bitmap(:,:,:), rotated_bitmap(:,:,:)
        integer :: text_width, text_height, text_descent
        integer :: rotated_width, rotated_height
        integer :: target_x, target_y
        integer :: y_tick_label_edge

        if (len_trim(ylabel) == 0) return

        ! Process LaTeX
        call process_latex_in_text(trim(ylabel), processed_text, processed_len)
        call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)

        ! Calculate text dimensions
        text_width = calculate_text_width(trim(escaped_text))
        text_height = calculate_text_height(trim(escaped_text))
        text_descent = calculate_text_descent(trim(escaped_text))

        ! Allocate text bitmap
        allocate(text_bitmap(text_width, text_height, 3))
        text_bitmap = -1_1  ! Initialize to white

        ! Render text to bitmap (upright). Position baseline to leave room for descenders
        call render_text_to_bitmap(text_bitmap, text_width, text_height, 0, text_height - text_descent, &
            trim(escaped_text))

        ! Rotate 90 degrees counter-clockwise
        rotated_width = text_height
        rotated_height = text_width
        allocate(rotated_bitmap(rotated_width, rotated_height, 3))
        call rotate_bitmap_90_ccw(text_bitmap, rotated_bitmap, text_width, text_height)

        ! Compute the rightmost edge of y-tick labels 
        y_tick_label_edge = y_tick_label_right_edge_at_axis(plot_area, last_y_tick_max_width)

        ! Compute ylabel position with dynamic gap
        target_x = compute_ylabel_x_pos(y_tick_label_edge, rotated_width, plot_area)

        ! Center vertically in plot area
        target_y = plot_area%bottom + plot_area%height/2 - rotated_height/2

        ! Composite to raster
        call composite_bitmap_to_raster(raster%image_data, width, height, rotated_bitmap, &
            rotated_width, rotated_height, target_x, target_y)

        deallocate(text_bitmap, rotated_bitmap)
    end subroutine raster_render_ylabel

    integer function y_tick_label_right_edge_at_axis(plot_area, max_width_measured)
        !! Compute the rightmost edge of y-tick labels relative to the y-axis
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: max_width_measured
        
        ! Y-tick labels are right-aligned with gap Y_TICK_LABEL_RIGHT_PAD from axis
        ! Their rightmost edge is at: axis_left - TICK_MARK_LENGTH - Y_TICK_LABEL_RIGHT_PAD
        y_tick_label_right_edge_at_axis = plot_area%left - TICK_MARK_LENGTH - Y_TICK_LABEL_RIGHT_PAD
    end function y_tick_label_right_edge_at_axis

    integer function compute_ylabel_x_pos(y_tick_label_edge, rotated_width, plot_area)
        !! Compute x-position for ylabel to avoid overlapping with y-tick labels
        integer, intent(in) :: y_tick_label_edge
        integer, intent(in) :: rotated_width
        type(plot_area_t), intent(in) :: plot_area
        
        ! The ylabel should be positioned to the left of the y-tick labels
        ! with an additional gap for clarity
        compute_ylabel_x_pos = y_tick_label_edge - last_y_tick_max_width - YLABEL_EXTRA_GAP - rotated_width
        
        ! Ensure ylabel doesn't go off the left edge
        if (compute_ylabel_x_pos < 5) then
            compute_ylabel_x_pos = 5
        end if
    end function compute_ylabel_x_pos

    subroutine render_title_centered(raster, width, height, plot_area, title_text)
        !! Render title centered above the plot area
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: title_text
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        integer :: title_px, title_py
        real(wp) :: dummy_real

        if (len_trim(title_text) == 0) return

        call compute_title_position(plot_area, title_text, processed_text, processed_len, &
            escaped_text, dummy_real, dummy_real)

        title_px = int(dummy_real)
        title_py = max(5, plot_area%bottom - TITLE_VERTICAL_OFFSET)

        call render_text_to_image(raster%image_data, width, height, title_px, title_py, &
            trim(escaped_text), 0_1, 0_1, 0_1)
    end subroutine render_title_centered

    subroutine compute_title_position(plot_area, title_text, processed_text, processed_len, escaped_text, title_px, title_py)
        !! Compute the position for centered title above plot area
        type(plot_area_t), intent(in) :: plot_area
        character(len=*), intent(in) :: title_text
        character(len=*), intent(out) :: processed_text, escaped_text
        integer, intent(out) :: processed_len
        real(wp), intent(out) :: title_px, title_py
        integer :: title_width

        call process_latex_in_text(trim(title_text), processed_text, processed_len)
        call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)

        ! Use character count for width approximation as it's more reliable
        ! Average character width is about 10 pixels for 16pt font
        title_width = len_trim(escaped_text) * 10
        
        ! Center the title properly over the plot area
        title_px = real(plot_area%left + plot_area%width/2 - title_width/2, wp)
        title_py = real(max(5, plot_area%bottom - TITLE_VERTICAL_OFFSET), wp)
    end subroutine compute_title_position

end module fortplot_raster_labels