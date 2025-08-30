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
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_draw_axes_and_labels, raster_render_ylabel

contains

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
        
        line_r = 0.0_wp; line_g = 0.0_wp; line_b = 0.0_wp  ! Black color
        text_r = 0; text_g = 0; text_b = 0
        tick_length = TICK_MARK_LENGTH
        
        call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, x_tick_positions, num_x_ticks)
        do i = 1, num_x_ticks
            tick_x = x_tick_positions(i)
            px = int((tick_x - x_min) / (x_max - x_min) * real(plot_area%width, wp) + real(plot_area%left, wp))
            py = plot_area%bottom + plot_area%height
            
            ! Draw tick mark
            dummy_pattern = 0.0_wp
            pattern_dist = 0.0_wp
            call draw_styled_line(raster%image_data, width, height, &
                                 real(px, wp), real(py, wp), real(px, wp), real(py + tick_length, wp), &
                                 line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
            
            ! Draw tick label
            tick_label = format_tick_label(tick_x, xscale)
            call process_latex_in_text(trim(tick_label), processed_text, processed_len)
            call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
            text_width = calculate_text_width(trim(escaped_text))
            call render_text_to_image(raster%image_data, width, height, &
                                    px - text_width/2, py + tick_length + 5, trim(escaped_text), text_r, text_g, text_b)
        end do
    end subroutine raster_draw_x_axis_ticks
    
    subroutine raster_draw_y_axis_ticks(raster, width, height, plot_area, yscale, symlog_threshold, &
                                       x_min, x_max, y_min, y_max)
        !! Draw Y-axis tick marks and labels
        use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
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
        real(wp) :: line_r, line_g, line_b
        integer(1) :: text_r, text_g, text_b
        real(wp) :: dummy_pattern(1), pattern_dist
        character(len=500) :: processed_text, escaped_text
        integer :: processed_len
        
        line_r = 0.0_wp; line_g = 0.0_wp; line_b = 0.0_wp  ! Black color
        text_r = 0; text_g = 0; text_b = 0
        tick_length = TICK_MARK_LENGTH
        
        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, y_tick_positions, num_y_ticks)
        do i = 1, num_y_ticks
            tick_y = y_tick_positions(i)
            px = plot_area%left
            py = int(real(plot_area%bottom + plot_area%height, wp) - &
                    (tick_y - y_min) / (y_max - y_min) * real(plot_area%height, wp))
            
            ! Draw tick mark
            dummy_pattern = 0.0_wp
            pattern_dist = 0.0_wp
            call draw_styled_line(raster%image_data, width, height, &
                                 real(px - tick_length, wp), real(py, wp), real(px, wp), real(py, wp), &
                                 line_r, line_g, line_b, 1.0_wp, 'solid', dummy_pattern, 0, 0.0_wp, pattern_dist)
            
            ! Draw tick label
            tick_label = format_tick_label(tick_y, yscale)
            call process_latex_in_text(trim(tick_label), processed_text, processed_len)
            call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
            text_width = calculate_text_width(trim(escaped_text))
            text_height = calculate_text_height(trim(escaped_text))
            call render_text_to_image(raster%image_data, width, height, &
                px - tick_length - text_width - 5, py - text_height/2, &
                trim(escaped_text), text_r, text_g, text_b)
        end do
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
        
        ! Render text horizontally to bitmap (at origin)
        call render_text_to_bitmap(text_bitmap, text_width, text_height, 0, 0, ylabel)
        
        ! Allocate rotated bitmap (dimensions swapped for 90° rotation)
        rotated_width = text_height
        rotated_height = text_width
        allocate(rotated_bitmap(rotated_width, rotated_height, 3))
        
        ! Rotate the text 90 degrees counter-clockwise
        call rotate_bitmap_90_ccw(text_bitmap, rotated_bitmap, text_width, text_height)
        
        ! Calculate position for rotated text (left of plot area, centered vertically)
        x_pos = plot_area%left - 40 - rotated_width / 2
        y_pos = plot_area%bottom + plot_area%height / 2 - rotated_height / 2
        
        ! Composite the rotated text onto the main raster
        call composite_bitmap_to_raster(raster%image_data, width, height, rotated_bitmap, &
                                       rotated_width, rotated_height, x_pos, y_pos)
        
        ! Clean up
        if (allocated(text_bitmap)) deallocate(text_bitmap)
        if (allocated(rotated_bitmap)) deallocate(rotated_bitmap)
    end subroutine raster_render_ylabel

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
        
        ! Process LaTeX commands and Unicode
        call process_latex_in_text(title_text, processed_text, processed_len)
        call escape_unicode_for_raster(processed_text(1:processed_len), escaped_text)
        
        ! Calculate title position centered over plot area
        ! X position: center of plot area horizontally
        title_px = real(plot_area%left + plot_area%width / 2, wp)
        
        ! Y position: above plot area (like matplotlib)  
        ! Place title approximately 30 pixels above the plot area
        title_py = real(plot_area%bottom - TITLE_VERTICAL_OFFSET, wp)
        
        ! Get current color and render title directly in pixel coordinates
        call raster%get_color_bytes(r, g, b)
        call render_text_to_image(raster%image_data, width, height, &
                                 int(title_px), int(title_py), trim(escaped_text), r, g, b)
    end subroutine render_title_centered

end module fortplot_raster_axes