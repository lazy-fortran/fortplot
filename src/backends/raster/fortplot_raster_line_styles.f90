module fortplot_raster_line_styles
    !! Module for handling line styling in raster images
    use iso_c_binding
    use fortplot_raster_drawing, only: draw_line_distance_aa
    use fortplot_line_styles, only: get_line_pattern, get_pattern_length, should_draw_at_distance
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: draw_styled_line, set_raster_line_style, reset_pattern_distance
    public :: PATTERN_SCALE_FACTOR

    ! Named constant to replace magic number
    ! Pattern scaling factor controls how many pattern units correspond
    ! to one pixel of geometric distance. Use a non-integer factor to
    ! avoid aliasing where fixed 2px segments never enter gap regions.
    real(wp), parameter :: PATTERN_SCALE_FACTOR = 1.3_wp

contains

    subroutine draw_styled_line(image_data, img_w, img_h, px1, py1, px2, py2, &
                               r, g, b, line_width, line_style, line_pattern, &
                               pattern_size, pattern_length, pattern_distance)
        !! Draw line with pattern support (dashed, dotted, etc.)
        integer(1), intent(inout) :: image_data(:)
        integer, intent(in) :: img_w, img_h
        real(wp), intent(in) :: px1, py1, px2, py2
        real(wp), intent(in) :: r, g, b, line_width
        character(len=*), intent(in) :: line_style
        real(wp), intent(in) :: line_pattern(:)
        integer, intent(in) :: pattern_size
        real(wp), intent(in) :: pattern_length
        real(wp), intent(inout) :: pattern_distance
        
        real(wp) :: dx, dy, line_length, segment_length
        real(wp) :: unit_x, unit_y, current_x, current_y, next_x, next_y
        real(wp) :: segment_distance
        integer :: num_segments, i
        
        ! Calculate line geometry
        dx = px2 - px1
        dy = py2 - py1
        line_length = sqrt(dx * dx + dy * dy)
        
        ! Handle degenerate case
        if (line_length < 1e-6_wp) return
        
        ! Unit direction vector
        unit_x = dx / line_length
        unit_y = dy / line_length
        
        ! For solid lines, draw the whole line at once
        if (trim(line_style) == '-' .or. trim(line_style) == 'solid') then
            call draw_line_distance_aa(image_data, img_w, img_h, &
                                      px1, py1, px2, py2, &
                                      r, g, b, line_width)
            return
        end if
        
        ! For patterned lines, break into small segments
        segment_length = 2.0_wp  ! Draw in 2-pixel segments for good pattern resolution
        num_segments = max(1, int(line_length / segment_length))
        segment_length = line_length / real(num_segments, wp)  ! Adjust to exact segments
        
        current_x = px1
        current_y = py1
        
        do i = 1, num_segments
            ! Calculate segment endpoints
            if (i == num_segments) then
                ! Last segment goes exactly to end point
                next_x = px2
                next_y = py2
            else
                next_x = current_x + segment_length * unit_x
                next_y = current_y + segment_length * unit_y
            end if
            
            ! Check if this segment should be drawn according to pattern
            if (should_draw_at_distance(pattern_distance, line_pattern, &
                                       pattern_size, pattern_length)) then
                call draw_line_distance_aa(image_data, img_w, img_h, &
                                          current_x, current_y, next_x, next_y, &
                                          r, g, b, line_width)
            end if
            
            ! Advance pattern state
            segment_distance = sqrt((next_x - current_x)**2 + (next_y - current_y)**2)
            pattern_distance = pattern_distance + segment_distance * PATTERN_SCALE_FACTOR
            
            current_x = next_x
            current_y = next_y
        end do
        
    end subroutine draw_styled_line

    subroutine set_raster_line_style(style, line_style, line_pattern, pattern_size, &
                                    pattern_length, pattern_distance)
        !! Set line style pattern for raster
        character(len=*), intent(in) :: style
        character(len=*), intent(out) :: line_style
        real(wp), intent(out) :: line_pattern(:)
        integer, intent(out) :: pattern_size
        real(wp), intent(out) :: pattern_length
        real(wp), intent(out) :: pattern_distance
        
        line_style = trim(style)
        call get_line_pattern(style, line_pattern, pattern_size)
        pattern_length = get_pattern_length(line_pattern, pattern_size)
        pattern_distance = 0.0_wp  ! Reset pattern distance
    end subroutine set_raster_line_style

    subroutine reset_pattern_distance(pattern_distance)
        !! Reset pattern distance to start of pattern
        real(wp), intent(out) :: pattern_distance
        pattern_distance = 0.0_wp
    end subroutine reset_pattern_distance

end module fortplot_raster_line_styles
