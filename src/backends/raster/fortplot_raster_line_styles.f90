module fortplot_raster_line_styles
    !! Module for handling line styling in raster images
    use iso_c_binding
    use fortplot_raster_drawing, only: draw_line_distance_aa
    use fortplot_line_styles, only: get_line_pattern, get_pattern_length
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: draw_styled_line, set_raster_line_style, reset_pattern_distance
    public :: PATTERN_SCALE_FACTOR

    ! Named constant to replace magic number
    ! Pattern scaling factor controls how many pattern units correspond
    ! to one pixel of geometric distance. Use a non-integer factor to
    ! avoid aliasing where fixed 2px segments never enter gap regions.
    ! Scale of pattern units to pixels. Using 1.0 aligns
    ! pattern units with pixel distances for intuitive lengths
    real(wp), parameter :: PATTERN_SCALE_FACTOR = 1.0_wp

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
        real(wp), contiguous, intent(in) :: line_pattern(:)
        integer, intent(in) :: pattern_size
        real(wp), intent(in) :: pattern_length
        real(wp), intent(inout) :: pattern_distance
        
        real(wp) :: dx, dy, line_length
        real(wp) :: unit_x, unit_y
        real(wp) :: pos, phase, seg_end, draw_start, draw_end
        real(wp) :: cap_inset, sx, sy, ex, ey
        integer :: phase_index
        logical :: in_draw

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

        ! Degenerate pattern -> draw solid to avoid an invisible line.
        if (pattern_size <= 0 .or. pattern_length <= 0.0_wp) then
            call draw_line_distance_aa(image_data, img_w, img_h, &
                                      px1, py1, px2, py2, &
                                      r, g, b, line_width)
            pattern_distance = pattern_distance + line_length * PATTERN_SCALE_FACTOR
            return
        end if

        ! Walk the dash pattern in exact device units (no per-pixel
        ! quantization). For each "on" interval, draw the precise sub-segment.
        ! The distance-AA primitive adds a rounded cap of ~half the line width
        ! beyond each endpoint; inset every drawn interval by that amount so
        ! short dots stay round and dashes keep matplotlib-like gaps instead of
        ! bleeding into the off intervals.
        cap_inset = line_width * 0.5_wp
        pos = 0.0_wp
        do while (pos < line_length - 1e-9_wp)
            ! Locate the current phase for the global pattern distance at pos.
            call locate_phase(pattern_distance + pos * PATTERN_SCALE_FACTOR, &
                              line_pattern, pattern_size, pattern_length, &
                              phase_index, phase)
            in_draw = (mod(phase_index, 2) == 1)

            ! Distance (device units) to the end of this phase along the line.
            seg_end = pos + (line_pattern(phase_index) - phase) / PATTERN_SCALE_FACTOR
            if (seg_end > line_length) seg_end = line_length
            if (seg_end <= pos) seg_end = pos + 1e-6_wp

            if (in_draw) then
                draw_start = pos
                draw_end = seg_end
                ! Inset for round-cap compensation, but never invert the
                ! interval; very short "on" phases collapse to a single dot.
                if (draw_end - draw_start > 2.0_wp * cap_inset) then
                    draw_start = draw_start + cap_inset
                    draw_end = draw_end - cap_inset
                else
                    draw_start = 0.5_wp * (pos + seg_end)
                    draw_end = draw_start
                end if
                sx = px1 + draw_start * unit_x
                sy = py1 + draw_start * unit_y
                ex = px1 + draw_end * unit_x
                ey = py1 + draw_end * unit_y
                call draw_line_distance_aa(image_data, img_w, img_h, &
                                          sx, sy, ex, ey, &
                                          r, g, b, line_width)
            end if

            pos = seg_end
        end do

        pattern_distance = pattern_distance + line_length * PATTERN_SCALE_FACTOR

    end subroutine draw_styled_line

    subroutine locate_phase(distance, pattern, pattern_size, pattern_length, &
                            phase_index, offset_in_phase)
        !! Map a cumulative pattern distance to its on/off phase. Returns the
        !! 1-based phase index (odd = draw, even = gap) and how far into that
        !! phase the distance falls.
        real(wp), intent(in) :: distance, pattern_length
        real(wp), contiguous, intent(in) :: pattern(:)
        integer, intent(in) :: pattern_size
        integer, intent(out) :: phase_index
        real(wp), intent(out) :: offset_in_phase

        real(wp) :: pos_in_cycle, cumulative
        integer :: i

        pos_in_cycle = mod(distance, pattern_length)
        if (pos_in_cycle < 0.0_wp) pos_in_cycle = pos_in_cycle + pattern_length

        cumulative = 0.0_wp
        do i = 1, pattern_size
            if (pos_in_cycle < cumulative + pattern(i)) then
                phase_index = i
                offset_in_phase = pos_in_cycle - cumulative
                return
            end if
            cumulative = cumulative + pattern(i)
        end do

        ! Numerical edge at the very end of the cycle: report last phase.
        phase_index = pattern_size
        offset_in_phase = pattern(pattern_size)

    end subroutine locate_phase

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
