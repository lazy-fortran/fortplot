module fortplot_line_styles
    !! Line pattern and style rendering algorithms
    !! 
    !! This module contains line style pattern rendering and segment
    !! drawing algorithms following the Single Responsibility Principle.
    !! 
    !! SOLID: Single responsibility for line style implementation
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: SOLID_LINE_PATTERN_LENGTH, REFERENCE_DPI, &
                                  DOTTED_PATTERN_PT, DASHED_PATTERN_PT, &
                                  DASHDOT_PATTERN_PT
    implicit none

    private
    public :: get_line_pattern, get_pattern_length, scale_pattern_to_pixels
    public :: should_draw_at_distance, advance_pattern_state

contains

    subroutine get_line_pattern(linestyle, pattern, pattern_size)
        !! Get line pattern array for a given style, in points.
        !! Single source of truth: the matplotlib default dash sequences.
        !! On/off lengths are in points; callers scale by line width (points)
        !! and convert to device units (see scale_pattern_to_pixels).
        character(len=*), intent(in) :: linestyle
        real(wp), intent(out) :: pattern(20)
        integer, intent(out) :: pattern_size

        select case (trim(linestyle))
        case ('-', 'solid')
            pattern(1) = SOLID_LINE_PATTERN_LENGTH  ! Very long solid segment
            pattern_size = 1

        case ('--', 'dashed')
            pattern(1:2) = DASHED_PATTERN_PT
            pattern_size = 2

        case (':', 'dotted')
            pattern(1:2) = DOTTED_PATTERN_PT
            pattern_size = 2

        case ('-.', 'dashdot')
            pattern(1:4) = DASHDOT_PATTERN_PT
            pattern_size = 4

        case default
            pattern(1) = SOLID_LINE_PATTERN_LENGTH  ! Default to solid
            pattern_size = 1
        end select

    end subroutine get_line_pattern

    subroutine scale_pattern_to_pixels(pattern, pattern_size, dpi, &
                                       line_width_pt)
        !! Scale a point-based pattern in place to device pixels, matching
        !! matplotlib: multiply each on/off length (points) by the line width
        !! (points), then convert points to pixels via px = pt * dpi / 72.
        !! The solid sentinel is left untouched.
        real(wp), intent(inout) :: pattern(20)
        integer, intent(in) :: pattern_size
        real(wp), intent(in) :: dpi, line_width_pt

        real(wp) :: factor

        if (pattern_size <= 1) return  ! Solid: keep sentinel length
        factor = line_width_pt * dpi / 72.0_wp
        pattern(1:pattern_size) = pattern(1:pattern_size) * factor

    end subroutine scale_pattern_to_pixels

    real(wp) function get_pattern_length(pattern, pattern_size)
        !! Calculate total length of pattern cycle
        !! Following KISS principle - simple sum calculation
        real(wp), intent(in) :: pattern(20)
        integer, intent(in) :: pattern_size
        
        integer :: i
        
        get_pattern_length = 0.0_wp
        do i = 1, pattern_size
            get_pattern_length = get_pattern_length + pattern(i)
        end do
        
    end function get_pattern_length

    logical function should_draw_at_distance(distance, pattern, pattern_size, pattern_length)
        !! Determine if line should be drawn at given distance in pattern
        !! Following SRP - handles only pattern position logic
        real(wp), intent(in) :: distance, pattern_length
        real(wp), intent(in) :: pattern(20)
        integer, intent(in) :: pattern_size
        
        real(wp) :: pos_in_cycle, cumulative
        integer :: i
        
        ! Handle edge cases
        if (pattern_size <= 0 .or. pattern_length <= 0.0_wp) then
            should_draw_at_distance = .true.
            return
        end if
        
        ! Find position within pattern cycle
        pos_in_cycle = mod(distance, pattern_length)
        
        ! Determine if we're in a draw or gap segment
        cumulative = 0.0_wp
        do i = 1, pattern_size
            cumulative = cumulative + pattern(i)
            if (pos_in_cycle <= cumulative) then
                ! Odd indices (1,3,5...) are draw segments
                ! Even indices (2,4,6...) are gap segments
                should_draw_at_distance = (mod(i, 2) == 1)
                return
            end if
        end do
        
        ! Fallback
        should_draw_at_distance = .true.
        
    end function should_draw_at_distance

    subroutine advance_pattern_state(current_distance, segment_length, pattern_length, new_distance)
        !! Advance pattern state for continuous rendering
        !! Following SRP - handles only state advancement
        real(wp), intent(in) :: current_distance, segment_length, pattern_length
        real(wp), intent(out) :: new_distance
        
        new_distance = current_distance + segment_length
        
        ! Keep distance within reasonable bounds to avoid overflow
        if (pattern_length > 0.0_wp .and. new_distance > SOLID_LINE_PATTERN_LENGTH * pattern_length) then
            new_distance = mod(new_distance, pattern_length)
        end if
        
    end subroutine advance_pattern_state

end module fortplot_line_styles
