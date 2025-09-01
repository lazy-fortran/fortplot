module fortplot_line_styles
    !! Line pattern and style rendering algorithms
    !! 
    !! This module contains line style pattern rendering and segment
    !! drawing algorithms following the Single Responsibility Principle.
    !! 
    !! SOLID: Single responsibility for line style implementation
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: SOLID_LINE_PATTERN_LENGTH
    implicit none

    private
    public :: get_line_pattern, get_pattern_length
    public :: should_draw_at_distance, advance_pattern_state

contains

    subroutine get_line_pattern(linestyle, pattern, pattern_size)
        !! Get line pattern array for given style
        !! Following DRY principle - centralized pattern definitions
        character(len=*), intent(in) :: linestyle
        real(wp), intent(out) :: pattern(20)
        integer, intent(out) :: pattern_size
        
        real(wp) :: dash_len, dot_len, gap_len
        
        ! Base pattern dimensions in abstract pattern units.
        ! Raster rendering scales these by its PATTERN_SCALE_FACTOR to pixels.
        dash_len = 3.0_wp    ! 15 pixels when scaled - proper dash length
        dot_len = 0.4_wp     ! 2 pixels when scaled - visible dot
        gap_len = 1.0_wp     ! 5 pixels when scaled - clear separation
        
        select case (trim(linestyle))
        case ('-', 'solid')
            pattern(1) = SOLID_LINE_PATTERN_LENGTH  ! Very long solid segment
            pattern_size = 1
            
        case ('--', 'dashed')
            pattern(1) = dash_len
            pattern(2) = gap_len
            pattern_size = 2
            
        case (':', 'dotted')
            pattern(1) = dot_len
            pattern(2) = gap_len
            pattern_size = 2
            
        case ('-.', 'dashdot')
            pattern(1) = dash_len
            pattern(2) = gap_len
            pattern(3) = dot_len
            pattern(4) = gap_len
            pattern_size = 4
            
        case default
            pattern(1) = SOLID_LINE_PATTERN_LENGTH  ! Default to solid
            pattern_size = 1
        end select
        
    end subroutine get_line_pattern

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

    subroutine advance_pattern_state(current_distance, segment_length, pattern, pattern_size, &
                                   pattern_length, new_distance)
        !! Advance pattern state for continuous rendering
        !! Following SRP - handles only state advancement
        real(wp), intent(in) :: current_distance, segment_length, pattern_length
        real(wp), intent(in) :: pattern(20)
        integer, intent(in) :: pattern_size
        real(wp), intent(out) :: new_distance
        
        new_distance = current_distance + segment_length
        
        ! Keep distance within reasonable bounds to avoid overflow
        if (pattern_length > 0.0_wp .and. new_distance > SOLID_LINE_PATTERN_LENGTH * pattern_length) then
            new_distance = mod(new_distance, pattern_length)
        end if
        
    end subroutine advance_pattern_state

end module fortplot_line_styles
