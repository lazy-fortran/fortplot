module fortplot_label_positioning
    !! Common tick label positioning calculations for all backends
    !! Follows DRY principle by centralizing positioning logic
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_text, only: calculate_text_width
    implicit none
    
    private
    public :: calculate_x_label_position, calculate_y_label_position
    public :: LABEL_SPACING_X, LABEL_SPACING_Y
    
    ! Constants for consistent spacing across backends (matplotlib-style)
    integer, parameter :: LABEL_SPACING_X = 25  ! Pixels below X-axis tick labels
    integer, parameter :: LABEL_SPACING_Y = 15  ! Pixels left of Y-axis tick labels  
    integer, parameter :: TEXT_HEIGHT = 12      ! Approximate text height for centering
    integer, parameter :: Y_LABEL_OFFSET = 4    ! Pixels down from center for better alignment
    
contains

    subroutine calculate_x_label_position(tick_x, plot_bottom, plot_height, label_text, label_x, label_y)
        !! Calculate X-axis label position with matplotlib-style centering and spacing
        real(wp), intent(in) :: tick_x, plot_bottom, plot_height
        character(len=*), intent(in) :: label_text
        real(wp), intent(out) :: label_x, label_y
        integer :: text_width, fallback_width
        
        ! Try to calculate actual text width
        text_width = calculate_text_width(label_text)
        
        ! Fallback if text width calculation fails (returns 0)
        if (text_width <= 0) then
            ! Approximate: 8 pixels per character (reasonable for typical fonts)
            fallback_width = len_trim(label_text) * 8
            text_width = fallback_width
        end if
        
        ! Center the label horizontally under the tick mark
        label_x = tick_x - real(text_width, wp) / 2.0_wp
        
        ! Position below the plot area with matplotlib-style closer spacing
        label_y = plot_bottom + plot_height + LABEL_SPACING_X
    end subroutine calculate_x_label_position

    subroutine calculate_y_label_position(tick_y, plot_left, label_text, label_x, label_y)
        !! Calculate Y-axis label position with right alignment and proper spacing
        real(wp), intent(in) :: tick_y, plot_left
        character(len=*), intent(in) :: label_text
        real(wp), intent(out) :: label_x, label_y
        integer :: text_width, fallback_width
        
        ! Try to calculate actual text width
        text_width = calculate_text_width(label_text)
        
        ! Fallback if text width calculation fails (returns 0)
        if (text_width <= 0) then
            ! Approximate: 8 pixels per character (reasonable for typical fonts)
            fallback_width = len_trim(label_text) * 8
            text_width = fallback_width
        end if
        
        ! Right-align the label to avoid overlapping with axis
        ! Position text_width pixels to the left of plot area, plus spacing
        label_x = plot_left - real(text_width, wp) - LABEL_SPACING_Y
        
        ! Vertically center the label with the tick mark, slightly below center
        ! Adjust for text baseline (text renders from baseline, not center)
        ! Move down slightly for better visual alignment with tick marks (PNG Y increases down)
        label_y = tick_y + real(Y_LABEL_OFFSET, wp)
    end subroutine calculate_y_label_position

end module fortplot_label_positioning