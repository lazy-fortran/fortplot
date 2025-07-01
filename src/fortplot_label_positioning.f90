module fortplot_label_positioning
    !! Separate positioning for tick labels vs axis labels
    !! Follows matplotlib exact spacing measurements
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_text, only: calculate_text_width
    implicit none
    
    private
    ! Legacy functions for backward compatibility
    public :: calculate_x_label_position, calculate_y_label_position
    ! New separate functions for tick vs axis labels
    public :: calculate_x_tick_label_position, calculate_y_tick_label_position
    public :: calculate_x_axis_label_position, calculate_y_axis_label_position
    ! PDF-specific positioning (PDF Y=0 at bottom, increases upward)
    public :: calculate_x_tick_label_position_pdf, calculate_y_tick_label_position_pdf
    public :: LABEL_SPACING_X, LABEL_SPACING_Y
    
    ! Matplotlib-exact spacing constants
    ! Matplotlib defaults: tick size=3.5pt, tick pad=3.5pt, total=7pt ≈ 9.3px
    integer, parameter :: X_TICK_SPACING = 10    ! X-tick labels: 10px below tick mark end (5px tick + 5px pad)
    integer, parameter :: Y_TICK_SPACING = 10    ! Y-tick labels: 10px left of plot (right edge)
    integer, parameter :: X_AXIS_SPACING = 40    ! X-axis label: 40px below plot  
    integer, parameter :: Y_AXIS_SPACING = 70    ! Y-axis label: right edge 10px from left edge
    
    ! Legacy constants for backward compatibility
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

    subroutine calculate_x_tick_label_position(tick_x, plot_bottom, label_text, label_x, label_y)
        !! Calculate X-axis tick label position using proper font metrics and matplotlib spacing
        use fortplot_text, only: get_font_metrics
        real(wp), intent(in) :: tick_x, plot_bottom
        character(len=*), intent(in) :: label_text
        real(wp), intent(out) :: label_x, label_y
        integer :: text_width, fallback_width
        real(wp) :: font_ascent, font_descent, line_gap, tick_end_y, label_padding
        logical :: success
        
        ! Calculate text width for centering
        text_width = calculate_text_width(label_text)
        if (text_width <= 0) then
            fallback_width = len_trim(label_text) * 8
            text_width = fallback_width
        end if
        
        ! Center horizontally on tick mark
        label_x = tick_x - real(text_width, wp) / 2.0_wp
        
        ! Calculate proper vertical positioning
        tick_end_y = plot_bottom + 5.0_wp  ! Tick marks extend 5px below axis
        
        ! Get actual font metrics
        call get_font_metrics(font_ascent, font_descent, line_gap, success)
        
        if (success) then
            ! Use matplotlib standard: 3.5pt pad ≈ 5px spacing from tick end to text top
            label_padding = 5.0_wp
            
            ! Position text baseline: tick_end + padding + ascent (text top at tick_end + padding)
            label_y = tick_end_y + label_padding + font_ascent
        else
            ! Fallback: use simple spacing
            label_y = tick_end_y + real(X_TICK_SPACING, wp)
        end if
    end subroutine calculate_x_tick_label_position

    subroutine calculate_y_tick_label_position(tick_y, plot_left, label_text, label_x, label_y)
        !! Calculate Y-axis tick label position using proper font metrics
        use fortplot_text, only: get_font_metrics
        real(wp), intent(in) :: tick_y, plot_left
        character(len=*), intent(in) :: label_text
        real(wp), intent(out) :: label_x, label_y
        integer :: text_width, fallback_width
        real(wp) :: font_ascent, font_descent, line_gap, baseline_offset
        logical :: success
        
        ! Calculate text width for right alignment
        text_width = calculate_text_width(label_text)
        if (text_width <= 0) then
            fallback_width = len_trim(label_text) * 8
            text_width = fallback_width
        end if
        
        ! Right-align: right edge 10 pixels left of plot
        label_x = plot_left - real(Y_TICK_SPACING, wp) - real(text_width, wp)
        
        ! Get actual font metrics
        call get_font_metrics(font_ascent, font_descent, line_gap, success)
        
        if (success) then
            ! Position baseline so text center aligns with tick_y
            ! In PNG coordinates: Y=0 at top, Y increases downward
            ! Text extends UP (smaller Y) from baseline by ascent
            ! Text extends DOWN (larger Y) from baseline by descent
            ! Text center is at: baseline - ascent + (ascent + descent)/2 = baseline - ascent/2 + descent/2
            ! For center at tick_y: tick_y = baseline - ascent/2 + descent/2
            ! So: baseline = tick_y + ascent/2 - descent/2
            baseline_offset = font_ascent/2.0_wp - font_descent/2.0_wp
            label_y = tick_y + baseline_offset
        else
            ! Fallback to approximation if font system fails
            label_y = tick_y - real(TEXT_HEIGHT, wp) * 0.25_wp
        end if
    end subroutine calculate_y_tick_label_position

    subroutine calculate_x_axis_label_position(center_x, plot_bottom, label_text, label_x, label_y)
        !! Calculate X-axis label position (40px below plot)
        real(wp), intent(in) :: center_x, plot_bottom
        character(len=*), intent(in) :: label_text
        real(wp), intent(out) :: label_x, label_y
        integer :: text_width, fallback_width
        
        ! Calculate text width for centering
        text_width = calculate_text_width(label_text)
        if (text_width <= 0) then
            fallback_width = len_trim(label_text) * 8
            text_width = fallback_width
        end if
        
        ! Center horizontally
        label_x = center_x - real(text_width, wp) / 2.0_wp
        
        ! Position 40 pixels below plot bottom (matplotlib exact)
        label_y = plot_bottom + real(X_AXIS_SPACING, wp)
    end subroutine calculate_x_axis_label_position

    subroutine calculate_y_axis_label_position(center_y, plot_left, label_text, label_x, label_y)
        !! Calculate Y-axis label position with proper spacing from tick labels
        real(wp), intent(in) :: center_y, plot_left
        character(len=*), intent(in) :: label_text
        real(wp), intent(out) :: label_x, label_y
        integer :: text_width, fallback_width, tick_label_space
        
        ! Calculate text width for right alignment
        text_width = calculate_text_width(label_text)
        if (text_width <= 0) then
            fallback_width = len_trim(label_text) * 8
            text_width = fallback_width
        end if
        
        ! For simple plots like sin(x), tick labels are typically short (like "0.5", "-1.0")
        ! Estimate realistic space: 4 chars * 6 pixels = 24 pixels for typical tick labels
        tick_label_space = 4 * 6  ! 24 pixels for typical short tick labels
        
        ! Position Y-axis label left of tick labels with 8 pixel gap
        label_x = plot_left - real(tick_label_space, wp) - 8.0_wp - real(text_width, wp)
        
        ! Ensure minimum visibility - if calculated position is too far left, use reasonable fallback
        if (label_x < 5.0_wp) then
            ! Fallback: position with minimal but visible spacing
            label_x = max(5.0_wp, plot_left - real(tick_label_space, wp) - 8.0_wp)
        end if
        
        ! Vertically center
        label_y = center_y
    end subroutine calculate_y_axis_label_position

    subroutine calculate_x_tick_label_position_pdf(tick_x, plot_bottom, label_text, label_x, label_y)
        !! Calculate X-axis tick label position for PDF (Y=0 at bottom, increases upward)
        use fortplot_text, only: get_font_metrics
        real(wp), intent(in) :: tick_x, plot_bottom
        character(len=*), intent(in) :: label_text
        real(wp), intent(out) :: label_x, label_y
        integer :: text_width, fallback_width
        real(wp) :: font_ascent, font_descent, line_gap, tick_end_y, label_padding
        logical :: success
        
        ! Calculate text width for centering
        text_width = calculate_text_width(label_text)
        if (text_width <= 0) then
            fallback_width = len_trim(label_text) * 8
            text_width = fallback_width
        end if
        
        ! Center horizontally on tick mark (fix: was too far left)
        label_x = tick_x - real(text_width, wp) / 2.0_wp
        
        ! Calculate proper vertical positioning for PDF coordinates
        tick_end_y = plot_bottom - 5.0_wp  ! Tick marks extend 5px below axis (down = smaller Y in PDF)
        
        ! Get actual font metrics
        call get_font_metrics(font_ascent, font_descent, line_gap, success)
        
        if (success) then
            ! Use matplotlib standard: 3.5pt pad ≈ 5px spacing from tick end to text top
            label_padding = 5.0_wp
            
            ! In PDF: Y=0 at bottom, text baseline above text bottom
            ! Text extends DOWN (smaller Y) from baseline by descent  
            ! Text extends UP (larger Y) from baseline by ascent
            ! For text top at tick_end - padding: baseline = tick_end - padding - ascent
            label_y = tick_end_y - label_padding - font_ascent
        else
            ! Fallback: use simple spacing
            label_y = tick_end_y - real(X_TICK_SPACING, wp)
        end if
    end subroutine calculate_x_tick_label_position_pdf

    subroutine calculate_y_tick_label_position_pdf(tick_y, plot_left, label_text, label_x, label_y)
        !! Calculate Y-axis tick label position for PDF (Y=0 at bottom, increases upward)
        use fortplot_text, only: get_font_metrics
        real(wp), intent(in) :: tick_y, plot_left
        character(len=*), intent(in) :: label_text
        real(wp), intent(out) :: label_x, label_y
        integer :: text_width, fallback_width
        real(wp) :: font_ascent, font_descent, line_gap, baseline_offset
        logical :: success
        
        ! Calculate text width for right alignment
        text_width = calculate_text_width(label_text)
        if (text_width <= 0) then
            fallback_width = len_trim(label_text) * 8
            text_width = fallback_width
        end if
        
        ! Right-align: right edge 10 pixels left of plot
        label_x = plot_left - real(Y_TICK_SPACING, wp) - real(text_width, wp)
        
        ! Get actual font metrics
        call get_font_metrics(font_ascent, font_descent, line_gap, success)
        
        if (success) then
            ! Position baseline so text center aligns with tick_y
            ! In PDF coordinates: Y=0 at bottom, Y increases upward
            ! Text extends DOWN (smaller Y) from baseline by descent
            ! Text extends UP (larger Y) from baseline by ascent
            ! Text center is at: baseline - descent + (ascent + descent)/2 = baseline - descent/2 + ascent/2
            ! For center at tick_y: tick_y = baseline - descent/2 + ascent/2
            ! So: baseline = tick_y + descent/2 - ascent/2
            baseline_offset = font_descent/2.0_wp - font_ascent/2.0_wp
            label_y = tick_y + baseline_offset
        else
            ! Fallback to approximation if font system fails
            label_y = tick_y - real(TEXT_HEIGHT, wp) * 0.25_wp
        end if
    end subroutine calculate_y_tick_label_position_pdf

end module fortplot_label_positioning