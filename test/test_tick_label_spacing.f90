program test_tick_label_spacing
    !! Test that tick labels have proper spacing from tick marks (Issue #9)
    !! X tick labels should not touch ticks, Y labels should be properly centered
    use fortplot_label_positioning, only: calculate_x_tick_label_position, calculate_y_tick_label_position
    use fortplot_text, only: init_text_system, cleanup_text_system, calculate_text_height, get_font_metrics
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    logical :: success
    
    ! Initialize text system for height calculations
    success = init_text_system()
    if (.not. success) then
        print *, "WARNING: Text system failed, using fallback measurements"
    end if
    
    call test_x_tick_label_spacing()
    call test_y_tick_label_centering()
    
    call cleanup_text_system()
    print *, "All tick label spacing tests passed!"
    
contains

    subroutine test_x_tick_label_spacing()
        !! Test that X tick labels have proper spacing below tick marks
        real(wp) :: tick_x, axis_y, label_x, label_y
        real(wp) :: expected_min_spacing, actual_spacing
        character(len=10) :: label_text
        integer :: text_height
        
        ! Test setup - typical plot coordinates
        tick_x = 200.0_wp
        axis_y = 400.0_wp  ! Bottom of plot area
        label_text = "0.5"
        
        ! Calculate label position
        call calculate_x_tick_label_position(tick_x, axis_y, label_text, label_x, label_y)
        
        ! Calculate spacing between tick mark end and label top  
        ! Tick marks extend 5 pixels below axis, so tick end is at axis_y + 5
        actual_spacing = label_y - (axis_y + 5.0_wp)
        
        ! Expected minimum spacing (matplotlib uses ~3-4 pixels)
        expected_min_spacing = 3.0_wp
        
        print *, "X tick label spacing test:"
        print *, "  Tick mark ends at Y:", axis_y + 5.0_wp
        print *, "  Label top at Y:", label_y
        print *, "  Actual spacing:", actual_spacing
        print *, "  Expected min spacing:", expected_min_spacing
        
        if (actual_spacing < expected_min_spacing) then
            print *, "FAIL: X tick labels too close to tick marks!"
            print *, "      Labels are touching or overlapping ticks"
            error stop 1
        end if
        
        print *, "PASS: X tick labels have proper spacing"
    end subroutine test_x_tick_label_spacing

    subroutine test_y_tick_label_centering()
        !! Test that Y tick labels are properly centered with tick marks
        real(wp) :: tick_y, axis_x, label_x, label_y
        real(wp) :: label_center_y, expected_center_y, centering_error
        real(wp) :: font_ascent, font_descent, line_gap_local
        character(len=10) :: label_text
        integer :: text_height
        logical :: metrics_success
        
        ! Test setup
        tick_y = 300.0_wp
        axis_x = 80.0_wp   ! Left edge of plot area
        label_text = "1.0"
        
        ! Calculate label position
        call calculate_y_tick_label_position(tick_y, axis_x, label_text, label_x, label_y)
        
        ! Calculate actual label center using proper font metrics
        
        call get_font_metrics(font_ascent, font_descent, line_gap_local, metrics_success)
        if (metrics_success) then
            ! label_y is baseline, text center in PNG coords is baseline - ascent/2 + descent/2
            label_center_y = label_y - font_ascent/2.0_wp + font_descent/2.0_wp
        else
            ! Fallback to simple calculation
            text_height = calculate_text_height(label_text)
            if (text_height <= 0) text_height = 16
            label_center_y = label_y + real(text_height, wp) / 2.0_wp
        end if
        expected_center_y = tick_y
        centering_error = abs(label_center_y - expected_center_y)
        
        print *, "Y tick label centering test:"
        print *, "  Tick Y position:", tick_y
        print *, "  Label Y position:", label_y
        print *, "  Label center Y:", label_center_y
        print *, "  Expected center Y:", expected_center_y
        print *, "  Centering error:", centering_error
        
        ! Allow reasonable tolerance for text positioning (within ~8 pixels is acceptable)
        if (centering_error > 8.0_wp) then
            print *, "FAIL: Y tick labels not properly centered!"
            print *, "      Labels are positioned too high by:", centering_error, "pixels"
            error stop 1
        end if
        
        print *, "PASS: Y tick labels properly centered"
    end subroutine test_y_tick_label_centering

end program test_tick_label_spacing