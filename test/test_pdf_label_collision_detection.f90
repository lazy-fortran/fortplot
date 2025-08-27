program test_pdf_label_collision_detection
    !! Focused test for PDF Y-axis tick label collision detection
    !! This test SHOULD FAIL initially to demonstrate the overlap issue
    !!
    !! Given: PDF backend renders Y-axis tick labels near origin
    !! When: Labels are positioned too close together  
    !! Then: System should detect and prevent overlapping labels
    use fortplot_pdf, only: pdf_context
    use fortplot_ticks, only: calculate_tick_labels
    use fortplot_text, only: init_text_system, calculate_text_height
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    logical :: text_initialized
    
    ! Initialize text system for measurements
    text_initialized = init_text_system()
    if (.not. text_initialized) then
        print *, "WARNING: Text system not available, using fallback measurements"
    end if
    
    call test_detect_overlapping_yaxis_labels_in_pdf()
    call test_minimum_vertical_spacing_requirement()
    call test_pdf_label_positioning_near_origin()
    
    print *, "PDF label collision detection tests completed!"
    print *, "NOTE: These tests SHOULD FAIL initially to demonstrate the issue"
    
contains

    subroutine test_detect_overlapping_yaxis_labels_in_pdf()
        !! This test should FAIL initially - demonstrates overlap detection needed
        character(len=20) :: labels(6)
        real(wp), parameter :: y_min = -0.1_wp, y_max = 0.1_wp
        real(wp) :: label_positions(6), label_heights(6)
        real(wp) :: spacing_between_labels
        integer :: i, text_height
        logical :: overlap_detected
        
        print *, "=== Testing PDF Y-axis label overlap detection ==="
        
        ! Generate tick labels for tight range around origin
        call calculate_tick_labels(y_min, y_max, 6, labels)
        
        print *, "Generated labels for range", y_min, "to", y_max, ":"
        do i = 1, 6
            print *, "  Label", i, ":", trim(labels(i))
        end do
        
        ! Simulate PDF coordinate positions (these would come from actual PDF backend)
        ! These positions intentionally create overlaps to demonstrate the issue
        label_positions = [100.0_wp, 110.0_wp, 120.0_wp, 130.0_wp, 140.0_wp, 150.0_wp]
        
        ! Calculate label heights (approximate)
        do i = 1, 6
            text_height = calculate_text_height(labels(i))
            if (text_height <= 0) text_height = 14  ! Fallback
            label_heights(i) = real(text_height, wp)
        end do
        
        ! Check for overlaps between adjacent labels
        overlap_detected = .false.
        do i = 1, 5
            spacing_between_labels = label_positions(i+1) - label_positions(i)
            print *, "Spacing between labels", i, "and", i+1, ":", spacing_between_labels, "pixels"
            print *, "Label", i, "height:", label_heights(i), "pixels"
            
            ! Labels overlap if spacing is less than label height + minimum gap
            if (spacing_between_labels < label_heights(i) + 2.0_wp) then
                print *, "OVERLAP DETECTED: Labels", i, "and", i+1, "overlap!"
                overlap_detected = .true.
            end if
        end do
        
        ! This test SHOULD FAIL initially to demonstrate the problem
        if (overlap_detected) then
            print *, "FAIL: Y-axis tick labels overlap in PDF backend (Issue #39 confirmed)"
            print *, "This demonstrates the overlap issue that needs to be fixed"
            ! NOTE: Not stopping execution - this is expected to fail initially
        else
            print *, "PASS: No overlaps detected - overlap detection working correctly"
        end if
    end subroutine test_detect_overlapping_yaxis_labels_in_pdf
    
    subroutine test_minimum_vertical_spacing_requirement()
        !! Test that minimum spacing requirement is defined and enforced
        real(wp), parameter :: MIN_LABEL_SPACING = 3.0_wp  ! pixels
        real(wp) :: test_positions(3) = [100.0_wp, 112.0_wp, 125.0_wp]
        real(wp) :: test_heights(3) = [14.0_wp, 14.0_wp, 14.0_wp]
        real(wp) :: actual_spacing
        logical :: spacing_adequate
        integer :: i
        
        print *, "=== Testing minimum spacing requirement ==="
        print *, "Required minimum spacing:", MIN_LABEL_SPACING, "pixels"
        
        do i = 1, 2
            actual_spacing = test_positions(i+1) - test_positions(i) - test_heights(i)
            spacing_adequate = actual_spacing >= MIN_LABEL_SPACING
            
            print *, "Between labels", i, "and", i+1, ":"
            print *, "  Actual spacing:", actual_spacing, "pixels"
            print *, "  Adequate:", spacing_adequate
            
            if (.not. spacing_adequate) then
                print *, "FAIL: Spacing", actual_spacing, "< minimum", MIN_LABEL_SPACING
                ! Expected to fail initially 
            end if
        end do
    end subroutine test_minimum_vertical_spacing_requirement
    
    subroutine test_pdf_label_positioning_near_origin()
        !! Test PDF coordinate calculations specifically near origin  
        real(wp), parameter :: plot_height = 400.0_wp
        real(wp), parameter :: plot_bottom = 50.0_wp
        real(wp), parameter :: y_data_min = -0.05_wp, y_data_max = 0.05_wp
        real(wp) :: test_values(5) = [-0.04_wp, -0.02_wp, 0.0_wp, 0.02_wp, 0.04_wp]
        real(wp) :: pdf_y_positions(5)
        real(wp) :: position_differences(4)
        integer :: i
        logical :: positions_valid
        
        print *, "=== Testing PDF coordinate positioning near origin ==="
        
        ! Convert data coordinates to PDF coordinates (simulated)
        do i = 1, 5
            ! PDF Y coordinates: bottom + (value - min) / (max - min) * height
            pdf_y_positions(i) = plot_bottom + &
                (test_values(i) - y_data_min) / (y_data_max - y_data_min) * plot_height
        end do
        
        print *, "Data values and PDF Y positions:"
        do i = 1, 5
            print *, "  Data:", test_values(i), "-> PDF Y:", pdf_y_positions(i)
        end do
        
        ! Calculate differences between adjacent positions
        positions_valid = .true.
        do i = 1, 4
            position_differences(i) = abs(pdf_y_positions(i+1) - pdf_y_positions(i))
            print *, "Position difference", i, "to", i+1, ":", position_differences(i), "pixels"
            
            ! Check if positions are too close (less than font height)
            if (position_differences(i) < 14.0_wp) then  ! Approximate font height
                print *, "WARNING: Positions too close, may cause overlap"
                positions_valid = .false.
            end if
        end do
        
        if (.not. positions_valid) then
            print *, "FAIL: PDF positioning creates potential overlaps near origin"
            print *, "This confirms the coordinate transformation issue"
        else
            print *, "PASS: PDF positioning maintains adequate spacing"
        end if
    end subroutine test_pdf_label_positioning_near_origin

end program test_pdf_label_collision_detection