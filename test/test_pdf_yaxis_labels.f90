program test_pdf_yaxis_labels
    !! Comprehensive Y-axis label positioning tests for PDF backend
    !! Given: Various Y-tick positions in PDF coordinate system
    !! When: Using calculate_y_tick_label_position_pdf function
    !! Then: Labels should have correct positioning and proper distribution
    !! 
    !! This test suite targets Issue #34: Y-axis labels clustering near origin
    !! Focus: Detect double-subtraction bug in PDF coordinate transformation

    use fortplot_label_positioning, only: calculate_y_tick_label_position_pdf
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_y_label_clustering_detection()
    call test_coordinate_transformation_accuracy()
    call test_label_distribution_validation()
    call test_png_pdf_consistency()
    call test_edge_case_coordinates()
    
    print *, "=== PDF Y-axis label positioning tests completed ==="

contains

    subroutine test_y_label_clustering_detection()
        !! Given: Multiple evenly spaced Y-tick positions
        !! When: Calculating PDF label positions
        !! Then: Label Y positions should maintain proportional spacing (not cluster)
        
        real(wp), parameter :: PLOT_LEFT = 80.0_wp
        real(wp), parameter :: EXPECTED_SPACING = 50.0_wp
        real(wp), parameter :: TOLERANCE = 5.0_wp
        integer, parameter :: NUM_TICKS = 5
        
        real(wp) :: tick_positions(NUM_TICKS)
        real(wp) :: label_x, label_y
        real(wp) :: label_y_positions(NUM_TICKS)
        real(wp) :: spacing_differences(NUM_TICKS-1)
        character(len=10) :: labels(NUM_TICKS) = ["0.0", "1.0", "2.0", "3.0", "4.0"]
        logical :: clustering_detected
        integer :: i
        
        print *, "=== Test: Y-label clustering detection ==="
        
        ! Create evenly spaced tick positions (50 pixel intervals)
        do i = 1, NUM_TICKS
            tick_positions(i) = 50.0_wp + real(i-1, wp) * EXPECTED_SPACING
        end do
        
        ! Calculate all label positions
        do i = 1, NUM_TICKS
            call calculate_y_tick_label_position_pdf(tick_positions(i), PLOT_LEFT, &
                                                   trim(labels(i)), label_x, label_y)
            label_y_positions(i) = label_y
            print *, "Tick", i, "at Y=", tick_positions(i), "-> Label Y=", label_y
        end do
        
        ! Check for clustering by measuring spacing consistency
        clustering_detected = .false.
        do i = 1, NUM_TICKS-1
            spacing_differences(i) = label_y_positions(i+1) - label_y_positions(i)
            print *, "  Spacing", i, "to", i+1, ":", spacing_differences(i), "px"
            
            ! If spacing deviates significantly from expected, clustering is occurring
            if (abs(spacing_differences(i) - EXPECTED_SPACING) > TOLERANCE) then
                clustering_detected = .true.
                print *, "  CLUSTERING DETECTED: Expected ~", EXPECTED_SPACING, &
                        "px, got", spacing_differences(i), "px"
            end if
        end do
        
        if (clustering_detected) then
            print *, "FAIL: Y-label clustering detected - Issue #34 reproduced"
            print *, "Expected: Consistent ~", EXPECTED_SPACING, "px spacing"
            print *, "Actual: Inconsistent spacing indicates coordinate transformation bug"
        else
            print *, "PASS: No clustering - labels properly distributed"
        end if
        print *, ""
        
    end subroutine test_y_label_clustering_detection

    subroutine test_coordinate_transformation_accuracy()
        !! Given: Known Y-tick positions in PDF coordinates
        !! When: Calculating label positions
        !! Then: Label Y should be close to tick Y (within text positioning tolerance)
        
        real(wp), parameter :: PLOT_LEFT = 80.0_wp
        real(wp), parameter :: MAX_OFFSET = 15.0_wp  ! Maximum reasonable offset for text centering
        
        real(wp) :: test_positions(4) = [100.0_wp, 150.0_wp, 200.0_wp, 250.0_wp]
        real(wp) :: label_x, label_y, position_error
        character(len=10) :: test_label = "test"
        logical :: accuracy_failed
        integer :: i
        
        print *, "=== Test: Coordinate transformation accuracy ==="
        
        accuracy_failed = .false.
        do i = 1, 4
            call calculate_y_tick_label_position_pdf(test_positions(i), PLOT_LEFT, &
                                                   test_label, label_x, label_y)
            
            position_error = abs(label_y - test_positions(i))
            print *, "Tick Y=", test_positions(i), "-> Label Y=", label_y, &
                    "Error=", position_error, "px"
            
            if (position_error > MAX_OFFSET) then
                accuracy_failed = .true.
                print *, "  FAIL: Label position error exceeds tolerance"
                print *, "  This indicates double-subtraction or coordinate transformation bug"
            else
                print *, "  PASS: Label position within tolerance"
            end if
        end do
        
        if (accuracy_failed) then
            print *, "FAIL: Coordinate transformation accuracy issues detected"
            print *, "Large position errors suggest double-subtraction bug in PDF backend"
        else
            print *, "PASS: Coordinate transformation appears accurate"
        end if
        print *, ""
        
    end subroutine test_coordinate_transformation_accuracy

    subroutine test_label_distribution_validation()
        !! Given: Y-tick positions spanning full plot height
        !! When: Calculating label positions
        !! Then: Labels should be distributed across full range, not clustered at origin
        
        real(wp), parameter :: PLOT_LEFT = 80.0_wp
        real(wp), parameter :: PLOT_HEIGHT = 300.0_wp
        real(wp), parameter :: MIN_DISTRIBUTION = 200.0_wp  ! Labels should span at least 200px
        
        real(wp) :: bottom_tick = 50.0_wp
        real(wp) :: top_tick = 350.0_wp
        real(wp) :: label_x, label_y_bottom, label_y_top
        real(wp) :: actual_distribution
        character(len=10) :: bottom_label = "0.0"
        character(len=10) :: top_label = "10.0"
        
        print *, "=== Test: Label distribution validation ==="
        print *, "Plot height:", PLOT_HEIGHT, "px"
        print *, "Tick range: Y=", bottom_tick, "to Y=", top_tick
        
        ! Calculate bottom and top label positions
        call calculate_y_tick_label_position_pdf(bottom_tick, PLOT_LEFT, &
                                               bottom_label, label_x, label_y_bottom)
        call calculate_y_tick_label_position_pdf(top_tick, PLOT_LEFT, &
                                               top_label, label_x, label_y_top)
        
        actual_distribution = label_y_top - label_y_bottom
        
        print *, "Bottom label Y:", label_y_bottom
        print *, "Top label Y:", label_y_top  
        print *, "Actual distribution:", actual_distribution, "px"
        print *, "Expected minimum:", MIN_DISTRIBUTION, "px"
        
        if (actual_distribution < MIN_DISTRIBUTION) then
            print *, "FAIL: Labels not properly distributed - clustering detected"
            print *, "Labels are clustered instead of spanning the plot area"
            print *, "This confirms Issue #34: Y-axis labels cluster near origin"
        else
            print *, "PASS: Labels properly distributed across plot area"
        end if
        print *, ""
        
    end subroutine test_label_distribution_validation

    subroutine test_png_pdf_consistency()
        !! Given: Same Y-tick positions
        !! When: Comparing PDF vs PNG positioning logic
        !! Then: Relative spacing should be consistent (accounting for coordinate differences)
        
        real(wp), parameter :: PLOT_LEFT = 80.0_wp
        
        real(wp) :: test_ticks(3) = [100.0_wp, 200.0_wp, 300.0_wp]
        real(wp) :: pdf_label_x, pdf_label_y
        real(wp) :: pdf_positions(3)
        real(wp) :: pdf_spacing_1, pdf_spacing_2
        character(len=10) :: labels(3) = ["low ", "mid ", "high"]
        integer :: i
        
        print *, "=== Test: PNG-PDF consistency check ==="
        
        ! Calculate PDF positions
        do i = 1, 3
            call calculate_y_tick_label_position_pdf(test_ticks(i), PLOT_LEFT, &
                                                   trim(labels(i)), pdf_label_x, pdf_label_y)
            pdf_positions(i) = pdf_label_y
            print *, "PDF: Tick Y=", test_ticks(i), "-> Label Y=", pdf_label_y
        end do
        
        ! Check PDF spacing consistency
        pdf_spacing_1 = pdf_positions(2) - pdf_positions(1)
        pdf_spacing_2 = pdf_positions(3) - pdf_positions(2)
        
        print *, "PDF spacing 1-2:", pdf_spacing_1, "px"
        print *, "PDF spacing 2-3:", pdf_spacing_2, "px"
        
        if (abs(pdf_spacing_1 - pdf_spacing_2) > 10.0_wp) then
            print *, "FAIL: PDF spacing inconsistent - coordinate transformation bug"
            print *, "Equal tick spacing should produce equal label spacing"
        else
            print *, "PASS: PDF spacing is consistent"
        end if
        
        ! Additional check: labels should follow tick order
        if (pdf_positions(2) <= pdf_positions(1) .or. pdf_positions(3) <= pdf_positions(2)) then
            print *, "FAIL: PDF label order doesn't match tick order"
            print *, "This indicates coordinate transformation reversal or clustering"
        else
            print *, "PASS: PDF label order matches tick order"
        end if
        print *, ""
        
    end subroutine test_png_pdf_consistency

    subroutine test_edge_case_coordinates()
        !! Given: Edge case Y-coordinates (very small, very large)
        !! When: Calculating label positions
        !! Then: Should handle edge cases without producing invalid coordinates
        
        real(wp), parameter :: PLOT_LEFT = 80.0_wp
        
        real(wp) :: edge_ticks(6) = [1.0_wp, 5.0_wp, 50.0_wp, 500.0_wp, 700.0_wp, 799.0_wp]
        real(wp) :: label_x, label_y
        character(len=10) :: edge_labels(6) = ["-99 ", "-90 ", "0   ", "90  ", "99  ", "100 "]
        logical :: edge_case_failed
        integer :: i
        
        print *, "=== Test: Edge case coordinates ==="
        
        edge_case_failed = .false.
        do i = 1, 6
            call calculate_y_tick_label_position_pdf(edge_ticks(i), PLOT_LEFT, &
                                                   trim(edge_labels(i)), label_x, label_y)
            
            print *, "Edge case", i, ": tick Y=", edge_ticks(i), "-> label Y=", label_y
            
            ! Check for reasonable positioning
            if (label_y < -100.0_wp .or. label_y > 1000.0_wp) then
                edge_case_failed = .true.
                print *, "  FAIL: Label Y position outside reasonable range"
                print *, "  Position", label_y, "suggests coordinate transformation error"
            else
                print *, "  PASS: Label Y position within reasonable range"
            end if
            
            ! Check X positioning is left of plot
            if (label_x >= PLOT_LEFT) then
                edge_case_failed = .true.
                print *, "  FAIL: Label X should be left of plot area"
            else
                print *, "  PASS: Label X positioned left of plot area"
            end if
        end do
        
        if (edge_case_failed) then
            print *, "FAIL: Edge case handling issues detected"
        else
            print *, "PASS: Edge cases handled properly"
        end if
        print *, ""
        
    end subroutine test_edge_case_coordinates

end program test_pdf_yaxis_labels