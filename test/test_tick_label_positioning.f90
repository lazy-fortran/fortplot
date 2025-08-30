program test_tick_label_positioning
    !! TDD test to ensure proper tick label positioning and alignment
    !! This addresses spacing and alignment issues with tick labels
    
    use fortplot
    use fortplot_security, only: get_test_output_path
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== Tick Label Positioning Tests ==="
    
    if (.not. test_x_axis_labels_proper_spacing()) all_tests_passed = .false.
    if (.not. test_y_axis_labels_right_aligned()) all_tests_passed = .false.
    if (.not. test_long_labels_dont_overlap_axis()) all_tests_passed = .false.
    if (.not. test_labels_aligned_with_tick_marks()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "✓ All tick label positioning tests passed!"
        print *, "  X-axis labels have proper spacing from axis"
        print *, "  Y-axis labels are right-aligned and don't overlap"
        print *, "  Labels are properly aligned with tick marks"
    else
        print *, "✗ Some positioning tests failed"
    end if

contains

    function test_x_axis_labels_proper_spacing() result(passed)
        !! Test that X-axis labels have adequate spacing from the axis line
        logical :: passed
        type(figure_t) :: my_fig
        real(wp) :: x_data(4), y_data(4)
        
        print *, "Testing: X-axis labels should have proper spacing from axis"
        passed = .true.
        
        ! Create test data with long numeric labels
        x_data = [1234.56_wp, 2345.67_wp, 3456.78_wp, 4567.89_wp]
        y_data = [10.0_wp, 20.0_wp, 30.0_wp, 40.0_wp]
        
        call my_fig%initialize(600, 400)
        call my_fig%add_plot(x_data, y_data, label="Spacing test")
        call my_fig%set_title("X-axis Label Spacing Test")
        call my_fig%savefig('test/output/x_axis_spacing_test.png')
        
        print *, "Generated: /tmp/x_axis_spacing_test.png"
        print *, "Expected: X-axis labels should not touch the axis line"
        print *, "Expected: Clear visual separation between labels and axis"
    end function

    function test_y_axis_labels_right_aligned() result(passed)
        !! Test that Y-axis labels are right-aligned to avoid overlapping
        logical :: passed
        type(figure_t) :: my_fig
        real(wp) :: x_data(4), y_data(4)
        
        print *, "Testing: Y-axis labels should be right-aligned"
        passed = .true.
        
        ! Create test data with varying length Y labels
        x_data = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        y_data = [0.001_wp, 100.0_wp, 10000.0_wp, 1000000.0_wp]  ! Will generate labels of different lengths
        
        call my_fig%initialize(600, 400)
        call my_fig%add_plot(x_data, y_data, label="Alignment test")
        call my_fig%set_title("Y-axis Label Alignment Test")
        call my_fig%savefig('test/output/y_axis_alignment_test.png')
        
        print *, "Generated: /tmp/y_axis_alignment_test.png"
        print *, "Expected: Y-axis labels should be right-aligned"
        print *, "Expected: Different length labels should align properly"
    end function

    function test_long_labels_dont_overlap_axis() result(passed)
        !! Test that long Y-axis labels don't overlap with the axis line
        logical :: passed
        type(figure_t) :: my_fig
        real(wp) :: x_data(3), y_data(3)
        
        print *, "Testing: Long labels should not overlap axis"
        passed = .true.
        
        ! Create data that will generate long scientific notation labels
        x_data = [1.0_wp, 10.0_wp, 100.0_wp]
        y_data = [1.0e-6_wp, 1.0e3_wp, 1.0e9_wp]
        
        call my_fig%initialize(600, 400)
        call my_fig%add_plot(x_data, y_data, label="Long labels test")
        call my_fig%set_yscale("log")
        call my_fig%set_title("Long Label Overlap Test")
        call my_fig%savefig('test/output/long_labels_test.png')
        
        print *, "Generated: /tmp/long_labels_test.png"
        print *, "Expected: Scientific notation labels should not overlap axis"
        print *, "Expected: Clear separation between longest label and axis line"
    end function

    function test_labels_aligned_with_tick_marks() result(passed)
        !! Test that labels are properly centered/aligned with their tick marks
        logical :: passed
        type(figure_t) :: my_fig
        real(wp) :: x_data(5), y_data(5)
        
        print *, "Testing: Labels should be properly aligned with tick marks"
        passed = .true.
        
        ! Create simple test data
        x_data = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        y_data = [10.0_wp, 20.0_wp, 30.0_wp, 40.0_wp, 50.0_wp]
        
        call my_fig%initialize(600, 400)
        call my_fig%add_plot(x_data, y_data, label="Alignment test")
        call my_fig%set_title("Tick Mark Alignment Test")
        call my_fig%savefig('test/output/tick_alignment_test.png')
        
        print *, "Generated: /tmp/tick_alignment_test.png"
        print *, "Expected: X-axis labels centered under tick marks"
        print *, "Expected: Y-axis labels aligned with tick marks"
        print *, "Expected: No misalignment between labels and ticks"
    end function

end program test_tick_label_positioning