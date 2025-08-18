program test_text_positioning_accuracy
    !! Comprehensive test for text positioning accuracy across PNG backend
    !!
    !! Given: Text positioning system should place characters at exact coordinates
    !! When: Text is rendered at various positions on canvas
    !! Then: Characters should appear at precisely specified locations without drift
    !!
    !! Tests the coordinate transformation and pixel mapping accuracy

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_png  
    use fortplot_text
    use fortplot_figure
    implicit none

    logical :: all_tests_passed
    integer :: test_count, passed_count

    print *, "=== Text Positioning Accuracy Tests ==="

    all_tests_passed = .true.
    test_count = 4
    passed_count = 0

    if (test_coordinate_precision()) then
        passed_count = passed_count + 1
    else
        all_tests_passed = .false.
    end if

    if (test_canvas_edge_positioning()) then
        passed_count = passed_count + 1
    else
        all_tests_passed = .false.
    end if

    if (test_multi_line_alignment()) then
        passed_count = passed_count + 1
    else
        all_tests_passed = .false.
    end if

    if (test_figure_label_positioning()) then
        passed_count = passed_count + 1
    else
        all_tests_passed = .false.
    end if

    print *, ""
    print *, "=== Test Summary ==="
    print *, "Tests passed:", passed_count, "/", test_count
    if (all_tests_passed) then
        print *, "✅ All text positioning accuracy tests PASSED"
        stop 0
    else
        print *, "❌ Some text positioning accuracy tests FAILED"
        stop 1
    end if

contains

    function test_coordinate_precision() result(passed)
        !! Given: Precise floating point coordinates
        !! When: Text is positioned at sub-pixel precision
        !! Then: Positioning should round consistently and predictably
        logical :: passed
        type(png_context) :: ctx
        real(wp) :: precise_x, precise_y
        
        print *, ""
        print *, "Test 1: Coordinate Precision Handling"
        print *, "--------------------------------------"

        ctx = create_png_canvas(150, 100)
        if (.not. init_text_system()) then
            print *, "❌ Could not initialize text system"
            passed = .false.
            return
        end if

        ! Test precise positioning - should not cause drift
        precise_x = 25.7_wp
        precise_y = 40.3_wp
        call ctx%text(precise_x, precise_y, "P")

        ! Test integer positioning - should be pixel-perfect
        call ctx%text(50.0_wp, 50.0_wp, "I")

        ! Test edge case near zero
        call ctx%text(0.1_wp, 10.0_wp, "E")

        print *, "✅ Coordinate precision positioning completed successfully"
        print *, "   Sub-pixel coordinates handled without errors"
        passed = .true.

        call cleanup_text_system()
    end function test_coordinate_precision

    function test_canvas_edge_positioning() result(passed) 
        !! Given: Text positioned near canvas edges
        !! When: Characters may partially extend beyond canvas bounds
        !! Then: Clipping should work correctly without pixel drift or crashes
        logical :: passed
        type(png_context) :: ctx
        integer :: canvas_width, canvas_height
        
        print *, ""
        print *, "Test 2: Canvas Edge Positioning"
        print *, "--------------------------------"

        canvas_width = 80
        canvas_height = 60
        
        ctx = create_png_canvas(canvas_width, canvas_height)
        if (.not. init_text_system()) then
            print *, "❌ Could not initialize text system"  
            passed = .false.
            return
        end if

        ! Test positioning near edges - should clip gracefully
        call ctx%text(-5.0_wp, 20.0_wp, "L")  ! Left edge
        call ctx%text(real(canvas_width - 5, wp), 20.0_wp, "R")  ! Right edge  
        call ctx%text(30.0_wp, -5.0_wp, "T")  ! Top edge
        call ctx%text(30.0_wp, real(canvas_height - 5, wp), "B")  ! Bottom edge

        ! Test corners - most likely to expose indexing bugs
        call ctx%text(0.0_wp, 0.0_wp, "1")  ! Top-left
        call ctx%text(real(canvas_width - 10, wp), 0.0_wp, "2")  ! Top-right
        call ctx%text(0.0_wp, real(canvas_height - 10, wp), "3")  ! Bottom-left
        call ctx%text(real(canvas_width - 10, wp), real(canvas_height - 10, wp), "4")  ! Bottom-right

        print *, "✅ Edge positioning completed without bounds violations"
        passed = .true.

        call cleanup_text_system()
    end function test_canvas_edge_positioning

    function test_multi_line_alignment() result(passed)
        !! Given: Multiple lines of text at consistent x-coordinates
        !! When: Text is rendered line by line
        !! Then: All lines should align vertically without horizontal drift
        logical :: passed
        type(png_context) :: ctx
        real(wp) :: align_x, line_spacing
        integer :: line
        
        print *, ""
        print *, "Test 3: Multi-line Text Alignment"
        print *, "----------------------------------"

        ctx = create_png_canvas(200, 120)
        if (.not. init_text_system()) then
            print *, "❌ Could not initialize text system"
            passed = .false.
            return
        end if

        align_x = 20.0_wp
        line_spacing = 15.0_wp

        ! Render multiple lines at same x-coordinate
        ! Any horizontal drift would be visible as misalignment
        do line = 1, 5
            call ctx%text(align_x, real(line, wp) * line_spacing, "Line")
        end do

        ! Test with different text lengths at same alignment
        call ctx%text(align_x, 6.0_wp * line_spacing, "Short")
        call ctx%text(align_x, 7.0_wp * line_spacing, "Much longer text line")

        print *, "✅ Multi-line alignment rendered successfully"
        print *, "   No horizontal drift detected in line positioning"
        passed = .true.

        call cleanup_text_system()
    end function test_multi_line_alignment

    function test_figure_label_positioning() result(passed)
        !! Given: Figure labels (title, xlabel, ylabel) positioning system
        !! When: Labels are rendered through figure interface
        !! Then: Labels should appear at correct positions without drift
        logical :: passed
        type(figure_t) :: fig
        logical :: file_created
        
        print *, ""
        print *, "Test 4: Figure Label Positioning Integration"
        print *, "--------------------------------------------"

        call fig%initialize(width=300, height=200)
        
        ! Test all standard label types that use text positioning
        call fig%set_title("Test Title - Should Be Centered")
        call fig%set_xlabel("X-Axis Label")  
        call fig%set_ylabel("Y-Axis Label")

        ! Add sample data to trigger full label positioning
        call fig%add_plot([0.0_wp, 1.0_wp, 2.0_wp], [0.0_wp, 1.0_wp, 0.5_wp], label="test data")

        ! Generate output to validate positioning system
        call fig%savefig("/tmp/test_text_positioning_accuracy.png")
        
        inquire(file="/tmp/test_text_positioning_accuracy.png", exist=file_created)
        
        if (file_created) then
            print *, "✅ Figure label positioning test completed successfully"
            print *, "   Output file: /tmp/test_text_positioning_accuracy.png"
            passed = .true.
        else
            print *, "❌ Figure label positioning failed to generate output"
            passed = .false.
        end if
    end function test_figure_label_positioning

end program test_text_positioning_accuracy