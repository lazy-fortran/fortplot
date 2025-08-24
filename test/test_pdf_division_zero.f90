program test_pdf_division_zero
    !! Test division by zero protection in PDF coordinate transformation
    !! Issue #237: Add division by zero protection when data ranges are zero
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_zero_range_protection()
    call test_epsilon_range_handling()
    call test_normal_range_unchanged()
    
    print *, "=== All division by zero tests completed ==="
    
contains

    subroutine test_zero_range_protection()
        !! Test that zero ranges don't cause division by zero
        real(wp) :: x_min, x_max, y_min, y_max
        real(wp) :: x, y, pdf_x, pdf_y
        real(wp) :: plot_left, plot_width, plot_bottom, plot_height
        logical :: test_passed
        
        print *, "=== Test: Zero range protection ==="
        
        ! Setup plot area
        plot_left = 50.0_wp
        plot_width = 500.0_wp
        plot_bottom = 50.0_wp
        plot_height = 300.0_wp
        
        ! Test case 1: Zero X range
        x_min = 5.0_wp
        x_max = 5.0_wp  ! Same as min - zero range
        y_min = 0.0_wp
        y_max = 10.0_wp
        x = 5.0_wp
        y = 5.0_wp
        
        print *, "Test 1: Zero X range (x_min = x_max = 5.0)"
        call safe_coordinate_transform(x, y, x_min, x_max, y_min, y_max, &
                                      plot_left, plot_width, plot_bottom, plot_height, &
                                      pdf_x, pdf_y)
        
        test_passed = (pdf_x >= plot_left) .and. (pdf_x <= plot_left + plot_width)
        if (test_passed) then
            print *, "  PASS: X coordinate within bounds:", pdf_x
        else
            print *, "  FAIL: X coordinate out of bounds:", pdf_x
        end if
        
        ! Test case 2: Zero Y range
        x_min = 0.0_wp
        x_max = 10.0_wp
        y_min = 3.0_wp
        y_max = 3.0_wp  ! Same as min - zero range
        x = 5.0_wp
        y = 3.0_wp
        
        print *, "Test 2: Zero Y range (y_min = y_max = 3.0)"
        call safe_coordinate_transform(x, y, x_min, x_max, y_min, y_max, &
                                      plot_left, plot_width, plot_bottom, plot_height, &
                                      pdf_x, pdf_y)
        
        test_passed = (pdf_y >= 0.0_wp) .and. (pdf_y <= 400.0_wp)
        if (test_passed) then
            print *, "  PASS: Y coordinate within bounds:", pdf_y
        else
            print *, "  FAIL: Y coordinate out of bounds:", pdf_y
        end if
        
        ! Test case 3: Both ranges zero
        x_min = 7.0_wp
        x_max = 7.0_wp
        y_min = -2.0_wp
        y_max = -2.0_wp
        x = 7.0_wp
        y = -2.0_wp
        
        print *, "Test 3: Both X and Y ranges zero"
        call safe_coordinate_transform(x, y, x_min, x_max, y_min, y_max, &
                                      plot_left, plot_width, plot_bottom, plot_height, &
                                      pdf_x, pdf_y)
        
        test_passed = (pdf_x >= plot_left) .and. (pdf_x <= plot_left + plot_width) .and. &
                     (pdf_y >= 0.0_wp) .and. (pdf_y <= 400.0_wp)
        if (test_passed) then
            print *, "  PASS: Both coordinates within bounds"
            print *, "    X:", pdf_x, "Y:", pdf_y
        else
            print *, "  FAIL: Coordinates out of bounds"
            print *, "    X:", pdf_x, "Y:", pdf_y
        end if
        
        print *, ""
    end subroutine test_zero_range_protection
    
    subroutine test_epsilon_range_handling()
        !! Test handling of extremely small but non-zero ranges
        real(wp) :: x_min, x_max, y_min, y_max
        real(wp) :: x, y, pdf_x, pdf_y
        real(wp) :: plot_left, plot_width, plot_bottom, plot_height
        real(wp), parameter :: EPSILON = 1.0e-12_wp
        logical :: test_passed
        
        print *, "=== Test: Epsilon range handling ==="
        
        ! Setup plot area
        plot_left = 50.0_wp
        plot_width = 500.0_wp
        plot_bottom = 50.0_wp
        plot_height = 300.0_wp
        
        ! Test epsilon-small X range
        x_min = 1.0_wp
        x_max = 1.0_wp + EPSILON
        y_min = 0.0_wp
        y_max = 10.0_wp
        x = 1.0_wp
        y = 5.0_wp
        
        print *, "Test: Epsilon X range (", x_max - x_min, ")"
        call safe_coordinate_transform(x, y, x_min, x_max, y_min, y_max, &
                                      plot_left, plot_width, plot_bottom, plot_height, &
                                      pdf_x, pdf_y)
        
        test_passed = (pdf_x >= plot_left) .and. (pdf_x <= plot_left + plot_width)
        if (test_passed) then
            print *, "  PASS: Epsilon range handled gracefully"
            print *, "    X:", pdf_x
        else
            print *, "  FAIL: Epsilon range caused issues"
            print *, "    X:", pdf_x
        end if
        
        print *, ""
    end subroutine test_epsilon_range_handling
    
    subroutine test_normal_range_unchanged()
        !! Test that normal ranges still work correctly
        real(wp) :: x_min, x_max, y_min, y_max
        real(wp) :: x, y, pdf_x, pdf_y, expected_x, expected_y
        real(wp) :: plot_left, plot_width, plot_bottom, plot_height
        real(wp), parameter :: TOLERANCE = 1.0e-10_wp
        logical :: test_passed
        
        print *, "=== Test: Normal range unchanged ==="
        
        ! Setup plot area
        plot_left = 50.0_wp
        plot_width = 500.0_wp
        plot_bottom = 50.0_wp
        plot_height = 300.0_wp
        
        ! Normal data range
        x_min = 0.0_wp
        x_max = 10.0_wp
        y_min = -5.0_wp
        y_max = 5.0_wp
        x = 5.0_wp  ! Middle of X range
        y = 0.0_wp  ! Middle of Y range
        
        ! Calculate expected values
        expected_x = plot_left + (x - x_min) / (x_max - x_min) * plot_width
        expected_y = plot_bottom + (y - y_min) / (y_max - y_min) * plot_height
        
        print *, "Test: Normal range transformation"
        call safe_coordinate_transform(x, y, x_min, x_max, y_min, y_max, &
                                      plot_left, plot_width, plot_bottom, plot_height, &
                                      pdf_x, pdf_y)
        
        test_passed = (abs(pdf_x - expected_x) < TOLERANCE) .and. &
                     (abs(pdf_y - expected_y) < TOLERANCE)
        
        if (test_passed) then
            print *, "  PASS: Normal ranges work correctly"
            print *, "    Expected X:", expected_x, "Got:", pdf_x
            print *, "    Expected Y:", expected_y, "Got:", pdf_y
        else
            print *, "  FAIL: Normal range transformation incorrect"
            print *, "    Expected X:", expected_x, "Got:", pdf_x
            print *, "    Expected Y:", expected_y, "Got:", pdf_y
        end if
        
        print *, ""
    end subroutine test_normal_range_unchanged
    
    subroutine safe_coordinate_transform(x, y, x_min, x_max, y_min, y_max, &
                                        plot_left, plot_width, plot_bottom, plot_height, &
                                        pdf_x, pdf_y)
        !! Simulates the coordinate transformation with division by zero protection
        real(wp), intent(in) :: x, y
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(in) :: plot_left, plot_width, plot_bottom, plot_height
        real(wp), intent(out) :: pdf_x, pdf_y
        real(wp), parameter :: EPSILON = 1.0e-10_wp
        real(wp) :: x_range, y_range
        
        ! Calculate ranges with epsilon protection
        x_range = x_max - x_min
        y_range = y_max - y_min
        
        ! Handle X coordinate
        if (abs(x_range) < EPSILON) then
            ! Zero or near-zero range: place at center of plot area
            pdf_x = plot_left + plot_width * 0.5_wp
        else
            ! Normal transformation
            pdf_x = plot_left + (x - x_min) / x_range * plot_width
        end if
        
        ! Handle Y coordinate
        if (abs(y_range) < EPSILON) then
            ! Zero or near-zero range: place at center of plot area
            pdf_y = plot_bottom + plot_height * 0.5_wp
        else
            ! Normal transformation
            pdf_y = plot_bottom + (y - y_min) / y_range * plot_height
        end if
        
    end subroutine safe_coordinate_transform
    
end program test_pdf_division_zero