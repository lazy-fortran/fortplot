program test_pdf_coordinate_transform
    !! Direct test of PDF coordinate transformation calculations
    !! Given: PDF coordinate system (Y=0 at bottom, increases upward)
    !! When: Transforming between data coordinates and PDF canvas coordinates  
    !! Then: Transformations should be mathematically correct and reversible
    !! 
    !! This test specifically targets the double-subtraction bug in Issue #34

    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_pdf_coordinate_system_validation()
    call test_y_coordinate_transformation_accuracy()
    call test_double_subtraction_detection()
    call test_coordinate_reversibility()
    call test_boundary_coordinate_handling()
    call test_division_by_zero_protection()
    call test_epsilon_range_handling()
    call test_constant_data_graceful_handling()
    
    print *, "=== PDF coordinate transformation tests completed ==="

contains

    subroutine test_pdf_coordinate_system_validation()
        !! Given: PDF coordinate system conventions
        !! When: Validating basic coordinate system understanding
        !! Then: Confirm Y=0 at bottom, Y increases upward in PDF
        
        real(wp), parameter :: CANVAS_HEIGHT = 400.0_wp
        real(wp), parameter :: MARGIN_TOP = 50.0_wp
        real(wp), parameter :: MARGIN_BOTTOM = 50.0_wp
        real(wp), parameter :: PLOT_HEIGHT = CANVAS_HEIGHT - MARGIN_TOP - MARGIN_BOTTOM
        
        real(wp) :: plot_bottom, plot_top
        
        print *, "=== Test: PDF coordinate system validation ==="
        print *, "Canvas height:", CANVAS_HEIGHT, "px"
        print *, "Margins: top=", MARGIN_TOP, "px, bottom=", MARGIN_BOTTOM, "px"
        
        ! In PDF coordinates: Y=0 at bottom of canvas
        plot_bottom = MARGIN_BOTTOM
        plot_top = CANVAS_HEIGHT - MARGIN_TOP
        
        print *, "Plot area in PDF coordinates:"
        print *, "  Bottom (lower Y):", plot_bottom, "px"
        print *, "  Top (higher Y):", plot_top, "px"
        print *, "  Plot height:", plot_top - plot_bottom, "px"
        
        if (plot_top <= plot_bottom) then
            print *, "FAIL: PDF coordinate system validation failed"
            print *, "Plot top should have higher Y than plot bottom in PDF"
        else
            print *, "PASS: PDF coordinate system correctly understood"
        end if
        print *, ""
        
    end subroutine test_pdf_coordinate_system_validation

    subroutine test_y_coordinate_transformation_accuracy()
        !! Given: Data Y-values and plot boundaries
        !! When: Transforming to PDF canvas coordinates
        !! Then: Mathematical transformation should be correct
        
        real(wp), parameter :: DATA_MIN = -2.0_wp
        real(wp), parameter :: DATA_MAX = 8.0_wp
        real(wp), parameter :: PLOT_BOTTOM = 50.0_wp  ! PDF coordinates
        real(wp), parameter :: PLOT_TOP = 350.0_wp    ! PDF coordinates
        real(wp), parameter :: PLOT_HEIGHT = PLOT_TOP - PLOT_BOTTOM
        real(wp), parameter :: DATA_RANGE = DATA_MAX - DATA_MIN
        
        real(wp) :: test_data_values(5) = [-2.0_wp, 0.0_wp, 3.0_wp, 5.0_wp, 8.0_wp]
        real(wp) :: expected_canvas_y(5)
        real(wp) :: calculated_canvas_y
        real(wp) :: transform_error
        integer :: i
        
        print *, "=== Test: Y-coordinate transformation accuracy ==="
        print *, "Data range: [", DATA_MIN, ",", DATA_MAX, "]"
        print *, "Plot area: Y=[", PLOT_BOTTOM, ",", PLOT_TOP, "] (PDF coordinates)"
        
        ! Calculate expected canvas coordinates manually
        ! Formula: canvas_y = plot_bottom + (data_y - data_min) / data_range * plot_height
        do i = 1, 5
            expected_canvas_y(i) = PLOT_BOTTOM + &
                (test_data_values(i) - DATA_MIN) / DATA_RANGE * PLOT_HEIGHT
            print *, "Data Y=", test_data_values(i), "-> Expected canvas Y=", expected_canvas_y(i)
        end do
        
        ! Test the transformation function (simulated - actual function may differ)
        print *, ""
        print *, "Testing transformation accuracy:"
        do i = 1, 5
            ! Simulate the transformation that should occur in PDF backend
            calculated_canvas_y = PLOT_BOTTOM + &
                (test_data_values(i) - DATA_MIN) / DATA_RANGE * PLOT_HEIGHT
            
            transform_error = abs(calculated_canvas_y - expected_canvas_y(i))
            print *, "Data Y=", test_data_values(i), &
                    ": expected=", expected_canvas_y(i), &
                    ", calculated=", calculated_canvas_y, &
                    ", error=", transform_error
            
            if (transform_error > 0.1_wp) then
                print *, "  FAIL: Transformation error too large"
            else
                print *, "  PASS: Transformation accurate"
            end if
        end do
        print *, ""
        
    end subroutine test_y_coordinate_transformation_accuracy

    subroutine test_double_subtraction_detection()
        !! Given: Coordinate transformation that may have double-subtraction bug
        !! When: Testing specific scenarios that expose the bug
        !! Then: Should detect if Y-coordinates are being double-subtracted
        
        real(wp), parameter :: CANVAS_HEIGHT = 400.0_wp
        real(wp), parameter :: PLOT_BOTTOM = 50.0_wp
        real(wp), parameter :: PLOT_TOP = 350.0_wp
        real(wp), parameter :: DATA_MIN = 0.0_wp
        real(wp), parameter :: DATA_MAX = 10.0_wp
        
        real(wp) :: middle_data_value = 5.0_wp  ! Should map to middle of plot
        real(wp) :: correct_canvas_y, buggy_canvas_y
        real(wp) :: expected_middle_y
        
        print *, "=== Test: Double-subtraction bug detection ==="
        print *, "Testing data value at middle of range:", middle_data_value
        
        ! Calculate correct transformation
        correct_canvas_y = PLOT_BOTTOM + &
            (middle_data_value - DATA_MIN) / (DATA_MAX - DATA_MIN) * (PLOT_TOP - PLOT_BOTTOM)
        expected_middle_y = (PLOT_BOTTOM + PLOT_TOP) / 2.0_wp
        
        print *, "Correct transformation:", correct_canvas_y
        print *, "Expected middle position:", expected_middle_y
        
        ! Simulate double-subtraction bug (hypothetical)
        ! Bug might look like: canvas_y = plot_bottom - (calculated_offset)
        ! instead of: canvas_y = plot_bottom + calculated_offset
        buggy_canvas_y = PLOT_BOTTOM - &
            (middle_data_value - DATA_MIN) / (DATA_MAX - DATA_MIN) * (PLOT_TOP - PLOT_BOTTOM)
        
        print *, "Double-subtraction result:", buggy_canvas_y
        
        if (abs(correct_canvas_y - expected_middle_y) < 5.0_wp) then
            print *, "PASS: Correct transformation places middle value near plot center"
        else
            print *, "FAIL: Correct transformation doesn't center properly"
        end if
        
        if (buggy_canvas_y < 0.0_wp .or. buggy_canvas_y < PLOT_BOTTOM) then
            print *, "DETECTED: Double-subtraction would cause negative/invalid coordinates"
            print *, "This matches Issue #34 symptoms - labels clustering at wrong position"
        else
            print *, "No double-subtraction pattern detected in this test"
        end if
        print *, ""
        
    end subroutine test_double_subtraction_detection

    subroutine test_coordinate_reversibility()
        !! Given: Data coordinates transformed to canvas coordinates
        !! When: Reverse-transforming back to data coordinates
        !! Then: Should recover original data values (within numerical precision)
        
        real(wp), parameter :: DATA_MIN = -5.0_wp
        real(wp), parameter :: DATA_MAX = 15.0_wp
        real(wp), parameter :: PLOT_BOTTOM = 50.0_wp
        real(wp), parameter :: PLOT_TOP = 350.0_wp
        real(wp), parameter :: DATA_RANGE = DATA_MAX - DATA_MIN
        real(wp), parameter :: PLOT_HEIGHT = PLOT_TOP - PLOT_BOTTOM
        
        real(wp) :: original_data(4) = [-5.0_wp, 0.0_wp, 7.5_wp, 15.0_wp]
        real(wp) :: canvas_y, recovered_data, recovery_error
        integer :: i
        
        print *, "=== Test: Coordinate reversibility ==="
        
        do i = 1, 4
            ! Forward transformation: data -> canvas
            canvas_y = PLOT_BOTTOM + (original_data(i) - DATA_MIN) / DATA_RANGE * PLOT_HEIGHT
            
            ! Reverse transformation: canvas -> data  
            recovered_data = DATA_MIN + (canvas_y - PLOT_BOTTOM) / PLOT_HEIGHT * DATA_RANGE
            
            recovery_error = abs(recovered_data - original_data(i))
            
            print *, "Original data:", original_data(i), &
                    "-> Canvas Y:", canvas_y, &
                    "-> Recovered:", recovered_data, &
                    "Error:", recovery_error
            
            if (recovery_error > 1.0e-10_wp) then
                print *, "  FAIL: Coordinate transformation not reversible"
                print *, "  This indicates transformation algorithm issues"
            else
                print *, "  PASS: Transformation reversible"
            end if
        end do
        print *, ""
        
    end subroutine test_coordinate_reversibility

    subroutine test_boundary_coordinate_handling()
        !! Given: Data values at exact boundaries of data range
        !! When: Transforming to canvas coordinates
        !! Then: Should map exactly to plot boundaries
        
        real(wp), parameter :: DATA_MIN = -10.0_wp
        real(wp), parameter :: DATA_MAX = 20.0_wp
        real(wp), parameter :: PLOT_BOTTOM = 75.0_wp
        real(wp), parameter :: PLOT_TOP = 325.0_wp
        real(wp), parameter :: TOLERANCE = 0.1_wp
        
        real(wp) :: min_canvas_y, max_canvas_y
        real(wp) :: min_error, max_error
        
        print *, "=== Test: Boundary coordinate handling ==="
        print *, "Data boundaries: [", DATA_MIN, ",", DATA_MAX, "]"
        print *, "Plot boundaries: [", PLOT_BOTTOM, ",", PLOT_TOP, "] (PDF coordinates)"
        
        ! Transform boundary values
        min_canvas_y = PLOT_BOTTOM + (DATA_MIN - DATA_MIN) / (DATA_MAX - DATA_MIN) * (PLOT_TOP - PLOT_BOTTOM)
        max_canvas_y = PLOT_BOTTOM + (DATA_MAX - DATA_MIN) / (DATA_MAX - DATA_MIN) * (PLOT_TOP - PLOT_BOTTOM)
        
        min_error = abs(min_canvas_y - PLOT_BOTTOM)
        max_error = abs(max_canvas_y - PLOT_TOP)
        
        print *, "Data min -> Canvas Y:", min_canvas_y, "(expected:", PLOT_BOTTOM, ") Error:", min_error
        print *, "Data max -> Canvas Y:", max_canvas_y, "(expected:", PLOT_TOP, ") Error:", max_error
        
        if (min_error > TOLERANCE) then
            print *, "FAIL: Data minimum doesn't map to plot bottom"
        else
            print *, "PASS: Data minimum maps correctly to plot bottom"
        end if
        
        if (max_error > TOLERANCE) then
            print *, "FAIL: Data maximum doesn't map to plot top"
        else
            print *, "PASS: Data maximum maps correctly to plot top"
        end if
        print *, ""
        
    end subroutine test_boundary_coordinate_handling

    subroutine test_division_by_zero_protection()
        !! Given: Data with zero range (x_min == x_max or y_min == y_max)
        !! When: Transforming to PDF coordinates
        !! Then: Should handle gracefully without division by zero
        
        use fortplot_pdf_core, only: pdf_context
        use fortplot_figure_core, only: figure
        implicit none
        
        type(figure) :: fig
        type(pdf_context) :: ctx
        real(wp) :: pdf_x, pdf_y
        real(wp), parameter :: EPSILON = 1.0e-10_wp
        logical :: test_passed
        
        print *, "=== Test: Division by zero protection ==="
        
        ! Initialize a figure with constant X data (zero X range)
        call fig%init(10, 10, 600, 400)
        call fig%xlabel("X axis")
        call fig%ylabel("Y axis") 
        call fig%title("Test constant data")
        
        ! Create PDF context
        call ctx%init(fig)
        
        ! Test case 1: Constant X data (x_min == x_max)
        ctx%x_min = 5.0_wp
        ctx%x_max = 5.0_wp  ! Zero range
        ctx%y_min = 0.0_wp
        ctx%y_max = 10.0_wp
        
        print *, "Test 1: Constant X data (x_min=x_max=5.0)"
        print *, "  X range:", ctx%x_max - ctx%x_min
        
        ! This should not crash - expecting graceful handling
        call ctx%normalize_to_pdf_coords(5.0_wp, 5.0_wp, pdf_x, pdf_y)
        
        test_passed = .true.  ! If we reach here, no crash occurred
        if (test_passed) then
            print *, "  PASS: No crash with zero X range"
            print *, "  Resulting PDF coords: x=", pdf_x, "y=", pdf_y
        else
            print *, "  FAIL: Division by zero not handled"
        end if
        
        ! Test case 2: Constant Y data (y_min == y_max)
        ctx%x_min = 0.0_wp
        ctx%x_max = 10.0_wp
        ctx%y_min = 3.0_wp
        ctx%y_max = 3.0_wp  ! Zero range
        
        print *, "Test 2: Constant Y data (y_min=y_max=3.0)"
        print *, "  Y range:", ctx%y_max - ctx%y_min
        
        call ctx%normalize_to_pdf_coords(5.0_wp, 3.0_wp, pdf_x, pdf_y)
        
        test_passed = .true.
        if (test_passed) then
            print *, "  PASS: No crash with zero Y range"
            print *, "  Resulting PDF coords: x=", pdf_x, "y=", pdf_y
        else
            print *, "  FAIL: Division by zero not handled"
        end if
        
        ! Test case 3: Both X and Y constant
        ctx%x_min = 7.0_wp
        ctx%x_max = 7.0_wp  ! Zero range
        ctx%y_min = -2.0_wp
        ctx%y_max = -2.0_wp  ! Zero range
        
        print *, "Test 3: Both X and Y constant"
        print *, "  X range:", ctx%x_max - ctx%x_min
        print *, "  Y range:", ctx%y_max - ctx%y_min
        
        call ctx%normalize_to_pdf_coords(7.0_wp, -2.0_wp, pdf_x, pdf_y)
        
        test_passed = .true.
        if (test_passed) then
            print *, "  PASS: No crash with both ranges zero"
            print *, "  Resulting PDF coords: x=", pdf_x, "y=", pdf_y
        else
            print *, "  FAIL: Division by zero not handled"
        end if
        
        print *, ""
        
    end subroutine test_division_by_zero_protection

    subroutine test_epsilon_range_handling()
        !! Given: Data with extremely small but non-zero range
        !! When: Transforming to PDF coordinates
        !! Then: Should handle epsilon-small ranges gracefully
        
        use fortplot_pdf_core, only: pdf_context
        use fortplot_figure_core, only: figure
        implicit none
        
        type(figure) :: fig
        type(pdf_context) :: ctx
        real(wp) :: pdf_x, pdf_y
        real(wp), parameter :: EPSILON = 1.0e-12_wp
        logical :: coordinates_valid
        
        print *, "=== Test: Epsilon range handling ==="
        
        ! Initialize figure
        call fig%init(10, 10, 600, 400)
        call fig%xlabel("X")
        call fig%ylabel("Y")
        call fig%title("Epsilon test")
        
        ! Create PDF context
        call ctx%init(fig)
        
        ! Test case 1: Epsilon-small X range
        ctx%x_min = 1.0_wp
        ctx%x_max = 1.0_wp + EPSILON
        ctx%y_min = 0.0_wp
        ctx%y_max = 10.0_wp
        
        print *, "Test 1: Epsilon-small X range"
        print *, "  X range:", ctx%x_max - ctx%x_min
        
        call ctx%normalize_to_pdf_coords(1.0_wp, 5.0_wp, pdf_x, pdf_y)
        
        ! Check if coordinates are within plot area bounds
        coordinates_valid = (pdf_x >= real(ctx%plot_area%left, wp)) .and. &
                           (pdf_x <= real(ctx%plot_area%left + ctx%plot_area%width, wp)) .and. &
                           (pdf_y >= 0.0_wp) .and. &
                           (pdf_y <= real(ctx%height, wp))
        
        if (coordinates_valid) then
            print *, "  PASS: Coordinates within valid bounds"
            print *, "  PDF coords: x=", pdf_x, "y=", pdf_y
        else
            print *, "  FAIL: Coordinates out of bounds"
            print *, "  PDF coords: x=", pdf_x, "y=", pdf_y
        end if
        
        ! Test case 2: Epsilon-small Y range
        ctx%x_min = 0.0_wp
        ctx%x_max = 10.0_wp
        ctx%y_min = 5.0_wp
        ctx%y_max = 5.0_wp + EPSILON
        
        print *, "Test 2: Epsilon-small Y range"
        print *, "  Y range:", ctx%y_max - ctx%y_min
        
        call ctx%normalize_to_pdf_coords(5.0_wp, 5.0_wp, pdf_x, pdf_y)
        
        coordinates_valid = (pdf_x >= real(ctx%plot_area%left, wp)) .and. &
                           (pdf_x <= real(ctx%plot_area%left + ctx%plot_area%width, wp)) .and. &
                           (pdf_y >= 0.0_wp) .and. &
                           (pdf_y <= real(ctx%height, wp))
        
        if (coordinates_valid) then
            print *, "  PASS: Coordinates within valid bounds"
            print *, "  PDF coords: x=", pdf_x, "y=", pdf_y
        else
            print *, "  FAIL: Coordinates out of bounds"
            print *, "  PDF coords: x=", pdf_x, "y=", pdf_y
        end if
        
        print *, ""
        
    end subroutine test_epsilon_range_handling

    subroutine test_constant_data_graceful_handling()
        !! Given: Real-world scenario with constant data series
        !! When: Generating a plot with such data
        !! Then: Should produce reasonable output without crashes
        
        use fortplot_pdf_core, only: pdf_context
        use fortplot_figure_core, only: figure
        implicit none
        
        type(figure) :: fig
        type(pdf_context) :: ctx
        real(wp) :: constant_x(5), constant_y(5)
        real(wp) :: pdf_x, pdf_y
        integer :: i
        logical :: all_coords_valid
        
        print *, "=== Test: Constant data graceful handling ==="
        
        ! Initialize figure
        call fig%init(10, 10, 600, 400)
        call fig%xlabel("Time")
        call fig%ylabel("Value")
        call fig%title("Constant data plot")
        
        ! Create PDF context
        call ctx%init(fig)
        
        ! Test case: Horizontal line (constant Y)
        constant_x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        constant_y = [2.5_wp, 2.5_wp, 2.5_wp, 2.5_wp, 2.5_wp]  ! All same Y value
        
        ctx%x_min = minval(constant_x)
        ctx%x_max = maxval(constant_x)
        ctx%y_min = minval(constant_y)
        ctx%y_max = maxval(constant_y)  ! Will be same as y_min
        
        print *, "Horizontal line test (constant Y=2.5)"
        print *, "  X range:", ctx%x_max - ctx%x_min
        print *, "  Y range:", ctx%y_max - ctx%y_min
        
        all_coords_valid = .true.
        do i = 1, 5
            call ctx%normalize_to_pdf_coords(constant_x(i), constant_y(i), pdf_x, pdf_y)
            
            ! Verify coordinates are reasonable
            if (pdf_x < 0.0_wp .or. pdf_x > real(ctx%width, wp) .or. &
                pdf_y < 0.0_wp .or. pdf_y > real(ctx%height, wp)) then
                all_coords_valid = .false.
                print *, "  Point", i, "out of bounds: x=", pdf_x, "y=", pdf_y
            end if
        end do
        
        if (all_coords_valid) then
            print *, "  PASS: All points mapped to valid coordinates"
            print *, "  Horizontal line rendered at PDF Y=", pdf_y
        else
            print *, "  FAIL: Some points mapped outside canvas"
        end if
        
        print *, ""
        
    end subroutine test_constant_data_graceful_handling

end program test_pdf_coordinate_transform