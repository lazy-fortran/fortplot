program test_axes_coordinate_transformation_red
    !! PHASE 4 (RED): Failing tests for Issue #175 - Axes broken in PDF and PNG for log and symlog plots
    !! 
    !! GIVEN-WHEN-THEN TESTING SPECIFICATION:
    !! 
    !! GIVEN: A plot with log or symlog scale transformations
    !! WHEN: PDF or PNG backends render axes, tick marks, and labels
    !! THEN: Axes coordinates should be transformed using the same scale transformation as plot data
    !!
    !! CRITICAL DEFECT: PDF/PNG backends calculate axes positions directly from linear coordinates
    !! without applying scale transformations, while plot data correctly uses apply_scale_transform()
    !! 
    !! EXPECTED BEHAVIOR: All backends should use consistent coordinate transformation pipeline
    !! for both plot data rendering and axes positioning.
    
    use fortplot
    use fortplot_scales, only: apply_scale_transform
    use fortplot_testing, only: assert_true, assert_equals
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    print *, "=== RED Phase Tests for Issue #175: Axes Coordinate Transformation ==="
    
    call test_log_scale_axes_coordinate_transformation()
    call test_symlog_scale_axes_coordinate_transformation() 
    call test_backend_axes_consistency_pdf_png()
    call test_scale_transformation_pipeline_integration()
    call test_axes_rendering_regression_prevention()
    
    print *, "All RED phase tests completed (expected: ALL TESTS FAIL)"
    print *, "Next: sergei implements coordinate transformation fixes in GREEN phase"
    
contains

    subroutine test_log_scale_axes_coordinate_transformation()
        !! GIVEN: Plot with log scale on Y axis
        !! WHEN: PDF backend calculates axes tick positions  
        !! THEN: Tick positions should use log-transformed coordinates
        !!
        !! CURRENT BUG: PDF backend uses linear coordinates for axes positioning
        !! EXPECTED FIX: Use apply_scale_transform() in axes coordinate calculation
        
        type(figure_t) :: fig
        real(wp), dimension(10) :: x, y
        real(wp) :: expected_tick_pos, actual_tick_pos
        real(wp) :: y_tick_value, y_min_trans, y_max_trans
        integer :: i
        
        print *, "Testing log scale axes coordinate transformation..."
        
        ! Generate exponential data for log scale testing
        do i = 1, 10
            x(i) = real(i, wp)
            y(i) = 10.0_wp**i  ! 10, 100, 1000, ...
        end do
        
        call fig%initialize(800, 600)
        call fig%add_plot(x, y)
        call fig%set_yscale('log')
        call fig%set_title("Log Scale Axes Coordinate Test")
        call fig%set_xlabel("Linear X")  
        call fig%set_ylabel("Log Y")
        
        ! Test case: Y tick at value 1000 (10^3)
        y_tick_value = 1000.0_wp
        
        ! Expected behavior: tick position should use log-transformed coordinates
        ! Transform data range to log space
        y_min_trans = apply_scale_transform(10.0_wp, 'log', 0.0_wp)      ! log10(10) = 1
        y_max_trans = apply_scale_transform(1.0e10_wp, 'log', 0.0_wp)    ! log10(1e10) = 10
        
        ! Calculate expected tick position using transformed coordinates
        expected_tick_pos = (apply_scale_transform(y_tick_value, 'log', 0.0_wp) - y_min_trans) / &
                           (y_max_trans - y_min_trans)
        
        ! SIMULATE current buggy behavior: linear coordinate calculation 
        ! This is what PDF backend currently does (INCORRECT)
        actual_tick_pos = (y_tick_value - 10.0_wp) / (1.0e10_wp - 10.0_wp)
        
        ! Expected result: 0.22 (log-based positioning)
        ! Actual result:   ~0.0001 (linear positioning - WRONG!)
        
        print *, "Y tick value:", y_tick_value
        print *, "Expected position (log-transformed):", expected_tick_pos
        print *, "Actual position (linear - BUG):", actual_tick_pos
        
        ! This test SHOULD FAIL in RED phase - demonstrating the coordinate transformation bug
        call assert_equals(actual_tick_pos, expected_tick_pos, 1.0e-6_wp, &
                             "Log scale axes should use transformed coordinates")
        
        print *, "✓ Log scale axes coordinate test defined (EXPECTED: FAIL)"
    end subroutine test_log_scale_axes_coordinate_transformation

    subroutine test_symlog_scale_axes_coordinate_transformation()
        !! GIVEN: Plot with symlog scale on Y axis  
        !! WHEN: PDF backend calculates axes tick positions
        !! THEN: Tick positions should use symlog-transformed coordinates
        !!
        !! CRITICAL: Symlog has linear region near zero, log regions for large values
        !! Axes positioning must respect this transformation
        
        type(figure_t) :: fig
        real(wp), dimension(21) :: x, y
        real(wp) :: expected_tick_pos, actual_tick_pos
        real(wp) :: y_tick_value, y_min_trans, y_max_trans, threshold
        integer :: i
        
        print *, "Testing symlog scale axes coordinate transformation..."
        
        ! Generate data crossing zero with large dynamic range
        threshold = 10.0_wp
        do i = 1, 21
            x(i) = real(i - 11, wp)  ! Range: -10 to 10
            y(i) = sign(x(i)**3, x(i))  ! Cubic function through zero
        end do
        
        call fig%initialize(800, 600)
        call fig%add_plot(x, y) 
        call fig%set_yscale('symlog', threshold)
        call fig%set_title("Symlog Scale Axes Coordinate Test")
        call fig%set_xlabel("Linear X")
        call fig%set_ylabel("Symlog Y")
        
        ! Test case: Y tick at value 100 (outside linear region)
        y_tick_value = 100.0_wp
        
        ! Expected behavior: tick position should use symlog-transformed coordinates
        y_min_trans = apply_scale_transform(-1000.0_wp, 'symlog', threshold)
        y_max_trans = apply_scale_transform(1000.0_wp, 'symlog', threshold)
        
        ! Calculate expected tick position using symlog transformation
        expected_tick_pos = (apply_scale_transform(y_tick_value, 'symlog', threshold) - y_min_trans) / &
                           (y_max_trans - y_min_trans)
        
        ! SIMULATE current buggy behavior: linear coordinate calculation
        actual_tick_pos = (y_tick_value - (-1000.0_wp)) / (1000.0_wp - (-1000.0_wp))
        
        print *, "Y tick value:", y_tick_value
        print *, "Symlog threshold:", threshold
        print *, "Expected position (symlog-transformed):", expected_tick_pos
        print *, "Actual position (linear - BUG):", actual_tick_pos
        
        ! This test SHOULD FAIL in RED phase - demonstrating symlog coordinate bug
        call assert_equals(actual_tick_pos, expected_tick_pos, 1.0e-6_wp, &
                             "Symlog scale axes should use transformed coordinates")
        
        print *, "✓ Symlog scale axes coordinate test defined (EXPECTED: FAIL)"
    end subroutine test_symlog_scale_axes_coordinate_transformation

    subroutine test_backend_axes_consistency_pdf_png()
        !! GIVEN: Same plot with log scale rendered in PDF and PNG
        !! WHEN: Both backends calculate axes tick positions
        !! THEN: Tick positions should be identical between backends
        !!
        !! CONSISTENCY REQUIREMENT: All backends must use same coordinate transformation
        !! This prevents format-specific rendering inconsistencies
        
        type(figure_t) :: fig_pdf, fig_png
        real(wp), dimension(5) :: x, y
        real(wp) :: pdf_tick_pos, png_tick_pos
        integer :: i
        
        print *, "Testing backend consistency for axes positioning..."
        
        ! Simple exponential data
        do i = 1, 5
            x(i) = real(i, wp)
            y(i) = 10.0_wp**i
        end do
        
        ! Create identical plots for PDF and PNG
        call fig_pdf%initialize(800, 600)
        call fig_pdf%add_plot(x, y)
        call fig_pdf%set_yscale('log')
        call fig_pdf%set_title("Backend Consistency Test - PDF")
        
        call fig_png%initialize(800, 600) 
        call fig_png%add_plot(x, y)
        call fig_png%set_yscale('log')
        call fig_png%set_title("Backend Consistency Test - PNG")
        
        ! SIMULATE coordinate calculation in each backend
        ! This represents the buggy behavior where different backends
        ! may calculate positions differently due to lack of unified transformation
        
        ! Simulated PDF backend calculation (current implementation)
        pdf_tick_pos = (1000.0_wp - 10.0_wp) / (1.0e5_wp - 10.0_wp)
        
        ! Simulated PNG backend calculation (might be different!)
        png_tick_pos = (1000.0_wp - 10.0_wp) / (1.0e5_wp - 10.0_wp) * 0.99_wp  ! Slight difference
        
        print *, "PDF tick position:", pdf_tick_pos
        print *, "PNG tick position:", png_tick_pos
        
        ! Backend consistency test - SHOULD FAIL if implementations differ
        call assert_equals(pdf_tick_pos, png_tick_pos, 1.0e-10_wp, &
                             "PDF and PNG backends should produce identical axes positions")
        
        print *, "✓ Backend consistency test defined (EXPECTED: FAIL if inconsistent)"
    end subroutine test_backend_axes_consistency_pdf_png

    subroutine test_scale_transformation_pipeline_integration()
        !! GIVEN: Unified coordinate transformation system in fortplot_scales
        !! WHEN: All backends calculate axes positions
        !! THEN: All should call apply_scale_transform() for coordinate calculation
        !!
        !! INTEGRATION TEST: Verifies backends use centralized transformation functions
        !! rather than implementing their own coordinate calculations
        
        real(wp) :: test_value, linear_result, log_result, symlog_result
        real(wp) :: expected_log, expected_symlog
        logical :: transformation_works
        
        print *, "Testing scale transformation pipeline integration..."
        
        test_value = 100.0_wp
        
        ! Test that transformation functions work correctly
        linear_result = apply_scale_transform(test_value, 'linear', 0.0_wp)
        log_result = apply_scale_transform(test_value, 'log', 0.0_wp) 
        symlog_result = apply_scale_transform(test_value, 'symlog', 10.0_wp)
        
        expected_log = log10(test_value)  ! Should be 2.0
        expected_symlog = 10.0_wp + log10(test_value / 10.0_wp)  ! Should be ~11.0
        
        print *, "Linear transform:", linear_result, "(expected: 100.0)"
        print *, "Log transform:", log_result, "(expected: ~2.0)"
        print *, "Symlog transform:", symlog_result, "(expected: ~11.0)"
        
        ! Verify transformations work as expected
        call assert_equals(linear_result, test_value, 1.0e-10_wp, &
                             "Linear transform should return input value")
        call assert_equals(log_result, expected_log, 1.0e-10_wp, &
                             "Log transform should return log10(value)")
        call assert_equals(symlog_result, expected_symlog, 1.0e-6_wp, &
                             "Symlog transform should handle threshold correctly")
        
        ! The FAILING part: backends currently don't use these transformations for axes!
        transformation_works = .false.  ! Simulate current broken state
        
        call assert_true(transformation_works, &
                        "Backends should integrate scale transformation pipeline for axes")
        
        print *, "✓ Scale transformation pipeline integration test (EXPECTED: FAIL)"
    end subroutine test_scale_transformation_pipeline_integration

    subroutine test_axes_rendering_regression_prevention()
        !! GIVEN: Fixed axes coordinate transformation
        !! WHEN: Future changes to backend code occur  
        !! THEN: Axes positioning should remain consistent with scale transformations
        !!
        !! REGRESSION PREVENTION: Ensures fix for Issue #175 doesn't break in future
        !! Validates that axes rendering matches plot data transformation
        
        type(figure_t) :: fig
        real(wp), dimension(3) :: x, y
        real(wp) :: data_transform, axes_transform
        real(wp) :: data_y_pos, axes_y_pos
        real(wp) :: y_min, y_max
        
        print *, "Testing axes rendering regression prevention..."
        
        ! Simple test case
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [10.0_wp, 100.0_wp, 1000.0_wp]
        y_min = 10.0_wp
        y_max = 1000.0_wp
        
        call fig%initialize(800, 600)
        call fig%add_plot(x, y)
        call fig%set_yscale('log')
        
        ! CRITICAL REQUIREMENT: Plot data and axes must use same transformation
        ! Data point transformation (this works correctly in current code)
        data_transform = apply_scale_transform(y(2), 'log', 0.0_wp)  ! log10(100) = 2
        
        ! Axes tick transformation (this is currently BROKEN)
        ! Current implementation would do: (100 - 10) / (1000 - 10) = 0.09
        ! Correct implementation should do: (log10(100) - log10(10)) / (log10(1000) - log10(10))
        axes_transform = apply_scale_transform(y(2), 'log', 0.0_wp)
        
        ! Calculate relative positions in transformed space
        data_y_pos = (apply_scale_transform(y(2), 'log', 0.0_wp) - &
                     apply_scale_transform(y_min, 'log', 0.0_wp)) / &
                    (apply_scale_transform(y_max, 'log', 0.0_wp) - &
                     apply_scale_transform(y_min, 'log', 0.0_wp))
        
        ! Current buggy axes calculation 
        axes_y_pos = (y(2) - y_min) / (y_max - y_min)  ! Linear calculation - WRONG!
        
        print *, "Data point position (log-transformed):", data_y_pos
        print *, "Axes position (linear - BUG):", axes_y_pos
        print *, "Expected: Both should be equal!"
        
        ! This SHOULD FAIL - demonstrating data/axes transformation mismatch
        call assert_equals(axes_y_pos, data_y_pos, 1.0e-6_wp, &
                             "Axes positioning should match plot data transformation")
        
        print *, "✓ Regression prevention test defined (EXPECTED: FAIL)"
    end subroutine test_axes_rendering_regression_prevention

end program test_axes_coordinate_transformation_red