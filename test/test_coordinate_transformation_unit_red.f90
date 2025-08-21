program test_coordinate_transformation_unit_red
    !! PHASE 4 (RED): Unit tests for coordinate transformation accuracy
    !!
    !! GIVEN-WHEN-THEN UNIT TESTING:
    !!
    !! GIVEN: Scale transformation functions in fortplot_scales
    !! WHEN: Backend coordinate calculations are performed
    !! THEN: Transformations should be applied consistently for both data and axes
    !!
    !! UNIT TEST FOCUS: Isolated testing of coordinate transformation mathematics
    !! without full figure context. Validates transformation accuracy at function level.
    
    use fortplot_scales, only: apply_scale_transform, apply_inverse_scale_transform
    use fortplot_testing, only: assert_equals, assert_true
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    print *, "=== Unit Tests: Coordinate Transformation Accuracy ==="
    
    call test_log_transformation_accuracy()
    call test_symlog_transformation_accuracy()
    call test_inverse_transformation_consistency()
    call test_coordinate_mapping_precision()
    call test_scale_aware_axes_positioning_math()
    
    print *, "All coordinate transformation unit tests completed (RED phase)"
    
contains

    subroutine test_log_transformation_accuracy()
        !! GIVEN: Log scale transformation function
        !! WHEN: Applied to test values
        !! THEN: Should return mathematically correct log10 results
        !!
        !! MATHEMATICAL CORRECTNESS: Validates log transformation implementation
        
        real(wp) :: input_val, expected, actual
        real(wp), parameter :: TOLERANCE = 1.0e-12_wp
        
        print *, "Testing log transformation mathematical accuracy..."
        
        ! Test case 1: Powers of 10
        input_val = 100.0_wp
        expected = 2.0_wp  ! log10(100) = 2
        actual = apply_scale_transform(input_val, 'log', 0.0_wp)
        
        print *, "Input:", input_val, "Expected:", expected, "Actual:", actual
        call assert_equals(actual, expected, TOLERANCE, &
                             "Log transform should return exact log10 for powers of 10")
        
        ! Test case 2: Unit value
        input_val = 1.0_wp
        expected = 0.0_wp  ! log10(1) = 0
        actual = apply_scale_transform(input_val, 'log', 0.0_wp)
        
        print *, "Input:", input_val, "Expected:", expected, "Actual:", actual
        call assert_equals(actual, expected, TOLERANCE, &
                             "Log transform should return 0 for input 1.0")
        
        ! Test case 3: Fractional value  
        input_val = 0.1_wp
        expected = -1.0_wp  ! log10(0.1) = -1
        actual = apply_scale_transform(input_val, 'log', 0.0_wp)
        
        print *, "Input:", input_val, "Expected:", expected, "Actual:", actual
        call assert_equals(actual, expected, TOLERANCE, &
                             "Log transform should handle fractional values correctly")
        
        print *, "✓ Log transformation accuracy verified"
    end subroutine test_log_transformation_accuracy

    subroutine test_symlog_transformation_accuracy()
        !! GIVEN: Symlog scale transformation function  
        !! WHEN: Applied to values in different regions (linear/log)
        !! THEN: Should return correct results based on threshold
        !!
        !! SYMLOG REGIONS: Linear for |x| <= threshold, logarithmic outside
        
        real(wp) :: input_val, threshold, expected, actual
        real(wp), parameter :: TOLERANCE = 1.0e-10_wp
        
        print *, "Testing symlog transformation mathematical accuracy..."
        
        threshold = 10.0_wp
        
        ! Test case 1: Within linear region (positive)
        input_val = 5.0_wp  ! |5| < 10, should be linear
        expected = 5.0_wp   ! No transformation in linear region
        actual = apply_scale_transform(input_val, 'symlog', threshold)
        
        print *, "Linear region (+):", input_val, "Expected:", expected, "Actual:", actual
        call assert_equals(actual, expected, TOLERANCE, &
                             "Symlog should be linear within threshold (positive)")
        
        ! Test case 2: Within linear region (negative)
        input_val = -5.0_wp  ! |-5| < 10, should be linear
        expected = -5.0_wp   ! No transformation in linear region
        actual = apply_scale_transform(input_val, 'symlog', threshold)
        
        print *, "Linear region (-):", input_val, "Expected:", expected, "Actual:", actual
        call assert_equals(actual, expected, TOLERANCE, &
                             "Symlog should be linear within threshold (negative)")
        
        ! Test case 3: Outside linear region (positive logarithmic)
        input_val = 100.0_wp  ! 100 > 10, should be logarithmic
        expected = threshold + log10(input_val / threshold)  ! 10 + log10(10) = 11
        actual = apply_scale_transform(input_val, 'symlog', threshold)
        
        print *, "Log region (+):", input_val, "Expected:", expected, "Actual:", actual
        call assert_equals(actual, expected, TOLERANCE, &
                             "Symlog should use log transformation outside threshold (positive)")
        
        ! Test case 4: Outside linear region (negative logarithmic)
        input_val = -100.0_wp  ! |-100| > 10, should be logarithmic
        expected = -threshold - log10(-input_val / threshold)  ! -10 - log10(10) = -11
        actual = apply_scale_transform(input_val, 'symlog', threshold)
        
        print *, "Log region (-):", input_val, "Expected:", expected, "Actual:", actual
        call assert_equals(actual, expected, TOLERANCE, &
                             "Symlog should use log transformation outside threshold (negative)")
        
        print *, "✓ Symlog transformation accuracy verified"
    end subroutine test_symlog_transformation_accuracy

    subroutine test_inverse_transformation_consistency()
        !! GIVEN: Forward and inverse scale transformations
        !! WHEN: Applied in sequence (forward then inverse)
        !! THEN: Should recover original value (round-trip consistency)
        !!
        !! ROUND-TRIP PROPERTY: f_inverse(f_forward(x)) = x
        
        real(wp) :: original, forward, recovered
        real(wp), parameter :: TOLERANCE = 1.0e-10_wp
        real(wp) :: threshold
        
        print *, "Testing forward/inverse transformation consistency..."
        
        ! Test log scale round-trip
        original = 123.456_wp
        forward = apply_scale_transform(original, 'log', 0.0_wp)
        recovered = apply_inverse_scale_transform(forward, 'log', 0.0_wp)
        
        print *, "Log round-trip: Original:", original, "Recovered:", recovered
        call assert_equals(recovered, original, TOLERANCE, &
                             "Log inverse should recover original value")
        
        ! Test symlog scale round-trip (linear region)
        threshold = 10.0_wp
        original = 5.0_wp  ! Within linear region
        forward = apply_scale_transform(original, 'symlog', threshold)
        recovered = apply_inverse_scale_transform(forward, 'symlog', threshold)
        
        print *, "Symlog linear round-trip: Original:", original, "Recovered:", recovered
        call assert_equals(recovered, original, TOLERANCE, &
                             "Symlog inverse should recover original value (linear region)")
        
        ! Test symlog scale round-trip (log region)
        original = 200.0_wp  ! Outside linear region
        forward = apply_scale_transform(original, 'symlog', threshold)
        recovered = apply_inverse_scale_transform(forward, 'symlog', threshold)
        
        print *, "Symlog log round-trip: Original:", original, "Recovered:", recovered
        call assert_equals(recovered, original, TOLERANCE, &
                             "Symlog inverse should recover original value (log region)")
        
        print *, "✓ Inverse transformation consistency verified"
    end subroutine test_inverse_transformation_consistency

    subroutine test_coordinate_mapping_precision()
        !! GIVEN: Coordinate mapping function for screen positioning
        !! WHEN: Applied to transformed data ranges
        !! THEN: Should provide precise screen coordinate mapping
        !!
        !! PRECISION TEST: Validates numerical accuracy of coordinate mapping
        
        real(wp) :: data_val, data_min, data_max, screen_pos, expected_pos
        real(wp) :: trans_val, trans_min, trans_max
        integer, parameter :: SCREEN_WIDTH = 800
        real(wp), parameter :: TOLERANCE = 1.0e-10_wp
        
        print *, "Testing coordinate mapping precision..."
        
        ! Test scenario: Log scale data mapping to screen coordinates
        data_val = 100.0_wp    ! Middle value
        data_min = 10.0_wp     ! Min value  
        data_max = 1000.0_wp   ! Max value
        
        ! Transform all values to log space
        trans_val = apply_scale_transform(data_val, 'log', 0.0_wp)  ! log10(100) = 2
        trans_min = apply_scale_transform(data_min, 'log', 0.0_wp)  ! log10(10) = 1  
        trans_max = apply_scale_transform(data_max, 'log', 0.0_wp)  ! log10(1000) = 3
        
        ! Calculate screen position using transformed coordinates
        screen_pos = (trans_val - trans_min) / (trans_max - trans_min) * real(SCREEN_WIDTH, wp)
        
        ! Expected: (2 - 1) / (3 - 1) * 800 = 0.5 * 800 = 400
        expected_pos = 400.0_wp  
        
        print *, "Data value:", data_val
        print *, "Transformed value:", trans_val
        print *, "Screen position:", screen_pos
        print *, "Expected position:", expected_pos
        
        call assert_equals(screen_pos, expected_pos, TOLERANCE, &
                             "Log scale coordinate mapping should be precise")
        
        print *, "✓ Coordinate mapping precision verified"
    end subroutine test_coordinate_mapping_precision

    subroutine test_scale_aware_axes_positioning_math()
        !! GIVEN: Mathematical requirements for scale-aware axes positioning
        !! WHEN: Backends calculate tick positions for non-linear scales
        !! THEN: Mathematics should match plot data coordinate transformation
        !!
        !! MATHEMATICAL CONSISTENCY: Ensures axes and data use identical math
        
        real(wp) :: tick_value, data_min, data_max
        real(wp) :: data_pos, axes_pos_correct, axes_pos_buggy
        real(wp), parameter :: TOLERANCE = 1.0e-12_wp
        integer, parameter :: PLOT_HEIGHT = 600
        
        print *, "Testing scale-aware axes positioning mathematics..."
        
        ! Example: Log scale axes positioning
        tick_value = 100.0_wp
        data_min = 1.0_wp
        data_max = 10000.0_wp
        
        ! CORRECT approach: Transform all values, then calculate position
        data_pos = (apply_scale_transform(tick_value, 'log', 0.0_wp) - &
                   apply_scale_transform(data_min, 'log', 0.0_wp)) / &
                  (apply_scale_transform(data_max, 'log', 0.0_wp) - &
                   apply_scale_transform(data_min, 'log', 0.0_wp)) * real(PLOT_HEIGHT, wp)
        
        ! BUGGY approach (current implementation): Linear calculation without transformation
        axes_pos_buggy = (tick_value - data_min) / (data_max - data_min) * real(PLOT_HEIGHT, wp)
        
        ! What the correct implementation should produce
        axes_pos_correct = data_pos
        
        print *, "Tick value:", tick_value
        print *, "Data position (correct):", data_pos
        print *, "Axes position (correct):", axes_pos_correct  
        print *, "Axes position (buggy):", axes_pos_buggy
        
        ! This test demonstrates the mathematical error in current implementation
        print *, "Expected: data_pos ≈ axes_pos_correct"
        print *, "Current bug: axes_pos_buggy ≠ data_pos"
        
        ! The assertion that SHOULD pass but currently fails
        call assert_equals(axes_pos_buggy, axes_pos_correct, TOLERANCE, &
                             "Axes positioning math should match data positioning math")
        
        ! Expected result:
        ! - data_pos ≈ 300 (middle of 600px height, since log10(100) is middle of log range)
        ! - axes_pos_correct ≈ 300 (same as data_pos - this is correct)
        ! - axes_pos_buggy ≈ 6 (nearly at bottom, since 100 is small compared to 10000)
        
        print *, "✓ Scale-aware axes positioning math test (EXPECTED: FAIL in RED phase)"
    end subroutine test_scale_aware_axes_positioning_math

end program test_coordinate_transformation_unit_red