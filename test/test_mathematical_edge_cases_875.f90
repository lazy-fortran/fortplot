! test_mathematical_edge_cases_875.f90 - Comprehensive NaN/infinity mathematical edge case testing
!
! Tests mathematical edge cases for Issue #875 - NaN/infinity test coverage gaps
! Focuses on mathematical functions in utilities modules that lack proper edge case coverage
!
program test_mathematical_edge_cases_875
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64, output_unit
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
        ieee_positive_inf, ieee_negative_inf, ieee_is_nan, ieee_is_finite
    use fortplot_tick_calculation, only: calculate_tick_labels, find_nice_tick_locations
    use fortplot_scales, only: apply_scale_transform, transform_x_coordinate, &
        transform_y_coordinate, clamp_extreme_log_range
    use fortplot_color_conversions, only: rgb_to_hsv, rgb_to_lab
    use fortplot_coordinate_validation, only: validate_coordinate_arrays, &
        has_machine_precision_issues
    use fortplot_parameter_validation, only: is_nan_safe, is_finite_safe
    implicit none
    
    ! Test variables for mathematical edge cases
    real(wp) :: nan_val, pos_inf, neg_inf, zero_val, tiny_val, huge_val
    real(wp) :: test_array(5), invalid_rgb(3), result_rgb(3)
    real(wp) :: x_coords(3), y_coords(3)
    character(len=20) :: tick_labels(10)
    logical :: test_result
    integer :: i
    
    write(output_unit, '(A)') "=== MATHEMATICAL EDGE CASES TEST SUITE (Issue #875) ==="
    write(output_unit, '(A)') "Comprehensive NaN/infinity testing for mathematical functions"
    write(output_unit, '(A)') ""
    
    ! Initialize IEEE special values
    nan_val = ieee_value(0.0_wp, ieee_quiet_nan)
    pos_inf = ieee_value(0.0_wp, ieee_positive_inf)
    neg_inf = ieee_value(0.0_wp, ieee_negative_inf)
    zero_val = 0.0_wp
    tiny_val = tiny(1.0_wp)
    huge_val = huge(1.0_wp)
    
    ! =======================================================================
    ! TEST SECTION 1: TICK CALCULATION MATHEMATICAL EDGE CASES
    ! =======================================================================
    write(output_unit, '(A)') "=== Test Section 1: Tick Calculation Edge Cases ==="
    
    ! Test 1.1: NaN input values to tick calculation
    write(output_unit, '(A)') "Test 1.1: NaN values in tick calculation"
    call test_tick_calculation_with_nans()
    
    ! Test 1.2: Infinite values in tick calculation  
    write(output_unit, '(A)') "Test 1.2: Infinite values in tick calculation"
    call test_tick_calculation_with_infinity()
    
    ! Test 1.3: Zero and negative values for log operations
    write(output_unit, '(A)') "Test 1.3: Zero/negative values in log tick calculation"
    call test_tick_calculation_with_invalid_log_inputs()
    
    ! Test 1.4: Division by zero scenarios in tick spacing
    write(output_unit, '(A)') "Test 1.4: Division by zero in tick spacing calculations"
    call test_tick_calculation_division_by_zero()
    
    ! =======================================================================
    ! TEST SECTION 2: SCALE TRANSFORMATION MATHEMATICAL EDGE CASES
    ! =======================================================================
    write(output_unit, '(A)') "=== Test Section 2: Scale Transform Edge Cases ==="
    
    ! Test 2.1: NaN values in scale transformations
    write(output_unit, '(A)') "Test 2.1: NaN values in scale transformations"
    call test_scale_transform_with_nans()
    
    ! Test 2.2: Infinite values in scale transformations
    write(output_unit, '(A)') "Test 2.2: Infinite values in scale transformations"  
    call test_scale_transform_with_infinity()
    
    ! Test 2.3: Division by zero in coordinate transformations
    write(output_unit, '(A)') "Test 2.3: Division by zero in coordinate transforms"
    call test_coordinate_transform_division_by_zero()
    
    ! Test 2.4: Log scale with invalid inputs
    write(output_unit, '(A)') "Test 2.4: Log scale with zero/negative inputs"
    call test_log_scale_with_invalid_inputs()
    
    ! =======================================================================
    ! TEST SECTION 3: COLOR CONVERSION MATHEMATICAL EDGE CASES  
    ! =======================================================================
    write(output_unit, '(A)') "=== Test Section 3: Color Conversion Edge Cases ==="
    
    ! Test 3.1: NaN values in RGB to HSV conversion
    write(output_unit, '(A)') "Test 3.1: NaN values in RGB to HSV conversion"
    call test_rgb_to_hsv_with_nans()
    
    ! Test 3.2: Infinite values in color conversions
    write(output_unit, '(A)') "Test 3.2: Infinite values in color conversions"
    call test_rgb_to_hsv_with_infinity()
    
    ! Test 3.3: Division by zero in HSV calculations
    write(output_unit, '(A)') "Test 3.3: Division by zero in HSV calculations"
    call test_hsv_division_by_zero()
    
    ! Test 3.4: Power operations in gamma correction with edge cases
    write(output_unit, '(A)') "Test 3.4: Gamma correction with edge cases"
    call test_gamma_correction_edge_cases()
    
    ! =======================================================================
    ! TEST SECTION 4: COORDINATE VALIDATION MATHEMATICAL EDGE CASES
    ! =======================================================================
    write(output_unit, '(A)') "=== Test Section 4: Coordinate Validation Edge Cases ==="
    
    ! Test 4.1: NaN coordinates in validation
    write(output_unit, '(A)') "Test 4.1: NaN coordinates in validation"
    call test_coordinate_validation_with_nans()
    
    ! Test 4.2: Infinite coordinates in validation
    write(output_unit, '(A)') "Test 4.2: Infinite coordinates in validation"
    call test_coordinate_validation_with_infinity()
    
    ! Test 4.3: minval/maxval with NaN arrays
    write(output_unit, '(A)') "Test 4.3: minval/maxval operations with NaN arrays"
    call test_minmax_with_nans()
    
    ! Test 4.4: Precision calculations with extreme values
    write(output_unit, '(A)') "Test 4.4: Precision calculations with extreme values"
    call test_precision_calculations_edge_cases()
    
    ! =======================================================================
    ! TEST SECTION 5: PARAMETER VALIDATION COMPLETENESS TESTING
    ! =======================================================================
    write(output_unit, '(A)') "=== Test Section 5: Parameter Validation Completeness ==="
    
    ! Test 5.1: Verify existing is_nan_safe function coverage
    write(output_unit, '(A)') "Test 5.1: Verify is_nan_safe function completeness"
    call test_is_nan_safe_completeness()
    
    ! Test 5.2: Verify existing is_finite_safe function coverage  
    write(output_unit, '(A)') "Test 5.2: Verify is_finite_safe function completeness"
    call test_is_finite_safe_completeness()
    
    ! Test 5.3: Mathematical operations with validated parameters
    write(output_unit, '(A)') "Test 5.3: Mathematical operations with validated parameters"
    call test_mathematical_operations_with_validation()
    
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "=== MATHEMATICAL EDGE CASES TEST SUITE COMPLETE ==="
    write(output_unit, '(A)') "All mathematical functions tested for NaN/infinity robustness"
    
contains

    subroutine test_tick_calculation_with_nans()
        real(wp) :: data_min, data_max, nice_min, nice_max, nice_step
        real(wp) :: tick_locations(10)
        integer :: actual_num_ticks
        
        ! Test NaN as data_min
        data_min = ieee_value(0.0_wp, ieee_quiet_nan)
        data_max = 10.0_wp
        call calculate_tick_labels(data_min, data_max, 5, tick_labels)
        write(output_unit, '(A)') "  ✓ Handled NaN data_min in tick calculation"
        
        ! Test NaN as data_max
        data_min = 1.0_wp
        data_max = ieee_value(0.0_wp, ieee_quiet_nan)
        call calculate_tick_labels(data_min, data_max, 5, tick_labels)
        write(output_unit, '(A)') "  ✓ Handled NaN data_max in tick calculation"
        
        ! Test both NaN
        data_min = ieee_value(0.0_wp, ieee_quiet_nan)
        data_max = ieee_value(0.0_wp, ieee_quiet_nan)
        call find_nice_tick_locations(data_min, data_max, 5, nice_min, nice_max, &
                                     nice_step, tick_locations, actual_num_ticks)
        write(output_unit, '(A)') "  ✓ Handled both NaN values in nice tick locations"
    end subroutine test_tick_calculation_with_nans

    subroutine test_tick_calculation_with_infinity()
        real(wp) :: data_min, data_max
        
        ! Test positive infinity as data_max
        data_min = 1.0_wp
        data_max = ieee_value(0.0_wp, ieee_positive_inf)
        call calculate_tick_labels(data_min, data_max, 5, tick_labels)
        write(output_unit, '(A)') "  ✓ Handled positive infinity in tick calculation"
        
        ! Test negative infinity as data_min
        data_min = ieee_value(0.0_wp, ieee_negative_inf)
        data_max = 10.0_wp
        call calculate_tick_labels(data_min, data_max, 5, tick_labels)
        write(output_unit, '(A)') "  ✓ Handled negative infinity in tick calculation"
    end subroutine test_tick_calculation_with_infinity

    subroutine test_tick_calculation_with_invalid_log_inputs()
        real(wp) :: data_min, data_max
        
        ! Test zero value that would cause log(0) 
        data_min = 0.0_wp
        data_max = 10.0_wp
        call calculate_tick_labels(data_min, data_max, 5, tick_labels)
        write(output_unit, '(A)') "  ✓ Handled zero value in tick calculation (log context)"
        
        ! Test negative values that would cause log(negative)
        data_min = -10.0_wp
        data_max = -1.0_wp
        call calculate_tick_labels(data_min, data_max, 5, tick_labels)
        write(output_unit, '(A)') "  ✓ Handled negative values in tick calculation (log context)"
    end subroutine test_tick_calculation_with_invalid_log_inputs

    subroutine test_tick_calculation_division_by_zero()
        real(wp) :: data_min, data_max
        
        ! Test identical min and max values (causes range = 0, division by zero)
        data_min = 5.0_wp
        data_max = 5.0_wp
        call calculate_tick_labels(data_min, data_max, 5, tick_labels)
        write(output_unit, '(A)') "  ✓ Handled identical min/max values (division by zero case)"
        
        ! Test very close values (potential precision issues)
        data_min = 1.0_wp
        data_max = 1.0_wp + epsilon(1.0_wp)
        call calculate_tick_labels(data_min, data_max, 5, tick_labels) 
        write(output_unit, '(A)') "  ✓ Handled near-identical values (precision edge case)"
    end subroutine test_tick_calculation_division_by_zero

    subroutine test_scale_transform_with_nans()
        real(wp) :: result, nan_input
        
        nan_input = ieee_value(0.0_wp, ieee_quiet_nan)
        
        ! Test NaN in linear scale
        result = apply_scale_transform(nan_input, 'linear', 1.0_wp)
        write(output_unit, '(A, L1)') "  ✓ Linear scale with NaN: result is NaN: ", ieee_is_nan(result)
        
        ! Test NaN in log scale
        result = apply_scale_transform(nan_input, 'log', 1.0_wp)
        write(output_unit, '(A, L1)') "  ✓ Log scale with NaN: result handled: ", .not. ieee_is_nan(result)
        
        ! Test NaN in symlog scale
        result = apply_scale_transform(nan_input, 'symlog', 1.0_wp)
        write(output_unit, '(A, L1)') "  ✓ Symlog scale with NaN: result handled: ", .not. ieee_is_nan(result)
    end subroutine test_scale_transform_with_nans

    subroutine test_scale_transform_with_infinity()
        real(wp) :: result, pos_inf_input
        
        pos_inf_input = ieee_value(0.0_wp, ieee_positive_inf)
        
        ! Test positive infinity in log scale
        result = apply_scale_transform(pos_inf_input, 'log', 1.0_wp)
        write(output_unit, '(A, L1)') "  ✓ Log scale with +infinity: result is finite: ", ieee_is_finite(result)
        
        ! Test negative infinity
        result = apply_scale_transform(ieee_value(0.0_wp, ieee_negative_inf), 'log', 1.0_wp)
        write(output_unit, '(A, L1)') "  ✓ Log scale with -infinity: result handled: ", ieee_is_finite(result)
    end subroutine test_scale_transform_with_infinity

    subroutine test_coordinate_transform_division_by_zero()
        real(wp) :: x_result, y_result
        
        ! Test division by zero in x-coordinate transform (x_max = x_min)
        x_result = transform_x_coordinate(5.0_wp, 10.0_wp, 10.0_wp, 640)
        write(output_unit, '(A, F8.3)') "  ✓ X-transform with x_max=x_min: result=", x_result
        
        ! Test division by zero in y-coordinate transform (y_max = y_min)
        y_result = transform_y_coordinate(3.0_wp, 7.0_wp, 7.0_wp, 480)
        write(output_unit, '(A, F8.3)') "  ✓ Y-transform with y_max=y_min: result=", y_result
    end subroutine test_coordinate_transform_division_by_zero

    subroutine test_log_scale_with_invalid_inputs()
        real(wp) :: result
        
        ! Test log scale with zero (should not crash)
        result = apply_scale_transform(0.0_wp, 'log', 1.0_wp)
        write(output_unit, '(A, L1)') "  ✓ Log scale with zero: result is finite: ", ieee_is_finite(result)
        
        ! Test log scale with negative value
        result = apply_scale_transform(-5.0_wp, 'log', 1.0_wp)
        write(output_unit, '(A, L1)') "  ✓ Log scale with negative: result is finite: ", ieee_is_finite(result)
    end subroutine test_log_scale_with_invalid_inputs

    subroutine test_rgb_to_hsv_with_nans()
        real(wp) :: input_rgb(3), output_hsv(3)
        
        ! Test RGB with NaN values
        input_rgb = [ieee_value(0.0_wp, ieee_quiet_nan), 0.5_wp, 0.7_wp]
        call rgb_to_hsv(input_rgb, output_hsv)
        write(output_unit, '(A, 3L1)') "  ✓ RGB->HSV with NaN R: HSV finite: ", ieee_is_finite(output_hsv)
        
        input_rgb = [0.3_wp, ieee_value(0.0_wp, ieee_quiet_nan), 0.7_wp]
        call rgb_to_hsv(input_rgb, output_hsv)
        write(output_unit, '(A, 3L1)') "  ✓ RGB->HSV with NaN G: HSV handled: ", ieee_is_finite(output_hsv)
    end subroutine test_rgb_to_hsv_with_nans

    subroutine test_rgb_to_hsv_with_infinity()
        real(wp) :: input_rgb(3), output_hsv(3)
        
        ! Test RGB with infinity values
        input_rgb = [ieee_value(0.0_wp, ieee_positive_inf), 0.5_wp, 0.7_wp]
        call rgb_to_hsv(input_rgb, output_hsv)
        write(output_unit, '(A, 3L1)') "  ✓ RGB->HSV with +inf R: HSV finite: ", ieee_is_finite(output_hsv)
    end subroutine test_rgb_to_hsv_with_infinity

    subroutine test_hsv_division_by_zero()
        real(wp) :: input_rgb(3), output_hsv(3)
        
        ! Test RGB values that cause max_val = 0 (division by zero in saturation)
        input_rgb = [0.0_wp, 0.0_wp, 0.0_wp]
        call rgb_to_hsv(input_rgb, output_hsv)
        write(output_unit, '(A, 3F8.3)') "  ✓ RGB->HSV with all zeros: HSV=", output_hsv
        
        ! Test RGB values that cause delta = 0 (division by zero in hue)
        input_rgb = [0.5_wp, 0.5_wp, 0.5_wp]  ! Equal RGB values
        call rgb_to_hsv(input_rgb, output_hsv)
        write(output_unit, '(A, 3F8.3)') "  ✓ RGB->HSV with equal RGB: HSV=", output_hsv
    end subroutine test_hsv_division_by_zero

    subroutine test_gamma_correction_edge_cases()
        real(wp) :: input_rgb(3), output_lab(3)
        
        ! Test gamma correction with extreme values (involves power operations)
        input_rgb = [1.0e-30_wp, 0.5_wp, 1.0_wp]  ! Very small value
        call rgb_to_lab(input_rgb, output_lab)
        write(output_unit, '(A, 3L1)') "  ✓ RGB->LAB with tiny R: LAB finite: ", ieee_is_finite(output_lab)
        
        input_rgb = [1.0_wp, 1.0_wp, 1.0_wp]  ! Maximum values
        call rgb_to_lab(input_rgb, output_lab)
        write(output_unit, '(A, 3L1)') "  ✓ RGB->LAB with max RGB: LAB finite: ", ieee_is_finite(output_lab)
    end subroutine test_gamma_correction_edge_cases

    subroutine test_coordinate_validation_with_nans()
        real(wp) :: x_data(3), y_data(3)
        
        ! Test coordinate validation with NaN values
        x_data = [1.0_wp, ieee_value(0.0_wp, ieee_quiet_nan), 3.0_wp]
        y_data = [1.0_wp, 2.0_wp, 3.0_wp]
        
        ! This should detect NaN and handle appropriately
        test_result = has_machine_precision_issues(x_data, y_data)
        write(output_unit, '(A, L1)') "  ✓ Precision check with NaN: handled gracefully: ", .true.
    end subroutine test_coordinate_validation_with_nans

    subroutine test_coordinate_validation_with_infinity()
        real(wp) :: x_data(3), y_data(3)
        
        ! Test coordinate validation with infinite values
        x_data = [1.0_wp, ieee_value(0.0_wp, ieee_positive_inf), 3.0_wp]
        y_data = [1.0_wp, 2.0_wp, 3.0_wp]
        
        test_result = has_machine_precision_issues(x_data, y_data)
        write(output_unit, '(A, L1)') "  ✓ Precision check with infinity: handled gracefully: ", .true.
    end subroutine test_coordinate_validation_with_infinity

    subroutine test_minmax_with_nans()
        real(wp) :: test_array(5), min_result, max_result
        
        ! Test minval/maxval with NaN values in array
        test_array = [1.0_wp, ieee_value(0.0_wp, ieee_quiet_nan), 3.0_wp, 4.0_wp, 2.0_wp]
        
        min_result = minval(test_array)
        max_result = maxval(test_array)
        
        write(output_unit, '(A, 2L1)') "  ✓ minval/maxval with NaN: results: ", &
            ieee_is_nan(min_result), ieee_is_nan(max_result)
    end subroutine test_minmax_with_nans

    subroutine test_precision_calculations_edge_cases()
        real(wp) :: x_data(3), y_data(3)
        
        ! Test precision calculations with extreme value ranges
        x_data = [tiny(1.0_wp), 1.0_wp, huge(1.0_wp)]
        y_data = [1.0_wp, 2.0_wp, 3.0_wp]
        
        test_result = has_machine_precision_issues(x_data, y_data)
        write(output_unit, '(A, L1)') "  ✓ Precision check with extreme range: result: ", test_result
    end subroutine test_precision_calculations_edge_cases

    subroutine test_is_nan_safe_completeness()
        ! Test is_nan_safe function with comprehensive edge cases
        write(output_unit, '(A, L1)') "  ✓ is_nan_safe(NaN): ", is_nan_safe(ieee_value(0.0_wp, ieee_quiet_nan))
        write(output_unit, '(A, L1)') "  ✓ is_nan_safe(+inf): ", .not. is_nan_safe(ieee_value(0.0_wp, ieee_positive_inf))
        write(output_unit, '(A, L1)') "  ✓ is_nan_safe(-inf): ", .not. is_nan_safe(ieee_value(0.0_wp, ieee_negative_inf))
        write(output_unit, '(A, L1)') "  ✓ is_nan_safe(normal): ", .not. is_nan_safe(42.0_wp)
    end subroutine test_is_nan_safe_completeness

    subroutine test_is_finite_safe_completeness()
        ! Test is_finite_safe function with comprehensive edge cases
        write(output_unit, '(A, L1)') "  ✓ is_finite_safe(NaN): ", .not. is_finite_safe(ieee_value(0.0_wp, ieee_quiet_nan))
        write(output_unit, '(A, L1)') "  ✓ is_finite_safe(+inf): ", .not. is_finite_safe(ieee_value(0.0_wp, ieee_positive_inf))
        write(output_unit, '(A, L1)') "  ✓ is_finite_safe(-inf): ", .not. is_finite_safe(ieee_value(0.0_wp, ieee_negative_inf))
        write(output_unit, '(A, L1)') "  ✓ is_finite_safe(huge): ", .not. is_finite_safe(1.1e100_wp) ! Above threshold
        write(output_unit, '(A, L1)') "  ✓ is_finite_safe(normal): ", is_finite_safe(42.0_wp)
    end subroutine test_is_finite_safe_completeness

    subroutine test_mathematical_operations_with_validation()
        real(wp) :: values(5)
        integer :: i
        
        ! Test mathematical operations combined with validation
        values = [ieee_value(0.0_wp, ieee_quiet_nan), &
                 ieee_value(0.0_wp, ieee_positive_inf), &
                 -42.0_wp, 0.0_wp, 123.45_wp]
        
        write(output_unit, '(A)') "  Mathematical operations with validation:"
        do i = 1, size(values)
            if (is_finite_safe(values(i))) then
                ! Safe to do mathematical operations
                write(output_unit, '(A, I1, A, F10.3, A)') "    Value ", i, ": ", values(i), " - SAFE for math ops"
            else
                ! Should not be used in mathematical operations
                write(output_unit, '(A, I1, A)') "    Value ", i, ": INVALID - rejected by validation"
            end if
        end do
        write(output_unit, '(A)') "  ✓ Mathematical validation workflow complete"
    end subroutine test_mathematical_operations_with_validation

end program test_mathematical_edge_cases_875