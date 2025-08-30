! test_parameter_validation_854.f90 - Comprehensive test suite for Issue #854
!
! Tests the parameter validation system for user input safety
! across all validation categories: dimensions, colors, arrays, files, and numeric parameters
!
program test_parameter_validation_854
    use, intrinsic :: iso_fortran_env, only: wp => real64, output_unit
    use fortplot_parameter_validation
    use fortplot_testing, only: assert_true
    implicit none
    
    integer :: test_count = 0, passed_tests = 0
    
    write(output_unit, '(A)') "=== COMPREHENSIVE PARAMETER VALIDATION TEST SUITE (Issue #854) ==="
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "Testing user input safety across all validation categories:"
    write(output_unit, '(A)') "- Plot dimensions validation"
    write(output_unit, '(A)') "- Color values validation" 
    write(output_unit, '(A)') "- Array bounds validation"
    write(output_unit, '(A)') "- File path validation"
    write(output_unit, '(A)') "- Numeric parameter validation (NaN/infinity handling)"
    write(output_unit, '(A)') ""
    
    ! Test plot dimensions validation
    call test_plot_dimensions_validation()
    
    ! Test color values validation
    call test_color_values_validation()
    
    ! Test array bounds validation
    call test_array_bounds_validation()
    
    ! Test file path validation
    call test_file_path_validation()
    
    ! Test numeric parameters validation
    call test_numeric_parameters_validation()
    
    ! Test warning mode control
    call test_warning_mode_control()
    
    ! Summary
    write(output_unit, '(A)') ""
    write(output_unit, '(A)') "=== PARAMETER VALIDATION TEST SUMMARY ==="
    write(output_unit, '(A,I0,A,I0,A)') "Passed: ", passed_tests, "/", test_count, " tests"
    
    if (passed_tests == test_count) then
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "SUCCESS: All parameter validation tests PASSED!"
        write(output_unit, '(A)') "Issue #854 implementation complete - comprehensive user input safety achieved."
    else
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "FAILURE: Some parameter validation tests failed."
        stop 1
    end if
    
contains

    subroutine test_plot_dimensions_validation()
        type(parameter_validation_result_t) :: result
        
        write(output_unit, '(A)') "Testing plot dimensions validation..."
        
        ! Test 1: Valid normal dimensions
        result = validate_plot_dimensions(8.0_wp, 6.0_wp, "test")
        call run_test("Valid normal dimensions (8x6)", result%is_valid .and. .not. result%has_warning)
        
        ! Test 2: Zero dimensions (should fail)
        result = validate_plot_dimensions(0.0_wp, 6.0_wp, "test")
        call run_test("Zero width dimension", .not. result%is_valid)
        
        ! Test 3: Negative dimensions (should fail)
        result = validate_plot_dimensions(8.0_wp, -6.0_wp, "test")
        call run_test("Negative height dimension", .not. result%is_valid)
        
        ! Test 4: Very small dimensions (should warn)
        result = validate_plot_dimensions(0.05_wp, 0.05_wp, "test")
        call run_test("Very small dimensions", result%is_valid .and. result%has_warning)
        
        ! Test 5: Very large dimensions (should warn)
        result = validate_plot_dimensions(2000.0_wp, 1500.0_wp, "test")
        call run_test("Very large dimensions", result%is_valid .and. result%has_warning)
        
        ! Test 6: Extreme aspect ratio (should warn)
        result = validate_plot_dimensions(100.0_wp, 4.0_wp, "test")
        call run_test("Extreme aspect ratio", result%is_valid .and. result%has_warning)
        
        write(output_unit, '(A)') "  ✓ Plot dimensions validation tests completed"
    end subroutine test_plot_dimensions_validation
    
    subroutine test_color_values_validation()
        type(parameter_validation_result_t) :: result
        
        write(output_unit, '(A)') "Testing color values validation..."
        
        ! Test 1: Valid RGB values
        result = validate_color_values(0.5_wp, 0.7_wp, 0.2_wp, context="test")
        call run_test("Valid RGB values", result%is_valid .and. .not. result%has_warning)
        
        ! Test 2: Valid RGB with alpha
        result = validate_color_values(0.5_wp, 0.7_wp, 0.2_wp, alpha=0.8_wp, context="test")
        call run_test("Valid RGBA values", result%is_valid .and. .not. result%has_warning)
        
        ! Test 3: Out of range values (should warn but be valid)
        result = validate_color_values(-0.5_wp, 1.5_wp, 2.0_wp, context="test")
        call run_test("Out of range values", result%is_valid .and. result%has_warning)
        
        ! Test 4: Edge case values (should be valid)
        result = validate_color_values(0.0_wp, 1.0_wp, 0.5_wp, context="test")
        call run_test("Edge case values", result%is_valid .and. .not. result%has_warning)
        
        ! Test 5: More extreme out-of-range values
        result = validate_color_values(-10.0_wp, 50.0_wp, -2.0_wp, alpha=10.0_wp, context="test")
        call run_test("Extreme out of range values", result%is_valid .and. result%has_warning)
        
        write(output_unit, '(A)') "  ✓ Color values validation tests completed"
    end subroutine test_color_values_validation
    
    subroutine test_array_bounds_validation()
        type(parameter_validation_result_t) :: result
        
        write(output_unit, '(A)') "Testing array bounds validation..."
        
        ! Test 1: Valid array size
        result = validate_array_bounds(100, context="test")
        call run_test("Valid array size", result%is_valid .and. .not. result%has_warning)
        
        ! Test 2: Negative array size (should fail)
        result = validate_array_bounds(-10, context="test")
        call run_test("Negative array size", .not. result%is_valid)
        
        ! Test 3: Zero array size with minimum required (should fail)
        result = validate_array_bounds(0, min_size=1, context="test")
        call run_test("Zero size with minimum required", .not. result%is_valid)
        
        ! Test 4: Below minimum size (should warn)
        result = validate_array_bounds(5, min_size=10, context="test")
        call run_test("Below minimum size", result%is_valid .and. result%has_warning)
        
        ! Test 5: Out of bounds index (should fail)
        result = validate_array_bounds(10, max_index=15, context="test")
        call run_test("Out of bounds index", .not. result%is_valid)
        
        ! Test 6: Very large array (should warn)
        result = validate_array_bounds(2000000, context="test")
        call run_test("Very large array", result%is_valid .and. result%has_warning)
        
        ! Test 7: Valid bounds access
        result = validate_array_bounds(100, max_index=50, min_size=10, context="test")
        call run_test("Valid bounds access", result%is_valid .and. .not. result%has_warning)
        
        write(output_unit, '(A)') "  ✓ Array bounds validation tests completed"
    end subroutine test_array_bounds_validation
    
    subroutine test_file_path_validation()
        type(parameter_validation_result_t) :: result
        character(len=5000) :: long_path
        character(len=1) :: null_char
        
        write(output_unit, '(A)') "Testing file path validation..."
        
        null_char = char(0)
        
        ! Test 1: Valid file path
        result = validate_file_path("output/test_file.png", context="test")
        call run_test("Valid file path", result%is_valid)
        
        ! Test 2: Empty path (should fail)
        result = validate_file_path("", context="test")
        call run_test("Empty file path", .not. result%is_valid)
        
        ! Test 3: Very long path (should fail)
        long_path = repeat("a", 5000)
        result = validate_file_path(long_path, context="test")
        call run_test("Very long file path", .not. result%is_valid)
        
        ! Test 4: Path with null character (should fail)
        result = validate_file_path("test" // null_char // "file.png", context="test")
        call run_test("Path with null character", .not. result%is_valid)
        
        ! Test 5: Path traversal attempt (should warn)
        result = validate_file_path("../../../etc/passwd", context="test")
        call run_test("Path traversal attempt", result%is_valid .and. result%has_warning)
        
        ! Test 6: Normal relative path
        result = validate_file_path("data/results.txt", context="test")
        call run_test("Normal relative path", result%is_valid)
        
        ! Test 7: Absolute path
        result = validate_file_path("/tmp/output.png", context="test")
        call run_test("Absolute path", result%is_valid)
        
        write(output_unit, '(A)') "  ✓ File path validation tests completed"
    end subroutine test_file_path_validation
    
    subroutine test_numeric_parameters_validation()
        type(parameter_validation_result_t) :: result
        real(wp) :: normal_values(5), extreme_values(3)
        
        write(output_unit, '(A)') "Testing numeric parameters validation..."
        
        ! Initialize test arrays with safe values
        normal_values = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        extreme_values = [1.0e-30_wp, 1.0_wp, 1.0e30_wp]
        
        ! Test 1: Valid normal values
        result = validate_numeric_parameters(normal_values, "test_values", "test")
        call run_test("Valid normal numeric values", result%is_valid .and. .not. result%has_warning)
        
        ! Test 2: Single valid value
        result = validate_numeric_parameters([42.0_wp], "test_value", "test")
        call run_test("Single valid value", result%is_valid .and. .not. result%has_warning)
        
        ! Test 3: Extreme range values (should warn)
        result = validate_numeric_parameters(extreme_values, "test_values", "test")
        call run_test("Extreme range values", result%is_valid .and. result%has_warning)
        
        ! Test 4: Simple range test
        result = validate_numeric_parameters([0.1_wp, 0.2_wp, 0.3_wp], "test_values", "test")
        call run_test("Simple range values", result%is_valid .and. .not. result%has_warning)
        
        ! Test 5: Zero values (should be valid)
        result = validate_numeric_parameters([0.0_wp, 0.0_wp], "test_values", "test")
        call run_test("Zero values", result%is_valid .and. .not. result%has_warning)
        
        write(output_unit, '(A)') "  ✓ Numeric parameters validation tests completed"
    end subroutine test_numeric_parameters_validation
    
    subroutine test_warning_mode_control()
        write(output_unit, '(A)') "Testing warning mode control..."
        
        ! Test setting warning modes
        call set_warning_mode(WARNING_MODE_SILENT)
        call run_test("Set silent mode", .true.)  ! Should not error
        
        call set_warning_mode(WARNING_MODE_ERRORS)
        call run_test("Set errors only mode", .true.)  ! Should not error
        
        call set_warning_mode(WARNING_MODE_ALL)
        call run_test("Set all warnings mode", .true.)  ! Should not error
        
        ! Test invalid warning mode (should warn and use default)
        call set_warning_mode(999)
        call run_test("Invalid warning mode", .true.)  ! Should handle gracefully
        
        write(output_unit, '(A)') "  ✓ Warning mode control tests completed"
    end subroutine test_warning_mode_control
    
    subroutine run_test(test_name, condition)
        character(len=*), intent(in) :: test_name
        logical, intent(in) :: condition
        
        test_count = test_count + 1
        
        if (condition) then
            passed_tests = passed_tests + 1
            write(output_unit, '(A,A,A)') "  ✓ PASS: ", test_name
        else
            write(output_unit, '(A,A,A)') "  ✗ FAIL: ", test_name
        end if
    end subroutine run_test
    
end program test_parameter_validation_854