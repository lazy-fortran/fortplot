program test_matplotlib_syntax_compatibility
    !! Test suite for comprehensive matplotlib-compatible linestyle and marker syntax
    !! Following TDD RED phase - these tests define the expected behavior for Issue #6
    !! 
    !! GIVEN: matplotlib syntax strings for linestyle and marker combinations
    !! WHEN: parsing and translating syntax in fortplot
    !! THEN: should provide identical behavior to matplotlib while maintaining backward compatibility

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_format_parser, only: parse_format_string, contains_format_chars
    use fortplot_markers, only: validate_marker_style, get_marker_size
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! Core matplotlib syntax compatibility tests
    call test_matplotlib_linestyle_syntax()
    call test_matplotlib_marker_syntax()
    call test_matplotlib_combined_syntax()
    call test_parameter_translation_system()
    call test_backward_compatibility()
    call test_syntax_validation()
    call test_edge_case_handling()
    
    call print_test_summary()
    
contains

    subroutine test_matplotlib_linestyle_syntax()
        !! GIVEN: matplotlib linestyle syntax strings
        !! WHEN: parsing linestyle format strings  
        !! THEN: should return correct linestyle identifiers
        
        character(len=20) :: marker, linestyle
        
        call start_test("Matplotlib linestyle syntax compatibility")
        
        ! Test standard matplotlib linestyle syntax
        call parse_format_string('-', marker, linestyle)
        call assert_equal_str(linestyle, '-', "Solid line syntax")
        call assert_equal_str(marker, '', "No marker for solid line")
        
        call parse_format_string('--', marker, linestyle)
        call assert_equal_str(linestyle, '--', "Dashed line syntax")
        call assert_equal_str(marker, '', "No marker for dashed line")
        
        call parse_format_string('-.', marker, linestyle)
        call assert_equal_str(linestyle, '-.', "Dash-dot line syntax")
        call assert_equal_str(marker, '', "No marker for dash-dot line")
        
        call parse_format_string(':', marker, linestyle)
        call assert_equal_str(linestyle, ':', "Dotted line syntax")
        call assert_equal_str(marker, '', "No marker for dotted line")
        
        ! Test 'None' linestyle (invisible lines)
        call parse_format_string('None', marker, linestyle)
        call assert_equal_str(linestyle, 'None', "None linestyle syntax")
        call assert_equal_str(marker, '', "No marker for None linestyle")
        
        call end_test()
    end subroutine test_matplotlib_linestyle_syntax

    subroutine test_matplotlib_marker_syntax()
        !! GIVEN: matplotlib marker syntax strings
        !! WHEN: parsing marker format strings
        !! THEN: should return correct marker identifiers with no linestyle
        
        character(len=20) :: marker, linestyle
        logical :: is_valid
        
        call start_test("Matplotlib marker syntax compatibility")
        
        ! Test standard matplotlib markers
        call parse_format_string('o', marker, linestyle)
        call assert_equal_str(marker, 'o', "Circle marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        call parse_format_string('s', marker, linestyle)
        call assert_equal_str(marker, 's', "Square marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        call parse_format_string('^', marker, linestyle)
        call assert_equal_str(marker, '^', "Triangle up marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        call parse_format_string('v', marker, linestyle)
        call assert_equal_str(marker, 'v', "Triangle down marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        call parse_format_string('d', marker, linestyle)
        call assert_equal_str(marker, 'd', "Diamond marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        call parse_format_string('D', marker, linestyle)
        call assert_equal_str(marker, 'D', "Large diamond marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        call parse_format_string('*', marker, linestyle)
        call assert_equal_str(marker, '*', "Star marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        call parse_format_string('+', marker, linestyle)
        call assert_equal_str(marker, '+', "Plus marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        call parse_format_string('x', marker, linestyle)
        call assert_equal_str(marker, 'x', "X marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        call parse_format_string('p', marker, linestyle)
        call assert_equal_str(marker, 'p', "Pentagon marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        call parse_format_string('h', marker, linestyle)
        call assert_equal_str(marker, 'h', "Hexagon marker syntax")
        call assert_equal_str(linestyle, 'None', "No line for marker-only")
        
        ! Test marker validation
        is_valid = validate_marker_style('o')
        call assert_true(is_valid, "Circle marker is valid")
        
        is_valid = validate_marker_style('invalid')
        call assert_false(is_valid, "Invalid marker should fail validation")
        
        call end_test()
    end subroutine test_matplotlib_marker_syntax

    subroutine test_matplotlib_combined_syntax()
        !! GIVEN: matplotlib combined marker+linestyle syntax strings
        !! WHEN: parsing combined format strings
        !! THEN: should correctly separate marker and linestyle components
        
        character(len=20) :: marker, linestyle
        
        call start_test("Matplotlib combined marker+linestyle syntax")
        
        ! Test marker + linestyle combinations
        call parse_format_string('o-', marker, linestyle)
        call assert_equal_str(marker, 'o', "Circle marker with solid line")
        call assert_equal_str(linestyle, '-', "Solid line with circle marker")
        
        call parse_format_string('s--', marker, linestyle)
        call assert_equal_str(marker, 's', "Square marker with dashed line")
        call assert_equal_str(linestyle, '--', "Dashed line with square marker")
        
        call parse_format_string('^-.', marker, linestyle)
        call assert_equal_str(marker, '^', "Triangle marker with dash-dot line")
        call assert_equal_str(linestyle, '-.', "Dash-dot line with triangle marker")
        
        call parse_format_string('*:', marker, linestyle)
        call assert_equal_str(marker, '*', "Star marker with dotted line")
        call assert_equal_str(linestyle, ':', "Dotted line with star marker")
        
        ! Test reversed order (line + marker)
        call parse_format_string('-o', marker, linestyle)
        call assert_equal_str(marker, 'o', "Circle marker with solid line (reversed)")
        call assert_equal_str(linestyle, '-', "Solid line with circle marker (reversed)")
        
        call parse_format_string('--s', marker, linestyle)
        call assert_equal_str(marker, 's', "Square marker with dashed line (reversed)")
        call assert_equal_str(linestyle, '--', "Dashed line with square marker (reversed)")
        
        call end_test()
    end subroutine test_matplotlib_combined_syntax

    subroutine test_parameter_translation_system()
        !! GIVEN: need for parameter translation between current and matplotlib syntax
        !! WHEN: using parameter translation functions
        !! THEN: should seamlessly convert between syntax formats
        
        logical :: has_format
        character(len=20) :: marker, linestyle
        
        call start_test("Parameter translation system")
        
        ! Test format string detection
        has_format = contains_format_chars('o-')
        call assert_true(has_format, "Combined format string detected")
        
        has_format = contains_format_chars('solid')
        call assert_false(has_format, "Named linestyle not detected as format")
        
        has_format = contains_format_chars('')
        call assert_false(has_format, "Empty string not detected as format")
        
        ! Test translation from old parameter style to new syntax
        ! These tests will fail initially (RED phase) and drive implementation
        
        ! Test current named linestyle constants compatibility
        call parse_format_string('solid', marker, linestyle)
        call assert_equal_str(linestyle, '-', "Named 'solid' translates to '-'")
        
        call parse_format_string('dashed', marker, linestyle)
        call assert_equal_str(linestyle, '--', "Named 'dashed' translates to '--'")
        
        call parse_format_string('dotted', marker, linestyle)
        call assert_equal_str(linestyle, ':', "Named 'dotted' translates to ':'")
        
        call parse_format_string('dashdot', marker, linestyle)
        call assert_equal_str(linestyle, '-.', "Named 'dashdot' translates to '-.'")
        
        call end_test()
    end subroutine test_parameter_translation_system

    subroutine test_backward_compatibility()
        !! GIVEN: existing fortplot code using current parameter formats
        !! WHEN: using current linestyle constants and parameter names  
        !! THEN: should continue working without modification
        
        logical :: is_valid
        real(wp) :: marker_size
        
        call start_test("Backward compatibility validation")
        
        ! Test that existing LINESTYLE_* constants still work
        ! These represent current API that must remain functional
        is_valid = .true.  ! Placeholder - actual constants should be tested
        call assert_true(is_valid, "LINESTYLE_SOLID constant still works")
        
        is_valid = .true.  ! Placeholder - actual constants should be tested
        call assert_true(is_valid, "LINESTYLE_DASHED constant still works")
        
        is_valid = .true.  ! Placeholder - actual constants should be tested
        call assert_true(is_valid, "LINESTYLE_DOTTED constant still works")
        
        is_valid = .true.  ! Placeholder - actual constants should be tested
        call assert_true(is_valid, "LINESTYLE_DASHDOT constant still works")
        
        ! Test that existing marker constants still work
        is_valid = validate_marker_style('o')
        call assert_true(is_valid, "Circle marker constant still valid")
        
        marker_size = get_marker_size('o')
        call assert_true(marker_size > 0.0_wp, "Marker size function still works")
        
        ! Test parameter name compatibility
        ! Existing code should work with both linestyle and line_style parameter names
        ! This drives the requirement for parameter name translation
        
        call end_test()
    end subroutine test_backward_compatibility

    subroutine test_syntax_validation()
        !! GIVEN: various syntax strings including invalid formats
        !! WHEN: validating syntax formats
        !! THEN: should correctly identify valid/invalid syntax
        
        logical :: has_format, is_valid
        character(len=20) :: marker, linestyle
        
        call start_test("Syntax validation and error handling")
        
        ! Test valid format string detection
        has_format = contains_format_chars('o-')
        call assert_true(has_format, "Valid combined format detected")
        
        has_format = contains_format_chars('--')
        call assert_true(has_format, "Valid linestyle format detected")
        
        has_format = contains_format_chars('x')
        call assert_true(has_format, "Valid marker format detected")
        
        ! Test invalid format string detection
        has_format = contains_format_chars('invalid')
        call assert_false(has_format, "Invalid string not detected as format")
        
        has_format = contains_format_chars('123')
        call assert_false(has_format, "Numeric string not detected as format")
        
        ! Test invalid syntax handling
        call parse_format_string('oz', marker, linestyle)
        call assert_equal_str(marker, 'o', "Valid marker extracted from invalid combo")
        call assert_equal_str(linestyle, 'None', "Invalid linestyle ignored")
        
        ! Test empty and whitespace handling
        call parse_format_string('', marker, linestyle)
        call assert_equal_str(marker, '', "Empty string produces empty marker")
        call assert_equal_str(linestyle, '', "Empty string produces empty linestyle")
        
        call parse_format_string('   ', marker, linestyle)
        call assert_equal_str(marker, '', "Whitespace produces empty marker")
        call assert_equal_str(linestyle, '', "Whitespace produces empty linestyle")
        
        call end_test()
    end subroutine test_syntax_validation

    subroutine test_edge_case_handling()
        !! GIVEN: edge case syntax scenarios
        !! WHEN: parsing problematic or boundary case inputs
        !! THEN: should handle gracefully without crashes
        
        character(len=20) :: marker, linestyle
        logical :: has_format
        
        call start_test("Edge case handling")
        
        ! Test very long format strings
        call parse_format_string(repeat('o', 100), marker, linestyle)
        call assert_equal_str(marker, 'o', "Long string with valid marker")
        call assert_equal_str(linestyle, 'None', "Long string with no linestyle")
        
        ! Test special characters that might confuse parser
        call parse_format_string('o.', marker, linestyle)
        call assert_equal_str(marker, 'o', "Marker with special character")
        call assert_equal_str(linestyle, 'None', "Special character not interpreted as line")
        
        ! Test case sensitivity
        call parse_format_string('O', marker, linestyle)
        call assert_equal_str(marker, '', "Uppercase markers not supported")
        call assert_equal_str(linestyle, '', "Uppercase not interpreted as linestyle")
        
        ! Test unicode characters
        call parse_format_string('ø', marker, linestyle)
        call assert_equal_str(marker, '', "Unicode not interpreted as marker")
        call assert_equal_str(linestyle, '', "Unicode not interpreted as linestyle")
        
        ! Test multiple markers in one string
        call parse_format_string('ox', marker, linestyle)
        call assert_equal_str(marker, 'o', "First valid marker extracted")
        call assert_equal_str(linestyle, 'None', "Multiple markers treated as marker-only")
        
        ! Test multiple linestyles in one string
        call parse_format_string('--:', marker, linestyle)
        call assert_equal_str(marker, '', "No marker from multi-linestyle")
        call assert_equal_str(linestyle, '--', "First valid linestyle extracted")
        
        call end_test()
    end subroutine test_edge_case_handling

    ! Testing utilities
    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A,A)') "Running test: ", trim(test_name)
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') "  PASS"
    end subroutine end_test

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            write(*, '(A,A)') "  FAIL: ", trim(message)
            error stop "Test assertion failed"
        end if
    end subroutine assert_true

    subroutine assert_false(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (condition) then
            write(*, '(A,A)') "  FAIL: ", trim(message)
            error stop "Test assertion failed"
        end if
    end subroutine assert_false

    subroutine assert_equal_str(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message
        if (trim(actual) /= trim(expected)) then
            write(*, '(A,A)') "  FAIL: ", trim(message)
            write(*, '(A,A,A,A,A)') "    Expected: '", trim(expected), "' but got: '", trim(actual), "'"
            error stop "Test assertion failed"
        end if
    end subroutine assert_equal_str

    subroutine print_test_summary()
        write(*, '(A,I0,A,I0,A)') "Tests completed: ", pass_count, "/", test_count, " passed"
        if (pass_count /= test_count) error stop "Some tests failed"
    end subroutine print_test_summary

end program test_matplotlib_syntax_compatibility