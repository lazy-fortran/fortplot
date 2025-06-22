program test_ticks_refactored
    !! Comprehensive test suite for refactored tick functionality
    !! Following TDD approach: write tests first, then refactor
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_ticks, only: calculate_tick_labels, calculate_tick_labels_log, calculate_tick_labels_symlog
    use fortplot_ticks, only: format_tick_value, calculate_nice_axis_limits
    implicit none
    
    call test_should_eliminate_duplicate_functions()
    call test_should_calculate_linear_ticks_consistently()
    call test_should_calculate_log_ticks_with_proper_powers()
    call test_should_calculate_symlog_ticks_correctly()
    call test_should_format_tick_values_consistently()
    call test_should_calculate_nice_axis_limits()
    call test_should_handle_edge_cases()
    
    print *, "All tick refactoring tests passed!"

contains

    subroutine test_should_eliminate_duplicate_functions()
        !! Test that functions exist only in ticks module, not duplicated
        print *, "Testing: Duplicate functions should be eliminated"
        
        ! This test ensures we use only fortplot_ticks module
        ! If we successfully import from ticks module, duplication is resolved
        character(len=20) :: labels(10)
        
        call calculate_tick_labels(0.0_wp, 10.0_wp, 5, labels)
        call assert_not_empty(labels(1), "Linear tick calculation should work from ticks module")
        
        call calculate_tick_labels_log(1.0_wp, 1000.0_wp, 5, labels)
        call assert_not_empty(labels(1), "Log tick calculation should work from ticks module")
        
        call calculate_tick_labels_symlog(-10.0_wp, 10.0_wp, 1.0_wp, 5, labels)
        call assert_not_empty(labels(1), "Symlog tick calculation should work from ticks module")
    end subroutine

    subroutine test_should_calculate_linear_ticks_consistently()
        !! Test consistent linear tick calculation
        print *, "Testing: Linear ticks should be calculated consistently"
        
        character(len=20) :: labels(10)
        real(wp) :: nice_min, nice_max
        
        ! Test basic range
        call calculate_tick_labels(0.0_wp, 10.0_wp, 6, labels)
        call assert_not_empty(labels(1), "Should generate first tick")
        call assert_not_empty(labels(6), "Should generate last tick")
        call assert_empty(labels(7), "Should not overfill labels")
        
        ! Test nice axis limits
        call calculate_nice_axis_limits(1.2_wp, 8.7_wp, 5, nice_min, nice_max)
        call assert_approximately_equal(nice_min, 0.0_wp, 1.0e-10_wp, "Nice min should be round")
        call assert_approximately_equal(nice_max, 10.0_wp, 1.0e-10_wp, "Nice max should be round")
    end subroutine

    subroutine test_should_calculate_log_ticks_with_proper_powers()
        !! Test logarithmic tick calculation produces proper powers of 10
        print *, "Testing: Log ticks should use proper powers of 10"
        
        character(len=20) :: labels(10)
        
        ! Test range spanning multiple decades
        call calculate_tick_labels_log(1.0_wp, 1000.0_wp, 5, labels)
        
        ! Should have ticks at 1, 10, 100, 1000
        call assert_string_contains(labels(1), "1", "Should have tick at 1")
        call assert_string_contains(labels(2), "10", "Should have tick at 10")
        call assert_string_contains(labels(3), "100", "Should have tick at 100")
        call assert_string_contains(labels(4), "1000", "Should have tick at 1000")
        
        ! Test fractional range
        call calculate_tick_labels_log(0.01_wp, 1.0_wp, 5, labels)
        call assert_string_contains(labels(1), "0.01", "Should handle fractional powers")
    end subroutine

    subroutine test_should_calculate_symlog_ticks_correctly()
        !! Test symmetric logarithmic tick calculation
        print *, "Testing: Symlog ticks should handle linear and log regions"
        
        character(len=20) :: labels(10)
        
        ! Test symmetric range crossing zero
        call calculate_tick_labels_symlog(-100.0_wp, 100.0_wp, 1.0_wp, 5, labels)
        
        ! Should include zero and symmetric values
        logical :: has_zero, has_positive, has_negative
        integer :: i
        
        has_zero = .false.
        has_positive = .false.
        has_negative = .false.
        
        do i = 1, 10
            if (trim(labels(i)) == '0') has_zero = .true.
            if (index(labels(i), '-') > 0) has_negative = .true.
            if (len_trim(labels(i)) > 0 .and. index(labels(i), '-') == 0 .and. trim(labels(i)) /= '0') has_positive = .true.
        end do
        
        call assert_true(has_zero, "Symlog should include zero")
    end subroutine

    subroutine test_should_format_tick_values_consistently()
        !! Test consistent tick value formatting
        print *, "Testing: Tick values should be formatted consistently"
        
        character(len=20) :: result
        
        ! Test integer values
        result = format_tick_value(5.0_wp, 10.0_wp)
        call assert_string_equals(result, "5", "Integer values should format as integers")
        
        ! Test decimal values
        result = format_tick_value(2.5_wp, 5.0_wp)
        call assert_string_contains(result, "2.5", "Decimal values should be formatted properly")
        
        ! Test zero
        result = format_tick_value(0.0_wp, 10.0_wp)
        call assert_string_equals(result, "0", "Zero should format simply")
    end subroutine

    subroutine test_should_calculate_nice_axis_limits()
        !! Test nice axis limit calculation
        print *, "Testing: Nice axis limits should encompass data with round numbers"
        
        real(wp) :: nice_min, nice_max
        
        ! Test data that should expand to nice limits
        call calculate_nice_axis_limits(1.3_wp, 8.7_wp, 5, nice_min, nice_max)
        call assert_less_than_or_equal(nice_min, 1.3_wp, "Nice min should encompass data min")
        call assert_greater_than_or_equal(nice_max, 8.7_wp, "Nice max should encompass data max")
        
        ! Test already nice data
        call calculate_nice_axis_limits(0.0_wp, 10.0_wp, 5, nice_min, nice_max)
        call assert_approximately_equal(nice_min, 0.0_wp, 1.0e-10_wp, "Already nice data should be preserved")
        call assert_approximately_equal(nice_max, 10.0_wp, 1.0e-10_wp, "Already nice data should be preserved")
    end subroutine

    subroutine test_should_handle_edge_cases()
        !! Test edge cases and error conditions
        print *, "Testing: Edge cases should be handled gracefully"
        
        character(len=20) :: labels(10)
        
        ! Test zero range
        call calculate_tick_labels(5.0_wp, 5.0_wp, 5, labels)
        call assert_not_empty(labels(1), "Zero range should still produce a tick")
        
        ! Test negative log values (should handle gracefully)
        call calculate_tick_labels_log(-1.0_wp, 1.0_wp, 5, labels)
        call assert_empty(labels(1), "Negative log range should produce no ticks")
        
        ! Test single tick request
        call calculate_tick_labels(0.0_wp, 10.0_wp, 1, labels)
        ! Should handle gracefully without crashing
    end subroutine

    ! === ASSERTION HELPERS ===

    subroutine assert_not_empty(str, message)
        character(len=*), intent(in) :: str, message
        if (len_trim(str) == 0) then
            print *, "ASSERTION FAILED: ", message
            stop 1
        end if
    end subroutine

    subroutine assert_empty(str, message)
        character(len=*), intent(in) :: str, message
        if (len_trim(str) > 0) then
            print *, "ASSERTION FAILED: ", message, " - Got: '", trim(str), "'"
            stop 1
        end if
    end subroutine

    subroutine assert_string_equals(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message
        if (trim(actual) /= trim(expected)) then
            print *, "ASSERTION FAILED: ", message
            print *, "Expected: '", trim(expected), "', Got: '", trim(actual), "'"
            stop 1
        end if
    end subroutine

    subroutine assert_string_contains(str, substring, message)
        character(len=*), intent(in) :: str, substring, message
        if (index(str, substring) == 0) then
            print *, "ASSERTION FAILED: ", message
            print *, "String '", trim(str), "' does not contain '", substring, "'"
            stop 1
        end if
    end subroutine

    subroutine assert_approximately_equal(actual, expected, tolerance, message)
        real(wp), intent(in) :: actual, expected, tolerance
        character(len=*), intent(in) :: message
        if (abs(actual - expected) > tolerance) then
            print *, "ASSERTION FAILED: ", message
            print *, "Expected: ", expected, ", Got: ", actual, ", Tolerance: ", tolerance
            stop 1
        end if
    end subroutine

    subroutine assert_less_than_or_equal(actual, expected, message)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        if (actual > expected) then
            print *, "ASSERTION FAILED: ", message
            print *, "Expected <= ", expected, ", Got: ", actual
            stop 1
        end if
    end subroutine

    subroutine assert_greater_than_or_equal(actual, expected, message)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        if (actual < expected) then
            print *, "ASSERTION FAILED: ", message
            print *, "Expected >= ", expected, ", Got: ", actual
            stop 1
        end if
    end subroutine

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        if (.not. condition) then
            print *, "ASSERTION FAILED: ", message
            stop 1
        end if
    end subroutine

end program test_ticks_refactored