program test_log_scale_ticks
    !! Unit tests for improved logarithmic scale tick functionality
    !! Tests the enhanced log and symlog tick generation
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_ticks, only: calculate_tick_labels_log, calculate_tick_labels_symlog
    implicit none
    
    call test_should_generate_proper_log_ticks_wide_range()
    call test_should_generate_subticks_narrow_range()
    call test_should_handle_fractional_log_range()
    call test_should_generate_symlog_ticks_with_zero()
    call test_should_handle_symlog_linear_region()
    call test_should_format_scientific_notation()
    
    print *, "All logarithmic tick tests passed!"

contains

    subroutine test_should_generate_proper_log_ticks_wide_range()
        !! Test log tick generation for wide decade ranges
        character(len=20) :: labels(10)
        
        print *, "Testing: Wide log range should generate major ticks only"
        
        ! Test 6 decades - should generate major ticks at powers of 10
        call calculate_tick_labels_log(1.0_wp, 1.0e6_wp, 8, labels)
        
        ! Should have ticks at 1, 10, 100, 1000, 10000, 100000, 1000000
        call assert_string_contains(labels(1), "1", "Should have tick at 1")
        call assert_string_contains(labels(2), "10", "Should have tick at 10")
        call assert_string_contains(labels(3), "100", "Should have tick at 100")
        call assert_string_contains(labels(4), "1000", "Should have tick at 1000")
        
        ! Should not have subticks in wide ranges
        call assert_no_subticks(labels, "Wide range should not include subticks")
    end subroutine

    subroutine test_should_generate_subticks_narrow_range()
        !! Test log tick generation for narrow ranges (should include subticks)
        character(len=20) :: labels(15)
        
        print *, "Testing: Narrow log range should include subticks"
        
        ! Test 1.5 decades - should generate subticks
        call calculate_tick_labels_log(1.0_wp, 30.0_wp, 15, labels)
        
        ! Should have major ticks
        call assert_string_contains(labels(1), "1", "Should have major tick at 1")
        call assert_string_contains(labels(2), "2", "Should have subtick at 2")
        call assert_string_contains(labels(3), "3", "Should have subtick at 3")
        call assert_string_contains(labels(4), "5", "Should have subtick at 5")
        
        ! Should continue with next decade
        call assert_has_tick_value(labels, "10", "Should have major tick at 10")
        call assert_has_tick_value(labels, "20", "Should have subtick at 20")
    end subroutine

    subroutine test_should_handle_fractional_log_range()
        !! Test log ticks in fractional ranges
        character(len=20) :: labels(10)
        
        print *, "Testing: Fractional log range should be handled properly"
        
        call calculate_tick_labels_log(0.001_wp, 1.0_wp, 8, labels)
        
        call assert_string_contains(labels(1), "0.001", "Should handle millis")
        call assert_string_contains(labels(2), "0.01", "Should handle centis")
        call assert_string_contains(labels(3), "0.1", "Should handle decis")
        call assert_string_contains(labels(4), "1", "Should handle units")
    end subroutine

    subroutine test_should_generate_symlog_ticks_with_zero()
        !! Test symlog tick generation across zero
        character(len=20) :: labels(10)
        
        print *, "Testing: Symlog should include zero and symmetric ticks"
        
        call calculate_tick_labels_symlog(-100.0_wp, 100.0_wp, 1.0_wp, 8, labels)
        
        call assert_has_tick_value(labels, "0", "Should include zero")
        call assert_has_negative_tick(labels, "Should include negative values")
        call assert_has_positive_tick(labels, "Should include positive values")
    end subroutine

    subroutine test_should_handle_symlog_linear_region()
        !! Test symlog behavior in linear region
        character(len=20) :: labels(10)
        
        print *, "Testing: Symlog linear region should use appropriate spacing"
        
        ! Range entirely within linear threshold
        call calculate_tick_labels_symlog(-0.5_wp, 0.5_wp, 1.0_wp, 5, labels)
        
        call assert_has_tick_value(labels, "0", "Linear region should include zero")
        call assert_not_empty(labels(1), "Should generate some ticks in linear region")
    end subroutine

    subroutine test_should_format_scientific_notation()
        !! Test scientific notation formatting for extreme values
        character(len=20) :: labels(5)
        
        print *, "Testing: Extreme values should use scientific notation"
        
        ! Very large range
        call calculate_tick_labels_log(1.0e-6_wp, 1.0e6_wp, 5, labels)
        
        ! Should handle both small and large values appropriately
        call assert_not_empty(labels(1), "Should format very small values")
        call assert_not_empty(labels(5), "Should format very large values")
    end subroutine

    ! === ASSERTION HELPERS ===

    subroutine assert_string_contains(str, substring, message)
        character(len=*), intent(in) :: str, substring, message
        if (index(str, substring) == 0) then
            print *, "ASSERTION FAILED: ", message
            print *, "String '", trim(str), "' does not contain '", substring, "'"
            stop 1
        end if
    end subroutine

    subroutine assert_has_tick_value(labels, value, message)
        character(len=*), intent(in) :: labels(:), value, message
        integer :: i
        logical :: found
        
        found = .false.
        do i = 1, size(labels)
            if (index(labels(i), value) > 0) then
                found = .true.
                exit
            end if
        end do
        
        if (.not. found) then
            print *, "ASSERTION FAILED: ", message
            print *, "Value '", value, "' not found in labels"
            stop 1
        end if
    end subroutine

    subroutine assert_no_subticks(labels, message)
        character(len=*), intent(in) :: labels(:), message
        integer :: i
        
        ! Check that we don't have values like 2, 3, 5, 6, 7, 8, 9 (subticks)
        do i = 1, size(labels)
            if (index(labels(i), "2") > 0 .or. index(labels(i), "3") > 0 .or. &
                index(labels(i), "5") > 0) then
                ! Only fail if it's a single digit (subtick), not part of a larger number
                if (len_trim(labels(i)) == 1) then
                    print *, "ASSERTION FAILED: ", message
                    print *, "Found subtick: '", trim(labels(i)), "'"
                    stop 1
                end if
            end if
        end do
    end subroutine

    subroutine assert_has_negative_tick(labels, message)
        character(len=*), intent(in) :: labels(:), message
        integer :: i
        logical :: found
        
        found = .false.
        do i = 1, size(labels)
            if (index(labels(i), "-") > 0) then
                found = .true.
                exit
            end if
        end do
        
        if (.not. found) then
            print *, "ASSERTION FAILED: ", message
            stop 1
        end if
    end subroutine

    subroutine assert_has_positive_tick(labels, message)
        character(len=*), intent(in) :: labels(:), message
        integer :: i
        logical :: found
        
        found = .false.
        do i = 1, size(labels)
            if (len_trim(labels(i)) > 0 .and. index(labels(i), "-") == 0 .and. &
                trim(labels(i)) /= "0") then
                found = .true.
                exit
            end if
        end do
        
        if (.not. found) then
            print *, "ASSERTION FAILED: ", message
            stop 1
        end if
    end subroutine

    subroutine assert_not_empty(str, message)
        character(len=*), intent(in) :: str, message
        if (len_trim(str) == 0) then
            print *, "ASSERTION FAILED: ", message
            stop 1
        end if
    end subroutine

end program test_log_scale_ticks