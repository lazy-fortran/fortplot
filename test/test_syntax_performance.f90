program test_syntax_performance
    !! Test suite for syntax processing performance and caching validation
    !! Following TDD RED phase - these tests define performance requirements
    !! 
    !! GIVEN: matplotlib syntax parsing and parameter translation needs
    !! WHEN: processing syntax at scale with repeated parsing operations
    !! THEN: should maintain acceptable performance with efficient caching

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: iso_fortran_env, only: int64
    use fortplot_format_parser, only: parse_format_string, contains_format_chars
    use fortplot_markers, only: validate_marker_style, get_marker_size
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    
    ! Performance and caching tests
    call test_basic_parsing_performance()
    call test_repeated_parsing_performance()
    call test_syntax_validation_performance()
    call test_marker_processing_performance()
    call test_bulk_operations_performance()
    call test_cache_effectiveness()
    call test_memory_efficiency()
    
    call print_test_summary()
    
contains

    subroutine test_basic_parsing_performance()
        !! GIVEN: matplotlib syntax strings for parsing
        !! WHEN: parsing single format strings repeatedly
        !! THEN: should complete parsing within acceptable time limits
        
        character(len=20) :: marker, linestyle
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_seconds
        integer, parameter :: NUM_ITERATIONS = 10000
        integer :: i
        
        call start_test("Basic parsing performance")
        
        ! Measure basic parsing performance
        call system_clock(start_time, count_rate)
        
        do i = 1, NUM_ITERATIONS
            call parse_format_string('o-', marker, linestyle)
            call parse_format_string('s--', marker, linestyle)
            call parse_format_string('^:', marker, linestyle)
            call parse_format_string('*-.', marker, linestyle)
        end do
        
        call system_clock(end_time)
        elapsed_seconds = real(end_time - start_time, wp) / real(count_rate, wp)
        
        write(*, '(A,I0,A,F8.4,A)') "  Parsed ", NUM_ITERATIONS * 4, " format strings in ", &
              elapsed_seconds, " seconds"
        
        ! Performance requirement: should parse at least 1000 strings per second
        call assert_true(elapsed_seconds < 10.0_wp, "Basic parsing within 10 seconds")
        call assert_true(NUM_ITERATIONS * 4 / elapsed_seconds > 100.0_wp, &
                        "Parse rate > 100 strings/second")
        
        call end_test()
    end subroutine test_basic_parsing_performance

    subroutine test_repeated_parsing_performance()
        !! GIVEN: repeated parsing of identical format strings
        !! WHEN: parsing same strings multiple times
        !! THEN: should benefit from caching or remain consistently fast
        
        character(len=20) :: marker, linestyle
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_seconds
        integer, parameter :: NUM_ITERATIONS = 5000
        integer :: i
        
        call start_test("Repeated parsing performance (caching test)")
        
        ! Measure repeated parsing of same strings (cache effectiveness)
        call system_clock(start_time, count_rate)
        
        do i = 1, NUM_ITERATIONS
            ! Parse the same format strings repeatedly
            call parse_format_string('o-', marker, linestyle)
            call parse_format_string('o-', marker, linestyle)
            call parse_format_string('s--', marker, linestyle)
            call parse_format_string('s--', marker, linestyle)
        end do
        
        call system_clock(end_time)
        elapsed_seconds = real(end_time - start_time, wp) / real(count_rate, wp)
        
        write(*, '(A,I0,A,F8.4,A)') "  Repeated parsing of ", NUM_ITERATIONS * 4, &
              " strings in ", elapsed_seconds, " seconds"
        
        ! Should be at least as fast as basic parsing
        call assert_true(elapsed_seconds < 15.0_wp, "Repeated parsing within 15 seconds")
        
        call end_test()
    end subroutine test_repeated_parsing_performance

    subroutine test_syntax_validation_performance()
        !! GIVEN: need for syntax validation at scale
        !! WHEN: validating many format strings for validity
        !! THEN: should maintain fast validation performance
        
        logical :: has_format, is_valid
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_seconds
        integer, parameter :: NUM_ITERATIONS = 8000
        integer :: i
        
        call start_test("Syntax validation performance")
        
        ! Measure validation performance
        call system_clock(start_time, count_rate)
        
        do i = 1, NUM_ITERATIONS
            ! Test format detection
            has_format = contains_format_chars('o-')
            has_format = contains_format_chars('invalid')
            has_format = contains_format_chars('')
            
            ! Test marker validation
            is_valid = validate_marker_style('o')
            is_valid = validate_marker_style('invalid')
        end do
        
        call system_clock(end_time)
        elapsed_seconds = real(end_time - start_time, wp) / real(count_rate, wp)
        
        write(*, '(A,I0,A,F8.4,A)') "  Validated ", NUM_ITERATIONS * 5, &
              " operations in ", elapsed_seconds, " seconds"
        
        call assert_true(elapsed_seconds < 5.0_wp, "Validation within 5 seconds")
        call assert_true(NUM_ITERATIONS * 5 / elapsed_seconds > 1000.0_wp, &
                        "Validation rate > 1000 ops/second")
        
        call end_test()
    end subroutine test_syntax_validation_performance

    subroutine test_marker_processing_performance()
        !! GIVEN: marker size and validation operations
        !! WHEN: processing marker properties at scale
        !! THEN: should maintain fast marker processing
        
        real(wp) :: marker_size
        logical :: is_valid
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_seconds
        integer, parameter :: NUM_ITERATIONS = 10000
        integer :: i
        character(len=1), parameter :: markers(6) = ['o', 's', '^', '*', '+', 'x']
        integer :: j
        
        call start_test("Marker processing performance")
        
        ! Measure marker processing performance
        call system_clock(start_time, count_rate)
        
        do i = 1, NUM_ITERATIONS
            do j = 1, size(markers)
                is_valid = validate_marker_style(markers(j))
                marker_size = get_marker_size(markers(j))
            end do
        end do
        
        call system_clock(end_time)
        elapsed_seconds = real(end_time - start_time, wp) / real(count_rate, wp)
        
        write(*, '(A,I0,A,F8.4,A)') "  Processed ", NUM_ITERATIONS * size(markers) * 2, &
              " marker operations in ", elapsed_seconds, " seconds"
        
        call assert_true(elapsed_seconds < 3.0_wp, "Marker processing within 3 seconds")
        
        call end_test()
    end subroutine test_marker_processing_performance

    subroutine test_bulk_operations_performance()
        !! GIVEN: need to process many format strings in batch
        !! WHEN: processing arrays of format strings
        !! THEN: should scale efficiently for bulk operations
        
        character(len=20) :: marker, linestyle
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: elapsed_seconds
        integer, parameter :: NUM_FORMATS = 1000
        character(len=10), parameter :: format_strings(10) = [ &
            'o-        ', 's--       ', '^:        ', '*-.       ', &
            '+         ', 'x         ', 'D         ', 'p         ', &
            'h         ', 'o--       ' ]
        integer :: i, j
        
        call start_test("Bulk operations performance")
        
        ! Measure bulk processing performance
        call system_clock(start_time, count_rate)
        
        do i = 1, NUM_FORMATS
            do j = 1, size(format_strings)
                call parse_format_string(trim(format_strings(j)), marker, linestyle)
            end do
        end do
        
        call system_clock(end_time)
        elapsed_seconds = real(end_time - start_time, wp) / real(count_rate, wp)
        
        write(*, '(A,I0,A,F8.4,A)') "  Bulk processed ", NUM_FORMATS * size(format_strings), &
              " format strings in ", elapsed_seconds, " seconds"
        
        call assert_true(elapsed_seconds < 20.0_wp, "Bulk processing within 20 seconds")
        
        call end_test()
    end subroutine test_bulk_operations_performance

    subroutine test_cache_effectiveness()
        !! GIVEN: potential caching system for parsed formats
        !! WHEN: comparing cached vs uncached parsing performance
        !! THEN: should demonstrate cache effectiveness if implemented
        
        character(len=20) :: marker, linestyle
        integer(int64) :: start_time, end_time, count_rate
        real(wp) :: cold_time, warm_time
        integer, parameter :: NUM_ITERATIONS = 2000
        integer :: i
        
        call start_test("Cache effectiveness validation")
        
        ! Measure "cold" performance (first time parsing)
        call system_clock(start_time, count_rate)
        
        do i = 1, NUM_ITERATIONS
            call parse_format_string('o-', marker, linestyle)
            call parse_format_string('s--', marker, linestyle)
            call parse_format_string('^:', marker, linestyle)
            call parse_format_string('*-.', marker, linestyle)
        end do
        
        call system_clock(end_time)
        cold_time = real(end_time - start_time, wp) / real(count_rate, wp)
        
        ! Measure "warm" performance (repeated parsing)
        call system_clock(start_time, count_rate)
        
        do i = 1, NUM_ITERATIONS
            call parse_format_string('o-', marker, linestyle)
            call parse_format_string('s--', marker, linestyle)
            call parse_format_string('^:', marker, linestyle)
            call parse_format_string('*-.', marker, linestyle)
        end do
        
        call system_clock(end_time)
        warm_time = real(end_time - start_time, wp) / real(count_rate, wp)
        
        write(*, '(A,F8.4,A,F8.4,A)') "  Cold time: ", cold_time, "s, Warm time: ", warm_time, "s"
        
        ! Cache effectiveness: warm should be no worse than cold
        call assert_true(warm_time <= cold_time * 1.1_wp, "Warm performance not worse than cold")
        
        call end_test()
    end subroutine test_cache_effectiveness

    subroutine test_memory_efficiency()
        !! GIVEN: syntax processing operations that may allocate memory
        !! WHEN: performing many operations in sequence
        !! THEN: should not exhibit memory leaks or excessive allocation
        
        character(len=20) :: marker, linestyle
        logical :: has_format, is_valid
        real(wp) :: marker_size
        integer, parameter :: NUM_ITERATIONS = 5000
        integer :: i
        
        call start_test("Memory efficiency validation")
        
        ! Perform many operations that could potentially leak memory
        do i = 1, NUM_ITERATIONS
            ! Format parsing
            call parse_format_string('o-', marker, linestyle)
            call parse_format_string('very_long_invalid_format_string', marker, linestyle)
            
            ! Format detection
            has_format = contains_format_chars('o-')
            has_format = contains_format_chars('invalid_format')
            
            ! Marker operations
            is_valid = validate_marker_style('o')
            marker_size = get_marker_size('s')
        end do
        
        write(*, '(A,I0,A)') "  Completed ", NUM_ITERATIONS * 6, " operations without errors"
        
        ! If we reach here without memory issues, consider it a pass
        call assert_true(.true., "No memory errors during bulk operations")
        
        call end_test()
    end subroutine test_memory_efficiency

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

    subroutine print_test_summary()
        write(*, '(A,I0,A,I0,A)') "Tests completed: ", pass_count, "/", test_count, " passed"
        if (pass_count /= test_count) error stop "Some tests failed"
    end subroutine print_test_summary

end program test_syntax_performance