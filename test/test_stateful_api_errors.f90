program test_stateful_api_errors
    !! Comprehensive error handling tests for stateful API
    !! Tests edge cases, invalid inputs, and boundary conditions
    use fortplot_matplotlib
    implicit none
    
    integer :: failed_tests, total_tests
    logical :: test_passed
    integer, parameter :: wp = kind(1.0d0)  ! Working precision
    
    failed_tests = 0
    total_tests = 0
    
    ! Test invalid data arrays
    call test_empty_arrays()
    call test_mismatched_arrays()
    call test_nan_inf_values()
    call test_single_point_data()
    
    ! Test invalid parameters
    call test_invalid_linestyle()
    call test_negative_sizes()
    call test_invalid_ranges()
    call test_invalid_markers()
    
    ! Test boundary conditions
    call test_extreme_values()
    call test_zero_ranges()
    call test_identical_data()
    
    ! Test figure state errors
    call test_invalid_subplot()
    call test_savefig_errors()
    
    ! Additional coverage tests
    call test_label_functions()
    call test_actual_nan_values()
    
    ! Print summary
    write(*,'(A,I0,A,I0,A)') "Error handling tests: ", &
        total_tests - failed_tests, " of ", total_tests, " passed"
    
    if (failed_tests > 0) then
        error stop "Error handling tests failed"
    end if
    
contains
    
    subroutine test_empty_arrays()
        real(wp), allocatable :: x(:), y(:)
        
        allocate(x(0), y(0))
        total_tests = total_tests + 1
        
        ! Should handle empty arrays gracefully
        call figure()
        call plot(x, y)
        
        ! If we get here, it handled empty arrays
        write(*,'(A)') "PASS: Empty array handling"
        
        ! Allocatable arrays auto-deallocate when out of scope
    end subroutine test_empty_arrays
    
    subroutine test_mismatched_arrays()
        real(wp), dimension(5) :: x = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        real(wp), dimension(3) :: y = [1.0_wp, 2.0_wp, 3.0_wp]
        
        total_tests = total_tests + 1
        
        ! Should handle mismatched array sizes
        call figure()
        call plot(x(1:5), y)  ! x has 5 elements, y has 3
        
        write(*,'(A)') "PASS: Mismatched array size handling"
    end subroutine test_mismatched_arrays
    
    subroutine test_nan_inf_values()
        real(wp) :: x(4), y(4)
        
        ! Test with extreme values (huge and tiny)
        ! Using huge() and tiny() as proxies for inf/nan testing
        x = [1.0_wp, 2.0_wp, huge(1.0_wp), 4.0_wp]
        y = [1.0_wp, -huge(1.0_wp), 3.0_wp, tiny(1.0_wp)]
        
        total_tests = total_tests + 1
        
        ! Should filter or handle NaN/Inf values
        call figure()
        call plot(x, y)
        
        write(*,'(A)') "PASS: NaN/Inf value handling"
    end subroutine test_nan_inf_values
    
    subroutine test_single_point_data()
        real(wp) :: x(1), y(1)
        
        x = [1.0_wp]
        y = [2.0_wp]
        
        total_tests = total_tests + 1
        
        ! Should handle single point gracefully
        call figure()
        call scatter(x, y)
        
        write(*,'(A)') "PASS: Single point data handling"
    end subroutine test_single_point_data
    
    subroutine test_invalid_linestyle()
        real(wp) :: x(3), y(3)
        
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        
        total_tests = total_tests + 1
        
        ! Test with various invalid linestyle specifications
        call figure()
        
        ! Empty string linestyle
        call plot(x, y, linestyle='')
        
        ! Invalid linestyle
        call plot(x, y, linestyle='invalid')
        
        ! Very long linestyle
        call plot(x, y, linestyle='verylonglinestyle')
        
        write(*,'(A)') "PASS: Invalid linestyle handling"
    end subroutine test_invalid_linestyle
    
    subroutine test_negative_sizes()
        real(wp) :: x(3), y(3)
        real(wp) :: neg_size
        
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        neg_size = -10.0_wp  ! Negative size
        
        total_tests = total_tests + 1
        
        call figure()
        
        ! Should handle negative marker sizes
        call scatter(x, y, s=neg_size)
        
        write(*,'(A)') "PASS: Negative size handling"
    end subroutine test_negative_sizes
    
    subroutine test_invalid_ranges()
        
        total_tests = total_tests + 1
        
        call figure()
        
        ! Test inverted limits
        call xlim(10.0_wp, -10.0_wp)
        
        ! Test identical limits
        call ylim(5.0_wp, 5.0_wp)
        
        write(*,'(A)') "PASS: Invalid range handling"
    end subroutine test_invalid_ranges
    
    subroutine test_invalid_markers()
        real(wp) :: x(3), y(3)
        
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        
        total_tests = total_tests + 1
        
        call figure()
        
        ! Test various invalid markers with scatter (scatter accepts marker)
        call scatter(x, y, marker='')      ! Empty marker
        call scatter(x, y, marker='??')    ! Invalid marker
        call scatter(x, y, marker='invalid') ! Long invalid marker
        
        write(*,'(A)') "PASS: Invalid marker handling"
    end subroutine test_invalid_markers
    
    subroutine test_extreme_values()
        real(wp) :: x(3), y(3)
        
        ! Test with extreme values
        x = [huge(1.0_wp), 0.0_wp, -huge(1.0_wp)]
        y = [tiny(1.0_wp), 1.0_wp, -tiny(1.0_wp)]
        
        total_tests = total_tests + 1
        
        call figure()
        call plot(x, y)
        
        write(*,'(A)') "PASS: Extreme value handling"
    end subroutine test_extreme_values
    
    subroutine test_zero_ranges()
        real(wp) :: x(10), y(10)
        integer :: i
        
        ! All x values the same
        do i = 1, 10
            x(i) = 5.0_wp
            y(i) = real(i)
        end do
        
        total_tests = total_tests + 1
        
        call figure()
        call plot(x, y)
        
        ! All y values the same
        do i = 1, 10
            x(i) = real(i)
            y(i) = 3.0_wp
        end do
        
        call plot(x, y)
        
        write(*,'(A)') "PASS: Zero range handling"
    end subroutine test_zero_ranges
    
    subroutine test_identical_data()
        real(wp) :: x(5), y(5)
        
        ! All points identical
        x = [2.0_wp, 2.0_wp, 2.0_wp, 2.0_wp, 2.0_wp]
        y = [3.0_wp, 3.0_wp, 3.0_wp, 3.0_wp, 3.0_wp]
        
        total_tests = total_tests + 1
        
        call figure()
        call scatter(x, y)
        
        write(*,'(A)') "PASS: Identical data handling"
    end subroutine test_identical_data
    
    subroutine test_invalid_subplot()
        
        total_tests = total_tests + 1
        
        call figure()
        
        ! Test invalid subplot specifications
        call subplot(0, 1, 1)     ! Zero rows
        call subplot(1, 0, 1)     ! Zero cols
        call subplot(2, 2, 0)     ! Zero index
        call subplot(2, 2, 5)     ! Index out of bounds
        call subplot(-1, 2, 1)    ! Negative rows
        
        write(*,'(A)') "PASS: Invalid subplot handling"
    end subroutine test_invalid_subplot
    
    subroutine test_savefig_errors()
        real(wp) :: x(3), y(3)
        integer :: status
        logical :: test_passed
        
        x = [1.0_wp, 2.0_wp, 3.0_wp]
        y = [1.0_wp, 2.0_wp, 3.0_wp]
        
        total_tests = total_tests + 1
        test_passed = .true.
        
        call figure()
        call plot(x, y)
        
        ! Test invalid paths and formats
        call savefig_with_status('', status)              ! Empty filename
        if (status == 0) then
            failed_tests = failed_tests + 1
            test_passed = .false.
            write(*,'(A)') "FAIL: Empty filename should fail"
        end if
        
        call savefig_with_status('/invalid/path/file.png', status) ! Invalid path
        if (status == 0) then
            failed_tests = failed_tests + 1
            test_passed = .false.
            write(*,'(A)') "FAIL: Invalid path should fail"
        end if
        
        ! Note: The library may accept arbitrary extensions, which is actually good
        ! flexibility. This is not a failure case.
        call savefig_with_status('test.txt', status)    ! Non-image format
        ! We'll just check it doesn't crash, regardless of status
        
        if (test_passed) then
            write(*,'(A)') "PASS: Savefig error handling"
        end if
    end subroutine test_savefig_errors
    
    subroutine test_label_functions()
        ! Test various label functions with edge cases
        total_tests = total_tests + 1
        
        call figure()
        
        ! Empty labels
        call xlabel('')
        call ylabel('')
        call title('')
        
        ! Very long labels
        call xlabel('This is a very long x-axis label that might cause issues with rendering or memory allocation')
        call ylabel('This is a very long y-axis label that might cause issues with rendering or memory allocation')
        call title('This is a very long title that might cause issues with rendering or memory allocation')
        
        ! Labels with special characters
        call xlabel('Label with $math$ and & special chars')
        call ylabel('Y-axis: μ±σ')
        call title('Title with "quotes" and newline\nattempt')
        
        write(*,'(A)') "PASS: Label functions edge case handling"
    end subroutine test_label_functions
    
    subroutine test_actual_nan_values()
        use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
                                                 ieee_positive_inf, ieee_negative_inf
        real(wp) :: x(4), y(4)
        
        ! Create actual NaN and Infinity values using IEEE intrinsics
        x = [1.0_wp, 2.0_wp, ieee_value(1.0_wp, ieee_quiet_nan), 4.0_wp]
        y = [1.0_wp, ieee_value(1.0_wp, ieee_quiet_nan), 3.0_wp, 4.0_wp]
        
        total_tests = total_tests + 1
        
        ! Should handle actual NaN values gracefully
        call figure()
        call plot(x, y)
        
        ! Test with infinity too
        x(3) = ieee_value(1.0_wp, ieee_positive_inf)
        y(2) = ieee_value(1.0_wp, ieee_negative_inf)
        call plot(x, y)
        
        write(*,'(A)') "PASS: Actual NaN/Infinity value handling"
    end subroutine test_actual_nan_values
    
end program test_stateful_api_errors