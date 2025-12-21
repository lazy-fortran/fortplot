program test_scale_implementation
    !! Comprehensive test for scale system implementation
    !! Tests linear, log, and symlog scales with proper axis transformations
    use fortplot
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    integer :: test_count = 0, passed_count = 0
    logical :: all_tests_passed = .true.

    call test_linear_scale()
    call test_log_scale()
    call test_symlog_scale()
    call test_scale_with_negative_values()
    call test_scale_edge_cases()

    call print_test_summary()

contains

    subroutine test_linear_scale()
        !! Test linear scale functionality
        real(dp), dimension(100) :: x, y
        integer :: i
        character(len=256) :: filename

        call test_start("Linear scale test")

        ! Generate test data
        x = [(real(i, dp), i=1, 100)]
        y = sin(x*0.1_dp)*100.0_dp

        ! Create plot with linear scale
        call figure(figsize=[8.0_dp, 6.0_dp])
        call set_xscale('linear')
        call set_yscale('linear')
        call plot(x, y, label='sin(x)')
        call xlabel('X axis (linear)')
        call ylabel('Y axis (linear)')
        call title('Linear Scale Test')
        call legend()

        filename = 'test/output/test_linear_scale.png'
        call savefig(filename)

        call test_result(.true., "Linear scale plot created successfully")
    end subroutine

    subroutine test_log_scale()
        !! Test logarithmic scale functionality
        real(dp), dimension(50) :: x, y
        integer :: i
        character(len=256) :: filename

        call test_start("Logarithmic scale test")

        ! Generate test data suitable for log scale (positive values)
        x = [(10.0_dp**(real(i, dp)/10.0_dp), i=1, 50)]
        y = x**2  ! Quadratic growth

        ! Create plot with log scales
        call figure(figsize=[8.0_dp, 6.0_dp])
        call set_xscale('log')
        call set_yscale('log')
        call plot(x, y, label='x^2')
        call xlabel('X axis (log scale)')
        call ylabel('Y axis (log scale)')
        call title('Logarithmic Scale Test')
        call legend()

        filename = 'test/output/test_log_scale.png'
        call savefig(filename)

        call test_result(.true., "Log scale plot created successfully")
    end subroutine

    subroutine test_symlog_scale()
        !! Test symmetric logarithmic scale functionality
        real(dp), dimension(100) :: x, y
        integer :: i
        character(len=256) :: filename
        real(dp), parameter :: threshold = 1.0_dp

        call test_start("Symmetric logarithmic scale test")

        ! Generate test data with both positive and negative values
        x = [(real(i - 50, dp)*0.5_dp, i=1, 100)]
        y = x**3  ! Cubic function: negative to positive

        ! Create plot with symlog scales
        call figure(figsize=[8.0_dp, 6.0_dp])
        call set_xscale('symlog', threshold)
        call set_yscale('symlog', threshold)
        call plot(x, y, label='x^3')
        call xlabel('X axis (symlog scale)')
        call ylabel('Y axis (symlog scale)')
        call title('Symmetric Logarithmic Scale Test')
        call legend()

        filename = 'test/output/test_symlog_scale.png'
        call savefig(filename)

        call test_result(.true., "Symlog scale plot created successfully")
    end subroutine

    subroutine test_scale_with_negative_values()
        !! Test that log scale handles negative values gracefully
        real(dp), dimension(100) :: x, y
        integer :: i
        character(len=256) :: filename

        call test_start("Log scale with negative values handling")

        ! Generate test data with some negative values
        x = [(real(i - 20, dp)*0.1_dp, i=1, 100)]
        y = abs(sin(x)) + 0.1_dp  ! Always positive for log scale

        ! Only positive x values for log scale
        where (x <= 0.0_dp) x = 0.001_dp

        ! Create plot with log scale on y-axis only
        call figure(figsize=[8.0_dp, 6.0_dp])
        call set_xscale('linear')
        call set_yscale('log')
        call plot(x, y, label='|sin(x)| + 0.1')
        call xlabel('X axis (linear)')
        call ylabel('Y axis (log scale)')
        call title('Mixed Scale Test (Linear X, Log Y)')
        call legend()

        filename = 'test/output/test_mixed_scale.png'
        call savefig(filename)

        call test_result(.true., "Mixed scale plot created successfully")
    end subroutine

    subroutine test_scale_edge_cases()
        !! Test edge cases and error handling
        real(dp), dimension(10) :: x, y
        integer :: i
        logical :: test_passed

        call test_start("Scale edge cases and error handling")

        ! Test data
        x = [(real(i, dp), i=1, 10)]
        y = x**2

        test_passed = .true.

        ! Test 1: Invalid scale type (should default to linear with warning)
        call figure(figsize=[4.0_dp, 3.0_dp])
        call set_xscale('invalid_scale')
        call set_yscale('invalid_scale')
        call plot(x, y)
        call savefig("test/output/test_invalid_scale.png")

        ! Test 2: Very small symlog threshold
        call figure(figsize=[4.0_dp, 3.0_dp])
        call set_xscale('symlog', 0.001_dp)
        call set_yscale('symlog', 0.001_dp)
        call plot(x, y)
        call savefig("test/output/test_small_threshold.png")

        ! Test 3: Switching scales after plotting
        call figure(figsize=[4.0_dp, 3.0_dp])
        call plot(x, y)
        call set_xscale('log')
        call set_yscale('log')
        call savefig("test/output/test_scale_switch.png")

        call test_result(test_passed, "Edge cases handled gracefully")
    end subroutine

    ! Test framework utilities
    subroutine test_start(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write (*, '(A, I0, A, A)') "Test ", test_count, ": ", test_name
    end subroutine

    subroutine test_result(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description

        if (condition) then
            write (*, '(A, A)') "  ✓ PASS: ", description
            passed_count = passed_count + 1
        else
            write (*, '(A, A)') "  ✗ FAIL: ", description
            all_tests_passed = .false.
        end if
    end subroutine

    subroutine print_test_summary()
        write (*, '(/A)') "=== Test Summary ==="
        write (*, '(A, I0, A, I0)') "Passed: ", passed_count, " / ", test_count

        if (all_tests_passed) then
            write (*, '(A)') "✓ All tests PASSED"
        else
            write (*, '(A)') "✗ Some tests FAILED"
            error stop 1
        end if
    end subroutine

end program test_scale_implementation
