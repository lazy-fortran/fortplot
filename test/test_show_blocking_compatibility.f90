program test_show_blocking_compatibility
    !! Test suite for show() blocking parameter functionality and matplotlib compatibility
    !! 
    !! This test suite validates:
    !! - blocking=True/False parameter handling
    !! - Matplotlib pyplot.show() compatibility  
    !! - User interaction patterns
    !! - Non-blocking behavior in scripts
    !! 
    !! Following TDD RED phase: tests should initially FAIL until
    !! proper blocking functionality is implemented
    
    use iso_fortran_env, only: output_unit, error_unit, wp => real64
    use fortplot_testing, only: assert_true
    use fortplot, only: show, show_viewer
    use fortplot_figure_core, only: figure_t
    implicit none

    integer :: total_tests = 0
    integer :: passed_tests = 0
    logical :: all_passed
    type(figure_t) :: test_fig

    write(output_unit, '(A)') "=== Show Blocking Compatibility Test Suite ==="
    write(output_unit, '(A)') "RED Phase: Testing blocking parameter and matplotlib compatibility"
    write(output_unit, '(A)') ""

    ! Initialize test figure
    call test_fig%initialize(640, 480)
    call test_fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 4.0_wp, 9.0_wp])

    ! Run blocking compatibility tests
    call test_blocking_true_behavior()
    call test_blocking_false_behavior()
    call test_default_blocking_behavior()
    call test_matplotlib_compatibility()
    call test_interactive_vs_script_mode()
    call test_blocking_performance_impact()
    call test_user_input_handling()

    ! Report results
    all_passed = (passed_tests == total_tests)
    write(output_unit, '(A)') ""
    write(output_unit, '(A,I0,A,I0,A)') "RESULTS: ", passed_tests, "/", total_tests, " tests passed"
    
    if (all_passed) then
        write(output_unit, '(A)') "STATUS: ALL TESTS PASSED - Blocking functionality working"
    else
        write(output_unit, '(A)') "STATUS: Tests failing as expected - Blocking functionality needs work"
        write(output_unit, '(A)') "REQUIREMENTS:"
        write(output_unit, '(A)') "  1. Implement proper blocking=True behavior"
        write(output_unit, '(A)') "  2. Implement proper blocking=False behavior"
        write(output_unit, '(A)') "  3. Maintain matplotlib.pyplot.show() compatibility"
        write(output_unit, '(A)') "  4. Handle interactive vs script mode differences"
    end if

contains

    subroutine test_blocking_true_behavior()
        !! Given: User calls show(blocking=True) 
        !! When: Function is executed
        !! Then: Should wait for user input before continuing
        logical :: blocking_works
        real :: execution_time

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: blocking=True behavior"

        ! Test blocking behavior - should wait for user input
        blocking_works = test_blocking_true_implementation(execution_time)
        
        if (blocking_works) then
            write(output_unit, '(A)') "  PASSED: blocking=True works correctly"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: blocking=True not working - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Implement blocking=True to wait for user input"
            write(output_unit, '(A,F6.3,A)') "  Execution time: ", execution_time, "s"
        end if
    end subroutine test_blocking_true_behavior

    subroutine test_blocking_false_behavior()
        !! Given: User calls show(blocking=False)
        !! When: Function is executed
        !! Then: Should return immediately without waiting
        logical :: non_blocking_works
        real :: execution_time

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: blocking=False behavior"

        ! Test non-blocking behavior - should return immediately
        non_blocking_works = test_blocking_false_implementation(execution_time)
        
        if (non_blocking_works) then
            write(output_unit, '(A)') "  PASSED: blocking=False works correctly"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: blocking=False not working - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Implement blocking=False to return immediately"
            write(output_unit, '(A,F6.3,A)') "  Execution time: ", execution_time, "s"
        end if
    end subroutine test_blocking_false_behavior

    subroutine test_default_blocking_behavior()
        !! Given: User calls show() without blocking parameter
        !! When: Function is executed
        !! Then: Should use appropriate default behavior (matplotlib compatible)
        logical :: default_appropriate
        logical :: default_blocking

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Default blocking behavior"

        ! Test default behavior - should match matplotlib expectations
        call test_default_blocking_implementation(default_blocking, default_appropriate)
        
        if (default_appropriate) then
            write(output_unit, '(A)') "  PASSED: Default blocking behavior appropriate"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Default blocking behavior inappropriate - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Set appropriate default blocking behavior"
            write(output_unit, '(A,L1)') "  Current default blocking: ", default_blocking
        end if
    end subroutine test_default_blocking_behavior

    subroutine test_matplotlib_compatibility()
        !! Given: Users expect matplotlib.pyplot.show() compatible behavior
        !! When: show() is called with various parameters
        !! Then: Behavior should match matplotlib expectations
        logical :: matplotlib_compatible

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Matplotlib compatibility"

        matplotlib_compatible = test_matplotlib_show_compatibility()
        
        if (matplotlib_compatible) then
            write(output_unit, '(A)') "  PASSED: Matplotlib compatibility maintained"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Matplotlib compatibility issues - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Match matplotlib.pyplot.show() behavior"
        end if
    end subroutine test_matplotlib_compatibility

    subroutine test_interactive_vs_script_mode()
        !! Given: Show behavior may differ in interactive vs script mode
        !! When: Called from different execution contexts
        !! Then: Should behave appropriately for each context
        logical :: context_appropriate

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Interactive vs script mode behavior"

        context_appropriate = test_execution_context_behavior()
        
        if (context_appropriate) then
            write(output_unit, '(A)') "  PASSED: Context-appropriate behavior implemented"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Context behavior not appropriate - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Adapt behavior to interactive vs script context"
        end if
    end subroutine test_interactive_vs_script_mode

    subroutine test_blocking_performance_impact()
        !! Given: Blocking parameter should not significantly impact performance
        !! When: show() is called with different blocking values  
        !! Then: Performance difference should be minimal (excluding user wait time)
        logical :: performance_impact_minimal
        real :: blocking_time, non_blocking_time

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Blocking parameter performance impact"

        call measure_blocking_performance(blocking_time, non_blocking_time)
        
        ! Performance difference should be minimal (excluding user wait)
        performance_impact_minimal = (abs(blocking_time - non_blocking_time) < 0.1)
        
        if (performance_impact_minimal) then
            write(output_unit, '(A)') "  PASSED: Blocking parameter performance impact minimal"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Blocking parameter performance impact significant - EXPECTED FAILURE"
            write(output_unit, '(A,F6.3,A)') "  Blocking time: ", blocking_time, "s"
            write(output_unit, '(A,F6.3,A)') "  Non-blocking time: ", non_blocking_time, "s"
            write(output_unit, '(A)') "  REQUIREMENT: Minimal performance difference between modes"
        end if
    end subroutine test_blocking_performance_impact

    subroutine test_user_input_handling()
        !! Given: Blocking mode requires user input handling
        !! When: User input is provided or not provided
        !! Then: Should handle input appropriately and robustly
        logical :: input_handling_robust

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: User input handling robustness"

        input_handling_robust = test_user_input_robustness()
        
        if (input_handling_robust) then
            write(output_unit, '(A)') "  PASSED: User input handling is robust"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: User input handling not robust - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Robust user input handling in blocking mode"
        end if
    end subroutine test_user_input_handling

    ! Helper functions - initially return false to cause test failures

    function test_blocking_true_implementation(execution_time) result(works)
        !! Test blocking=True implementation
        logical :: works
        real, intent(out) :: execution_time
        real :: start_time, end_time
        
        call cpu_time(start_time)
        
        ! This should wait for user input, but we'll simulate for testing
        ! In real implementation, this would wait
        call cpu_time(end_time)
        execution_time = end_time - start_time
        
        ! Should take significant time if actually blocking (waiting for user)
        ! For testing, we expect this to fail until proper implementation
        works = .false.  ! Force failure until blocking=True properly implemented
        ! Implementation needed - see issue #489
    end function test_blocking_true_implementation

    function test_blocking_false_implementation(execution_time) result(works)
        !! Test blocking=False implementation
        logical :: works
        real, intent(out) :: execution_time
        real :: start_time, end_time
        
        call cpu_time(start_time)
        
        ! This should return immediately without waiting
        ! Current implementation may not support this
        call cpu_time(end_time)
        execution_time = end_time - start_time
        
        ! Should take minimal time (< 0.1s) if non-blocking
        works = (execution_time < 0.1)
        
        ! Force failure until blocking=False properly implemented
        works = .false.  
        ! Implementation needed - see issue #489
    end function test_blocking_false_implementation

    subroutine test_default_blocking_implementation(default_blocking, appropriate)
        !! Test default blocking behavior implementation
        logical, intent(out) :: default_blocking, appropriate
        
        ! Determine what the current default behavior is
        default_blocking = .true.  ! Current implementation appears to always block
        
        ! Determine if default is appropriate (should match matplotlib)
        ! matplotlib.pyplot.show() is blocking by default
        appropriate = default_blocking
        
        ! Force failure until default behavior is verified and appropriate
        appropriate = .false.
        ! Implementation needed - see issue #489
    end subroutine test_default_blocking_implementation

    function test_matplotlib_show_compatibility() result(compatible)
        !! Test matplotlib.pyplot.show() compatibility
        logical :: compatible
        
        ! Test various aspects of matplotlib compatibility
        compatible = .false.  ! Force failure until compatibility verified
        ! Implementation needed - see issue #489
    end function test_matplotlib_show_compatibility

    function test_execution_context_behavior() result(appropriate)
        !! Test behavior in different execution contexts
        logical :: appropriate
        
        appropriate = .false.  ! Force failure until context behavior implemented
        ! Implementation needed - see issue #489
    end function test_execution_context_behavior

    subroutine measure_blocking_performance(blocking_time, non_blocking_time)
        !! Measure performance of blocking vs non-blocking calls
        real, intent(out) :: blocking_time, non_blocking_time
        
        ! Placeholder measurements
        blocking_time = 0.05     ! 50ms for blocking call (excluding wait)
        non_blocking_time = 0.15 ! 150ms for non-blocking (worse - should be better)
        
        ! Implementation needed - see issue #489
    end subroutine measure_blocking_performance

    function test_user_input_robustness() result(robust)
        !! Test robustness of user input handling
        logical :: robust
        
        robust = .false.  ! Force failure until input handling tested
        ! Implementation needed - see issue #489
    end function test_user_input_robustness

end program test_show_blocking_compatibility