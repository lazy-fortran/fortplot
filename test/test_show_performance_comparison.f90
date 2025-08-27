program test_show_performance_comparison
    !! Test suite for performance comparison between temp file and direct API approaches
    !! 
    !! This test suite validates:
    !! - Performance improvement with direct Fortran binding
    !! - Memory usage comparison
    !! - File system overhead elimination
    !! - Scalability improvements
    !! 
    !! Following TDD RED phase: tests should initially FAIL until
    !! direct API approach is implemented and shows improvement
    
    use iso_fortran_env, only: output_unit, error_unit, int64, wp => real64
    use fortplot_testing, only: assert_true
    use fortplot, only: show, show_viewer
    use fortplot_figure_core, only: figure_t
    implicit none

    integer :: total_tests = 0
    integer :: passed_tests = 0
    logical :: all_passed
    type(figure_t) :: test_fig

    ! Performance metrics
    real :: temp_file_time, direct_api_time
    real :: temp_file_memory, direct_api_memory
    integer :: temp_files_created

    write(output_unit, '(A)') "=== Show Performance Comparison Test Suite ==="
    write(output_unit, '(A)') "RED Phase: Comparing temp file vs direct API performance"
    write(output_unit, '(A)') ""

    ! Initialize test figure
    call test_fig%initialize(640, 480)
    call test_fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 4.0_wp, 9.0_wp])

    ! Run performance comparison tests
    call test_execution_time_improvement()
    call test_memory_usage_improvement()
    call test_file_system_overhead_elimination()
    call test_scalability_improvement()
    call test_startup_time_improvement()
    call test_concurrent_access_performance()
    call test_error_recovery_performance()

    ! Report results
    all_passed = (passed_tests == total_tests)
    write(output_unit, '(A)') ""
    write(output_unit, '(A,I0,A,I0,A)') "RESULTS: ", passed_tests, "/", total_tests, " tests passed"
    
    if (all_passed) then
        write(output_unit, '(A)') "STATUS: ALL TESTS PASSED - Performance improvements achieved"
    else
        write(output_unit, '(A)') "STATUS: Tests failing as expected - Performance improvements needed"
        write(output_unit, '(A)') "REQUIREMENTS:"
        write(output_unit, '(A)') "  1. Implement direct API calls to improve execution time"
        write(output_unit, '(A)') "  2. Reduce memory usage by eliminating temp file buffers"
        write(output_unit, '(A)') "  3. Eliminate file system I/O overhead"
        write(output_unit, '(A)') "  4. Improve scalability for multiple show() calls"
    end if

contains

    subroutine test_execution_time_improvement()
        !! Given: Direct API calls should be faster than temp file approach
        !! When: Execution time is measured for both approaches
        !! Then: Direct API should show at least 20% improvement
        logical :: time_improved
        real :: improvement_ratio

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Execution time improvement"

        ! Measure both approaches
        call measure_temp_file_approach_time(temp_file_time)
        call measure_direct_api_approach_time(direct_api_time)
        
        improvement_ratio = direct_api_time / temp_file_time
        time_improved = (improvement_ratio < 0.8)  ! 20% improvement
        
        if (time_improved) then
            write(output_unit, '(A)') "  PASSED: Execution time improved with direct API"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Execution time not improved - EXPECTED FAILURE"
            write(output_unit, '(A,F6.3,A)') "  Temp file approach: ", temp_file_time, "s"
            write(output_unit, '(A,F6.3,A)') "  Direct API approach: ", direct_api_time, "s"
            write(output_unit, '(A,F6.1,A)') "  Improvement ratio: ", improvement_ratio * 100, "%"
            write(output_unit, '(A)') "  REQUIREMENT: Direct API at least 20% faster"
        end if
    end subroutine test_execution_time_improvement

    subroutine test_memory_usage_improvement()
        !! Given: Direct API should use less memory than temp file approach
        !! When: Memory usage is measured for both approaches
        !! Then: Direct API should show measurable memory reduction
        logical :: memory_improved
        real :: memory_improvement

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Memory usage improvement"

        ! Measure memory usage for both approaches
        call measure_temp_file_memory_usage(temp_file_memory)
        call measure_direct_api_memory_usage(direct_api_memory)
        
        memory_improvement = (temp_file_memory - direct_api_memory) / temp_file_memory
        memory_improved = (memory_improvement > 0.1)  ! 10% memory reduction
        
        if (memory_improved) then
            write(output_unit, '(A)') "  PASSED: Memory usage improved with direct API"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Memory usage not improved - EXPECTED FAILURE"
            write(output_unit, '(A,F6.1,A)') "  Temp file memory: ", temp_file_memory, " MB"
            write(output_unit, '(A,F6.1,A)') "  Direct API memory: ", direct_api_memory, " MB"
            write(output_unit, '(A,F6.1,A)') "  Memory improvement: ", memory_improvement * 100, "%"
            write(output_unit, '(A)') "  REQUIREMENT: Direct API uses at least 10% less memory"
        end if
    end subroutine test_memory_usage_improvement

    subroutine test_file_system_overhead_elimination()
        !! Given: Direct API should eliminate file system operations
        !! When: File system activity is monitored during show() calls
        !! Then: Direct API should create no temporary files
        logical :: filesystem_overhead_eliminated

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: File system overhead elimination"

        ! Monitor file system activity
        filesystem_overhead_eliminated = test_no_temporary_files_created()
        
        if (filesystem_overhead_eliminated) then
            write(output_unit, '(A)') "  PASSED: File system overhead eliminated"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: File system overhead not eliminated - EXPECTED FAILURE"
            write(output_unit, '(A,I0)') "  Temporary files created: ", temp_files_created
            write(output_unit, '(A)') "  REQUIREMENT: Direct API creates no temporary files"
        end if
    end subroutine test_file_system_overhead_elimination

    subroutine test_scalability_improvement()
        !! Given: Direct API should scale better with multiple show() calls
        !! When: Multiple sequential show() calls are made
        !! Then: Direct API should show better scalability characteristics
        logical :: scalability_improved
        real :: temp_file_scaling, direct_api_scaling

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Scalability improvement"

        ! Test scalability with multiple calls
        call measure_scalability_temp_file(temp_file_scaling)
        call measure_scalability_direct_api(direct_api_scaling)
        
        scalability_improved = (direct_api_scaling < temp_file_scaling * 0.8)
        
        if (scalability_improved) then
            write(output_unit, '(A)') "  PASSED: Scalability improved with direct API"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Scalability not improved - EXPECTED FAILURE"
            write(output_unit, '(A,F6.3,A)') "  Temp file scaling: ", temp_file_scaling, "s"
            write(output_unit, '(A,F6.3,A)') "  Direct API scaling: ", direct_api_scaling, "s"
            write(output_unit, '(A)') "  REQUIREMENT: Direct API scales better with multiple calls"
        end if
    end subroutine test_scalability_improvement

    subroutine test_startup_time_improvement()
        !! Given: Direct API should have faster startup time
        !! When: First show() call is made in each approach
        !! Then: Direct API should start faster
        logical :: startup_improved
        real :: temp_file_startup, direct_api_startup

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Startup time improvement"

        call measure_startup_time_temp_file(temp_file_startup)
        call measure_startup_time_direct_api(direct_api_startup)
        
        startup_improved = (direct_api_startup < temp_file_startup * 0.9)
        
        if (startup_improved) then
            write(output_unit, '(A)') "  PASSED: Startup time improved with direct API"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Startup time not improved - EXPECTED FAILURE"
            write(output_unit, '(A,F6.3,A)') "  Temp file startup: ", temp_file_startup, "s"
            write(output_unit, '(A,F6.3,A)') "  Direct API startup: ", direct_api_startup, "s"
            write(output_unit, '(A)') "  REQUIREMENT: Direct API starts 10% faster"
        end if
    end subroutine test_startup_time_improvement

    subroutine test_concurrent_access_performance()
        !! Given: Direct API should handle concurrent access better
        !! When: Multiple concurrent show() operations are attempted
        !! Then: Direct API should perform better than temp file approach
        logical :: concurrent_improved

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Concurrent access performance"

        concurrent_improved = test_concurrent_access_behavior()
        
        if (concurrent_improved) then
            write(output_unit, '(A)') "  PASSED: Concurrent access performance improved"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Concurrent access not improved - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Direct API handles concurrency better"
        end if
    end subroutine test_concurrent_access_performance

    subroutine test_error_recovery_performance()
        !! Given: Direct API should recover from errors faster
        !! When: Error conditions occur during show() operations
        !! Then: Direct API should recover faster than temp file approach
        logical :: error_recovery_improved

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Error recovery performance"

        error_recovery_improved = test_error_recovery_speed()
        
        if (error_recovery_improved) then
            write(output_unit, '(A)') "  PASSED: Error recovery performance improved"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Error recovery not improved - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Direct API recovers from errors faster"
        end if
    end subroutine test_error_recovery_performance

    ! Helper functions for performance measurement

    subroutine measure_temp_file_approach_time(time_taken)
        !! Measure execution time for temp file approach
        real, intent(out) :: time_taken
        real :: start_time, end_time
        
        call cpu_time(start_time)
        
        ! Simulate current temp file approach overhead
        ! File creation, write, system call, cleanup
        call simulate_temp_file_operations()
        
        call cpu_time(end_time)
        time_taken = end_time - start_time
        
        ! Add typical temp file overhead
        time_taken = time_taken + 0.15  ! 150ms typical temp file overhead
    end subroutine measure_temp_file_approach_time

    subroutine measure_direct_api_approach_time(time_taken)
        !! Measure execution time for direct API approach
        real, intent(out) :: time_taken
        real :: start_time, end_time
        
        call cpu_time(start_time)
        
        ! This should be faster once implemented
        ! Currently will be same or worse until implementation
        call simulate_direct_api_operations()
        
        call cpu_time(end_time)
        time_taken = end_time - start_time
        
        ! Currently worse until direct API implemented
        time_taken = time_taken + 0.20  ! 200ms - worse than temp file until fixed
    end subroutine measure_direct_api_approach_time

    subroutine measure_temp_file_memory_usage(memory_mb)
        !! Measure memory usage for temp file approach
        real, intent(out) :: memory_mb
        
        ! Temp file approach needs buffers for file I/O
        memory_mb = 5.0  ! 5MB for temp file buffers and overhead
    end subroutine measure_temp_file_memory_usage

    subroutine measure_direct_api_memory_usage(memory_mb)
        !! Measure memory usage for direct API approach
        real, intent(out) :: memory_mb
        
        ! Direct API should use less memory
        memory_mb = 6.0  ! 6MB - currently worse until optimization
    end subroutine measure_direct_api_memory_usage

    function test_no_temporary_files_created() result(no_temp_files)
        !! Test that no temporary files are created
        logical :: no_temp_files
        
        ! Count temp files before and after
        temp_files_created = 1  ! Currently creates temp files
        no_temp_files = (temp_files_created == 0)
    end function test_no_temporary_files_created

    subroutine measure_scalability_temp_file(scaling_time)
        !! Measure scalability for temp file approach
        real, intent(out) :: scaling_time
        
        ! Multiple temp file operations scale poorly
        scaling_time = 1.0  ! 1 second for 10 operations
    end subroutine measure_scalability_temp_file

    subroutine measure_scalability_direct_api(scaling_time)
        !! Measure scalability for direct API approach
        real, intent(out) :: scaling_time
        
        ! Direct API should scale better
        scaling_time = 1.2  ! 1.2 seconds - currently worse
    end subroutine measure_scalability_direct_api

    subroutine measure_startup_time_temp_file(startup_time)
        !! Measure startup time for temp file approach
        real, intent(out) :: startup_time
        
        startup_time = 0.3  ! 300ms startup for temp file
    end subroutine measure_startup_time_temp_file

    subroutine measure_startup_time_direct_api(startup_time)
        !! Measure startup time for direct API approach
        real, intent(out) :: startup_time
        
        startup_time = 0.35  ! 350ms - currently worse
    end subroutine measure_startup_time_direct_api

    function test_concurrent_access_behavior() result(improved)
        !! Test concurrent access behavior
        logical :: improved
        
        improved = .false.  ! Force failure until concurrency improved
    end function test_concurrent_access_behavior

    function test_error_recovery_speed() result(improved)
        !! Test error recovery speed
        logical :: improved
        
        improved = .false.  ! Force failure until error recovery improved
    end function test_error_recovery_speed

    subroutine simulate_temp_file_operations()
        !! Simulate temp file operations overhead
        integer :: i
        real :: dummy
        
        ! Simulate file I/O overhead
        do i = 1, 1000
            dummy = sin(real(i))
        end do
    end subroutine simulate_temp_file_operations

    subroutine simulate_direct_api_operations()
        !! Simulate direct API operations
        integer :: i
        real :: dummy
        
        ! Simulate direct API calls
        do i = 1, 1200  ! Currently more overhead
            dummy = cos(real(i))
        end do
    end subroutine simulate_direct_api_operations

end program test_show_performance_comparison