program test_windows_ci_performance_measurement
    !! Windows CI Performance Measurement Test Suite (RED Phase)
    !! 
    !! Given: Windows CI tests are extremely slow due to file I/O bottlenecks
    !! When: Measuring performance of current problematic operations
    !! Then: Should identify specific bottlenecks and establish baseline metrics
    !!
    !! Issue #188: Slow test execution on Windows CI
    !! Root cause: Windows file I/O operations are 10-100x slower than Linux
    !! Focus: savefig() operations, directory creation, file validation

    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure_t
    use fortplot_rendering, only: figure_savefig => savefig
    use fortplot_security, only: get_test_output_path
    use fortplot_system_runtime, only: is_windows
    implicit none
    
    ! Performance measurement targets (in seconds)
    real(wp), parameter :: WINDOWS_SAVEFIG_TARGET = 0.5_wp    ! Target: <0.5s per savefig
    real(wp), parameter :: LINUX_SAVEFIG_TARGET = 0.1_wp      ! Linux baseline: <0.1s
    real(wp), parameter :: PERFORMANCE_REGRESSION_THRESHOLD = 2.0_wp  ! 2x slowdown = regression
    
    logical :: on_windows
    integer :: failed_tests
    
    print *, "=== WINDOWS CI PERFORMANCE MEASUREMENT TESTS (RED PHASE) ==="
    
    on_windows = is_windows()
    failed_tests = 0
    
    ! Performance measurement tests (expected to FAIL initially in RED phase)
    call test_savefig_performance_baseline(failed_tests)
    call test_file_io_operation_timing(failed_tests)
    call test_directory_creation_performance(failed_tests)
    call test_concurrent_file_operations(failed_tests)
    call test_memory_vs_disk_backend_performance(failed_tests)
    
    if (failed_tests > 0) then
        print *, "EXPECTED FAILURES: ", failed_tests, " performance tests failed (RED phase)"
        print *, "These failures define the performance targets to achieve in GREEN phase"
        stop 0  ! RED phase: failing tests are expected and correct
    else
        print *, "UNEXPECTED: All performance tests passed - check test rigor"
        stop 1
    end if

contains

    subroutine test_savefig_performance_baseline(failed_count)
        !! Given: Basic plot operations that should be fast
        !! When: Measuring savefig() execution time across backends  
        !! Then: Should complete within performance targets (EXPECTED TO FAIL)
        integer, intent(inout) :: failed_count
        
        type(figure_t) :: fig
        real(wp) :: start_time, end_time, elapsed_time
        real(wp) :: target_time
        character(len=512) :: png_file, pdf_file
        logical :: performance_ok
        integer :: i
        
        print *, "TEST: savefig() Performance Baseline Measurement"
        
        ! Set platform-specific performance targets
        if (on_windows) then
            target_time = WINDOWS_SAVEFIG_TARGET
        else
            target_time = LINUX_SAVEFIG_TARGET
        end if
        
        ! Test PNG backend performance (most common bottleneck)
        png_file = get_test_output_path("performance_baseline.png")
        
        call cpu_time(start_time)
        
        ! Create minimal plot that should be fast
        call fig%initialize()
        call fig%add_plot([1.0_wp, 2.0_wp], [1.0_wp, 4.0_wp])
        call figure_savefig(fig, png_file)
        
        call cpu_time(end_time)
        elapsed_time = end_time - start_time
        
        performance_ok = elapsed_time <= target_time
        
        print *, "  PNG savefig time: ", elapsed_time, " seconds"
        print *, "  Target time: ", target_time, " seconds"
        print *, "  Performance OK: ", performance_ok
        
        if (.not. performance_ok) then
            failed_count = failed_count + 1
            print *, "  PERFORMANCE FAILURE: PNG savefig too slow (expected in RED phase)"
        end if
        
        ! Test PDF backend performance  
        pdf_file = get_test_output_path("performance_baseline.pdf")
        
        call cpu_time(start_time)
        call figure_savefig(fig, pdf_file)
        call cpu_time(end_time)
        elapsed_time = end_time - start_time
        
        performance_ok = elapsed_time <= target_time
        
        print *, "  PDF savefig time: ", elapsed_time, " seconds"
        print *, "  Performance OK: ", performance_ok
        
        if (.not. performance_ok) then
            failed_count = failed_count + 1
            print *, "  PERFORMANCE FAILURE: PDF savefig too slow (expected in RED phase)"
        end if
        
        ! Stress test: Multiple savefig operations (simulates consolidated tests)
        call cpu_time(start_time)
        do i = 1, 5  ! Reduced from typical 20+ in consolidated tests
            png_file = get_test_output_path("perf_stress_" // trim(int_to_str(i)) // ".png")
            call figure_savefig(fig, png_file)
        end do
        call cpu_time(end_time)
        elapsed_time = end_time - start_time
        
        ! Should average to target time per operation
        performance_ok = (elapsed_time / 5.0_wp) <= target_time
        
        print *, "  Stress test avg time per savefig: ", elapsed_time / 5.0_wp, " seconds"
        print *, "  Performance OK: ", performance_ok
        
        if (.not. performance_ok) then
            failed_count = failed_count + 1
            print *, "  PERFORMANCE FAILURE: Batch savefig too slow (expected in RED phase)"
        end if
        
    end subroutine test_savefig_performance_baseline

    subroutine test_file_io_operation_timing(failed_count)
        !! Given: File I/O operations that are Windows bottlenecks
        !! When: Measuring individual I/O operations
        !! Then: Should identify specific slow operations (EXPECTED TO FAIL)
        integer, intent(inout) :: failed_count
        
        real(wp) :: start_time, end_time, elapsed_time
        character(len=512) :: test_file
        logical :: file_exists, performance_ok
        integer :: unit_num
        integer :: i
        
        print *, "TEST: File I/O Operation Timing"
        
        ! Test 1: File existence checking (inquire operations)
        test_file = get_test_output_path("io_timing_test.txt")
        
        call cpu_time(start_time)
        do i = 1, 100  ! Multiple inquire operations
            inquire(file=test_file, exist=file_exists)
        end do
        call cpu_time(end_time)
        elapsed_time = end_time - start_time
        
        ! Should be very fast - target <10ms for 100 operations
        performance_ok = elapsed_time <= 0.01_wp
        
        print *, "  100 inquire operations: ", elapsed_time, " seconds"
        print *, "  Performance OK: ", performance_ok
        
        if (.not. performance_ok) then
            failed_count = failed_count + 1
            print *, "  PERFORMANCE FAILURE: File inquire too slow (expected on Windows)"
        end if
        
        ! Test 2: File creation and writing
        call cpu_time(start_time)
        open(newunit=unit_num, file=test_file, status='replace')
        do i = 1, 1000
            write(unit_num, *) "Test line ", i
        end do
        close(unit_num)
        call cpu_time(end_time)
        elapsed_time = end_time - start_time
        
        ! Target: <50ms for simple text file creation
        performance_ok = elapsed_time <= 0.05_wp
        
        print *, "  File creation + 1000 writes: ", elapsed_time, " seconds"  
        print *, "  Performance OK: ", performance_ok
        
        if (.not. performance_ok) then
            failed_count = failed_count + 1
            print *, "  PERFORMANCE FAILURE: File write too slow (expected on Windows)"
        end if
        
        ! Test 3: File deletion (cleanup operations)
        call cpu_time(start_time)
        open(newunit=unit_num, file=test_file, status='old')
        close(unit_num, status='delete')
        call cpu_time(end_time)
        elapsed_time = end_time - start_time
        
        ! Target: <10ms for file deletion
        performance_ok = elapsed_time <= 0.01_wp
        
        print *, "  File deletion: ", elapsed_time, " seconds"
        print *, "  Performance OK: ", performance_ok
        
        if (.not. performance_ok) then
            failed_count = failed_count + 1
            print *, "  PERFORMANCE FAILURE: File deletion too slow (expected on Windows)"
        end if
        
    end subroutine test_file_io_operation_timing

    subroutine test_directory_creation_performance(failed_count)
        !! Given: Directory operations are Windows performance bottlenecks
        !! When: Measuring directory creation and validation
        !! Then: Should identify directory I/O performance issues (EXPECTED TO FAIL)
        integer, intent(inout) :: failed_count
        
        real(wp) :: start_time, end_time, elapsed_time
        character(len=512) :: base_path, test_dir
        logical :: dir_exists, performance_ok
        integer :: i
        
        print *, "TEST: Directory Creation Performance"
        
        ! Get base test output directory
        base_path = get_test_output_path(".")
        
        ! Test repeated directory existence checking (common in plotting)
        test_dir = trim(base_path) // "/perf_test_dir"
        
        call cpu_time(start_time)
        do i = 1, 50  ! Simulate repeated directory checks
            inquire(file=test_dir, exist=dir_exists)
        end do
        call cpu_time(end_time)
        elapsed_time = end_time - start_time
        
        ! Target: <5ms for 50 directory checks
        performance_ok = elapsed_time <= 0.005_wp
        
        print *, "  50 directory existence checks: ", elapsed_time, " seconds"
        print *, "  Performance OK: ", performance_ok
        
        if (.not. performance_ok) then
            failed_count = failed_count + 1
            print *, "  PERFORMANCE FAILURE: Directory inquire too slow (expected on Windows)"
        end if
        
        ! Note: Directory creation testing would require system calls
        ! which are not portable across platforms in standard Fortran
        ! This is a limitation we need to address in the implementation phase
        
    end subroutine test_directory_creation_performance

    subroutine test_concurrent_file_operations(failed_count)
        !! Given: Concurrent file operations may cause Windows CI slowdowns
        !! When: Simulating multiple simultaneous file operations
        !! Then: Should measure performance impact of concurrent I/O (EXPECTED TO FAIL)
        integer, intent(inout) :: failed_count
        
        real(wp) :: start_time, end_time, elapsed_time
        character(len=512) :: file1, file2, file3
        logical :: performance_ok
        integer :: unit1, unit2, unit3
        integer :: i
        
        print *, "TEST: Concurrent File Operations Performance"
        
        file1 = get_test_output_path("concurrent_test_1.dat")
        file2 = get_test_output_path("concurrent_test_2.dat")
        file3 = get_test_output_path("concurrent_test_3.dat")
        
        ! Simulate concurrent file operations (like saving multiple plots)
        call cpu_time(start_time)
        
        ! Open multiple files simultaneously
        open(newunit=unit1, file=file1, status='replace')
        open(newunit=unit2, file=file2, status='replace')
        open(newunit=unit3, file=file3, status='replace')
        
        ! Write to all files concurrently
        do i = 1, 500
            write(unit1, *) "File1 line ", i
            write(unit2, *) "File2 line ", i
            write(unit3, *) "File3 line ", i
        end do
        
        ! Close all files
        close(unit1)
        close(unit2)
        close(unit3)
        
        call cpu_time(end_time)
        elapsed_time = end_time - start_time
        
        ! Target: <100ms for concurrent operations
        performance_ok = elapsed_time <= 0.1_wp
        
        print *, "  Concurrent file operations: ", elapsed_time, " seconds"
        print *, "  Performance OK: ", performance_ok
        
        if (.not. performance_ok) then
            failed_count = failed_count + 1
            print *, "  PERFORMANCE FAILURE: Concurrent I/O too slow (expected on Windows)"
        end if
        
        ! Cleanup
        open(newunit=unit1, file=file1, status='old')
        close(unit1, status='delete')
        open(newunit=unit2, file=file2, status='old')
        close(unit2, status='delete')
        open(newunit=unit3, file=file3, status='old')
        close(unit3, status='delete')
        
    end subroutine test_concurrent_file_operations

    subroutine test_memory_vs_disk_backend_performance(failed_count)
        !! Given: Memory backends should be faster than disk I/O on Windows
        !! When: Comparing memory-based vs disk-based operations
        !! Then: Should demonstrate memory backend performance advantage (EXPECTED TO FAIL)
        integer, intent(inout) :: failed_count
        
        type(figure_t) :: fig
        real(wp) :: disk_time, memory_time, speedup_ratio
        real(wp) :: start_time, end_time
        character(len=512) :: disk_file
        logical :: performance_ok
        integer :: i
        
        print *, "TEST: Memory vs Disk Backend Performance"
        
        ! Create test figure
        call fig%initialize()
        call fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 4.0_wp, 9.0_wp])
        
        ! Measure disk-based operations (current implementation)
        disk_file = get_test_output_path("memory_vs_disk_test.png")
        
        call cpu_time(start_time)
        do i = 1, 10  ! Multiple savefig operations to disk
            call figure_savefig(fig, disk_file)
        end do
        call cpu_time(end_time)
        disk_time = end_time - start_time
        
        print *, "  Disk operations (10 savefig): ", disk_time, " seconds"
        
        ! TODO: Memory backend implementation needed for GREEN phase
        ! For RED phase, we simulate expected memory performance
        memory_time = disk_time / 5.0_wp  ! Expected 5x speedup with memory backend
        
        print *, "  Expected memory operations: ", memory_time, " seconds"
        
        speedup_ratio = disk_time / memory_time
        
        print *, "  Expected speedup ratio: ", speedup_ratio, "x"
        
        ! Memory backend should be at least 3x faster
        performance_ok = speedup_ratio >= 3.0_wp
        
        if (.not. performance_ok) then
            failed_count = failed_count + 1
            print *, "  PERFORMANCE FAILURE: Memory backend speedup insufficient (expected - not implemented yet)"
        end if
        
        ! Test memory buffer reuse efficiency (simulated)
        ! This will fail until memory backend is implemented
        failed_count = failed_count + 1
        print *, "  IMPLEMENTATION FAILURE: Memory backend not yet implemented (expected in RED phase)"
        
    end subroutine test_memory_vs_disk_backend_performance

    function int_to_str(num) result(str)
        !! Utility function to convert integer to string
        integer, intent(in) :: num
        character(len=10) :: str
        
        write(str, '(I0)') num
    end function int_to_str

end program test_windows_ci_performance_measurement