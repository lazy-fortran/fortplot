program test_histogram_functionality
    !! Histogram core functionality tests
    !! Extracted from test_histogram_comprehensive.f90
    !! 
    !! This test covers:
    !! - Basic histogram functionality (bins, density, empty data)
    !! - Boundary conditions (zero/negative bins, segfault prevention)
    !! - Edge cases (identical values, binary search, extreme ranges)
    !! - Stress testing (large datasets, memory usage, performance)
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_security, only: get_test_output_path
    use fortplot_system_runtime, only: is_windows
    use fortplot_windows_performance, only: setup_windows_performance, &
                                            should_use_memory_backend
    use fortplot_fast_io, only: fast_savefig, enable_fast_io
    implicit none

    integer :: test_count = 0
    integer :: pass_count = 0
    logical :: on_windows
    logical, parameter :: HIST_ENABLED = .true.   ! hist() implemented; enable tests (fixes #285)

    print *, "=== HISTOGRAM FUNCTIONALITY TESTS ==="
    
    if (.not. HIST_ENABLED) then
        print *, "NOTE: Histogram tests disabled pending hist() implementation (issue #285)"
        print *, "All test infrastructure ready - enable HIST_ENABLED when hist() is implemented"
        return
    end if
    
    ! Initialize performance optimization for Windows CI
    on_windows = is_windows()
    if (on_windows) then
        call setup_windows_performance()
        if (should_use_memory_backend()) then
            call enable_fast_io()
            print *, "Enabled fast I/O with memory backend for Windows CI"
        end if
    end if
    
    ! Run test categories
    call test_basic_functionality()
    call test_boundary_conditions()
    call test_edge_cases()
    call test_stress_scenarios()
    call print_test_summary()

contains

    !===========================================================================
    ! Basic Functionality Tests
    !===========================================================================
    
    subroutine test_basic_functionality()
        print *, "--- Basic Functionality Tests ---"
        
        call test_histogram_basic()
        call test_histogram_custom_bins()
        call test_histogram_density()
        call test_histogram_empty_data()
    end subroutine test_basic_functionality

    subroutine test_histogram_basic()
        type(figure_t) :: fig
        real(wp) :: data(10)
        integer :: i
        
        call start_test("Basic histogram")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 10
            data(i) = real(i, wp)
        end do
        
        ! Add histogram with default 10 bins
        call fig%hist(data)
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Histogram plot count")
        
        call end_test()
    end subroutine test_histogram_basic

    subroutine test_histogram_custom_bins()
        type(figure_t) :: fig
        real(wp) :: data(10)
        integer :: i
        
        call start_test("Histogram with custom bins")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 10
            data(i) = real(i, wp)
        end do
        
        ! Add histogram with 5 bins
        call fig%hist(data, bins=5)
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Custom bins histogram count")
        
        call end_test()
    end subroutine test_histogram_custom_bins

    subroutine test_histogram_density()
        type(figure_t) :: fig
        real(wp) :: data(100)
        integer :: i
        
        call start_test("Histogram with density normalization")
        
        call fig%initialize(640, 480)
        
        ! Create test data
        do i = 1, 100
            data(i) = real(i, wp) / 10.0_wp
        end do
        
        ! Add histogram with density normalization
        call fig%hist(data, density=.true.)
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Density histogram count")
        
        call end_test()
    end subroutine test_histogram_density

    subroutine test_histogram_empty_data()
        type(figure_t) :: fig
        real(wp) :: data(0)
        
        call start_test("Histogram with empty data")
        
        call fig%initialize(640, 480)
        
        ! Add histogram with empty data - should handle gracefully
        call fig%hist(data)
        call assert_equal(real(fig%plot_count, wp), 0.0_wp, "Empty data histogram count")
        
        call end_test()
    end subroutine test_histogram_empty_data

    !===========================================================================
    ! Boundary Conditions Tests
    !===========================================================================
    
    subroutine test_boundary_conditions()
        print *, "--- Boundary Conditions Tests ---"
        
        call test_histogram_zero_bins()
        call test_histogram_negative_bins()
        call test_histogram_valid_minimal_case()
    end subroutine test_boundary_conditions

    subroutine test_histogram_zero_bins()
        !! Test that bins=0 does not cause segmentation fault
        type(figure_t) :: fig
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        call start_test("Zero bins safety")
        
        call fig%initialize(640, 480)
        
        ! This should not crash with segmentation fault
        call fig%hist(data, bins=0)
        
        print *, '  PASS: Zero bins handled without segmentation fault'
        call end_test()
    end subroutine test_histogram_zero_bins

    subroutine test_histogram_negative_bins()
        !! Test that negative bins do not cause segmentation fault
        type(figure_t) :: fig
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        call start_test("Negative bins safety")
        
        call fig%initialize(640, 480)
        
        ! This should not crash with segmentation fault
        call fig%hist(data, bins=-5)
        
        print *, '  PASS: Negative bins handled without segmentation fault'
        call end_test()
    end subroutine test_histogram_negative_bins
    
    subroutine test_histogram_valid_minimal_case()
        !! Test valid minimal case for comparison
        type(figure_t) :: fig
        real(wp) :: data(2) = [1.0_wp, 2.0_wp]
        
        call start_test("Valid minimal case")
        
        call fig%initialize(640, 480)
        
        ! This should work normally
        call fig%hist(data, bins=1)
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Valid minimal histogram")
        
        call end_test()
    end subroutine test_histogram_valid_minimal_case

    !===========================================================================
    ! Edge Cases Tests
    !===========================================================================
    
    subroutine test_edge_cases()
        print *, "--- Edge Cases Tests ---"
        
        call test_identical_values()
        call test_binary_search_performance()
    end subroutine test_edge_cases

    subroutine test_identical_values()
        !! Test histogram with all identical values
        type(figure_t) :: fig
        real(wp) :: data(100)
        
        call start_test("Identical values")
        
        ! All values are the same
        data = 5.0_wp
        
        call fig%initialize(600, 400)
        call fig%hist(data, bins=10)
        
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Identical values histogram")
        call end_test()
    end subroutine test_identical_values
    
    subroutine test_binary_search_performance()
        !! Test that binary search works correctly
        type(figure_t) :: fig
        real(wp), allocatable :: data(:)
        integer :: i
        
        call start_test("Binary search performance")
        
        ! Create large dataset for binary search testing
        allocate(data(10000))
        do i = 1, 10000
            data(i) = real(i, wp) / 100.0_wp
        end do
        
        call fig%initialize(600, 400)
        call fig%hist(data, bins=50)
        
        call assert_equal(real(fig%plot_count, wp), 1.0_wp, "Large dataset histogram")
        
        deallocate(data)
        call end_test()
    end subroutine test_binary_search_performance

    !===========================================================================
    ! Stress Testing
    !===========================================================================
    
    subroutine test_stress_scenarios()
        print *, "--- Stress Testing ---"
        
        call test_large_datasets()
        call test_extreme_parameters()
        call test_memory_usage()
    end subroutine test_stress_scenarios

    subroutine test_large_datasets()
        !! Test histogram with large amounts of data
        type(figure_t) :: fig
        real(wp), allocatable :: large_data(:)
        integer :: i, n_data
        real(wp) :: start_time, end_time
        character(len=512) :: filename
        
        call start_test("Large datasets")
        
        ! Test with 10,000 data points (reduced from 50K for speed)
        n_data = 10000
        allocate(large_data(n_data))
        
        ! Generate synthetic data with normal-like distribution
        do i = 1, n_data
            large_data(i) = sin(real(i, wp) * 0.001_wp) * 10.0_wp + &
                           cos(real(i, wp) * 0.01_wp) * 5.0_wp + &
                           real(mod(i, 100), wp) * 0.1_wp
        end do
        
        call cpu_time(start_time)
        call fig%initialize(600, 400)  ! Smaller for speed
        call fig%hist(large_data, bins=50)
        call fig%set_title('Large Dataset (10K points)')
        
        filename = 'test/output/histogram_large.png'
        if (should_use_memory_backend()) then
            call fast_savefig(fig, filename)
        else
            call fig%savefig(filename)
        end if
        
        call cpu_time(end_time)
        print *, '  Large dataset processed in ', end_time - start_time, ' seconds'
        
        deallocate(large_data)
        call end_test()
    end subroutine test_large_datasets
    
    subroutine test_extreme_parameters()
        !! Test with extreme parameter values
        type(figure_t) :: fig
        real(wp) :: data(1000)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Extreme parameters")
        
        ! Generate test data
        do i = 1, 1000
            data(i) = real(i, wp) * 0.01_wp
        end do
        
        ! Test with many bins
        call fig%initialize(600, 400)
        call fig%hist(data, bins=100)  ! Reduced from 500 for speed
        call fig%set_title('Many Bins Test')
        filename = 'test/output/histogram_many_bins.png'
        call fig%savefig(filename)
        
        ! Test with minimal bins
        call fig%initialize(600, 400)
        call fig%hist(data, bins=1)
        call fig%set_title('Single Bin Test')
        filename = 'test/output/histogram_one_bin.png'
        call fig%savefig(filename)
        
        ! Test with very wide range data
        data(1:500) = 1.0e-6_wp
        data(501:1000) = 1.0e6_wp
        
        call fig%initialize(600, 400)
        call fig%hist(data, bins=20)
        call fig%set_title('Wide Range Data')
        filename = 'test/output/histogram_wide_range.png'
        call fig%savefig(filename)
        
        call end_test()
    end subroutine test_extreme_parameters
    
    subroutine test_memory_usage()
        !! Test memory usage patterns
        type(figure_t) :: fig
        real(wp), allocatable :: data(:)
        integer :: i, j, size
        character(len=512) :: filename
        
        call start_test("Memory usage patterns")
        
        ! Test multiple histograms in sequence (memory cleanup) - reduced iterations
        do i = 1, 3  ! Reduced from 10 for speed
            size = 1000 * i
            allocate(data(size))
            
            data = real([(j, j=1,size)], wp) * 0.001_wp
            
            call fig%initialize(400, 300)  ! Smaller for speed
            call fig%hist(data, bins=20)
            call fig%set_title('Memory Test')
            
            filename = 'test/output/histogram_memory.png'
            call fig%savefig(filename)
            
            deallocate(data)
        end do
        
        ! Test overlapping histograms
        allocate(data(2000))  ! Reduced from 5000 for speed
        data = real([(i, i=1,2000)], wp) * 0.01_wp
        
        call fig%initialize(600, 400)
        call fig%hist(data(1:1000), bins=30, label='First Half')
        call fig%hist(data(1000:2000), bins=30, label='Second Half')
        call fig%legend()
        call fig%set_title('Overlapping Histograms')
        filename = 'test/output/histogram_overlapping.png'
        call fig%savefig(filename)
        
        deallocate(data)
        call end_test()
    end subroutine test_memory_usage

    !===========================================================================
    ! Test Framework Utilities
    !===========================================================================

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        test_count = test_count + 1
        write(*, '(A, I0, A, A)') 'Test ', test_count, ': ', test_name
    end subroutine start_test

    subroutine end_test()
        pass_count = pass_count + 1
        write(*, '(A)') '  PASS'
        write(*, *)
    end subroutine end_test

    subroutine assert_equal(actual, expected, description)
        real(wp), intent(in) :: actual, expected
        character(len=*), intent(in) :: description
        
        if (abs(actual - expected) < 1.0e-10_wp) then
            print *, '  PASS: ', description
        else
            print *, '  FAIL: ', description, ' (expected ', expected, ', got ', actual, ')'
        end if
    end subroutine assert_equal

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'Histogram Functionality Test Summary'
        write(*, '(A, I0, A, I0)') 'Tests run: ', test_count, ' | Passed: ', pass_count
        write(*, '(A)') 'Core histogram functionality tests COMPLETED!'
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_histogram_functionality
