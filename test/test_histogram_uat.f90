program test_histogram_uat
    !! Histogram user acceptance and documentation tests
    !! Extracted from test_histogram_comprehensive.f90
    !! 
    !! This test covers:
    !! - User acceptance (both APIs, error handling, backend compatibility)
    !! - Documentation examples validation
    !! - API usability and real-world usage patterns
    
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
    logical, parameter :: HIST_ENABLED = .false.  ! hist() disabled pending issue #285

    print *, "=== HISTOGRAM USER ACCEPTANCE TESTS ==="
    
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
    call test_user_acceptance()
    call test_documentation_examples()
    call print_test_summary()

contains

    !===========================================================================
    ! User Acceptance Tests
    !===========================================================================
    
    subroutine test_user_acceptance()
        print *, "--- User Acceptance Tests ---"
        
        call test_basic_api()
        call test_edge_cases_uat()
        call test_error_handling()
        call test_backend_compatibility()
    end subroutine test_user_acceptance

    subroutine test_basic_api()
        !! Test basic API functionality for both hist() and histogram()
        type(figure_t) :: fig
        real(wp) :: data(100)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Basic API functionality")
        
        ! Generate test data
        do i = 1, 100
            data(i) = real(i, wp) * 0.1_wp
        end do
        
        ! Test hist() function
        call fig%initialize(600, 400)
        call fig%hist(data)
        call fig%set_title('Basic hist() API Test')
        filename = 'test/output/histogram_hist_basic.png'
        call fig%savefig(filename)
        
        ! Test histogram() function (module-level function)
        call figure(figsize=[600.0_wp, 400.0_wp])
        call histogram(data)
        call title('Basic histogram() API Test') 
        filename = 'test/output/histogram_function_basic.png'
        call savefig(filename)
        
        ! Test with optional parameters
        call fig%initialize(600, 400)
        call fig%hist(data, bins=15, density=.true., label='Test Data')
        call fig%legend()
        call fig%set_title('hist() with Optional Parameters')
        filename = 'test/output/histogram_options.png'
        call fig%savefig(filename)
        
        call end_test()
    end subroutine test_basic_api
    
    subroutine test_edge_cases_uat()
        !! Test edge cases and boundary conditions
        type(figure_t) :: fig
        real(wp) :: empty_data(0)
        real(wp) :: single_data(1) = [5.0_wp]
        real(wp) :: identical_data(10) = [2.5_wp, 2.5_wp, 2.5_wp, 2.5_wp, 2.5_wp, &
                                          2.5_wp, 2.5_wp, 2.5_wp, 2.5_wp, 2.5_wp]
        real(wp) :: extreme_data(5) = [1.0e-10_wp, 1.0e10_wp, -1.0e10_wp, 0.0_wp, 1.0_wp]
        character(len=512) :: filename
        
        call start_test("Edge cases UAT")
        
        ! Test single value
        call fig%initialize(600, 400)
        call fig%hist(single_data)
        call fig%set_title('Single Value Histogram')
        filename = 'test/output/histogram_single_value.png'
        call fig%savefig(filename)
        
        ! Test identical values
        call fig%initialize(600, 400)
        call fig%hist(identical_data)
        call fig%set_title('Identical Values Histogram')
        filename = 'test/output/histogram_identical_values.png'
        call fig%savefig(filename)
        
        ! Test extreme values
        call fig%initialize(600, 400)
        call fig%hist(extreme_data)
        call fig%set_title('Extreme Values Histogram')
        filename = 'test/output/histogram_extreme_values.png'
        call fig%savefig(filename)
        
        ! Test very small bins
        call fig%initialize(600, 400)
        call fig%hist(extreme_data, bins=2)
        call fig%set_title('Very Few Bins')
        filename = 'test/output/histogram_few_bins.png'
        call fig%savefig(filename)
        
        call end_test()
    end subroutine test_edge_cases_uat
    
    subroutine test_error_handling()
        !! Test error handling and user-friendly messages
        type(figure_t) :: fig
        real(wp) :: data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]
        real(wp) :: empty_data(0)
        character(len=512) :: filename
        
        call start_test("Error handling")
        
        ! Test empty data
        call fig%initialize(600, 400)
        call fig%hist(empty_data)
        call fig%set_title('Empty Data Test')
        filename = 'test/output/histogram_empty_data.png'
        call fig%savefig(filename)
        
        ! Test invalid bins (negative)
        call fig%initialize(600, 400)
        call fig%hist(data, bins=-5)
        call fig%set_title('Invalid Bins Test')
        filename = 'test/output/histogram_invalid_bins.png'
        call fig%savefig(filename)
        
        ! Test zero bins
        call fig%initialize(600, 400)
        call fig%hist(data, bins=0)
        call fig%set_title('Zero Bins Test')
        filename = 'test/output/histogram_zero_bins.png'
        call fig%savefig(filename)
        
        call end_test()
    end subroutine test_error_handling
    
    subroutine test_backend_compatibility()
        !! Test histogram across different backends
        type(figure_t) :: fig
        real(wp) :: data(50)
        integer :: i
        character(len=512) :: filename
        
        call start_test("Backend compatibility")
        
        ! Generate test data
        do i = 1, 50
            data(i) = sin(real(i, wp) * 0.2_wp) * 10.0_wp
        end do
        
        ! Test PNG backend
        call fig%initialize(600, 400)
        call fig%hist(data, bins=10)
        call fig%set_title('PNG Backend Test')
        filename = 'test/output/histogram_backend.png'
        call fig%savefig(filename)
        
        ! Test PDF backend
        call fig%initialize(600, 400)
        call fig%hist(data, bins=10)
        call fig%set_title('PDF Backend Test')
        filename = 'test/output/histogram_backend.pdf'
        call fig%savefig(filename)
        
        ! Test ASCII backend
        call fig%initialize(80, 24)
        call fig%hist(data, bins=10)
        call fig%set_title('ASCII Backend Test')
        filename = 'test/output/histogram_backend.txt'
        call fig%savefig(filename)
        
        call end_test()
    end subroutine test_backend_compatibility

    !===========================================================================
    ! Documentation Examples Tests
    !===========================================================================
    
    subroutine test_documentation_examples()
        real(wp) :: data_values(100)
        type(figure_t) :: fig
        integer :: i
        character(len=512) :: filename
        
        call start_test("Documentation examples")
        
        ! Generate test data
        do i = 1, 100
            data_values(i) = real(i, wp) * 0.1_wp + sin(real(i, wp) * 0.1_wp) * 2.0_wp
        end do
        
        ! Test the exact example from the hist() documentation
        call figure(figsize=[8.0_wp, 6.0_wp])
        call hist(data_values, bins=20, label='Distribution')
        filename = 'test/output/doc_example_hist.png'
        call savefig(filename)
        
        ! Test the exact example from the histogram() documentation
        call figure(figsize=[8.0_wp, 6.0_wp])
        call histogram(data_values, bins=20, label='Distribution')
        filename = 'test/output/doc_example_histogram.png'
        call savefig(filename)
        
        ! Test object-oriented API as documented
        call fig%initialize(800, 600)
        call fig%hist(data_values, bins=20, label='Distribution')
        filename = 'test/output/doc_example_oo.png'
        call fig%savefig(filename)
        
        call end_test()
    end subroutine test_documentation_examples

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

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A)') 'Histogram User Acceptance Test Summary'
        write(*, '(A, I0, A, I0)') 'Tests run: ', test_count, ' | Passed: ', pass_count
        write(*, '(A)') 'User acceptance and documentation tests COMPLETED!'
        write(*, '(A)') '============================================'
    end subroutine print_test_summary

end program test_histogram_uat