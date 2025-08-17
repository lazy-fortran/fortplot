program test_histogram_uat
    !! User Acceptance Test for histogram functionality
    !! Tests both hist() and histogram() APIs with various scenarios
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    write(*,*) '=== HISTOGRAM USER ACCEPTANCE TESTING ==='
    
    ! Test 1: Basic API functionality
    call test_basic_api()
    
    ! Test 2: Edge cases
    call test_edge_cases()
    
    ! Test 3: Error handling
    call test_error_handling()
    
    ! Test 4: Backend compatibility
    call test_backend_compatibility()
    
    write(*,*) '=== ALL TESTS COMPLETED ==='
    
contains

    subroutine test_basic_api()
        !! Test basic API functionality for both hist() and histogram()
        type(figure_t) :: fig
        real(wp) :: data(100)
        integer :: i
        
        write(*,*) 'Testing basic API functionality...'
        
        ! Generate test data
        do i = 1, 100
            data(i) = real(i, wp) * 0.1_wp
        end do
        
        ! Test hist() function
        call fig%initialize(600, 400)
        call fig%hist(data)
        call fig%set_title('Basic hist() API Test')
        call fig%savefig('build/uat_hist_basic.png')
        write(*,*) '  ✓ hist() function works'
        
        ! Test histogram() function (module-level function)
        call figure(600, 400)
        call histogram(data)
        call title('Basic histogram() API Test') 
        call savefig('build/uat_histogram_basic.png')
        write(*,*) '  ✓ histogram() function works'
        
        ! Test with optional parameters
        call fig%initialize(600, 400)
        call fig%hist(data, bins=15, density=.true., label='Test Data')
        call fig%legend()
        call fig%set_title('hist() with Optional Parameters')
        call fig%savefig('build/uat_hist_options.png')
        write(*,*) '  ✓ Optional parameters work'
        
    end subroutine test_basic_api
    
    subroutine test_edge_cases()
        !! Test edge cases and boundary conditions
        type(figure_t) :: fig
        real(wp) :: empty_data(0)
        real(wp) :: single_data(1) = [5.0_wp]
        real(wp) :: identical_data(10) = [2.5_wp, 2.5_wp, 2.5_wp, 2.5_wp, 2.5_wp, &
                                          2.5_wp, 2.5_wp, 2.5_wp, 2.5_wp, 2.5_wp]
        real(wp) :: extreme_data(5) = [1.0e-10_wp, 1.0e10_wp, -1.0e10_wp, 0.0_wp, 1.0_wp]
        
        write(*,*) 'Testing edge cases...'
        
        ! Test single value
        call fig%initialize(600, 400)
        call fig%hist(single_data)
        call fig%set_title('Single Value Histogram')
        call fig%savefig('build/uat_single_value.png')
        write(*,*) '  ✓ Single value handled'
        
        ! Test identical values
        call fig%initialize(600, 400)
        call fig%hist(identical_data)
        call fig%set_title('Identical Values Histogram')
        call fig%savefig('build/uat_identical_values.png')
        write(*,*) '  ✓ Identical values handled'
        
        ! Test extreme values
        call fig%initialize(600, 400)
        call fig%hist(extreme_data)
        call fig%set_title('Extreme Values Histogram')
        call fig%savefig('build/uat_extreme_values.png')
        write(*,*) '  ✓ Extreme values handled'
        
        ! Test very small bins
        call fig%initialize(600, 400)
        call fig%hist(extreme_data, bins=2)
        call fig%set_title('Very Few Bins')
        call fig%savefig('build/uat_few_bins.png')
        write(*,*) '  ✓ Very few bins handled'
        
    end subroutine test_edge_cases
    
    subroutine test_error_handling()
        !! Test error handling and user-friendly messages
        type(figure_t) :: fig
        real(wp) :: data(10) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, &
                                6.0_wp, 7.0_wp, 8.0_wp, 9.0_wp, 10.0_wp]
        real(wp) :: empty_data(0)
        
        write(*,*) 'Testing error handling...'
        
        ! Test empty data
        call fig%initialize(600, 400)
        call fig%hist(empty_data)
        call fig%set_title('Empty Data Test')
        call fig%savefig('build/uat_empty_data.png')
        write(*,*) '  ✓ Empty data handled gracefully'
        
        ! Test invalid bins (negative)
        call fig%initialize(600, 400)
        call fig%hist(data, bins=-5)
        call fig%set_title('Invalid Bins Test')
        call fig%savefig('build/uat_invalid_bins.png')
        write(*,*) '  ✓ Invalid bins handled gracefully'
        
        ! Test zero bins
        call fig%initialize(600, 400)
        call fig%hist(data, bins=0)
        call fig%set_title('Zero Bins Test')
        call fig%savefig('build/uat_zero_bins.png')
        write(*,*) '  ✓ Zero bins handled gracefully'
        
    end subroutine test_error_handling
    
    subroutine test_backend_compatibility()
        !! Test histogram across different backends
        type(figure_t) :: fig
        real(wp) :: data(50)
        integer :: i
        
        write(*,*) 'Testing backend compatibility...'
        
        ! Generate test data
        do i = 1, 50
            data(i) = sin(real(i, wp) * 0.2_wp) * 10.0_wp
        end do
        
        ! Test PNG backend
        call fig%initialize(600, 400)
        call fig%hist(data, bins=10)
        call fig%set_title('PNG Backend Test')
        call fig%savefig('build/uat_backend_png.png')
        write(*,*) '  ✓ PNG backend works'
        
        ! Test PDF backend
        call fig%initialize(600, 400)
        call fig%hist(data, bins=10)
        call fig%set_title('PDF Backend Test')
        call fig%savefig('build/uat_backend_pdf.pdf')
        write(*,*) '  ✓ PDF backend works'
        
        ! Test ASCII backend
        call fig%initialize(80, 24)
        call fig%hist(data, bins=10)
        call fig%set_title('ASCII Backend Test')
        call fig%savefig('build/uat_backend_ascii.txt')
        write(*,*) '  ✓ ASCII backend works'
        
    end subroutine test_backend_compatibility

end program test_histogram_uat