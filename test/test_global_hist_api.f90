program test_global_hist_api
    !! Test global histogram API functions with boundary conditions
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    call test_global_hist_without_init()
    call test_global_hist_zero_bins()
    call test_global_histogram_negative_bins()
    call test_global_hist_valid()
    
    print *, 'All global histogram API tests passed!'
    
contains

    subroutine test_global_hist_without_init()
        !! Test global hist() function without explicit figure initialization
        !! This should work like matplotlib.pyplot.hist() - auto-initialize
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        ! Should auto-initialize and not crash (matplotlib compatibility)
        call hist(data, bins=5, label='Auto Init Test')
        
        print *, 'PASS: Global hist() auto-initializes figure like matplotlib'
    end subroutine test_global_hist_without_init

    subroutine test_global_hist_zero_bins()
        !! Test global hist() function with zero bins
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        call figure(640, 480)
        
        ! Should not crash with zero bins
        call hist(data, bins=0)
        
        print *, 'PASS: Global hist() with zero bins handled safely'
    end subroutine test_global_hist_zero_bins

    subroutine test_global_histogram_negative_bins()
        !! Test global histogram() function with negative bins
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        call figure(640, 480)
        
        ! Should not crash with negative bins
        call histogram(data, bins=-10)
        
        print *, 'PASS: Global histogram() with negative bins handled safely'
    end subroutine test_global_histogram_negative_bins
    
    subroutine test_global_hist_valid()
        !! Test global hist() function with valid parameters
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        call figure(640, 480)
        
        ! Should work correctly with valid bins
        call hist(data, bins=5)
        
        print *, 'PASS: Global hist() with valid bins works correctly'
    end subroutine test_global_hist_valid

end program test_global_hist_api