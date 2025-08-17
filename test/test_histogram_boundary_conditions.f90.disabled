program test_histogram_boundary_conditions
    !! Test boundary conditions for histogram function to prevent segmentation faults
    !! 
    !! Tests critical safety issues with invalid bins parameter:
    !! - bins = 0 should not cause segmentation fault
    !! - negative bins should not cause segmentation fault
    !! - System should handle invalid input gracefully
    
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    call test_histogram_zero_bins()
    call test_histogram_negative_bins()
    call test_histogram_valid_minimal_case()
    
    print *, 'All histogram boundary condition tests passed!'
    
contains

    subroutine test_histogram_zero_bins()
        !! Test that bins=0 does not cause segmentation fault
        type(figure_t) :: fig
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        call fig%initialize(640, 480)
        
        ! This should not crash with segmentation fault
        ! Should handle gracefully and return without error
        call fig%hist(data, bins=0)
        
        print *, 'PASS: Zero bins handled without segmentation fault'
    end subroutine test_histogram_zero_bins

    subroutine test_histogram_negative_bins()
        !! Test that negative bins do not cause segmentation fault
        type(figure_t) :: fig
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        call fig%initialize(640, 480)
        
        ! This should not crash with segmentation fault
        ! Should handle gracefully and return without error
        call fig%hist(data, bins=-5)
        
        print *, 'PASS: Negative bins handled without segmentation fault'
    end subroutine test_histogram_negative_bins
    
    subroutine test_histogram_valid_minimal_case()
        !! Test valid minimal case for comparison
        type(figure_t) :: fig
        real(wp) :: data(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
        
        call fig%initialize(640, 480)
        
        ! This should work properly
        call fig%hist(data, bins=1)
        
        ! Verify plot was added successfully
        if (fig%plot_count == 0) then
            error stop 'FAIL: Valid histogram was not added to figure'
        end if
        
        print *, 'PASS: Valid minimal histogram case works correctly'
    end subroutine test_histogram_valid_minimal_case

end program test_histogram_boundary_conditions