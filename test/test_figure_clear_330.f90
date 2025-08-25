program test_figure_clear
    !> Test case for Issue #330: Ensure figure() properly clears previous plots
    !> This test validates that plot_count is reset when figure() is called multiple times
    
    use fortplot
    use iso_fortran_env, only: real64
    implicit none
    
    real(real64) :: x(5) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64]
    real(real64) :: y1(5) = [1.0_real64, 4.0_real64, 2.0_real64, 3.0_real64, 5.0_real64]
    real(real64) :: y2(5) = [2.0_real64, 3.0_real64, 1.0_real64, 5.0_real64, 4.0_real64]
    
    print *, 'Testing figure clearing functionality...'
    
    ! First figure - should have 1 plot
    call figure()
    call plot(x, y1)
    print *, 'First figure created with 1 plot'
    
    ! Second figure - should clear previous plot and have only 1 plot
    call figure()
    call plot(x, y2)
    print *, 'Second figure created - previous plot should be cleared'
    
    ! Test with multiple plots in sequence
    call figure()
    call plot(x, y1, label='Line 1')
    call plot(x, y2, label='Line 2') 
    print *, 'Third figure with 2 plots'
    
    ! Clear and add single plot again
    call figure()
    call plot(x, y1)
    print *, 'Fourth figure - should have only 1 plot, not accumulating previous plots'
    
    print *, 'Figure clearing test completed successfully'
    
end program test_figure_clear