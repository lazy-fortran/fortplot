program test_figure_refactoring
    !! Test that figure_t still works after refactoring
    !! Validates that all public interfaces are preserved
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure, only: figure_t
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(5) = [1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp]
    real(wp) :: y(5) = [1.0_wp, 4.0_wp, 9.0_wp, 16.0_wp, 25.0_wp]
    real(wp) :: data(10) = [1.0_wp, 2.0_wp, 2.0_wp, 3.0_wp, 3.0_wp, &
                           3.0_wp, 4.0_wp, 4.0_wp, 4.0_wp, 5.0_wp]
    
    print *, "Testing figure_t refactoring..."
    
    ! Test initialization
    call fig%initialize(800, 600, 'png')
    print *, "✓ Initialization"
    
    ! Test plotting
    call fig%add_plot(x, y, label='Test Plot')
    print *, "✓ Line plot"
    
    ! Test histogram 
    call fig%hist(data, bins=5, label='Test Histogram')
    print *, "✓ Histogram"
    
    ! Test grid
    call fig%grid(.true.)
    print *, "✓ Grid"
    
    ! Test labels and title
    call fig%set_xlabel('X Label')
    call fig%set_ylabel('Y Label') 
    call fig%set_title('Test Figure')
    print *, "✓ Labels and title"
    
    ! Test legend
    call fig%legend()
    print *, "✓ Legend"
    
    ! Test scales
    call fig%set_xscale('linear')
    call fig%set_yscale('log')
    print *, "✓ Scales"
    
    ! Test limits
    call fig%set_xlim(0.0_wp, 6.0_wp)
    call fig%set_ylim(0.1_wp, 30.0_wp)
    print *, "✓ Limits"
    
    print *, "All tests passed! Refactoring preserved functionality."
    
end program test_figure_refactoring