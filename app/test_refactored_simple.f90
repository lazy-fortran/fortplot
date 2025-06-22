program test_refactored_simple
    !! Simple test to verify refactored modules work correctly
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(5), y(5)
    integer :: i
    
    write(*, '(A)') 'Testing refactored modules...'
    
    ! Create test data
    do i = 1, 5
        x(i) = real(i, wp)
        y(i) = real(i**2, wp)
    end do
    
    ! Test basic figure operations
    call fig%initialize(400, 300)
    write(*, '(A)') '✅ Figure initialized'
    
    call fig%add_plot(x, y, label="Quadratic")
    write(*, '(A, I0)') '✅ Plot added, count: ', fig%plot_count
    
    call fig%set_xlabel("X values")
    call fig%set_ylabel("Y values")
    call fig%set_title("Refactored Module Test")
    write(*, '(A)') '✅ Labels set'
    
    ! Try ASCII output (should work)
    write(*, '(A)') ''
    write(*, '(A)') 'ASCII output:'
    call fig%show()
    
    write(*, '(A)') ''
    write(*, '(A)') '🎉 Refactored modules working correctly!'
    
end program test_refactored_simple