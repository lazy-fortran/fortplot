program debug_png_error
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3) = [1.0_wp, 2.0_wp, 3.0_wp]
    real(wp) :: y(3) = [1.0_wp, 4.0_wp, 2.0_wp]
    
    print *, "Testing PNG generation that might cause 'bad adaptive filter value' error"
    
    call fig%initialize(640, 480)
    call fig%add_plot(x, y, label="test")
    call fig%set_title("Debug PNG Test")
    call fig%savefig('debug_test.png')
    
    print *, "PNG saved successfully - no error occurred"
    
end program debug_png_error