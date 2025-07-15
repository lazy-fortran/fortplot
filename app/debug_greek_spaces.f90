program debug_greek_spaces
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(2), y(2)
    
    x = [0.0_wp, 1.0_wp]
    y = [0.0_wp, 1.0_wp]
    
    call fig%initialize(400, 300)
    
    ! Test Greek letters with spaces
    call fig%add_plot(x, y, label="alpha beta")
    call fig%add_plot(x, y*2, label="alpha  beta")
    call fig%add_plot(x, y*3, label="before alpha after")
    
    call fig%savefig("debug_greek_spaces.pdf")
    
    print *, "Greek spaces test completed"
    
end program debug_greek_spaces