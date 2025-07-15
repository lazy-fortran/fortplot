program debug_spaces2
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(2), y(2)
    
    x = [0.0_wp, 1.0_wp]
    y = [0.0_wp, 1.0_wp]
    
    call fig%initialize(400, 300)
    call fig%add_plot(x, y, label="before omega after")
    call fig%savefig("debug_spaces2.pdf")
    
    print *, "Space test completed"
    
end program debug_spaces2