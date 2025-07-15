program debug_space_detailed
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(2), y(2)
    
    x = [0.0_wp, 1.0_wp]
    y = [0.0_wp, 1.0_wp]
    
    call fig%initialize(400, 300)
    
    ! Test specific space cases
    call fig%add_plot(x, y, label="a b")      ! Simple space
    call fig%add_plot(x, y*2, label="a  b")   ! Double space
    call fig%add_plot(x, y*3, label="a   b")  ! Triple space
    
    call fig%savefig("debug_space_detailed.pdf")
    
    print *, "Detailed space test completed"
    
end program debug_space_detailed