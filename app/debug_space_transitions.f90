program debug_space_transitions
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3), y(3)
    
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 0.0_wp]
    
    call fig%initialize(600, 400)
    call fig%add_plot(x, y, label="test")
    
    ! Test very specific space transition cases
    call fig%set_xlabel("A alpha B")    ! Space before and after Greek letter
    call fig%set_ylabel("C  beta  D")   ! Double spaces around Greek letter
    call fig%set_title("E gamma F")     ! Simple case
    
    ! Test different combinations
    call fig%add_plot(x, y*2, label="X alpha")      ! Space before Greek
    call fig%add_plot(x, y*3, label="beta Y")       ! Space after Greek
    call fig%add_plot(x, y*4, label="alpha beta")   ! Space between Greek letters
    call fig%add_plot(x, y*5, label="a alpha b")    ! Mixed ASCII and Greek
    
    call fig%legend("upper right")
    
    call fig%savefig("debug_space_transitions.pdf")
    
    print *, "Testing space transitions around Greek letters"
    print *, "Check debug_space_transitions.pdf for missing spaces"
    
end program debug_space_transitions