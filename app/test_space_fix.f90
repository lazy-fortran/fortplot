program test_space_fix
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3), y(3)
    
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 0.0_wp]
    
    call fig%initialize(600, 400)
    call fig%add_plot(x, y, label="test")
    
    ! Test the exact cases that should now work
    call fig%set_xlabel("Before alpha after")
    call fig%set_ylabel("Before beta after")
    call fig%set_title("Before gamma after")
    
    ! Test legend labels too
    call fig%add_plot(x, y*2, label="A alpha B")
    call fig%add_plot(x, y*3, label="X  beta  Y")
    call fig%add_plot(x, y*4, label="P gamma Q")
    
    call fig%legend("upper right")
    
    call fig%savefig("test_space_fix.pdf")
    
    print *, "Space fix test completed"
    print *, "Check test_space_fix.pdf - spaces should now appear around Greek letters"
    
end program test_space_fix