program test_spaces_greek
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3), y(3)
    
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 0.0_wp]
    
    call fig%initialize(600, 400)
    call fig%add_plot(x, y, label="test")
    
    ! Test specific cases with spaces around Greek letters
    call fig%set_xlabel("Before alpha after")
    call fig%set_ylabel("Before beta after")
    call fig%set_title("Before gamma after")
    
    call fig%savefig("test_spaces_greek.pdf")
    
    print *, "Testing spaces around Greek letters"
    print *, "Check test_spaces_greek.pdf carefully"
    
end program test_spaces_greek