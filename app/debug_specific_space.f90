program debug_specific_space
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3), y(3)
    
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 0.0_wp]
    
    call fig%initialize(500, 400)
    call fig%add_plot(x, y, label="test")
    
    ! Test the exact string from unicode demo
    call fig%set_xlabel("Time tau (normalized: tau = omega t / 2pi)")
    call fig%set_ylabel("Amplitude Psi (V)")
    call fig%set_title("Test exact unicode demo text")
    
    call fig%savefig("debug_specific_space.pdf")
    
    print *, "Specific space test completed"
    
end program debug_specific_space