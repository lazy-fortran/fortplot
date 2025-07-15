program debug_axis_spaces
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3), y(3)
    
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 0.0_wp]
    
    call fig%initialize(500, 400)
    call fig%add_plot(x, y, label="test")
    
    ! Test axis labels with spaces around Greek letters
    call fig%set_xlabel("Time tau (normalized)")
    call fig%set_ylabel("Amplitude Psi (V)")
    call fig%set_title("Test spaces in axis labels")
    
    call fig%savefig("debug_axis_spaces.pdf")
    
    print *, "Axis spaces test completed - check debug_axis_spaces.pdf"
    
end program debug_axis_spaces