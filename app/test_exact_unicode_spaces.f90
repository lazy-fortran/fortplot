program test_exact_unicode_spaces
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(figure_t) :: fig
    real(wp) :: x(3), y(3)
    
    x = [0.0_wp, 1.0_wp, 2.0_wp]
    y = [0.0_wp, 1.0_wp, 0.0_wp]
    
    call fig%initialize(600, 400)
    call fig%add_plot(x, y, label="test")
    
    ! Test the exact problematic cases
    call fig%set_xlabel("Time tau (normalized: tau = omega t / 2pi)")
    call fig%set_ylabel("Amplitude Psi (V)")
    call fig%set_title("Greek letters with spaces")
    
    ! Also test legend labels
    call fig%add_plot(x, y*2, label="alpha damped: sin(omega t)")
    call fig%add_plot(x, y*3, label="beta damped: cos(omega t)")
    
    call fig%legend("upper right")
    
    call fig%savefig("test_exact_unicode_spaces.pdf")
    
    print *, "Testing exact Unicode space cases"
    print *, "Check test_exact_unicode_spaces.pdf for:"
    print *, "1. Spaces around Greek letters in axis labels"
    print *, "2. Spaces around Greek letters in legend labels"
    print *, "3. Spaces inside parentheses with Greek letters"
    
end program test_exact_unicode_spaces