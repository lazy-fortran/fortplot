program main
    !> FPM project template for fortplot users
    !! This is a complete working example of how to structure
    !! a Fortran project that uses fortplot with FPM.
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    ! Data arrays
    integer, parameter :: n = 200
    real(wp), dimension(n) :: x, y1, y2, y3
    integer :: i
    
    print *, "=== FPM Project Template Demo ==="
    print *, "This template shows how to use fortplot in your own FPM projects."
    
    ! Generate demonstration data
    do i = 1, n
        x(i) = real(i-1, wp) * 0.05_wp  ! 0.0 to 9.95
        y1(i) = sin(x(i)) * exp(-x(i)/10.0_wp)  ! Damped sine
        y2(i) = cos(x(i)) * exp(-x(i)/8.0_wp)   ! Damped cosine
        y3(i) = exp(-x(i)/5.0_wp)               ! Exponential decay
    end do
    
    ! Create professional-looking plot
    call figure(figsize=[10.0_wp, 7.0_wp])
    call plot(x, y1, label="Damped sine", linestyle="b-")
    call plot(x, y2, label="Damped cosine", linestyle="r--")
    call plot(x, y3, label="Exponential decay", linestyle="g:")
    
    call title("FMP Project Template - Signal Processing Example")
    call xlabel("Time (s)")
    call ylabel("Amplitude")
    call xlim(0.0_wp, 10.0_wp)
    call legend()
    
    ! Save output
    call savefig("fpm_template_demo.png")
    call savefig("fmp_template_demo.pdf")
    
    print *, "SUCCESS: Plots saved as fpm_template_demo.png and .pdf"
    print *, "To use this template:"
    print *, "  1. Copy this directory structure to your project"
    print *, "  2. Modify app/main.f90 with your code"
    print *, "  3. Run: fpm build && fpm run"
    
end program main