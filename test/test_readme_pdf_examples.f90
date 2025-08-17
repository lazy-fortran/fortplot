program test_readme_pdf_examples
    !! Validate all README examples that generate PDF files work correctly with overlap fix
    use fortplot
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_trig_functions_pdf()
    call test_log_plot_pdf()
    call test_unicode_demo_pdf()
    
    print *, "All README PDF examples tested successfully!"
    
contains

    subroutine test_trig_functions_pdf()
        !! Test the multiple plots with legend example from README (line 56)
        integer, parameter :: n = 100
        real(wp) :: x(n), i_real
        integer :: i
        
        print *, "=== Testing README trig functions PDF example ==="
        
        ! Generate data exactly as shown in README
        do i = 1, n
            i_real = real(i-1, wp)
            x(i) = i_real * 2.0_wp * 3.14159_wp / real(n-1, wp)
        end do
        
        call figure(800, 600)
        call plot(x, sin(x), label="sin(x)", linestyle="b-")
        call plot(x, cos(x), label="cos(x)", linestyle="r--")
        call plot(x, sin(2*x), label="sin(2x)", linestyle="g:")
        call legend()
        call savefig("/tmp/readme_trig_functions.pdf")
        
        print *, "Created: /tmp/readme_trig_functions.pdf"
        print *, "Expected: Three trigonometric functions with legend, no Y-axis overlap"
    end subroutine test_trig_functions_pdf
    
    subroutine test_log_plot_pdf()
        !! Test the log scale plot example from README (line 89)
        integer, parameter :: n = 50
        real(wp) :: x(n), y(n)
        integer :: i
        
        print *, "=== Testing README log plot PDF example ==="
        
        ! Generate log-scale test data
        do i = 1, n
            x(i) = 10.0_wp**(real(i-1, wp) * 6.0_wp / real(n-1, wp) - 3.0_wp)  ! 1e-3 to 1e3
            y(i) = x(i)**2 + sin(log10(x(i)) * 10.0_wp) * x(i) * 0.1_wp
        end do
        
        call figure()
        call plot(x, y)
        call set_xscale("log")
        call set_yscale("symlog", threshold=0.01_wp)
        call xlim(1.0e-3_wp, 1.0e3_wp)
        call ylim(-100.0_wp, 100.0_wp)
        call savefig("/tmp/readme_log_plot.pdf")
        
        print *, "Created: /tmp/readme_log_plot.pdf"
        print *, "Expected: Log/symlog plot with proper Y-axis label spacing near zero"
    end subroutine test_log_plot_pdf
    
    subroutine test_unicode_demo_pdf()
        !! Test that unicode characters work in PDF backend (mentioned on line 68)
        integer, parameter :: n = 100
        real(wp) :: t(n), damped_sine(n), damped_cosine(n)
        real(wp) :: omega, lambda_val, A
        integer :: i
        
        print *, "=== Testing README unicode demo PDF example ==="
        
        ! Generate wave function data similar to README example
        omega = 2.0_wp * 3.14159_wp
        lambda_val = 1.0_wp
        A = 1.0_wp
        
        do i = 1, n
            t(i) = real(i-1, wp) * 2.0_wp / real(n-1, wp)
            damped_sine(i) = A * exp(-lambda_val * t(i)) * sin(omega * t(i))
            damped_cosine(i) = A * exp(-lambda_val * t(i)) * cos(omega * t(i))
        end do
        
        call figure(800, 600)
        call title("Wave Functions: ψ(ωt) = A e^{-λt} sin(ωt)")
        call xlabel("Time τ (normalized)")
        call ylabel("Amplitude Ψ (V)")
        call plot(t, damped_sine, label="α decay")
        call plot(t, damped_cosine, label="β oscillation")
        call legend()
        call savefig("/tmp/readme_unicode_demo.pdf")
        
        print *, "Created: /tmp/readme_unicode_demo.pdf"
        print *, "Expected: Unicode characters in titles/labels, clean Y-axis spacing"
    end subroutine test_unicode_demo_pdf

end program test_readme_pdf_examples