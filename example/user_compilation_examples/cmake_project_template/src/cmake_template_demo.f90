program cmake_demo
    !> CMake project template demonstration
    !! This program shows how to use fortplot in a CMake-based project
    !! and demonstrates various plotting capabilities for user education.
    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none
    
    print *, "=== CMake Project Template Demo ==="
    print *, "Demonstrating fortplot integration with CMake build system"
    print *, ""
    
    ! Call demonstration subroutines
    call demo_basic_plots()
    call demo_scientific_visualization()
    call demo_multiple_formats()
    
    print *, ""
    print *, "SUCCESS: All demonstrations completed!"
    print *, "Check the generated files to verify your CMake setup."
    
contains

    subroutine demo_basic_plots()
        !> Demonstrate basic plotting functionality
        integer, parameter :: n = 50
        real(wp), dimension(n) :: x, y_linear, y_quadratic, y_cubic
        integer :: i
        
        print *, "1. Creating basic mathematical plots..."
        
        ! Generate polynomial data
        do i = 1, n
            x(i) = real(i-1, wp) * 0.2_wp  ! 0.0 to 9.8
            y_linear(i) = x(i)
            y_quadratic(i) = 0.1_wp * x(i)**2
            y_cubic(i) = 0.01_wp * x(i)**3
        end do
        
        call figure(figsize=[8.0_wp, 6.0_wp])
        call plot(x, y_linear, label="Linear", linestyle="b-")
        call plot(x, y_quadratic, label="Quadratic", linestyle="r--")
        call plot(x, y_cubic, label="Cubic", linestyle="g:")
        
        call title("CMake Demo - Polynomial Functions")
        call xlabel("x")
        call ylabel("f(x)")
        call legend()
        call savefig("cmake_polynomials.png")
        
        print *, "   ✓ Basic plots saved as cmake_polynomials.png"
    end subroutine demo_basic_plots

    subroutine demo_scientific_visualization()
        !> Demonstrate scientific plotting with error bars and subplots
        integer, parameter :: n = 30
        real(wp), dimension(n) :: x, y_exp, y_err
        real(wp), dimension(n) :: y_theory
        integer :: i
        
        print *, "2. Creating scientific visualization with error bars..."
        
        ! Generate experimental-style data with errors
        do i = 1, n
            x(i) = real(i-1, wp) * 0.3_wp
            y_theory(i) = exp(-x(i)/3.0_wp)
            ! Add random noise to create "experimental" data
            y_exp(i) = y_theory(i) * (1.0_wp + 0.1_wp * sin(real(i, wp)))
            y_err(i) = 0.05_wp * y_theory(i)  ! 5% error bars
        end do
        
        call figure(figsize=[10.0_wp, 6.0_wp])
        call subplot(1, 2, 1)
        call errorbar(x, y_exp, yerr=y_err, label="Experimental", &
                      capsize=3.0_wp, marker="o")
        call plot(x, y_theory, label="Theory", linestyle="r-")
        call title("Experimental vs Theory")
        call xlabel("Time")
        call ylabel("Signal")
        call legend()
        
        call subplot(1, 2, 2)
        call scatter(x, y_exp - y_theory)
        call title("Residuals")
        call xlabel("Time")  
        call ylabel("Exp - Theory")
        
        call savefig("cmake_scientific.png")
        
        print *, "   ✓ Scientific plots saved as cmake_scientific.png"
    end subroutine demo_scientific_visualization

    subroutine demo_multiple_formats()
        !> Demonstrate multiple output format capability
        integer, parameter :: n = 100
        real(wp), dimension(n) :: t, signal
        integer :: i
        
        print *, "3. Demonstrating multiple output formats..."
        
        ! Generate a signal with multiple frequency components
        do i = 1, n
            t(i) = real(i-1, wp) * 0.1_wp
            signal(i) = sin(t(i)) + 0.5_wp * sin(3.0_wp * t(i)) + &
                        0.25_wp * sin(5.0_wp * t(i))
        end do
        
        call figure(figsize=[9.0_wp, 5.0_wp])
        call plot(t, signal, label="Composite Signal", linestyle="b-")
        call title("CMake Demo - Multiple Format Output")
        call xlabel("Time (s)")
        call ylabel("Amplitude")
        call xlim(0.0_wp, 10.0_wp)
        call legend()
        
        ! Save in all available formats
        call savefig("cmake_formats.png")  ! PNG for web/presentations
        call savefig("cmake_formats.pdf")  ! PDF for publications  
        call savefig("cmake_formats.txt")  ! ASCII for terminals
        
        print *, "   ✓ Multiple formats saved:"
        print *, "     - cmake_formats.png (raster graphics)"
        print *, "     - cmake_formats.pdf (vector graphics)" 
        print *, "     - cmake_formats.txt (ASCII terminal)"
    end subroutine demo_multiple_formats

end program cmake_demo