program module_refactoring_demo
    !! Demonstrates backward compatibility after module refactoring
    !! Shows that existing code works unchanged with refactored structure
    
    use iso_fortran_env, only: wp => real64
    use fortplot, only: figure, plot, title, xlabel, ylabel, savefig
    implicit none
    
    integer, parameter :: n = 100
    real(wp), dimension(n) :: x, y
    integer :: i
    
    ! Generate sample data
    do i = 1, n
        x(i) = real(i-1, wp) * 0.1_wp
        y(i) = sin(x(i)) * exp(-x(i)/5.0_wp)
    end do
    
    ! Create plot using stateful API (works unchanged)
    call figure()
    call plot(x, y, label="Damped sine wave")
    call title("Module Refactoring Demo")
    call xlabel("Time")
    call ylabel("Amplitude")
    call savefig("module_demo.png")
    
    print *, "Plot saved to module_demo.png"
    print *, "Refactored modules work with existing code!"
    
end program module_refactoring_demo