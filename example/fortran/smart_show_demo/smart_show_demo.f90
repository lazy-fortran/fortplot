program smart_show_demo
    !! Demonstration of intelligent show() functionality
    !! 
    !! This program creates a simple plot and displays it using show(),
    !! which automatically detects if GUI is available:
    !!   - If GUI available: Opens in system PDF viewer (like matplotlib.pyplot.show())
    !!   - If no GUI: Falls back to ASCII terminal display
    !!
    !! Usage:
    !!   make example ARGS="smart_show_demo"

    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none

    integer, parameter :: n = 50
    real(wp), dimension(n) :: x, y
    integer :: i

    ! Generate sample data  
    do i = 1, n
        x(i) = real(i-1, wp) * 4.0_wp / real(n-1, wp)
        y(i) = exp(-x(i)) * cos(2.0_wp * 3.14159_wp * x(i))
    end do

    ! Create figure and add plot
    call figure(figsize=[600.0_wp, 400.0_wp])
    call title('Smart Show Demo - Damped Oscillation')
    call xlabel('Time')
    call ylabel('Amplitude')
    call plot(x, y, label='exp(-t)*cos(2πt)', linestyle='b-o')
    call legend()

    ! Display using intelligent show()
    print *, '=== Smart Show Demo ==='
    print *, 'Using show() - will automatically detect GUI availability:'
    print *, '  - GUI available: Opens in PDF viewer'
    print *, '  - No GUI: Shows ASCII plot in terminal'
    print *, ''
    
    ! This will automatically choose the best display method
    call show()
    
    print *, 'Demo completed!'

end program smart_show_demo