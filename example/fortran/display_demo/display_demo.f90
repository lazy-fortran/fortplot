program display_demo
    !! Demonstration of plot display methods
    !!
    !! FortPlot provides two ways to display plots:
    !!   - show_viewer(): Always opens in system PDF viewer
    !!   - show(): Intelligent display - uses viewer if GUI available, else ASCII
    !!
    !! Usage:
    !!   make example ARGS="display_demo"

    use fortplot
    use iso_fortran_env, only: wp => real64
    implicit none

    print *, "=== FortPlot Display Demo ==="
    print *, ""
    call demo_show_viewer()
    call demo_smart_show()
    print *, "Display demo completed!"

contains

    subroutine demo_show_viewer()
        !! Demonstrates show_viewer() - opens in system PDF viewer
        integer, parameter :: n = 100
        real(wp), dimension(n) :: x, y1, y2
        integer :: i

        do i = 1, n
            x(i) = real(i-1, wp) * 2.0_wp * 3.14159_wp / real(n-1, wp)
            y1(i) = sin(x(i))
            y2(i) = cos(x(i))
        end do

        call figure(figsize=[8.0_wp, 6.0_wp])
        call title('show_viewer() Demo - Sine and Cosine')
        call xlabel('x (radians)')
        call ylabel('y')
        call plot(x, y1, label='sin(x)', linestyle='b-')
        call plot(x, y2, label='cos(x)', linestyle='r--')
        call legend()

        print *, "1. show_viewer() Demo"
        print *, "   Opens plot in system PDF viewer..."
        call show_viewer()
        print *, "   Viewer closed."
        print *, ""
    end subroutine demo_show_viewer

    subroutine demo_smart_show()
        !! Demonstrates show() - intelligent display selection
        integer, parameter :: n = 50
        real(wp), dimension(n) :: x, y
        integer :: i

        do i = 1, n
            x(i) = real(i-1, wp) * 4.0_wp / real(n-1, wp)
            y(i) = exp(-x(i)) * cos(2.0_wp * 3.14159_wp * x(i))
        end do

        call figure(figsize=[6.0_wp, 4.0_wp])
        call title('show() Demo - Damped Oscillation')
        call xlabel('Time')
        call ylabel('Amplitude')
        call plot(x, y, label='exp(-t)*cos(2*pi*t)', linestyle='b-o')
        call legend()

        print *, "2. show() Demo"
        print *, "   Intelligent display - detects GUI availability:"
        print *, "     - GUI available: Opens in PDF viewer"
        print *, "     - No GUI: Shows ASCII plot in terminal"
        call show()
        print *, ""
    end subroutine demo_smart_show

end program display_demo
