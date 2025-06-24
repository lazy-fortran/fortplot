program fortplotlib_test
    !! Simple test program using fortplotlib via CMake
    use fortplot
    implicit none
    
    call test_simple_plot()
    print *, "fortplotlib CMake test completed successfully!"
    
contains

    subroutine test_simple_plot()
        real(wp), dimension(20) :: x, y
        integer :: i
        
        print *, "Testing fortplotlib with CMake..."
        
        ! Generate simple test data
        x = [(real(i-1, wp) * 2.0_wp * 3.141592653589793_wp / 19.0_wp, i=1, 20)]
        y = sin(x)
        
        ! Create a simple plot
        call figure()
        call plot(x, y, label='sin(x)')
        call title('CMake Test Plot')
        call xlabel('x')
        call ylabel('sin(x)')
        call savefig('cmake_test_plot.png')
        
        print *, "Created: cmake_test_plot.png"
        
    end subroutine test_simple_plot

end program fortplotlib_test