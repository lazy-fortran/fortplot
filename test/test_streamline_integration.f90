program test_streamline_integration
    use fortplot_streamline, only: integrate_streamline, rk4_step
    implicit none
    
    call test_constant_field()
    call test_circular_field()
    call test_rk4_accuracy()
    
    print *, "All streamline integration tests passed!"
    
contains
    
    subroutine test_constant_field()
        real :: x0 = 0.0, y0 = 0.0
        real, allocatable :: path_x(:), path_y(:)
        integer :: n_points
        
        call integrate_streamline(x0, y0, constant_u_field, constant_v_field, &
                                 0.1, 100, path_x, path_y, n_points)
        
        if (n_points < 10) error stop "Too few integration points"
        if (abs(path_x(n_points) - 10.0) > 1.0) error stop "Incorrect x integration"
        if (abs(path_y(n_points)) > 0.1) error stop "y should remain constant"
        
        deallocate(path_x, path_y)
    end subroutine
    
    subroutine test_circular_field()
        real :: x0 = 1.0, y0 = 0.0
        real, allocatable :: path_x(:), path_y(:)
        integer :: n_points, i
        real :: radius
        
        call integrate_streamline(x0, y0, circular_u_field, circular_v_field, &
                                 0.01, 1000, path_x, path_y, n_points)
        
        do i = 1, n_points
            radius = sqrt(path_x(i)**2 + path_y(i)**2)
            if (abs(radius - 1.0) > 0.1) error stop "Path deviates from circle"
        end do
        
        deallocate(path_x, path_y)
    end subroutine
    
    subroutine test_rk4_accuracy()
        real :: x = 0.0, y = 0.0, dt = 0.1
        real :: x_new, y_new
        
        call rk4_step(x, y, constant_u_field, constant_v_field, dt, x_new, y_new)
        
        if (abs(x_new - 0.1) > 1e-6) error stop "RK4 x step incorrect"
        if (abs(y_new) > 1e-6) error stop "RK4 y step incorrect"
    end subroutine
    
    real function constant_u_field(x, y) result(u)
        real, intent(in) :: x, y
        u = 1.0
    end function
    
    real function constant_v_field(x, y) result(v)
        real, intent(in) :: x, y
        v = 0.0
    end function
    
    real function circular_u_field(x, y) result(u)
        real, intent(in) :: x, y
        u = -y
    end function
    
    real function circular_v_field(x, y) result(v)
        real, intent(in) :: x, y
        v = x
    end function
    
end program test_streamline_integration