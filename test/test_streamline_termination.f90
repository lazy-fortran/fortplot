program test_streamline_termination
    use fortplot_streamline, only: check_termination
    implicit none
    
    call test_boundary_termination()
    call test_convergence_termination()
    call test_existing_streamline_termination()
    
    print *, "All streamline termination tests passed!"
    
contains
    
    subroutine test_boundary_termination()
        real :: x_bounds(2) = [0.0, 10.0]
        real :: y_bounds(2) = [0.0, 10.0]
        logical :: terminate
        
        call check_termination(11.0, 5.0, x_bounds, y_bounds, terminate)
        if (.not. terminate) error stop "Should terminate outside x boundary"
        
        call check_termination(5.0, -1.0, x_bounds, y_bounds, terminate)
        if (.not. terminate) error stop "Should terminate outside y boundary"
        
        call check_termination(5.0, 5.0, x_bounds, y_bounds, terminate)
        if (terminate) error stop "Should not terminate inside boundaries"
    end subroutine
    
    subroutine test_convergence_termination()
        real :: x_bounds(2) = [0.0, 10.0]
        real :: y_bounds(2) = [0.0, 10.0]
        real :: u = 0.0, v = 0.0
        logical :: terminate
        
        call check_termination_with_velocity(5.0, 5.0, u, v, x_bounds, y_bounds, terminate)
        if (.not. terminate) error stop "Should terminate at zero velocity"
        
        u = 1.0
        v = 1.0
        call check_termination_with_velocity(5.0, 5.0, u, v, x_bounds, y_bounds, terminate)
        if (terminate) error stop "Should not terminate with non-zero velocity"
    end subroutine
    
    subroutine test_existing_streamline_termination()
        real :: x_bounds(2) = [0.0, 10.0]
        real :: y_bounds(2) = [0.0, 10.0]
        real :: existing_x(3) = [2.0, 3.0, 4.0]
        real :: existing_y(3) = [2.0, 3.0, 4.0]
        logical :: terminate
        
        call check_termination_near_existing(3.01, 3.01, existing_x, existing_y, 3, terminate)
        if (.not. terminate) error stop "Should terminate near existing streamline"
        
        call check_termination_near_existing(7.0, 7.0, existing_x, existing_y, 3, terminate)
        if (terminate) error stop "Should not terminate far from existing streamlines"
    end subroutine
    
    subroutine check_termination_with_velocity(x, y, u, v, x_bounds, y_bounds, terminate)
        real, intent(in) :: x, y, u, v
        real, dimension(2), intent(in) :: x_bounds, y_bounds
        logical, intent(out) :: terminate
        
        call check_termination(x, y, x_bounds, y_bounds, terminate)
        if (.not. terminate .and. sqrt(u**2 + v**2) < 1e-6) terminate = .true.
    end subroutine
    
    subroutine check_termination_near_existing(x, y, existing_x, existing_y, n, terminate)
        real, intent(in) :: x, y
        real, dimension(:), intent(in) :: existing_x, existing_y
        integer, intent(in) :: n
        logical, intent(out) :: terminate
        integer :: i
        real :: min_dist
        
        min_dist = 1e10
        do i = 1, n
            min_dist = min(min_dist, sqrt((x - existing_x(i))**2 + (y - existing_y(i))**2))
        end do
        
        terminate = (min_dist < 0.1)
    end subroutine
    
end program test_streamline_termination