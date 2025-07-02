program debug_streamline_integration
    !! Debug streamline integration to understand why circles don't fill center
    use fortplot_streamline
    use fortplot_streamline_integrator
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    real :: x0, y0
    real, allocatable :: path_x(:), path_y(:)
    integer :: n_points
    type(integration_params_t) :: params
    
    ! Test integration from center point (0,0)
    x0 = 0.1  ! Slightly off center to avoid singularity
    y0 = 0.1
    
    print *, "=== Testing Integration from Near-Center ==="
    print *, "Starting point: (", x0, ",", y0, ")"
    
    ! Test with default DOPRI5 parameters
    call integrate_streamline_dopri5(x0, y0, &
                                   test_u_field, test_v_field, &
                                   max_time=10.0, &
                                   path_x=path_x, path_y=path_y, &
                                   n_points=n_points)
    
    print *, "DOPRI5 integration: Generated", n_points, "points"
    if (n_points > 0) then
        print *, "Final point: (", path_x(n_points), ",", path_y(n_points), ")"
        print *, "Distance traveled:", sqrt((path_x(n_points) - x0)**2 + (path_y(n_points) - y0)**2)
    end if
    
    ! Test with old RK4 method for comparison
    call integrate_streamline(x0, y0, test_u_field, test_v_field, 0.01, 1000, &
                             path_x, path_y, n_points)
    
    print *, "RK4 integration: Generated", n_points, "points"
    if (n_points > 0) then
        print *, "Final point: (", path_x(n_points), ",", path_y(n_points), ")"
        print *, "Distance traveled:", sqrt((path_x(n_points) - x0)**2 + (path_y(n_points) - y0)**2)
    end if
    
    ! Test from boundary point
    print *, "=== Testing Integration from Boundary ==="
    x0 = -1.5
    y0 = 0.0
    
    call integrate_streamline_dopri5(x0, y0, &
                                   test_u_field, test_v_field, &
                                   max_time=10.0, &
                                   path_x=path_x, path_y=path_y, &
                                   n_points=n_points)
    
    print *, "DOPRI5 from boundary: Generated", n_points, "points"
    if (n_points > 0) then
        print *, "Final point: (", path_x(n_points), ",", path_y(n_points), ")"
    end if
    
    print *, "Debug completed!"
    
contains

    real function test_u_field(x, y) result(u)
        real, intent(in) :: x, y
        u = -y  ! Circular flow: u = -y
    end function

    real function test_v_field(x, y) result(v)
        real, intent(in) :: x, y
        v = x   ! Circular flow: v = x
    end function

end program debug_streamline_integration