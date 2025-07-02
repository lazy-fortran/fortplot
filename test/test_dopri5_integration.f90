program test_dopri5_integration
    !! Test DOPRI5 integration accuracy for streamline computation
    !! Following TDD approach - write failing tests first
    use fortplot_streamline_integrator
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    call test_should_integrate_simple_velocity_field()
    call test_should_handle_adaptive_step_size()
    call test_should_achieve_specified_tolerance()
    call test_should_handle_circular_velocity_field()
    call test_should_be_more_accurate_than_rk4()
    
    print *, "All DOPRI5 integration tests passed!"
    
contains

    subroutine test_should_integrate_simple_velocity_field()
        !! Test DOPRI5 with constant velocity field: u=1, v=0
        !! Expected: x(t) = x0 + t, y(t) = y0
        type(integration_params_t) :: params
        real(wp) :: x0, y0, t_final, x_final, y_final
        real(wp), allocatable :: path_x(:), path_y(:), times(:)
        integer :: n_points, n_accepted, n_rejected
        logical :: success
        
        print *, "Testing DOPRI5 with constant velocity field..."
        
        ! Initial conditions
        x0 = 0.0_wp
        y0 = 0.0_wp
        t_final = 2.0_wp
        
        ! Integration parameters
        params%rtol = 1.0e-8_wp
        params%atol = 1.0e-10_wp
        params%h_initial = 0.01_wp
        params%max_steps = 1000
        
        ! Should integrate exactly: x(t) = t, y(t) = 0
        call dopri5_integrate(x0, y0, 0.0_wp, t_final, &
                             constant_u_field, constant_v_field, &
                             params, path_x, path_y, times, &
                             n_points, n_accepted, n_rejected, success)
        
        ! Verify integration succeeded
        if (.not. success) then
            error stop "DOPRI5 integration failed for simple constant field"
        end if
        
        ! Check final position accuracy
        x_final = path_x(n_points)
        y_final = path_y(n_points)
        
        if (abs(x_final - t_final) > 1.0e-6_wp) then
            print *, "Expected x_final:", t_final, "Got:", x_final
            error stop "DOPRI5 failed accuracy test for x-component"
        end if
        
        if (abs(y_final - 0.0_wp) > 1.0e-6_wp) then
            print *, "Expected y_final: 0.0, Got:", y_final
            error stop "DOPRI5 failed accuracy test for y-component"
        end if
        
        print *, "✓ DOPRI5 constant field integration passed"
    end subroutine test_should_integrate_simple_velocity_field

    subroutine test_should_handle_adaptive_step_size()
        !! Test that DOPRI5 adapts step size appropriately
        type(integration_params_t) :: params
        real(wp) :: x0, y0, t_final
        real(wp), allocatable :: path_x(:), path_y(:), times(:)
        integer :: n_points, n_accepted, n_rejected
        logical :: success
        real(wp) :: min_dt, max_dt
        integer :: i
        
        print *, "Testing DOPRI5 adaptive step size control..."
        
        x0 = 1.0_wp
        y0 = 0.0_wp
        t_final = 2.0_wp * 3.141592653589793_wp  ! One full period
        
        params%rtol = 1.0e-6_wp
        params%atol = 1.0e-9_wp
        params%h_initial = 0.1_wp
        params%max_steps = 2000
        
        ! Use oscillatory field that should require step adaptation
        call dopri5_integrate(x0, y0, 0.0_wp, t_final, &
                             oscillatory_u_field, oscillatory_v_field, &
                             params, path_x, path_y, times, &
                             n_points, n_accepted, n_rejected, success)
        
        if (.not. success) then
            error stop "DOPRI5 integration failed for oscillatory field"
        end if
        
        ! Check that step size varied (adaptive behavior)
        min_dt = huge(min_dt)
        max_dt = 0.0_wp
        do i = 2, n_points
            min_dt = min(min_dt, times(i) - times(i-1))
            max_dt = max(max_dt, times(i) - times(i-1))
        end do
        
        ! Step size should have varied by at least factor of 2
        if (max_dt / min_dt < 2.0_wp) then
            error stop "DOPRI5 did not show adaptive step size behavior"
        end if
        
        ! Should have rejected some steps (showing error control)
        if (n_rejected == 0) then
            error stop "DOPRI5 should reject some steps for error control"
        end if
        
        print *, "✓ DOPRI5 adaptive step size test passed"
        print *, "  Steps: accepted =", n_accepted, "rejected =", n_rejected
    end subroutine test_should_handle_adaptive_step_size

    subroutine test_should_achieve_specified_tolerance()
        !! Test that DOPRI5 meets specified error tolerance
        type(integration_params_t) :: params
        real(wp) :: x0, y0, t_final
        real(wp), allocatable :: path_x(:), path_y(:), times(:)
        integer :: n_points, n_accepted, n_rejected
        logical :: success
        real(wp) :: analytical_x, analytical_y, error_x, error_y
        
        print *, "Testing DOPRI5 error tolerance..."
        
        ! Circular motion: x' = -y, y' = x
        ! Analytical solution: x(t) = cos(t), y(t) = sin(t)
        x0 = 1.0_wp
        y0 = 0.0_wp
        t_final = 2.0_wp * 3.141592653589793_wp  ! One full period
        
        params%rtol = 1.0e-8_wp
        params%atol = 1.0e-10_wp
        params%h_initial = 0.01_wp
        params%max_steps = 2000
        
        call dopri5_integrate(x0, y0, 0.0_wp, t_final, &
                             circular_u_field, circular_v_field, &
                             params, path_x, path_y, times, &
                             n_points, n_accepted, n_rejected, success)
        
        if (.not. success) then
            error stop "DOPRI5 integration failed for circular motion"
        end if
        
        ! Check final position against analytical solution
        analytical_x = cos(t_final)  ! Should return to (1, 0)
        analytical_y = sin(t_final)
        
        error_x = abs(path_x(n_points) - analytical_x)
        error_y = abs(path_y(n_points) - analytical_y)
        
        ! Error should be much smaller than tolerance
        if (error_x > 10.0_wp * params%rtol) then
            print *, "Error x:", error_x, "Tolerance:", params%rtol
            error stop "DOPRI5 exceeded error tolerance for x-component"
        end if
        
        if (error_y > 10.0_wp * params%rtol) then
            print *, "Error y:", error_y, "Tolerance:", params%rtol
            error stop "DOPRI5 exceeded error tolerance for y-component"
        end if
        
        print *, "✓ DOPRI5 error tolerance test passed"
        print *, "  Final error: x =", error_x, "y =", error_y
    end subroutine test_should_achieve_specified_tolerance

    subroutine test_should_handle_circular_velocity_field()
        !! Test integration of circular flow used in streamplot_demo
        type(integration_params_t) :: params
        real(wp) :: x0, y0, t_final
        real(wp), allocatable :: path_x(:), path_y(:), times(:)
        integer :: n_points, n_accepted, n_rejected
        logical :: success
        real(wp) :: radius_initial, radius_final
        
        print *, "Testing DOPRI5 with circular flow field..."
        
        ! Same as streamplot_demo: u = -y, v = x
        x0 = 1.5_wp
        y0 = 0.5_wp
        t_final = 3.141592653589793_wp  ! Half period
        
        params%rtol = 1.0e-6_wp
        params%atol = 1.0e-9_wp
        params%h_initial = 0.01_wp
        params%max_steps = 1000
        
        call dopri5_integrate(x0, y0, 0.0_wp, t_final, &
                             demo_u_field, demo_v_field, &
                             params, path_x, path_y, times, &
                             n_points, n_accepted, n_rejected, success)
        
        if (.not. success) then
            error stop "DOPRI5 integration failed for demo circular field"
        end if
        
        ! For circular motion, radius should be conserved
        radius_initial = sqrt(x0**2 + y0**2)
        radius_final = sqrt(path_x(n_points)**2 + path_y(n_points)**2)
        
        if (abs(radius_final - radius_initial) > 1.0e-3_wp) then
            print *, "Initial radius:", radius_initial, "Final radius:", radius_final
            error stop "DOPRI5 failed to conserve radius in circular motion"
        end if
        
        print *, "✓ DOPRI5 circular field test passed"
    end subroutine test_should_handle_circular_velocity_field

    subroutine test_should_be_more_accurate_than_rk4()
        !! Compare DOPRI5 accuracy with fixed-step RK4
        !! This will initially fail until DOPRI5 is implemented
        type(integration_params_t) :: params
        real(wp) :: x0, y0, t_final
        real(wp), allocatable :: path_x_dopri5(:), path_y_dopri5(:), times(:)
        real(wp) :: x_rk4, y_rk4, x_dopri5, y_dopri5
        real(wp) :: error_rk4, error_dopri5, analytical_x, analytical_y
        integer :: n_points, n_accepted, n_rejected
        logical :: success
        
        print *, "Comparing DOPRI5 vs RK4 accuracy..."
        
        ! Use circular motion test case
        x0 = 1.0_wp
        y0 = 0.0_wp
        t_final = 2.0_wp * 3.141592653589793_wp
        
        params%rtol = 1.0e-6_wp
        params%atol = 1.0e-9_wp
        params%h_initial = 0.01_wp
        params%max_steps = 2000
        
        ! DOPRI5 integration
        call dopri5_integrate(x0, y0, 0.0_wp, t_final, &
                             circular_u_field, circular_v_field, &
                             params, path_x_dopri5, path_y_dopri5, times, &
                             n_points, n_accepted, n_rejected, success)
        
        if (.not. success) then
            error stop "DOPRI5 integration failed in comparison test"
        end if
        
        ! Fixed-step RK4 with same average step size
        call rk4_fixed_step_integrate(x0, y0, t_final, 0.01_wp, &
                                     circular_u_field, circular_v_field, &
                                     x_rk4, y_rk4)
        
        ! Analytical solution
        analytical_x = cos(t_final)
        analytical_y = sin(t_final)
        
        ! Calculate errors
        x_dopri5 = path_x_dopri5(n_points)
        y_dopri5 = path_y_dopri5(n_points)
        
        error_dopri5 = sqrt((x_dopri5 - analytical_x)**2 + (y_dopri5 - analytical_y)**2)
        error_rk4 = sqrt((x_rk4 - analytical_x)**2 + (y_rk4 - analytical_y)**2)
        
        print *, "DOPRI5 error:", error_dopri5
        print *, "RK4 error:", error_rk4
        
        ! DOPRI5 should be significantly more accurate
        if (error_dopri5 >= error_rk4) then
            error stop "DOPRI5 should be more accurate than fixed-step RK4"
        end if
        
        print *, "✓ DOPRI5 vs RK4 accuracy test passed"
        print *, "  DOPRI5 is", error_rk4/error_dopri5, "times more accurate"
    end subroutine test_should_be_more_accurate_than_rk4

    ! Test velocity field functions
    real(wp) function constant_u_field(x, y) result(u)
        real(wp), intent(in) :: x, y
        u = 1.0_wp
    end function constant_u_field

    real(wp) function constant_v_field(x, y) result(v)
        real(wp), intent(in) :: x, y
        v = 0.0_wp
    end function constant_v_field

    real(wp) function circular_u_field(x, y) result(u)
        real(wp), intent(in) :: x, y
        u = -y
    end function circular_u_field

    real(wp) function circular_v_field(x, y) result(v)
        real(wp), intent(in) :: x, y
        v = x
    end function circular_v_field

    real(wp) function oscillatory_u_field(x, y) result(u)
        real(wp), intent(in) :: x, y
        u = sin(10.0_wp * x) * cos(5.0_wp * y)
    end function oscillatory_u_field

    real(wp) function oscillatory_v_field(x, y) result(v)
        real(wp), intent(in) :: x, y
        v = cos(5.0_wp * x) * sin(10.0_wp * y)
    end function oscillatory_v_field

    real(wp) function demo_u_field(x, y) result(u)
        real(wp), intent(in) :: x, y
        u = -y  ! Same as streamplot_demo
    end function demo_u_field

    real(wp) function demo_v_field(x, y) result(v)
        real(wp), intent(in) :: x, y
        v = x   ! Same as streamplot_demo
    end function demo_v_field

    subroutine rk4_fixed_step_integrate(x0, y0, t_final, dt, u_func, v_func, x_final, y_final)
        !! Fixed-step RK4 for comparison
        real(wp), intent(in) :: x0, y0, t_final, dt
        interface
            real(wp) function u_func(x, y)
                import wp
                real(wp), intent(in) :: x, y
            end function u_func
            real(wp) function v_func(x, y)
                import wp
                real(wp), intent(in) :: x, y
            end function v_func
        end interface
        real(wp), intent(out) :: x_final, y_final
        
        real(wp) :: x, y, t, k1x, k1y, k2x, k2y, k3x, k3y, k4x, k4y
        integer :: n_steps, i
        
        n_steps = int(t_final / dt)
        x = x0
        y = y0
        
        do i = 1, n_steps
            k1x = u_func(x, y)
            k1y = v_func(x, y)
            
            k2x = u_func(x + 0.5_wp*dt*k1x, y + 0.5_wp*dt*k1y)
            k2y = v_func(x + 0.5_wp*dt*k1x, y + 0.5_wp*dt*k1y)
            
            k3x = u_func(x + 0.5_wp*dt*k2x, y + 0.5_wp*dt*k2y)
            k3y = v_func(x + 0.5_wp*dt*k2x, y + 0.5_wp*dt*k2y)
            
            k4x = u_func(x + dt*k3x, y + dt*k3y)
            k4y = v_func(x + dt*k3x, y + dt*k3y)
            
            x = x + dt * (k1x + 2*k2x + 2*k3x + k4x) / 6.0_wp
            y = y + dt * (k1y + 2*k2y + 2*k3y + k4y) / 6.0_wp
        end do
        
        x_final = x
        y_final = y
    end subroutine rk4_fixed_step_integrate

end program test_dopri5_integration