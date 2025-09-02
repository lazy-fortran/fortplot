module fortplot_streamline_integrator
    !! DOPRI5 (Dormand-Prince 5(4)) integrator for streamline computation
    !! Provides adaptive step size control and high-order accuracy
    !! 
    !! Following SOLID principles:
    !! - Single responsibility: Only handles numerical integration
    !! - Open/closed: Extensible to other integration methods
    !! - Interface segregation: Clean, focused interface
    !! - Dependency inversion: Depends on abstract velocity field interface
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    private
    
    public :: integration_params_t, dopri5_integrate
    
    !! Integration parameters for DOPRI5 method
    type :: integration_params_t
        real(wp) :: rtol = 1.0e-6_wp           !! Relative tolerance
        real(wp) :: atol = 1.0e-9_wp           !! Absolute tolerance
        real(wp) :: h_initial = 0.01_wp        !! Initial step size
        real(wp) :: h_min = 1.0e-12_wp         !! Minimum step size
        real(wp) :: h_max = 1.0_wp             !! Maximum step size
        integer :: max_steps = 10000           !! Maximum integration steps
        real(wp) :: safety_factor = 0.9_wp     !! Step size safety factor
        real(wp) :: max_factor = 5.0_wp        !! Maximum step increase factor
        real(wp) :: min_factor = 0.2_wp        !! Minimum step decrease factor
    end type integration_params_t
    
    !! DOPRI5 Butcher tableau coefficients
    real(wp), parameter :: A21 = 1.0_wp/5.0_wp
    real(wp), parameter :: A31 = 3.0_wp/40.0_wp, A32 = 9.0_wp/40.0_wp
    real(wp), parameter :: A41 = 44.0_wp/45.0_wp, A42 = -56.0_wp/15.0_wp, A43 = 32.0_wp/9.0_wp
    real(wp), parameter :: A51 = 19372.0_wp/6561.0_wp, A52 = -25360.0_wp/2187.0_wp
    real(wp), parameter :: A53 = 64448.0_wp/6561.0_wp, A54 = -212.0_wp/729.0_wp
    real(wp), parameter :: A61 = 9017.0_wp/3168.0_wp, A62 = -355.0_wp/33.0_wp
    real(wp), parameter :: A63 = 46732.0_wp/5247.0_wp, A64 = 49.0_wp/176.0_wp, A65 = -5103.0_wp/18656.0_wp
    real(wp), parameter :: A71 = 35.0_wp/384.0_wp, A72 = 0.0_wp
    real(wp), parameter :: A73 = 500.0_wp/1113.0_wp, A74 = 125.0_wp/192.0_wp
    real(wp), parameter :: A75 = -2187.0_wp/6784.0_wp, A76 = 11.0_wp/84.0_wp
    
    !! 5th order solution weights (b_i)
    real(wp), parameter :: B1 = 35.0_wp/384.0_wp, B2 = 0.0_wp
    real(wp), parameter :: B3 = 500.0_wp/1113.0_wp, B4 = 125.0_wp/192.0_wp
    real(wp), parameter :: B5 = -2187.0_wp/6784.0_wp, B6 = 11.0_wp/84.0_wp, B7 = 0.0_wp
    
    !! 4th order embedded solution weights (b*_i)
    real(wp), parameter :: BS1 = 5179.0_wp/57600.0_wp, BS2 = 0.0_wp
    real(wp), parameter :: BS3 = 7571.0_wp/16695.0_wp, BS4 = 393.0_wp/640.0_wp
    real(wp), parameter :: BS5 = -92097.0_wp/339200.0_wp, BS6 = 187.0_wp/2100.0_wp, BS7 = 1.0_wp/40.0_wp
    
    !! c_i time coefficients
    real(wp), parameter :: C2 = 1.0_wp/5.0_wp, C3 = 3.0_wp/10.0_wp, C4 = 4.0_wp/5.0_wp
    real(wp), parameter :: C5 = 8.0_wp/9.0_wp, C6 = 1.0_wp, C7 = 1.0_wp
    
contains

    subroutine dopri5_integrate(x0, y0, t0, t_final, u_func, v_func, params, &
                               path_x, path_y, times, n_points, n_accepted, n_rejected, success)
        !! Integrate streamline using DOPRI5 method with adaptive step size
        real(wp), intent(in) :: x0, y0, t0, t_final
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
        type(integration_params_t), intent(in) :: params
        real(wp), allocatable, intent(out) :: path_x(:), path_y(:), times(:)
        integer, intent(out) :: n_points, n_accepted, n_rejected
        logical, intent(out) :: success
        
        ! Local variables
        real(wp) :: x, y, t, h, error_est
        real(wp) :: k1x, k1y, k2x, k2y, k3x, k3y, k4x, k4y, k5x, k5y, k6x, k6y, k7x, k7y
        real(wp) :: x5, y5, x4, y4  ! 5th and 4th order solutions
        real(wp) :: h_new
        integer :: step_count, array_size
        logical :: step_accepted
        
        ! Initialize integration
        array_size = params%max_steps
        allocate(path_x(array_size), path_y(array_size), times(array_size))
        
        x = x0
        y = y0
        t = t0
        h = params%h_initial
        n_points = 1
        n_accepted = 0
        n_rejected = 0
        success = .true.
        
        ! Store initial point
        path_x(1) = x
        path_y(1) = y
        times(1) = t
        
        ! Main integration loop
        do step_count = 1, params%max_steps
            ! Check if we've reached the final time
            if (t >= t_final) exit
            
            ! Adjust step size to not overshoot
            if (t + h > t_final) h = t_final - t
            
            ! DOPRI5 step with error estimation
            call dopri5_step(x, y, h, u_func, v_func, x5, y5, x4, y4, &
                           k1x, k1y, k2x, k2y, k3x, k3y, k4x, k4y, k5x, k5y, k6x, k6y, k7x, k7y)
            
            ! Calculate error estimate
            error_est = calculate_error_estimate(x5, y5, x4, y4, params)
            
            ! Determine if step is accepted
            step_accepted = (error_est <= 1.0_wp)
            
            if (step_accepted) then
                ! Accept step
                n_accepted = n_accepted + 1
                x = x5
                y = y5
                t = t + h
                n_points = n_points + 1
                
                ! Store point (resize arrays if needed)
                if (n_points > array_size) then
                    call resize_arrays(path_x, path_y, times, array_size)
                end if
                
                path_x(n_points) = x
                path_y(n_points) = y
                times(n_points) = t
                
                ! Use FSAL property: k1 for next step = k7 from current step
                k1x = k7x
                k1y = k7y
            else
                ! Reject step
                n_rejected = n_rejected + 1
            end if
            
            ! Calculate new step size
            if (error_est > 0.0_wp) then
                h_new = h * params%safety_factor * (1.0_wp / error_est)**(1.0_wp/5.0_wp)
            else
                h_new = h * params%max_factor
            end if
            
            ! Apply step size bounds
            h_new = max(params%h_min, min(params%h_max, h_new))
            h_new = max(h_new, h * params%min_factor)
            h_new = min(h_new, h * params%max_factor)
            
            h = h_new
            
            ! Check for step size underflow
            if (h < params%h_min) then
                success = .false.
                return
            end if
        end do
        
        ! Check if integration completed successfully
        if (step_count >= params%max_steps .and. t < t_final) then
            success = .false.
        end if
        
        ! Trim arrays to actual size
        call trim_arrays(path_x, path_y, times, n_points)
        
    end subroutine dopri5_integrate

    subroutine dopri5_step(x, y, h, u_func, v_func, x5, y5, x4, y4, &
                          k1x, k1y, k2x, k2y, k3x, k3y, k4x, k4y, k5x, k5y, k6x, k6y, k7x, k7y)
        !! Single DOPRI5 step with 5th and 4th order solutions
        real(wp), intent(in) :: x, y, h
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
        real(wp), intent(out) :: x5, y5, x4, y4  ! 5th and 4th order solutions
        real(wp), intent(out) :: k1x, k1y, k2x, k2y, k3x, k3y, k4x, k4y, k5x, k5y, k6x, k6y, k7x, k7y
        
        ! Stage 1
        k1x = u_func(x, y)
        k1y = v_func(x, y)
        
        ! Stage 2
        k2x = u_func(x + h*A21*k1x, y + h*A21*k1y)
        k2y = v_func(x + h*A21*k1x, y + h*A21*k1y)
        
        ! Stage 3
        k3x = u_func(x + h*(A31*k1x + A32*k2x), y + h*(A31*k1y + A32*k2y))
        k3y = v_func(x + h*(A31*k1x + A32*k2x), y + h*(A31*k1y + A32*k2y))
        
        ! Stage 4
        k4x = u_func(x + h*(A41*k1x + A42*k2x + A43*k3x), &
                     y + h*(A41*k1y + A42*k2y + A43*k3y))
        k4y = v_func(x + h*(A41*k1x + A42*k2x + A43*k3x), &
                     y + h*(A41*k1y + A42*k2y + A43*k3y))
        
        ! Stage 5
        k5x = u_func(x + h*(A51*k1x + A52*k2x + A53*k3x + A54*k4x), &
                     y + h*(A51*k1y + A52*k2y + A53*k3y + A54*k4y))
        k5y = v_func(x + h*(A51*k1x + A52*k2x + A53*k3x + A54*k4x), &
                     y + h*(A51*k1y + A52*k2y + A53*k3y + A54*k4y))
        
        ! Stage 6
        k6x = u_func(x + h*(A61*k1x + A62*k2x + A63*k3x + A64*k4x + A65*k5x), &
                     y + h*(A61*k1y + A62*k2y + A63*k3y + A64*k4y + A65*k5y))
        k6y = v_func(x + h*(A61*k1x + A62*k2x + A63*k3x + A64*k4x + A65*k5x), &
                     y + h*(A61*k1y + A62*k2y + A63*k3y + A64*k4y + A65*k5y))
        
        ! 5th order solution
        x5 = x + h*(B1*k1x + B2*k2x + B3*k3x + B4*k4x + B5*k5x + B6*k6x)
        y5 = y + h*(B1*k1y + B2*k2y + B3*k3y + B4*k4y + B5*k5y + B6*k6y)
        
        ! Stage 7 (FSAL - First Same As Last)
        k7x = u_func(x5, y5)
        k7y = v_func(x5, y5)
        
        ! 4th order embedded solution  
        x4 = x + h*(BS1*k1x + BS2*k2x + BS3*k3x + BS4*k4x + BS5*k5x + BS6*k6x + BS7*k7x)
        y4 = y + h*(BS1*k1y + BS2*k2y + BS3*k3y + BS4*k4y + BS5*k5y + BS6*k6y + BS7*k7y)
        
    end subroutine dopri5_step

    real(wp) function calculate_error_estimate(x5, y5, x4, y4, params) result(error_est)
        !! Calculate scaled error estimate for step size control
        real(wp), intent(in) :: x5, y5, x4, y4
        type(integration_params_t), intent(in) :: params
        
        real(wp) :: scale_x, scale_y, error_x, error_y
        
        ! Calculate scaling factors
        scale_x = params%atol + params%rtol * max(abs(x5), abs(x4))
        scale_y = params%atol + params%rtol * max(abs(y5), abs(y4))
        
        ! Calculate scaled errors
        error_x = abs(x5 - x4) / scale_x
        error_y = abs(y5 - y4) / scale_y
        
        ! RMS error estimate
        error_est = sqrt(0.5_wp * (error_x**2 + error_y**2))
        
    end function calculate_error_estimate

    subroutine resize_arrays(path_x, path_y, times, array_size)
        !! Resize arrays when more space is needed
        real(wp), allocatable, intent(inout) :: path_x(:), path_y(:), times(:)
        integer, intent(inout) :: array_size
        
        real(wp), allocatable :: temp_x(:), temp_y(:), temp_t(:)
        integer :: old_size, new_size
        
        old_size = array_size
        new_size = int(old_size * 1.5)  ! Increase by 50%
        
        ! Allocate temporary arrays
        allocate(temp_x(new_size), temp_y(new_size), temp_t(new_size))
        
        ! Copy existing data
        temp_x(1:old_size) = path_x(1:old_size)
        temp_y(1:old_size) = path_y(1:old_size)
        temp_t(1:old_size) = times(1:old_size)
        
        ! Replace originals using move_alloc to avoid manual deallocation
        call move_alloc(from=temp_x, to=path_x)
        call move_alloc(from=temp_y, to=path_y)
        call move_alloc(from=temp_t, to=times)
        
        array_size = new_size
        ! temp_* are now unallocated after move_alloc
        
    end subroutine resize_arrays

    subroutine trim_arrays(path_x, path_y, times, n_points)
        !! Trim arrays to actual used size
        real(wp), allocatable, intent(inout) :: path_x(:), path_y(:), times(:)
        integer, intent(in) :: n_points
        
        real(wp), allocatable :: temp_x(:), temp_y(:), temp_t(:)
        
        ! Allocate temporary arrays of correct size
        allocate(temp_x(n_points), temp_y(n_points), temp_t(n_points))
        
        ! Copy data
        temp_x = path_x(1:n_points)
        temp_y = path_y(1:n_points)
        temp_t = times(1:n_points)
        
        ! Replace originals using move_alloc to avoid manual deallocation
        call move_alloc(from=temp_x, to=path_x)
        call move_alloc(from=temp_y, to=path_y)
        call move_alloc(from=temp_t, to=times)
        
        ! temp_* are now unallocated after move_alloc
        
    end subroutine trim_arrays

end module fortplot_streamline_integrator
