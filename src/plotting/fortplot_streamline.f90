module fortplot_streamline
    use fortplot_streamline_integrator, only: integration_params_t, dopri5_integrate
    use fortplot_streamline_placement, only: stream_mask_t, coordinate_mapper_t, generate_spiral_seeds
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_logging, only: log_error
    implicit none
    private
    
    public :: calculate_seed_points, calculate_seed_points_matplotlib, integrate_streamline, &
              integrate_streamline_dopri5, integrate_streamline_bidirectional, rk4_step, &
              calculate_arrow_positions, check_termination, bilinear_interpolate, integration_params_t, &
              velocity_function_context_t

    !! Abstract interface for velocity functions
    abstract interface
        real function velocity_function_interface(x, y)
            real, intent(in) :: x, y
        end function velocity_function_interface
    end interface

    !! Context type to hold velocity function pointers (eliminates trampoline need)
    type :: velocity_function_context_t
        procedure(velocity_function_interface), pointer, nopass :: u_func => null()
        procedure(velocity_function_interface), pointer, nopass :: v_func => null()
        logical :: negate_functions = .false.  !! For backward integration
    end type velocity_function_context_t

    !! Module-level context for current velocity functions (thread-unsafe but trampoline-free)
    type(velocity_function_context_t) :: current_velocity_context
    
contains
    
    subroutine calculate_seed_points(x, y, density, seed_x, seed_y, n_seeds)
        real, dimension(:), intent(in) :: x, y
        real, intent(in) :: density
        real, allocatable, intent(out) :: seed_x(:), seed_y(:)
        integer, intent(out) :: n_seeds
        
        integer :: nx, ny, i, j, idx
        real :: dx, dy, spacing
        
        nx = size(x)
        ny = size(y)
        
        spacing = 1.0 / density
        n_seeds = int((nx - 1) * (ny - 1) * density * density)
        
        allocate(seed_x(n_seeds))
        allocate(seed_y(n_seeds))
        
        idx = 0
        do j = 1, int((ny - 1) * density)
            do i = 1, int((nx - 1) * density)
                idx = idx + 1
                if (idx > n_seeds) exit
                
                seed_x(idx) = x(1) + (x(nx) - x(1)) * real(i - 1) / (int((nx - 1) * density) - 1)
                seed_y(idx) = y(1) + (y(ny) - y(1)) * real(j - 1) / (int((ny - 1) * density) - 1)
            end do
        end do
        
        n_seeds = idx
    end subroutine calculate_seed_points
    
    subroutine integrate_streamline(x0, y0, u_func, v_func, dt, max_steps, &
                                   path_x, path_y, n_points)
        real, intent(in) :: x0, y0, dt
        integer, intent(in) :: max_steps
        interface
            real function u_func(x, y)
                real, intent(in) :: x, y
            end function u_func
            real function v_func(x, y)
                real, intent(in) :: x, y
            end function v_func
        end interface
        real, allocatable, intent(out) :: path_x(:), path_y(:)
        integer, intent(out) :: n_points
        
        real :: x, y, x_new, y_new
        integer :: i
        
        allocate(path_x(max_steps))
        allocate(path_y(max_steps))
        
        x = x0
        y = y0
        path_x(1) = x
        path_y(1) = y
        
        do i = 2, max_steps
            call rk4_step(x, y, u_func, v_func, dt, x_new, y_new)
            
            if (sqrt((x_new - x)**2 + (y_new - y)**2) < 1e-6) exit
            
            x = x_new
            y = y_new
            path_x(i) = x
            path_y(i) = y
        end do
        
        n_points = i - 1
    end subroutine integrate_streamline

    subroutine integrate_streamline_dopri5(x0, y0, u_func, v_func, params, max_time, &
                                          path_x, path_y, n_points)
        !! High-accuracy streamline integration using DOPRI5 method
        !! Provides adaptive step size control and superior accuracy
        real, intent(in) :: x0, y0
        interface
            real function u_func(x, y)
                real, intent(in) :: x, y
            end function u_func
            real function v_func(x, y)
                real, intent(in) :: x, y
            end function v_func
        end interface
        type(integration_params_t), intent(in), optional :: params
        real, intent(in), optional :: max_time
        real, allocatable, intent(out) :: path_x(:), path_y(:)
        integer, intent(out) :: n_points
        
        ! Local variables
        type(integration_params_t) :: local_params
        real(wp) :: t_final
        real(wp), allocatable :: times(:), path_x_wp(:), path_y_wp(:)
        integer :: n_accepted, n_rejected
        logical :: success
        
        ! Set default parameters
        if (present(params)) then
            local_params = params
        else
            ! Default parameters optimized for streamline integration
            local_params%rtol = 1.0e-6_wp
            local_params%atol = 1.0e-9_wp
            local_params%h_initial = 0.01_wp
            local_params%h_min = 1.0e-8_wp
            local_params%h_max = 0.5_wp
            local_params%max_steps = 2000
            local_params%safety_factor = 0.9_wp
        end if
        
        ! Set integration time limit
        if (present(max_time)) then
            t_final = real(max_time, wp)
        else
            t_final = 10.0_wp  ! Default time limit
        end if
        
        ! Set up velocity context for module-level wrappers (eliminates trampolines)
        current_velocity_context%u_func => u_func
        current_velocity_context%v_func => v_func
        current_velocity_context%negate_functions = .false.
        
        ! Call DOPRI5 integrator with module-level wrapper functions (no trampolines)
        call dopri5_integrate(real(x0, wp), real(y0, wp), 0.0_wp, t_final, &
                             module_u_func_wrapper, module_v_func_wrapper, &
                             local_params, path_x_wp, path_y_wp, times, &
                             n_points, n_accepted, n_rejected, success)
        
        if (.not. success) then
            ! Fall back to single point if integration fails
            n_points = 1
            allocate(path_x(1), path_y(1))
            path_x(1) = x0
            path_y(1) = y0
            return
        end if
        
        ! Convert back to single precision
        allocate(path_x(n_points), path_y(n_points))
        path_x = real(path_x_wp)
        path_y = real(path_y_wp)
        
        if (allocated(path_x_wp)) deallocate(path_x_wp)
        if (allocated(path_y_wp)) deallocate(path_y_wp)
        if (allocated(times)) deallocate(times)
    end subroutine integrate_streamline_dopri5

    subroutine calculate_seed_points_matplotlib(x, y, density, seed_x, seed_y, n_seeds, mask)
        !! Calculate seed points using matplotlib-compatible spiral algorithm
        !! with collision detection for proper streamline spacing
        real, dimension(:), intent(in) :: x, y
        real(wp), intent(in) :: density
        real, allocatable, intent(out) :: seed_x(:), seed_y(:)
        integer, intent(out) :: n_seeds
        type(stream_mask_t), intent(inout) :: mask
        
        integer, allocatable :: spiral_seeds(:,:)
        integer :: n_spiral_seeds, i, xm, ym
        real(wp) :: xg, yg, xd, yd
        type(coordinate_mapper_t) :: mapper
        
        ! Initialize mask with matplotlib sizing
        call mask%initialize(density)
        
        ! Initialize coordinate mapper
        call mapper%initialize([real(x(1), wp), real(x(size(x)), wp)], &
                              [real(y(1), wp), real(y(size(y)), wp)], &
                              [size(x), size(y)], &
                              [mask%nx, mask%ny])
        
        ! Generate spiral seed points in mask coordinates
        call generate_spiral_seeds([mask%nx, mask%ny], spiral_seeds, n_spiral_seeds)
        
        ! Allocate output arrays (will be trimmed later)
        allocate(seed_x(n_spiral_seeds))
        allocate(seed_y(n_spiral_seeds))
        
        n_seeds = 0
        
        ! Convert spiral seeds to data coordinates and check availability
        ! Use more seeds to make streamlines more visible
        do i = 1, min(n_spiral_seeds, nint(200 * density))
            xm = spiral_seeds(1, i)
            ym = spiral_seeds(2, i)
            
            ! Check if mask position is free
            if (mask%is_free(xm, ym)) then
                ! Convert mask → grid → data coordinates
                call mapper%mask2grid(xm, ym, xg, yg)
                call mapper%grid2data(xg, yg, xd, yd)
                
                ! Check if within data bounds
                if (xd >= x(1) .and. xd <= x(size(x)) .and. &
                    yd >= y(1) .and. yd <= y(size(y))) then
                    
                    n_seeds = n_seeds + 1
                    seed_x(n_seeds) = real(xd)
                    seed_y(n_seeds) = real(yd)
                    
                    ! Don't reserve position yet - will be done during integration
                end if
            end if
        end do
        
        ! Trim arrays to actual size
        if (n_seeds > 0) then
            seed_x = seed_x(1:n_seeds)
            seed_y = seed_y(1:n_seeds)
        end if
        
        if (allocated(spiral_seeds)) deallocate(spiral_seeds)
    end subroutine calculate_seed_points_matplotlib

    subroutine integrate_streamline_bidirectional(x0, y0, u_func, v_func, params, max_time, &
                                                 path_x, path_y, n_points)
        !! Bidirectional streamline integration like matplotlib
        !! Integrates both forward and backward from seed point for complete streamlines
        real, intent(in) :: x0, y0
        interface
            real function u_func(x, y)
                real, intent(in) :: x, y
            end function u_func
            real function v_func(x, y)
                real, intent(in) :: x, y
            end function v_func
        end interface
        type(integration_params_t), intent(in), optional :: params
        real, intent(in), optional :: max_time
        real, allocatable, intent(out) :: path_x(:), path_y(:)
        integer, intent(out) :: n_points
        
        real, allocatable :: forward_x(:), forward_y(:), backward_x(:), backward_y(:)
        integer :: n_forward, n_backward, i
        real :: half_time
        
        ! Use half the time for each direction
        if (present(max_time)) then
            half_time = max_time * 0.5
        else
            half_time = 5.0  ! Default half time
        end if
        
        ! Set up velocity context for forward integration
        current_velocity_context%u_func => u_func
        current_velocity_context%v_func => v_func
        current_velocity_context%negate_functions = .false.
        
        ! Integrate forward direction
        call integrate_streamline_dopri5(x0, y0, u_func, v_func, params, half_time, &
                                        forward_x, forward_y, n_forward)
        
        ! Set up velocity context for backward integration (negate velocity field)
        current_velocity_context%negate_functions = .true.
        
        ! Integrate backward direction (module wrappers will negate functions automatically)
        call integrate_streamline_dopri5(x0, y0, u_func, v_func, params, half_time, &
                                        backward_x, backward_y, n_backward)
        
        ! Combine backward (reversed) + forward trajectories
        n_points = n_backward + n_forward - 1  ! -1 to avoid duplicate start point
        allocate(path_x(n_points), path_y(n_points))
        
        ! Add backward trajectory (reversed order, excluding start point)
        do i = 1, n_backward - 1
            path_x(n_backward - i) = backward_x(i + 1)
            path_y(n_backward - i) = backward_y(i + 1)
        end do
        
        ! Add forward trajectory (starting from seed point)
        do i = 1, n_forward
            path_x(n_backward - 1 + i) = forward_x(i)
            path_y(n_backward - 1 + i) = forward_y(i)
        end do
        
        if (allocated(forward_x)) deallocate(forward_x)
        if (allocated(forward_y)) deallocate(forward_y)
        if (allocated(backward_x)) deallocate(backward_x)
        if (allocated(backward_y)) deallocate(backward_y)
    end subroutine integrate_streamline_bidirectional
    
    subroutine rk4_step(x, y, u_func, v_func, dt, x_new, y_new)
        real, intent(in) :: x, y, dt
        interface
            real function u_func(x, y)
                real, intent(in) :: x, y
            end function u_func
            real function v_func(x, y)
                real, intent(in) :: x, y
            end function v_func
        end interface
        real, intent(out) :: x_new, y_new
        
        real :: k1x, k1y, k2x, k2y, k3x, k3y, k4x, k4y
        
        k1x = u_func(x, y)
        k1y = v_func(x, y)
        
        k2x = u_func(x + 0.5*dt*k1x, y + 0.5*dt*k1y)
        k2y = v_func(x + 0.5*dt*k1x, y + 0.5*dt*k1y)
        
        k3x = u_func(x + 0.5*dt*k2x, y + 0.5*dt*k2y)
        k3y = v_func(x + 0.5*dt*k2x, y + 0.5*dt*k2y)
        
        k4x = u_func(x + dt*k3x, y + dt*k3y)
        k4y = v_func(x + dt*k3x, y + dt*k3y)
        
        x_new = x + dt * (k1x + 2*k2x + 2*k3x + k4x) / 6.0
        y_new = y + dt * (k1y + 2*k2y + 2*k3y + k4y) / 6.0
    end subroutine rk4_step
    
    subroutine calculate_arrow_positions(path_x, path_y, n_points, arrow_density, arrow_mask)
        real, dimension(:), intent(in) :: path_x, path_y
        integer, intent(in) :: n_points
        real, intent(in) :: arrow_density
        logical, allocatable, intent(out) :: arrow_mask(:)
        
        real :: total_length, target_spacing, current_distance
        integer :: i, last_arrow
        
        allocate(arrow_mask(n_points))
        arrow_mask = .false.
        
        if (n_points < 2) return
        
        total_length = 0.0
        do i = 2, n_points
            total_length = total_length + sqrt((path_x(i) - path_x(i-1))**2 + &
                                              (path_y(i) - path_y(i-1))**2)
        end do
        
        target_spacing = total_length / (arrow_density * 10.0)
        
        arrow_mask(max(1, n_points/2)) = .true.
        
        last_arrow = max(1, n_points/2)
        current_distance = 0.0
        
        do i = last_arrow + 1, n_points
            current_distance = current_distance + sqrt((path_x(i) - path_x(i-1))**2 + &
                                                      (path_y(i) - path_y(i-1))**2)
            if (current_distance >= target_spacing) then
                arrow_mask(i) = .true.
                current_distance = 0.0
                last_arrow = i
            end if
        end do
        
        last_arrow = max(1, n_points/2)
        current_distance = 0.0
        
        do i = last_arrow - 1, 1, -1
            current_distance = current_distance + sqrt((path_x(i+1) - path_x(i))**2 + &
                                                      (path_y(i+1) - path_y(i))**2)
            if (current_distance >= target_spacing) then
                arrow_mask(i) = .true.
                current_distance = 0.0
                last_arrow = i
            end if
        end do
    end subroutine calculate_arrow_positions
    
    subroutine check_termination(x, y, x_bounds, y_bounds, terminate)
        real, intent(in) :: x, y
        real, dimension(2), intent(in) :: x_bounds, y_bounds
        logical, intent(out) :: terminate
        
        terminate = (x < x_bounds(1) .or. x > x_bounds(2) .or. &
                    y < y_bounds(1) .or. y > y_bounds(2))
    end subroutine check_termination
    
    subroutine bilinear_interpolate(x_grid, y_grid, values, x, y, result)
        real, dimension(:), intent(in) :: x_grid, y_grid
        real, dimension(:,:), intent(in) :: values
        real, intent(in) :: x, y
        real, intent(out) :: result
        
        integer :: i, j, i1, i2, j1, j2
        real :: fx, fy
        
        ! Find grid indices
        i1 = 1
        i2 = size(x_grid)
        do i = 1, size(x_grid)-1
            if (x >= x_grid(i) .and. x <= x_grid(i+1)) then
                i1 = i
                i2 = i + 1
                exit
            end if
        end do
        
        j1 = 1
        j2 = size(y_grid)
        do j = 1, size(y_grid)-1
            if (y >= y_grid(j) .and. y <= y_grid(j+1)) then
                j1 = j
                j2 = j + 1
                exit
            end if
        end do
        
        ! Handle boundary cases
        if (i1 == i2) then
            if (i1 == 1) i2 = 2
            if (i1 == size(x_grid)) i1 = size(x_grid) - 1
        end if
        if (j1 == j2) then
            if (j1 == 1) j2 = 2
            if (j1 == size(y_grid)) j1 = size(y_grid) - 1
        end if
        
        ! Interpolation factors
        fx = (x - x_grid(i1)) / (x_grid(i2) - x_grid(i1))
        fy = (y - y_grid(j1)) / (y_grid(j2) - y_grid(j1))
        
        ! Bilinear interpolation
        result = values(i1,j1) * (1-fx) * (1-fy) + &
                 values(i2,j1) * fx * (1-fy) + &
                 values(i1,j2) * (1-fx) * fy + &
                 values(i2,j2) * fx * fy
    end subroutine bilinear_interpolate

    !! Module-level wrapper functions to eliminate trampolines
    real(wp) function module_u_func_wrapper(x, y) result(u_vel)
        !! Wrapper for U velocity function - no trampoline since module-level
        real(wp), intent(in) :: x, y
        
        if (.not. associated(current_velocity_context%u_func)) then
            call log_error('streamline: u_func not set in velocity context')
            u_vel = 0.0_wp
            return
        end if
        
        u_vel = real(current_velocity_context%u_func(real(x), real(y)), wp)
        
        if (current_velocity_context%negate_functions) then
            u_vel = -u_vel
        end if
    end function module_u_func_wrapper

    real(wp) function module_v_func_wrapper(x, y) result(v_vel)
        !! Wrapper for V velocity function - no trampoline since module-level  
        real(wp), intent(in) :: x, y
        
        if (.not. associated(current_velocity_context%v_func)) then
            call log_error('streamline: v_func not set in velocity context')
            v_vel = 0.0_wp
            return
        end if
        
        v_vel = real(current_velocity_context%v_func(real(x), real(y)), wp)
        
        if (current_velocity_context%negate_functions) then
            v_vel = -v_vel
        end if
    end function module_v_func_wrapper
    
end module fortplot_streamline
