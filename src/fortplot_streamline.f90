module fortplot_streamline
    implicit none
    private
    
    public :: calculate_seed_points, integrate_streamline, rk4_step, &
              calculate_arrow_positions, check_termination, bilinear_interpolate
    
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
    
end module fortplot_streamline