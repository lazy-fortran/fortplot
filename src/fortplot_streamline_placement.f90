module fortplot_streamline_placement
    !! Streamline placement algorithms for matplotlib compatibility
    !! 
    !! Implements matplotlib's approach:
    !! - StreamMask for collision detection (30x30 base grid scaled by density)
    !! - Spiral seed point generation starting from boundaries
    !! - Coordinate system mapping between data, grid, and mask coordinates
    !! 
    !! Following SOLID principles with single responsibility for placement logic
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    private
    
    public :: stream_mask_t, coordinate_mapper_t, generate_spiral_seeds
    
    !! StreamMask for collision detection - equivalent to matplotlib's StreamMask
    type :: stream_mask_t
        integer :: nx, ny                    !! Mask dimensions
        integer, allocatable :: mask(:,:)    !! Collision mask (0=free, 1=occupied)
        integer, allocatable :: trajectory(:,:) !! Current trajectory points for undo
        integer :: traj_length = 0          !! Length of current trajectory
        integer :: current_x = -1, current_y = -1  !! Current position
    contains
        procedure :: initialize => mask_initialize
        procedure :: is_free => mask_is_free
        procedure :: start_trajectory => mask_start_trajectory
        procedure :: update_trajectory => mask_update_trajectory
        procedure :: try_update_trajectory => mask_try_update_trajectory
        procedure :: undo_trajectory => mask_undo_trajectory
    end type stream_mask_t
    
    !! Coordinate mapper for data ↔ grid ↔ mask transformations
    type :: coordinate_mapper_t
        real(wp) :: x_min, x_max, y_min, y_max  !! Data bounds
        integer :: grid_nx, grid_ny             !! Grid dimensions
        integer :: mask_nx, mask_ny             !! Mask dimensions
        real(wp) :: x_grid2mask, y_grid2mask    !! Grid to mask scaling
        real(wp) :: x_mask2grid, y_mask2grid    !! Mask to grid scaling
        real(wp) :: x_data2grid, y_data2grid    !! Data to grid scaling
    contains
        procedure :: initialize => mapper_initialize
        procedure :: data2grid => mapper_data2grid
        procedure :: grid2data => mapper_grid2data
        procedure :: mask2grid => mapper_mask2grid
        procedure :: grid2mask => mapper_grid2mask
    end type coordinate_mapper_t
    
contains

    subroutine mask_initialize(self, density)
        !! Initialize StreamMask with matplotlib-compatible sizing
        !! density=1 → 30x30, density=2 → 60x60, etc.
        class(stream_mask_t), intent(inout) :: self
        real(wp), intent(in) :: density
        
        ! Calculate mask dimensions: 30x30 base scaled by density
        self%nx = nint(30.0_wp * density)
        self%ny = nint(30.0_wp * density)
        
        ! Allocate mask array initialized to 0 (free)
        if (allocated(self%mask)) deallocate(self%mask)
        allocate(self%mask(self%ny, self%nx))
        self%mask = 0
        
        ! Allocate trajectory tracking
        if (allocated(self%trajectory)) deallocate(self%trajectory)
        allocate(self%trajectory(2, self%nx * self%ny))  ! Max possible trajectory length
        self%traj_length = 0
        self%current_x = -1
        self%current_y = -1
    end subroutine mask_initialize

    logical function mask_is_free(self, x, y) result(is_free)
        !! Check if mask position is free for streamline placement
        class(stream_mask_t), intent(in) :: self
        integer, intent(in) :: x, y
        
        ! Check bounds
        if (x < 1 .or. x > self%nx .or. y < 1 .or. y > self%ny) then
            is_free = .false.
            return
        end if
        
        is_free = (self%mask(y, x) == 0)
    end function mask_is_free

    subroutine mask_start_trajectory(self, x, y)
        !! Start recording new streamline trajectory
        class(stream_mask_t), intent(inout) :: self
        integer, intent(in) :: x, y
        
        self%traj_length = 0
        call self%update_trajectory(x, y)
    end subroutine mask_start_trajectory

    subroutine mask_update_trajectory(self, x, y)
        !! Update trajectory position and mark mask
        class(stream_mask_t), intent(inout) :: self
        integer, intent(in) :: x, y
        
        ! Skip if same position
        if (self%current_x == x .and. self%current_y == y) return
        
        ! Check if position is available
        if (.not. self%is_free(x, y)) return
        
        ! Mark position as occupied
        self%mask(y, x) = 1
        
        ! Record in trajectory for potential undo
        self%traj_length = self%traj_length + 1
        self%trajectory(1, self%traj_length) = x
        self%trajectory(2, self%traj_length) = y
        
        self%current_x = x
        self%current_y = y
    end subroutine mask_update_trajectory

    logical function mask_try_update_trajectory(self, x, y) result(success)
        !! Try to update trajectory, return false if collision occurs
        class(stream_mask_t), intent(inout) :: self
        integer, intent(in) :: x, y
        
        ! Skip if same position
        if (self%current_x == x .and. self%current_y == y) then
            success = .true.
            return
        end if
        
        ! Check if position is available
        if (.not. self%is_free(x, y)) then
            success = .false.
            return
        end if
        
        ! Mark position as occupied
        self%mask(y, x) = 1
        
        ! Record in trajectory for potential undo
        self%traj_length = self%traj_length + 1
        self%trajectory(1, self%traj_length) = x
        self%trajectory(2, self%traj_length) = y
        
        self%current_x = x
        self%current_y = y
        success = .true.
    end function mask_try_update_trajectory

    subroutine mask_undo_trajectory(self)
        !! Remove current trajectory from mask (for short/bad streamlines)
        class(stream_mask_t), intent(inout) :: self
        integer :: i, x, y
        
        do i = 1, self%traj_length
            x = self%trajectory(1, i)
            y = self%trajectory(2, i)
            self%mask(y, x) = 0
        end do
        
        self%traj_length = 0
        self%current_x = -1
        self%current_y = -1
    end subroutine mask_undo_trajectory

    subroutine mapper_initialize(self, x_bounds, y_bounds, grid_shape, mask_shape)
        !! Initialize coordinate mapper for data ↔ grid ↔ mask conversions
        class(coordinate_mapper_t), intent(inout) :: self
        real(wp), intent(in) :: x_bounds(2), y_bounds(2)
        integer, intent(in) :: grid_shape(2), mask_shape(2)
        
        self%x_min = x_bounds(1)
        self%x_max = x_bounds(2)
        self%y_min = y_bounds(1)
        self%y_max = y_bounds(2)
        
        self%grid_nx = grid_shape(1)
        self%grid_ny = grid_shape(2)
        self%mask_nx = mask_shape(1)
        self%mask_ny = mask_shape(2)
        
        ! Calculate scaling factors
        self%x_grid2mask = real(self%mask_nx - 1, wp) / real(self%grid_nx - 1, wp)
        self%y_grid2mask = real(self%mask_ny - 1, wp) / real(self%grid_ny - 1, wp)
        self%x_mask2grid = 1.0_wp / self%x_grid2mask
        self%y_mask2grid = 1.0_wp / self%y_grid2mask
        
        self%x_data2grid = real(self%grid_nx - 1, wp) / (self%x_max - self%x_min)
        self%y_data2grid = real(self%grid_ny - 1, wp) / (self%y_max - self%y_min)
    end subroutine mapper_initialize

    subroutine mapper_data2grid(self, x_data, y_data, x_grid, y_grid)
        !! Convert from data coordinates to grid coordinates
        class(coordinate_mapper_t), intent(in) :: self
        real(wp), intent(in) :: x_data, y_data
        real(wp), intent(out) :: x_grid, y_grid
        
        x_grid = (x_data - self%x_min) * self%x_data2grid
        y_grid = (y_data - self%y_min) * self%y_data2grid
    end subroutine mapper_data2grid

    subroutine mapper_grid2data(self, x_grid, y_grid, x_data, y_data)
        !! Convert from grid coordinates to data coordinates
        class(coordinate_mapper_t), intent(in) :: self
        real(wp), intent(in) :: x_grid, y_grid
        real(wp), intent(out) :: x_data, y_data
        
        x_data = self%x_min + x_grid / self%x_data2grid
        y_data = self%y_min + y_grid / self%y_data2grid
    end subroutine mapper_grid2data

    subroutine mapper_mask2grid(self, x_mask, y_mask, x_grid, y_grid)
        !! Convert from mask coordinates to grid coordinates
        class(coordinate_mapper_t), intent(in) :: self
        integer, intent(in) :: x_mask, y_mask
        real(wp), intent(out) :: x_grid, y_grid
        
        x_grid = real(x_mask - 1, wp) * self%x_mask2grid
        y_grid = real(y_mask - 1, wp) * self%y_mask2grid
    end subroutine mapper_mask2grid

    subroutine mapper_grid2mask(self, x_grid, y_grid, x_mask, y_mask)
        !! Convert from grid coordinates to mask coordinates (nearest)
        class(coordinate_mapper_t), intent(in) :: self
        real(wp), intent(in) :: x_grid, y_grid
        integer, intent(out) :: x_mask, y_mask
        
        x_mask = nint(x_grid * self%x_grid2mask) + 1
        y_mask = nint(y_grid * self%y_grid2mask) + 1
    end subroutine mapper_grid2mask

    subroutine generate_spiral_seeds(mask_shape, seed_points, n_seeds)
        !! Generate seed points in spiral pattern starting from boundary
        !! Implements matplotlib's _gen_starting_points algorithm
        integer, intent(in) :: mask_shape(2)
        integer, allocatable, intent(out) :: seed_points(:,:)
        integer, intent(out) :: n_seeds
        
        integer :: nx, ny, x, y, i
        integer :: xfirst, yfirst, xlast, ylast
        character(len=5) :: direction
        
        nx = mask_shape(1)
        ny = mask_shape(2)
        
        ! Allocate for maximum possible seeds
        allocate(seed_points(2, nx * ny))
        
        ! Initialize spiral parameters
        xfirst = 1
        yfirst = 2
        xlast = nx
        ylast = ny
        x = 1
        y = 1
        direction = 'right'
        
        n_seeds = 0
        
        ! Generate spiral sequence
        do i = 1, nx * ny
            n_seeds = n_seeds + 1
            seed_points(1, n_seeds) = x
            seed_points(2, n_seeds) = y
            
            ! Move in current direction
            select case (direction)
            case ('right')
                x = x + 1
                if (x >= xlast) then
                    xlast = xlast - 1
                    direction = 'up'
                end if
            case ('up')
                y = y + 1
                if (y >= ylast) then
                    ylast = ylast - 1
                    direction = 'left'
                end if
            case ('left')
                x = x - 1
                if (x <= xfirst) then
                    xfirst = xfirst + 1
                    direction = 'down'
                end if
            case ('down')
                y = y - 1
                if (y <= yfirst) then
                    yfirst = yfirst + 1
                    direction = 'right'
                end if
            end select
        end do
        
        ! Trim to actual size
        seed_points = seed_points(:, 1:n_seeds)
    end subroutine generate_spiral_seeds

end module fortplot_streamline_placement