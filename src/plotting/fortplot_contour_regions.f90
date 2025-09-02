module fortplot_contour_regions
    !! Contour polygon decomposition for extracting filled regions
    !! 
    !! This module provides functions for decomposing 2D contour data into
    !! polygonal regions between contour levels, supporting filled contour
    !! visualization across PNG, PDF, and ASCII backends.
    !!
    !! = Key Types =
    !! - contour_region_t: Represents a single contour region with boundary
    !! - contour_polygon_t: Polygon boundary with points
    !!
    !! = Core Functions =
    !! - extract_contour_regions: Main polygon decomposition function
    !! - get_region_boundaries: Extract boundary points for regions
    !!
    !! Author: fortplot contributors
    
    use iso_fortran_env, only: wp => real64
    use fortplot_constants, only: EPSILON_GEOMETRY
    implicit none
    
    private
    
    ! Public interface
    public :: contour_region_t, contour_polygon_t
    public :: extract_contour_regions
    
    type :: contour_polygon_t
        !! Polygon boundary with ordered vertex points
        real(wp), allocatable :: x(:)    ! X coordinates of boundary vertices
        real(wp), allocatable :: y(:)    ! Y coordinates of boundary vertices
        logical :: is_closed = .false.   ! Whether polygon is closed
    end type contour_polygon_t
    
    type :: contour_region_t
        !! Region between contour levels with boundary polygons
        real(wp) :: level_min = 0.0_wp   ! Lower contour level
        real(wp) :: level_max = 0.0_wp   ! Upper contour level
        type(contour_polygon_t), allocatable :: boundaries(:)  ! Boundary polygons
        integer :: region_id = 0         ! Unique region identifier
    end type contour_region_t
    
contains

    function extract_contour_regions(x_grid, y_grid, z_grid, levels) result(regions)
        !! Extract polygonal regions between contour levels using marching squares
        !! 
        !! Given: 2D grid data and contour levels
        !! When: Processing grid to find regions between levels
        !! Then: Returns array of regions with boundary polygons
        !!
        !! This is the main function for contour polygon decomposition that
        !! enables filled contour rendering across all backends.
        
        real(wp), intent(in) :: x_grid(:)     ! X coordinates of grid points
        real(wp), intent(in) :: y_grid(:)     ! Y coordinates of grid points
        real(wp), intent(in) :: z_grid(:, :)  ! Z values at grid points
        real(wp), intent(in) :: levels(:)     ! Contour levels to extract
        type(contour_region_t), allocatable :: regions(:)
        
        integer :: n_levels, n_regions
        integer :: i
        
        n_levels = size(levels)
        n_regions = n_levels + 1  ! One more region than levels
        
        ! Allocate regions array
        allocate(regions(n_regions))
        
        ! Create regions between and around contour levels
        do i = 1, n_regions
            regions(i)%region_id = i
            
            if (i == 1) then
                ! Region below first level
                regions(i)%level_min = minval(z_grid) - 1.0_wp
                regions(i)%level_max = levels(1)
            else if (i == n_regions) then
                ! Region above last level
                regions(i)%level_min = levels(n_levels)
                regions(i)%level_max = maxval(z_grid) + 1.0_wp
            else
                ! Region between two levels
                regions(i)%level_min = levels(max(1, i - 1))
                regions(i)%level_max = levels(min(n_levels, i))
            end if
            
            ! Extract boundary polygons for this region
            call extract_region_boundaries(x_grid, y_grid, z_grid, &
                                          regions(i)%level_min, &
                                          regions(i)%level_max, &
                                          regions(i)%boundaries)
        end do
        
    end function extract_contour_regions
    
    subroutine extract_region_boundaries(x_grid, y_grid, z_grid, level_min, level_max, boundaries)
        !! Extract boundary polygons for a single region using marching squares
        !!
        !! This implements a production-quality marching squares algorithm to identify
        !! boundary contours for regions between two contour levels.
        
        real(wp), intent(in) :: x_grid(:)
        real(wp), intent(in) :: y_grid(:)
        real(wp), intent(in) :: z_grid(:, :)
        real(wp), intent(in) :: level_min
        real(wp), intent(in) :: level_max
        type(contour_polygon_t), allocatable, intent(out) :: boundaries(:)
        
        real(wp), allocatable :: contour_x(:), contour_y(:)
        integer :: contour_count
        integer :: nx, ny
        
        nx = size(x_grid)
        ny = size(y_grid)
        
        ! Allocate working arrays for contour points
        allocate(contour_x(2 * (nx + ny)))  ! Conservative estimate
        allocate(contour_y(2 * (nx + ny)))
        contour_count = 0
        
        ! Process grid using marching squares algorithm
        call process_marching_squares(x_grid, y_grid, z_grid, level_min, level_max, &
                                     contour_x, contour_y, contour_count)
        
        ! Create boundary polygon from collected contour points
        call finalize_boundaries(contour_x, contour_y, contour_count, boundaries)
        
    end subroutine extract_region_boundaries
    
    subroutine process_marching_squares(x_grid, y_grid, z_grid, level_min, level_max, &
                                       contour_x, contour_y, contour_count)
        !! Process grid cells using marching squares algorithm
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: level_min, level_max
        real(wp), intent(inout) :: contour_x(:), contour_y(:)
        integer, intent(inout) :: contour_count
        
        integer :: nx, ny, i, j
        integer :: grid_case
        real(wp) :: corner_values(4)
        
        nx = size(x_grid)
        ny = size(y_grid)
        
        ! Process each grid cell
        ! Note: z_grid is indexed as (ny, nx) i.e., (y, x)
        do j = 1, ny - 1
            do i = 1, nx - 1
                ! Get corner values for current cell (y-major indexing)
                corner_values(1) = z_grid(j    , i    )  ! Bottom-left
                corner_values(2) = z_grid(j    , i + 1)  ! Bottom-right
                corner_values(3) = z_grid(j + 1, i + 1)  ! Top-right
                corner_values(4) = z_grid(j + 1, i    )  ! Top-left
                
                ! Calculate marching squares case
                grid_case = calculate_marching_squares_case(corner_values, level_min, level_max)
                
                ! Handle the marching squares case
                call handle_marching_squares_case(grid_case, i, j, corner_values, &
                                                 level_min, level_max, x_grid, y_grid, &
                                                 contour_x, contour_y, contour_count)
            end do
        end do
    end subroutine process_marching_squares
    
    function calculate_marching_squares_case(corner_values, level_min, level_max) result(grid_case)
        !! Calculate marching squares case from corner values
        real(wp), intent(in) :: corner_values(4)
        real(wp), intent(in) :: level_min, level_max
        integer :: grid_case
        
        logical :: corner_mask(4)
        integer :: k
        
        ! Create mask for corners inside the region
        do k = 1, 4
            corner_mask(k) = (corner_values(k) >= level_min .and. corner_values(k) < level_max)
        end do
        
        ! Convert mask to marching squares case (0-15)
        grid_case = 0
        if (corner_mask(1)) grid_case = grid_case + 1
        if (corner_mask(2)) grid_case = grid_case + 2
        if (corner_mask(3)) grid_case = grid_case + 4
        if (corner_mask(4)) grid_case = grid_case + 8
    end function calculate_marching_squares_case
    
    subroutine handle_marching_squares_case(grid_case, i, j, corner_values, level_min, level_max, &
                                           x_grid, y_grid, contour_x, contour_y, contour_count)
        !! Handle marching squares case and add appropriate contour segments
        integer, intent(in) :: grid_case, i, j
        real(wp), intent(in) :: corner_values(4), level_min, level_max
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), intent(inout) :: contour_x(:), contour_y(:)
        integer, intent(inout) :: contour_count
        
        ! Generate contour line segments based on case
        select case (grid_case)
        case (0, 15)
            ! No contour or fully inside - no line segments
            continue
        case (1, 14)
            call add_contour_segment(i, j, 1, 4, corner_values, level_min, level_max, &
                                   x_grid, y_grid, contour_x, contour_y, contour_count)
        case (2, 13)
            call add_contour_segment(i, j, 1, 2, corner_values, level_min, level_max, &
                                   x_grid, y_grid, contour_x, contour_y, contour_count)
        case (3, 12)
            call add_contour_segment(i, j, 2, 4, corner_values, level_min, level_max, &
                                   x_grid, y_grid, contour_x, contour_y, contour_count)
        case (4, 11)
            call add_contour_segment(i, j, 2, 3, corner_values, level_min, level_max, &
                                   x_grid, y_grid, contour_x, contour_y, contour_count)
        case (5, 10)
            ! Saddle case - two line segments
            call add_contour_segment(i, j, 1, 4, corner_values, level_min, level_max, &
                                   x_grid, y_grid, contour_x, contour_y, contour_count)
            call add_contour_segment(i, j, 2, 3, corner_values, level_min, level_max, &
                                   x_grid, y_grid, contour_x, contour_y, contour_count)
        case (6, 9)
            call add_contour_segment(i, j, 1, 3, corner_values, level_min, level_max, &
                                   x_grid, y_grid, contour_x, contour_y, contour_count)
        case (7, 8)
            call add_contour_segment(i, j, 3, 4, corner_values, level_min, level_max, &
                                   x_grid, y_grid, contour_x, contour_y, contour_count)
        end select
    end subroutine handle_marching_squares_case
    
    subroutine finalize_boundaries(contour_x, contour_y, contour_count, boundaries)
        !! Create boundary polygon from collected contour points
        real(wp), intent(in) :: contour_x(:), contour_y(:)
        integer, intent(in) :: contour_count
        type(contour_polygon_t), allocatable, intent(out) :: boundaries(:)
        
        if (contour_count > 0) then
            allocate(boundaries(1))
            allocate(boundaries(1)%x(contour_count + 1))
            allocate(boundaries(1)%y(contour_count + 1))
            
            boundaries(1)%x(1:contour_count) = contour_x(1:contour_count)
            boundaries(1)%y(1:contour_count) = contour_y(1:contour_count)
            
            ! Close the polygon
            boundaries(1)%x(contour_count + 1) = boundaries(1)%x(1)
            boundaries(1)%y(contour_count + 1) = boundaries(1)%y(1)
            boundaries(1)%is_closed = .true.
        else
            ! No contour found - create empty boundary
            allocate(boundaries(0))
        end if
    end subroutine finalize_boundaries
    
    subroutine add_contour_segment(i, j, edge1, edge2, corner_values, level_min, level_max, &
                                  x_grid, y_grid, contour_x, contour_y, contour_count)
        !! Add contour line segment based on edge intersections
        integer, intent(in) :: i, j, edge1, edge2
        real(wp), intent(in) :: corner_values(4), level_min, level_max
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), intent(inout) :: contour_x(:), contour_y(:)
        integer, intent(inout) :: contour_count
        
        real(wp) :: interp_level, t1, t2
        real(wp) :: x1, y1, x2, y2
        
        ! Use middle of level range for interpolation
        interp_level = 0.5_wp * (level_min + level_max)
        
        ! Get first intersection point
        call get_edge_intersection(i, j, edge1, corner_values, interp_level, &
                                  x_grid, y_grid, x1, y1)
        
        ! Get second intersection point
        call get_edge_intersection(i, j, edge2, corner_values, interp_level, &
                                  x_grid, y_grid, x2, y2)
        
        ! Add line segment to contour
        if (contour_count < size(contour_x) - 1) then
            contour_count = contour_count + 1
            contour_x(contour_count) = x1
            contour_y(contour_count) = y1
            
            contour_count = contour_count + 1
            contour_x(contour_count) = x2
            contour_y(contour_count) = y2
        end if
    end subroutine add_contour_segment
    
    subroutine get_edge_intersection(i, j, edge, corner_values, level, &
                                    x_grid, y_grid, x_int, y_int)
        !! Get intersection point on grid cell edge
        integer, intent(in) :: i, j, edge
        real(wp), intent(in) :: corner_values(4), level
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), intent(out) :: x_int, y_int
        
        real(wp) :: t, v1, v2
        
        select case (edge)
        case (1)  ! Bottom edge (corners 1-2)
            v1 = corner_values(1)
            v2 = corner_values(2)
            if (abs(v2 - v1) > EPSILON_GEOMETRY) then
                t = (level - v1) / (v2 - v1)
            else
                t = 0.5_wp
            end if
            x_int = x_grid(i) + t * (x_grid(i+1) - x_grid(i))
            y_int = y_grid(j)
            
        case (2)  ! Right edge (corners 2-3)
            v1 = corner_values(2)
            v2 = corner_values(3)
            if (abs(v2 - v1) > EPSILON_GEOMETRY) then
                t = (level - v1) / (v2 - v1)
            else
                t = 0.5_wp
            end if
            x_int = x_grid(i+1)
            y_int = y_grid(j) + t * (y_grid(j+1) - y_grid(j))
            
        case (3)  ! Top edge (corners 3-4)
            v1 = corner_values(3)
            v2 = corner_values(4)
            if (abs(v2 - v1) > EPSILON_GEOMETRY) then
                t = (level - v1) / (v2 - v1)
            else
                t = 0.5_wp
            end if
            x_int = x_grid(i+1) + t * (x_grid(i) - x_grid(i+1))
            y_int = y_grid(j+1)
            
        case (4)  ! Left edge (corners 4-1)
            v1 = corner_values(4)
            v2 = corner_values(1)
            if (abs(v2 - v1) > EPSILON_GEOMETRY) then
                t = (level - v1) / (v2 - v1)
            else
                t = 0.5_wp
            end if
            x_int = x_grid(i)
            y_int = y_grid(j+1) + t * (y_grid(j) - y_grid(j+1))
            
        case default
            ! Fallback to center of cell
            x_int = 0.5_wp * (x_grid(i) + x_grid(i+1))
            y_int = 0.5_wp * (y_grid(j) + y_grid(j+1))
        end select
    end subroutine get_edge_intersection

end module fortplot_contour_regions
