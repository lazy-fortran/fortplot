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
        
        ! Collect boundary segments for the band using robust per-edge thresholding
        allocate(contour_x(0))
        allocate(contour_y(0))
        contour_count = 0

        call process_band_segments(x_grid, y_grid, z_grid, level_min, level_max, &
                                   contour_x, contour_y, contour_count)

        call finalize_boundaries(contour_x, contour_y, contour_count, boundaries)
        
    end subroutine extract_region_boundaries

    subroutine process_band_segments(x_grid, y_grid, z_grid, level_min, level_max, &
                                     contour_x, contour_y, contour_count)
        !! Process each cell to extract boundary segments for the band [level_min, level_max)
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: level_min, level_max
        real(wp), intent(inout) :: contour_x(:), contour_y(:)
        integer, intent(inout) :: contour_count

        integer :: nx, ny, i, j
        real(wp) :: z(4), x(4), y(4)
        real(wp) :: px(8), py(8)   ! up to 2 points per edge -> 8 points max across 4 edges
        integer :: pcount

        nx = size(x_grid)
        ny = size(y_grid)

        do j = 1, ny - 1
            do i = 1, nx - 1
                ! Corner ordering: 1=BL, 2=BR, 3=TR, 4=TL
                x(1) = x_grid(i)  ; y(1) = y_grid(j)  ; z(1) = z_grid(j    , i    )
                x(2) = x_grid(i+1); y(2) = y_grid(j)  ; z(2) = z_grid(j    , i + 1)
                x(3) = x_grid(i+1); y(3) = y_grid(j+1); z(3) = z_grid(j + 1, i + 1)
                x(4) = x_grid(i)  ; y(4) = y_grid(j+1); z(4) = z_grid(j + 1, i    )

                call collect_cell_band_intersections(x, y, z, level_min, level_max, px, py, pcount)

                if (pcount == 2) then
                    call append_segment(contour_x, contour_y, contour_count, px(1), py(1), px(2), py(2))
                else if (pcount == 4) then
                    ! Points are ordered around perimeter; connect (1-2) and (3-4)
                    call append_segment(contour_x, contour_y, contour_count, px(1), py(1), px(2), py(2))
                    call append_segment(contour_x, contour_y, contour_count, px(3), py(3), px(4), py(4))
                end if
            end do
        end do
    end subroutine process_band_segments

    subroutine collect_cell_band_intersections(x, y, z, level_min, level_max, px, py, pcount)
        !! For a single cell, collect band boundary edge intersections in perimeter order
        real(wp), intent(in) :: x(4), y(4), z(4)
        real(wp), intent(in) :: level_min, level_max
        real(wp), intent(out) :: px(8), py(8)
        integer, intent(out) :: pcount

        integer :: e
        pcount = 0

        do e = 1, 4
            call add_edge_band_intersections(e, x, y, z, level_min, level_max, px, py, pcount)
        end do
    end subroutine collect_cell_band_intersections

    subroutine add_edge_band_intersections(edge, x, y, z, level_min, level_max, px, py, pcount)
        !! Add 0, 1, or 2 intersections for a cell edge, ordered along perimeter
        integer, intent(in) :: edge
        real(wp), intent(in) :: x(4), y(4), z(4)
        real(wp), intent(in) :: level_min, level_max
        real(wp), intent(inout) :: px(8), py(8)
        integer, intent(inout) :: pcount

        integer :: a, b
        real(wp) :: z1, z2, t
        real(wp) :: x1, y1, x2, y2

        select case (edge)
        case (1)  ! bottom: 1->2
            a = 1; b = 2
        case (2)  ! right: 2->3
            a = 2; b = 3
        case (3)  ! top: 3->4
            a = 3; b = 4
        case (4)  ! left: 4->1
            a = 4; b = 1
        end select

        z1 = z(a); z2 = z(b)
        x1 = x(a); y1 = y(a)
        x2 = x(b); y2 = y(b)

        ! Crossing at lower threshold?
        if ((z1 < level_min .and. z2 >= level_min) .or. (z2 < level_min .and. z1 >= level_min)) then
            if (abs(z2 - z1) > EPSILON_GEOMETRY) then
                t = (level_min - z1) / (z2 - z1)
                pcount = pcount + 1
                px(pcount) = x1 + t * (x2 - x1)
                py(pcount) = y1 + t * (y2 - y1)
            end if
        end if

        ! Crossing at upper threshold? (may be a second point on same edge)
        if ((z1 < level_max .and. z2 >= level_max) .or. (z2 < level_max .and. z1 >= level_max)) then
            if (abs(z2 - z1) > EPSILON_GEOMETRY) then
                t = (level_max - z1) / (z2 - z1)
                pcount = pcount + 1
                px(pcount) = x1 + t * (x2 - x1)
                py(pcount) = y1 + t * (y2 - y1)
            end if
        end if
    end subroutine add_edge_band_intersections

    subroutine append_segment(cx, cy, cc, x1, y1, x2, y2)
        real(wp), intent(inout), allocatable :: cx(:), cy(:)
        integer, intent(inout) :: cc
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp), allocatable :: tx(:), ty(:)
        integer :: n

        n = size(cx)
        allocate(tx(n + 2))
        allocate(ty(n + 2))
        if (n > 0) then
            tx(1:n) = cx
            ty(1:n) = cy
        end if
        tx(n + 1) = x1; ty(n + 1) = y1
        tx(n + 2) = x2; ty(n + 2) = y2
        call move_alloc(tx, cx)
        call move_alloc(ty, cy)
        cc = cc + 2
    end subroutine append_segment

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
        !! Create one or more boundary polygons by chaining contour segments
        real(wp), intent(in) :: contour_x(:), contour_y(:)
        integer, intent(in) :: contour_count
        type(contour_polygon_t), allocatable, intent(out) :: boundaries(:)

        integer :: nseg, k, i_poly, max_pts
        real(wp), allocatable :: xs(:), ys(:)          ! segment endpoints flattened [x1,y1,x2,y2] per segment
        logical, allocatable :: used(:)
        real(wp), allocatable :: px(:), py(:)
        integer :: npts, s
        real(wp) :: sx, sy, cx, cy, nxp, nyp

        ! Internal helpers are defined in the CONTAINS block below

        ! No segments collected
        if (contour_count < 2) then
            allocate(boundaries(0))
            return
        end if

        ! Interpret consecutive point pairs as independent segments
        nseg = contour_count / 2
        allocate(xs(2*nseg))
        allocate(ys(2*nseg))
        allocate(used(nseg))
        used = .false.
        do k = 1, nseg
            xs(2*k-1) = contour_x(2*k-1)
            ys(2*k-1) = contour_y(2*k-1)
            xs(2*k  ) = contour_x(2*k  )
            ys(2*k  ) = contour_y(2*k  )
        end do

        ! We do not know polygon count upfront
        allocate(boundaries(0))
        i_poly = 0
        max_pts = contour_count + 1
        allocate(px(max_pts))
        allocate(py(max_pts))

        do
            ! Find next unused segment to start a chain
            s = 0
            do k = 1, nseg
                if (.not. used(k)) then
                    s = k
                    exit
                end if
            end do
            if (s == 0) exit  ! no more segments

            ! Initialize chain with segment s
            sx = xs(2*s-1); sy = ys(2*s-1)
            cx = xs(2*s  ); cy = ys(2*s  )
            used(s) = .true.
            npts = 2
            px(1) = sx; py(1) = sy
            px(2) = cx; py(2) = cy

            ! Greedily connect subsequent segments by matching endpoints
            do
                ! Closed loop formed?
                if (points_close(cx, cy, sx, sy)) then
                    npts = npts + 1
                    px(npts) = sx
                    py(npts) = sy
                    exit
                end if

                ! Search for a segment connecting to current endpoint
                s = 0
                do k = 1, nseg
                    if (used(k)) cycle
                    if (points_close(xs(2*k-1), ys(2*k-1), cx, cy)) then
                        nxp = xs(2*k  ); nyp = ys(2*k  )
                        s = k
                        exit
                    else if (points_close(xs(2*k  ), ys(2*k  ), cx, cy)) then
                        nxp = xs(2*k-1); nyp = ys(2*k-1)
                        s = k
                        exit
                    end if
                end do

                if (s == 0) then
                    ! Could not close chain; discard open polyline
                    npts = 0
                    exit
                end if

                ! Append next point and continue
                used(s) = .true.
                cx = nxp; cy = nyp
                npts = npts + 1
                if (npts > max_pts) exit
                px(npts) = cx
                py(npts) = cy
            end do

            ! If a closed polygon was formed with >= 4 points (including closure), store it
            if (npts >= 4) then
                call append_boundary(boundaries, px, py, npts)
            end if
        end do

        if (.not. allocated(boundaries)) then
            allocate(boundaries(0))
        end if

        contains

        logical function points_close(ax, ay, bx, by)
            real(wp), intent(in) :: ax, ay, bx, by
            real(wp) :: dx, dy
            dx = ax - bx
            dy = ay - by
            points_close = (abs(dx) <= EPSILON_GEOMETRY .and. abs(dy) <= EPSILON_GEOMETRY)
        end function points_close

        subroutine append_boundary(arr, vx, vy, n)
            type(contour_polygon_t), allocatable, intent(inout) :: arr(:)
            real(wp), intent(in) :: vx(:), vy(:)
            integer, intent(in) :: n
            type(contour_polygon_t), allocatable :: tmp(:)
            integer :: oldn

            oldn = size(arr)
            allocate(tmp(oldn + 1))
            if (oldn > 0) tmp(1:oldn) = arr
            call move_alloc(tmp, arr)

            allocate(arr(oldn + 1)%x(n))
            allocate(arr(oldn + 1)%y(n))
            arr(oldn + 1)%x(1:n) = vx(1:n)
            arr(oldn + 1)%y(1:n) = vy(1:n)
            arr(oldn + 1)%is_closed = .true.
        end subroutine append_boundary
    end subroutine finalize_boundaries
    
    subroutine add_contour_segment(i, j, edge1, edge2, corner_values, level_min, level_max, &
                                  x_grid, y_grid, contour_x, contour_y, contour_count)
        !! Add contour line segment based on edge intersections
        integer, intent(in) :: i, j, edge1, edge2
        real(wp), intent(in) :: corner_values(4), level_min, level_max
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), intent(inout) :: contour_x(:), contour_y(:)
        integer, intent(inout) :: contour_count
        
        real(wp) :: interp_level
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
