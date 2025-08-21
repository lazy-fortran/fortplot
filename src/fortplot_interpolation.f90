module fortplot_interpolation
    !! Shared interpolation utilities for contour plotting
    !!
    !! This module provides bilinear interpolation functions used by
    !! both raster and PDF backends for Z-value interpolation in contour plots.
    !!
    !! Author: fortplot contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: interpolate_z_bilinear
    
contains

    subroutine interpolate_z_bilinear(x_grid, y_grid, z_grid, world_x, world_y, z_value)
        !! Bilinear interpolation of Z value at world coordinates
        !!
        !! Given: Grid coordinates and query point
        !! When: Point falls within or outside grid bounds
        !! Then: Returns interpolated Z value using bilinear interpolation
        !!
        !! Handles edge cases with linear interpolation when point coincides
        !! with grid boundaries or falls outside grid domain.
        
        real(wp), intent(in) :: x_grid(:)     ! X coordinates of grid points
        real(wp), intent(in) :: y_grid(:)     ! Y coordinates of grid points
        real(wp), intent(in) :: z_grid(:,:)   ! Z values at grid points
        real(wp), intent(in) :: world_x       ! X coordinate to interpolate
        real(wp), intent(in) :: world_y       ! Y coordinate to interpolate
        real(wp), intent(out) :: z_value      ! Interpolated Z value
        
        integer :: nx, ny, i, j, i1, i2, j1, j2
        real(wp) :: x1, x2, y1, y2, dx_norm, dy_norm
        real(wp) :: z11, z12, z21, z22
        
        nx = size(x_grid)
        ny = size(y_grid)
        
        ! Initialize indices to avoid compiler warnings
        i1 = 0; i2 = 0
        j1 = 0; j2 = 0
        
        ! Find grid indices for interpolation - X direction
        if (world_x <= x_grid(1)) then
            i1 = 1; i2 = 1
        else if (world_x >= x_grid(nx)) then
            i1 = nx; i2 = nx
        else
            do i = 1, nx - 1
                if (world_x >= x_grid(i) .and. world_x <= x_grid(i + 1)) then
                    i1 = i; i2 = i + 1
                    exit
                end if
            end do
        end if
        
        ! Find grid indices for interpolation - Y direction
        if (world_y <= y_grid(1)) then
            j1 = 1; j2 = 1
        else if (world_y >= y_grid(ny)) then
            j1 = ny; j2 = ny
        else
            do j = 1, ny - 1
                if (world_y >= y_grid(j) .and. world_y <= y_grid(j + 1)) then
                    j1 = j; j2 = j + 1
                    exit
                end if
            end do
        end if
        
        ! Handle edge cases where indices weren't found
        if (i1 == 0) then
            i1 = 1; i2 = min(2, nx)
        end if
        if (j1 == 0) then
            j1 = 1; j2 = min(2, ny)
        end if
        
        ! Get coordinates and values at corners
        x1 = x_grid(i1); x2 = x_grid(i2)
        y1 = y_grid(j1); y2 = y_grid(j2)
        z11 = z_grid(i1, j1)
        z12 = z_grid(i1, j2)
        z21 = z_grid(i2, j1)
        z22 = z_grid(i2, j2)
        
        ! Bilinear interpolation with special cases
        if (i1 == i2 .and. j1 == j2) then
            ! Point coincides with grid point
            z_value = z11
        else if (i1 == i2) then
            ! Linear interpolation in Y direction only
            if (abs(y2 - y1) > 1e-10_wp) then
                dy_norm = (world_y - y1) / (y2 - y1)
                z_value = z11 + dy_norm * (z12 - z11)
            else
                z_value = z11
            end if
        else if (j1 == j2) then
            ! Linear interpolation in X direction only
            if (abs(x2 - x1) > 1e-10_wp) then
                dx_norm = (world_x - x1) / (x2 - x1)
                z_value = z11 + dx_norm * (z21 - z11)
            else
                z_value = z11
            end if
        else
            ! Full bilinear interpolation
            if (abs(x2 - x1) > 1e-10_wp .and. abs(y2 - y1) > 1e-10_wp) then
                dx_norm = (world_x - x1) / (x2 - x1)
                dy_norm = (world_y - y1) / (y2 - y1)
                
                z_value = z11 * (1.0_wp - dx_norm) * (1.0_wp - dy_norm) + &
                         z21 * dx_norm * (1.0_wp - dy_norm) + &
                         z12 * (1.0_wp - dx_norm) * dy_norm + &
                         z22 * dx_norm * dy_norm
            else
                z_value = z11
            end if
        end if
        
    end subroutine interpolate_z_bilinear

end module fortplot_interpolation