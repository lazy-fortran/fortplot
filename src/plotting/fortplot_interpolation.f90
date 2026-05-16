module fortplot_interpolation
    !! Shared interpolation utilities for contour plotting
    !!
    !! This module provides bilinear interpolation functions used by
    !! both raster and PDF backends for Z-value interpolation in contour plots.
    !!
    !! Author: fortplot contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: EPSILON_COMPARE
    implicit none
    
    private
    public :: interpolate_z_bilinear
    
contains

    subroutine interpolate_z_bilinear(x_grid, y_grid, z_grid, world_x, world_y, z_value)
        !! Bilinear interpolation of Z value at world coordinates
        !! Refactored to be under 100 lines (QADS compliance)
        real(wp), intent(in) :: x_grid(:)     ! X coordinates of grid points
        real(wp), intent(in) :: y_grid(:)     ! Y coordinates of grid points
        real(wp), intent(in) :: z_grid(:,:)   ! Z values at grid points
        real(wp), intent(in) :: world_x       ! X coordinate to interpolate
        real(wp), intent(in) :: world_y       ! Y coordinate to interpolate
        real(wp), intent(out) :: z_value      ! Interpolated Z value
        
        integer :: i1, i2, j1, j2
        real(wp) :: x1, x2, y1, y2, z11, z12, z21, z22
        
        ! Find grid indices for interpolation
        call find_interpolation_indices(x_grid, y_grid, world_x, world_y, i1, i2, j1, j2)
        
        ! Get coordinates and values at corners
        call get_corner_values(x_grid, y_grid, z_grid, i1, i2, j1, j2, &
                              x1, x2, y1, y2, z11, z12, z21, z22)
        
        ! Perform interpolation
        call perform_bilinear_interpolation(world_x, world_y, x1, x2, y1, y2, &
                                           z11, z12, z21, z22, i1, i2, j1, j2, z_value)
        
    end subroutine interpolate_z_bilinear
    
    subroutine find_interpolation_indices(x_grid, y_grid, world_x, world_y, i1, i2, j1, j2)
        !! Find grid indices for interpolation
        real(wp), intent(in) :: x_grid(:), y_grid(:)
        real(wp), intent(in) :: world_x, world_y
        integer, intent(out) :: i1, i2, j1, j2
        
        integer :: nx, ny
        
        nx = size(x_grid)
        ny = size(y_grid)
        
        ! Find X direction indices
        call find_x_indices(x_grid, world_x, nx, i1, i2)
        
        ! Find Y direction indices
        call find_y_indices(y_grid, world_y, ny, j1, j2)
        
        ! Handle edge cases where indices weren't found
        if (i1 == 0) then
            i1 = 1; i2 = min(2, nx)
        end if
        if (j1 == 0) then
            j1 = 1; j2 = min(2, ny)
        end if
    end subroutine find_interpolation_indices
    
    subroutine find_x_indices(x_grid, world_x, nx, i1, i2)
        !! Find X direction grid indices
        real(wp), intent(in) :: x_grid(:), world_x
        integer, intent(in) :: nx
        integer, intent(out) :: i1, i2
        
        integer :: i
        
        i1 = 0; i2 = 0
        
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
    end subroutine find_x_indices
    
    subroutine find_y_indices(y_grid, world_y, ny, j1, j2)
        !! Find Y direction grid indices
        real(wp), intent(in) :: y_grid(:), world_y
        integer, intent(in) :: ny
        integer, intent(out) :: j1, j2
        
        integer :: j
        
        j1 = 0; j2 = 0
        
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
    end subroutine find_y_indices
    
    subroutine get_corner_values(x_grid, y_grid, z_grid, i1, i2, j1, j2, &
                                 x1, x2, y1, y2, z11, z12, z21, z22)
        !! Get coordinates and values at interpolation corners
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        integer, intent(in) :: i1, i2, j1, j2
        real(wp), intent(out) :: x1, x2, y1, y2, z11, z12, z21, z22
        
        x1 = x_grid(i1); x2 = x_grid(i2)
        y1 = y_grid(j1); y2 = y_grid(j2)
        z11 = z_grid(j1, i1)
        z12 = z_grid(j2, i1)
        z21 = z_grid(j1, i2)
        z22 = z_grid(j2, i2)
    end subroutine get_corner_values
    
    subroutine perform_bilinear_interpolation(world_x, world_y, x1, x2, y1, y2, &
                                             z11, z12, z21, z22, i1, i2, j1, j2, z_value)
        !! Perform the actual bilinear interpolation
        real(wp), intent(in) :: world_x, world_y, x1, x2, y1, y2
        real(wp), intent(in) :: z11, z12, z21, z22
        integer, intent(in) :: i1, i2, j1, j2
        real(wp), intent(out) :: z_value
        
        real(wp) :: dx_norm, dy_norm
        
        if (i1 == i2 .and. j1 == j2) then
            z_value = z11  ! Point coincides with grid point
        else if (i1 == i2) then
            ! Linear interpolation in Y direction only
            if (abs(y2 - y1) > EPSILON_COMPARE) then
                dy_norm = (world_y - y1) / (y2 - y1)
                z_value = z11 + dy_norm * (z12 - z11)
            else
                z_value = z11
            end if
        else if (j1 == j2) then
            ! Linear interpolation in X direction only
            if (abs(x2 - x1) > EPSILON_COMPARE) then
                dx_norm = (world_x - x1) / (x2 - x1)
                z_value = z11 + dx_norm * (z21 - z11)
            else
                z_value = z11
            end if
        else
            ! Full bilinear interpolation
            call compute_full_bilinear(world_x, world_y, x1, x2, y1, y2, &
                                      z11, z12, z21, z22, z_value)
        end if
    end subroutine perform_bilinear_interpolation
    
    subroutine compute_full_bilinear(world_x, world_y, x1, x2, y1, y2, &
                                     z11, z12, z21, z22, z_value)
        !! Compute full bilinear interpolation
        real(wp), intent(in) :: world_x, world_y, x1, x2, y1, y2
        real(wp), intent(in) :: z11, z12, z21, z22
        real(wp), intent(out) :: z_value
        
        real(wp) :: dx_norm, dy_norm
        
        if (abs(x2 - x1) > EPSILON_COMPARE .and. abs(y2 - y1) > EPSILON_COMPARE) then
            dx_norm = (world_x - x1) / (x2 - x1)
            dy_norm = (world_y - y1) / (y2 - y1)
            
            z_value = z11 * (1.0_wp - dx_norm) * (1.0_wp - dy_norm) + &
                     z21 * dx_norm * (1.0_wp - dy_norm) + &
                     z12 * (1.0_wp - dx_norm) * dy_norm + &
                     z22 * dx_norm * dy_norm
        else
            z_value = z11
        end if
    end subroutine compute_full_bilinear

end module fortplot_interpolation
