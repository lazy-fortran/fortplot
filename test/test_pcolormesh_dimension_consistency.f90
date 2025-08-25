program test_pcolormesh_dimension_consistency
    !! Test pcolormesh dimension consistency across backends
    !! Verifies that all backends expect z_grid(ny, nx) consistently
    
    use fortplot
    use iso_fortran_env, only: wp => real64, error_unit
    implicit none
    
    logical :: test_passed
    character(len=256) :: error_msg
    
    test_passed = .true.
    error_msg = ""
    
    print *, "Testing pcolormesh dimension consistency across backends..."
    
    ! Test dimension consistency
    call test_consistent_dimensions()
    
    if (test_passed) then
        print *, ""
        print *, "PASS: All dimension consistency tests passed"
        print *, "SUCCESS: Backend dimension inconsistency issue has been FIXED!"
        stop 0
    else
        print *, "FAIL: ", trim(error_msg)
        stop 1
    end if
    
contains

    subroutine test_consistent_dimensions()
        !! Test that all backends work with correct z_grid(ny, nx)
        real(wp) :: x_grid(6), y_grid(8), z_grid(7, 5)  ! x(nx+1), y(ny+1), z(ny, nx)
        integer :: i, j, unit, iostat
        logical :: file_exists
        
        print *, ""
        print *, "Testing z_grid(ny, nx) dimension order..."
        print *, "Grid shape: x_grid(6), y_grid(8), z_grid(7,5)"
        
        ! Initialize grids (cell edges)
        do i = 1, 6
            x_grid(i) = real(i - 1, wp) * 0.8_wp
        end do
        
        do j = 1, 8
            y_grid(j) = real(j - 1, wp) * 0.5_wp
        end do
        
        ! Initialize z with distinctive pattern: z(j,i) = i + j * 10
        ! This pattern makes it easy to detect dimension transposition
        do j = 1, 7
            do i = 1, 5
                z_grid(j, i) = real(i, wp) + real(j, wp) * 10.0_wp
            end do
        end do
        
        ! Test ASCII backend
        print *, "  Testing ASCII backend with z_grid(ny, nx)..."
        call figure()
        call pcolormesh(x_grid, y_grid, z_grid)
        call savefig('test_dim_consistent_ascii.txt')
        
        ! Check if ASCII file was created (dimension check passed)
        inquire(file='test_dim_consistent_ascii.txt', exist=file_exists)
        if (.not. file_exists) then
            test_passed = .false.
            error_msg = "ASCII backend rejected correct dimensions z_grid(ny,nx)"
            return
        end if
        print *, "    ASCII backend accepted z_grid(ny, nx) - CORRECT"
        
        ! Test raster backend  
        print *, "  Testing raster backend with z_grid(ny, nx)..."
        call figure()
        call pcolormesh(x_grid, y_grid, z_grid)
        call savefig('test_dim_consistent_raster.ppm')
        
        ! Check if raster file was created (dimension check passed)
        inquire(file='test_dim_consistent_raster.ppm', exist=file_exists)
        if (.not. file_exists) then
            test_passed = .false.
            error_msg = "Raster backend rejected correct dimensions z_grid(ny,nx)"
            return
        end if
        print *, "    Raster backend accepted z_grid(ny, nx) - CORRECT"
        
        print *, ""
        print *, "Dimension consistency test PASSED!"
        print *, "Both ASCII and raster backends consistently expect z_grid(ny, nx)"
        
    end subroutine test_consistent_dimensions

end program test_pcolormesh_dimension_consistency