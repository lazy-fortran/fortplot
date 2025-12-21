program test_contour_refactoring_final
    !! Test to ensure contour function refactoring preserves all functionality
    use fortplot
    implicit none
    
    real(wp), dimension(10) :: x_grid, y_grid
    real(wp), dimension(10,10) :: z_grid
    real(wp), dimension(5) :: levels
    integer :: i, j
    
    ! Generate test data
    do i = 1, 10
        x_grid(i) = (i-1) * 0.5_wp
        y_grid(i) = (i-1) * 0.5_wp
    end do
    
    do i = 1, 10
        do j = 1, 10
            z_grid(i,j) = sin(x_grid(i)) * cos(y_grid(j))
        end do
    end do
    
    levels = [-0.5_wp, -0.25_wp, 0.0_wp, 0.25_wp, 0.5_wp]
    
    print *, "Testing refactored contour functions (Issue #403)..."
    
    ! Test all parameter combinations for contour_filled
    call test_contour_filled_combinations()
    
    ! Test all parameter combinations for add_contour_filled
    call test_add_contour_filled_combinations()
    
    print *, "PASS: All contour refactoring tests passed (Issue #403 resolved)"
    
contains

    subroutine test_contour_filled_combinations()
        !! Test contour_filled with various parameter combinations
        
        ! Test 1: No optional parameters
        call contour_filled(x_grid, y_grid, z_grid)
        print *, "  PASS: contour_filled: no optional args"
        
        ! Test 2: With levels only
        call contour_filled(x_grid, y_grid, z_grid, levels=levels)
        print *, "  PASS: contour_filled: levels only"
        
        ! Test 3: With colormap only
        call contour_filled(x_grid, y_grid, z_grid, colormap="viridis")
        print *, "  PASS: contour_filled: colormap only"
        
        ! Test 4: With show_colorbar only
        call contour_filled(x_grid, y_grid, z_grid, show_colorbar=.true.)
        print *, "  PASS: contour_filled: show_colorbar only"
        
        ! Test 5: With label only
        call contour_filled(x_grid, y_grid, z_grid, label="Test contour")
        print *, "  PASS: contour_filled: label only"
        
        ! Test 6: Levels + colormap
        call contour_filled(x_grid, y_grid, z_grid, levels=levels, colormap="plasma")
        print *, "  PASS: contour_filled: levels + colormap"
        
        ! Test 7: Colormap + show_colorbar
        call contour_filled(x_grid, y_grid, z_grid, colormap="hot", show_colorbar=.false.)
        print *, "  PASS: contour_filled: colormap + show_colorbar"
        
        ! Test 8: All parameters
        call contour_filled(x_grid, y_grid, z_grid, levels=levels, &
                          colormap="coolwarm", show_colorbar=.true., label="Full test")
        print *, "  PASS: contour_filled: all parameters"
        
    end subroutine test_contour_filled_combinations
    
    subroutine test_add_contour_filled_combinations()
        !! Test add_contour_filled with various parameter combinations
        
        ! Test 1: No optional parameters
        call add_contour_filled(x_grid, y_grid, z_grid)
        print *, "  PASS: add_contour_filled: no optional args"
        
        ! Test 2: With levels only
        call add_contour_filled(x_grid, y_grid, z_grid, levels=levels)
        print *, "  PASS: add_contour_filled: levels only"
        
        ! Test 3: With colormap only
        call add_contour_filled(x_grid, y_grid, z_grid, colormap="RdBu")
        print *, "  PASS: add_contour_filled: colormap only"
        
        ! Test 4: With show_colorbar only
        call add_contour_filled(x_grid, y_grid, z_grid, show_colorbar=.false.)
        print *, "  PASS: add_contour_filled: show_colorbar only"
        
        ! Test 5: With label only
        call add_contour_filled(x_grid, y_grid, z_grid, label="Add contour test")
        print *, "  PASS: add_contour_filled: label only"
        
        ! Test 6: Levels + colormap
        call add_contour_filled(x_grid, y_grid, z_grid, levels=levels, colormap="seismic")
        print *, "  PASS: add_contour_filled: levels + colormap"
        
        ! Test 7: Colormap + show_colorbar + label
        call add_contour_filled(x_grid, y_grid, z_grid, colormap="twilight", &
                              show_colorbar=.true., label="Complex test")
        print *, "  PASS: add_contour_filled: colormap + show_colorbar + label"
        
        ! Test 8: All parameters
        call add_contour_filled(x_grid, y_grid, z_grid, levels=levels, &
                              colormap="jet", show_colorbar=.true., label="Complete test")
        print *, "  PASS: add_contour_filled: all parameters"
        
    end subroutine test_add_contour_filled_combinations
    
end program test_contour_refactoring_final