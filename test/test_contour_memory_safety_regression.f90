program test_contour_memory_safety_regression
    !! Test to prevent memory safety regressions in contour plotting
    !! Specifically tests the segfault issue fixed in Issue #401 related to merge() function
    use fortplot
    implicit none
    
    call test_contour_filled_no_optional_args()
    call test_contour_filled_with_colormap_only()
    call test_add_contour_filled_no_optional_args() 
    call test_add_contour_filled_with_colormap_only()
    
    print *, "All contour memory safety regression tests passed!"
    
contains

    subroutine test_contour_filled_no_optional_args()
        !! Test contour_filled with no optional arguments - this caused segfault in Issue #401
        real(wp), dimension(10) :: x_grid, y_grid
        real(wp), dimension(10,10) :: z_grid
        integer :: i, j
        
        ! Generate simple test data
        do i = 1, 10
            x_grid(i) = (i-1) * 0.5_wp
            y_grid(i) = (i-1) * 0.5_wp
        end do
        
        do i = 1, 10
            do j = 1, 10
                z_grid(i,j) = sin(x_grid(i)) * cos(y_grid(j))
            end do
        end do
        
        ! This should NOT segfault - the merge() issue was here
        call figure()
        call contour_filled(x_grid, y_grid, z_grid)  
        
        print *, "✓ contour_filled with no optional args - PASSED"
    end subroutine test_contour_filled_no_optional_args

    subroutine test_contour_filled_with_colormap_only()
        !! Test contour_filled with only colormap argument
        real(wp), dimension(5) :: x_grid, y_grid
        real(wp), dimension(5,5) :: z_grid
        integer :: i, j
        
        ! Generate simple test data
        do i = 1, 5
            x_grid(i) = (i-1) * 1.0_wp
            y_grid(i) = (i-1) * 1.0_wp
        end do
        
        do i = 1, 5
            do j = 1, 5
                z_grid(i,j) = x_grid(i) + y_grid(j)
            end do
        end do
        
        call figure()
        call contour_filled(x_grid, y_grid, z_grid, colormap="plasma")
        
        print *, "✓ contour_filled with colormap only - PASSED"
    end subroutine test_contour_filled_with_colormap_only

    subroutine test_add_contour_filled_no_optional_args()
        !! Test add_contour_filled with no optional arguments 
        real(wp), dimension(8) :: x_grid, y_grid
        real(wp), dimension(8,8) :: z_grid
        integer :: i, j
        
        ! Generate simple test data
        do i = 1, 8
            x_grid(i) = (i-1) * 0.25_wp
            y_grid(i) = (i-1) * 0.25_wp
        end do
        
        do i = 1, 8
            do j = 1, 8
                z_grid(i,j) = exp(-(x_grid(i)**2 + y_grid(j)**2))
            end do
        end do
        
        call figure()
        call add_contour_filled(x_grid, y_grid, z_grid)  
        
        print *, "✓ add_contour_filled with no optional args - PASSED"
    end subroutine test_add_contour_filled_no_optional_args

    subroutine test_add_contour_filled_with_colormap_only()
        !! Test add_contour_filled with only colormap argument
        real(wp), dimension(6) :: x_grid, y_grid
        real(wp), dimension(6,6) :: z_grid
        integer :: i, j
        
        ! Generate simple test data
        do i = 1, 6
            x_grid(i) = (i-1) * 0.4_wp
            y_grid(i) = (i-1) * 0.4_wp
        end do
        
        do i = 1, 6
            do j = 1, 6
                z_grid(i,j) = x_grid(i)**2 - y_grid(j)**2
            end do
        end do
        
        call figure()
        call add_contour_filled(x_grid, y_grid, z_grid, colormap="inferno")
        
        print *, "✓ add_contour_filled with colormap only - PASSED"
    end subroutine test_add_contour_filled_with_colormap_only

end program test_contour_memory_safety_regression