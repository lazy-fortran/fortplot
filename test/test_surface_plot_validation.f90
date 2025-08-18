program test_surface_plot_validation
    !! Test surface plot API validation for Issue #99
    !! Given: Current implementation has dimension validation
    !! When: Various dimension combinations are tested
    !! Then: Validation should work correctly
    
    use iso_fortran_env, only: wp => real64, output_unit
    use fortplot_figure_core, only: figure_t
    implicit none
    
    call test_surface_dimension_validation_working()
    call test_surface_empty_arrays_validation()
    call test_surface_single_point_edge_case()
    call test_surface_large_grid_validation()
    call test_surface_validation_error_messages()
    call test_surface_valid_scenarios_work()
    
    print *, "All surface plot validation tests passed!"
    
contains

    subroutine test_surface_dimension_validation_working()
        !! Given: Surface plot with mismatched dimensions
        !! When: add_surface is called with wrong dimensions
        !! Then: Should not add plot and show error message
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(2,3) :: z_wrong  ! Wrong: should be (3,2) not (2,3)
        
        z_wrong = reshape([1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp, 5.0_wp, 6.0_wp], [2,3])
        
        call fig%initialize(640, 480)
        
        ! This should fail validation due to dimension mismatch
        call fig%add_surface(x, y, z_wrong)
        
        ! Assert: Should not add plot due to dimension mismatch
        if (fig%plot_count /= 0) then
            error stop "Surface with wrong dimensions should not be added"
        end if
        
    end subroutine test_surface_dimension_validation_working

    subroutine test_surface_empty_arrays_validation()
        !! Given: Empty arrays for surface plot
        !! When: add_surface is called with empty arrays
        !! Then: Should handle gracefully (passes dimension validation but adds empty plot)
        type(figure_t) :: fig
        real(wp), dimension(0) :: x_empty, y_empty
        real(wp), dimension(0,0) :: z_empty
        
        call fig%initialize(640, 480)
        
        ! This should handle empty arrays gracefully - they pass validation
        ! because size(z,1) == size(x) == 0 and size(z,2) == size(y) == 0
        call fig%add_surface(x_empty, y_empty, z_empty)
        
        ! Empty arrays technically pass validation and get added as a plot
        if (fig%plot_count /= 1) then
            error stop "Empty surface passes validation and should be added"
        end if
        
        ! Verify the empty plot has correct empty allocations
        if (.not. allocated(fig%plots(1)%z_grid)) then
            error stop "Empty surface should have z_grid allocated (though empty)"
        end if
        
        if (size(fig%plots(1)%z_grid, 1) /= 0 .or. size(fig%plots(1)%z_grid, 2) /= 0) then
            error stop "Empty surface should have zero-sized z_grid"
        end if
        
    end subroutine test_surface_empty_arrays_validation

    subroutine test_surface_single_point_edge_case()
        !! Given: Single point surface plot
        !! When: add_surface is called with single point
        !! Then: Should work with correct dimensions
        type(figure_t) :: fig
        real(wp), dimension(1) :: x = [0.0_wp]
        real(wp), dimension(1) :: y = [0.0_wp]
        real(wp), dimension(1,1) :: z = reshape([1.0_wp], [1,1])
        
        call fig%initialize(640, 480)
        
        ! Single point with correct dimensions should work
        call fig%add_surface(x, y, z)
        
        ! Should successfully add the single-point surface
        if (fig%plot_count /= 1) then
            error stop "Single-point surface should be added successfully"
        end if
        
    end subroutine test_surface_single_point_edge_case

    subroutine test_surface_large_grid_validation()
        !! Given: Large grid surface plot with various wrong dimensions
        !! When: add_surface is called with large mismatched grids
        !! Then: Should validate dimensions correctly
        type(figure_t) :: fig
        real(wp), dimension(100) :: x_large
        real(wp), dimension(50) :: y_large
        real(wp), dimension(50,100) :: z_wrong_order  ! Wrong order
        real(wp), dimension(100,50) :: z_correct
        integer :: i, j
        
        ! Initialize arrays
        do i = 1, 100
            x_large(i) = real(i, wp)
        end do
        
        do i = 1, 50
            y_large(i) = real(i, wp)
        end do
        
        do i = 1, 50
            do j = 1, 100
                z_wrong_order(i,j) = real(i + j, wp)
            end do
        end do
        
        do i = 1, 100
            do j = 1, 50
                z_correct(i,j) = real(i + j, wp)
            end do
        end do
        
        call fig%initialize(640, 480)
        
        ! Test wrong order dimensions
        call fig%add_surface(x_large, y_large, z_wrong_order)
        if (fig%plot_count /= 0) then
            error stop "Large surface with wrong dimensions should not be added"
        end if
        
        ! Test correct dimensions
        call fig%add_surface(x_large, y_large, z_correct)
        if (fig%plot_count /= 1) then
            error stop "Large surface with correct dimensions should be added"
        end if
        
    end subroutine test_surface_large_grid_validation

    subroutine test_surface_validation_error_messages()
        !! Given: Surface plot with dimension mismatches
        !! When: add_surface is called with various wrong dimensions
        !! Then: Should output appropriate error messages (this test documents behavior)
        type(figure_t) :: fig
        real(wp), dimension(4) :: x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), dimension(3) :: y = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(3,4) :: z_wrong_both  ! Wrong: should be (4,3)
        real(wp), dimension(5,3) :: z_wrong_x     ! Wrong x dimension
        real(wp), dimension(4,2) :: z_wrong_y     ! Wrong y dimension
        
        z_wrong_both = 1.0_wp
        z_wrong_x = 2.0_wp  
        z_wrong_y = 3.0_wp
        
        call fig%initialize(640, 480)
        
        ! Test wrong both dimensions - should show error message
        print *, "Testing wrong both dimensions (expect error message):"
        call fig%add_surface(x, y, z_wrong_both)
        
        ! Test wrong x dimension - should show error message
        print *, "Testing wrong x dimension (expect error message):"
        call fig%add_surface(x, y, z_wrong_x)
        
        ! Test wrong y dimension - should show error message  
        print *, "Testing wrong y dimension (expect error message):"
        call fig%add_surface(x, y, z_wrong_y)
        
        ! No plots should have been added due to validation failures
        if (fig%plot_count /= 0) then
            error stop "No plots should be added when validation fails"
        end if
        
    end subroutine test_surface_validation_error_messages

    subroutine test_surface_valid_scenarios_work()
        !! Given: Various valid surface plot configurations
        !! When: add_surface is called with correct dimensions
        !! Then: Should successfully add surfaces
        type(figure_t) :: fig
        
        ! Test case 1: Small square grid
        real(wp), dimension(2) :: x1 = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: y1 = [0.0_wp, 1.0_wp]
        real(wp), dimension(2,2) :: z1 = reshape([0.0_wp, 1.0_wp, 1.0_wp, 2.0_wp], [2,2])
        
        ! Test case 2: Rectangular grid
        real(wp), dimension(3) :: x2 = [0.0_wp, 0.5_wp, 1.0_wp]
        real(wp), dimension(4) :: y2 = [0.0_wp, 0.33_wp, 0.67_wp, 1.0_wp]
        real(wp), dimension(3,4) :: z2
        
        ! Test case 3: Different rectangular grid
        real(wp), dimension(5) :: x3 = [0.0_wp, 0.25_wp, 0.5_wp, 0.75_wp, 1.0_wp]
        real(wp), dimension(2) :: y3 = [0.0_wp, 1.0_wp]
        real(wp), dimension(5,2) :: z3
        
        integer :: i, j
        
        ! Initialize z2
        do i = 1, 3
            do j = 1, 4
                z2(i,j) = real(i, wp) * real(j, wp)
            end do
        end do
        
        ! Initialize z3
        do i = 1, 5
            do j = 1, 2
                z3(i,j) = real(i, wp) + real(j, wp)
            end do
        end do
        
        call fig%initialize(640, 480)
        
        ! All these should succeed
        call fig%add_surface(x1, y1, z1, label="Small square")
        call fig%add_surface(x2, y2, z2, label="Rectangular 3x4")
        call fig%add_surface(x3, y3, z3, label="Rectangular 5x2")
        
        ! Should have added 3 plots successfully
        if (fig%plot_count /= 3) then
            error stop "All valid surface configurations should be added"
        end if
        
        ! Check that data is stored correctly for first plot
        if (.not. allocated(fig%plots(1)%z_grid)) then
            error stop "First surface should have z_grid allocated"
        end if
        
        if (size(fig%plots(1)%z_grid, 1) /= 2 .or. size(fig%plots(1)%z_grid, 2) /= 2) then
            error stop "First surface should have correct z_grid dimensions"
        end if
        
        if (fig%plots(1)%label /= "Small square") then
            error stop "First surface should have correct label"
        end if
        
        ! Check second plot dimensions
        if (size(fig%plots(2)%z_grid, 1) /= 3 .or. size(fig%plots(2)%z_grid, 2) /= 4) then
            error stop "Second surface should have correct z_grid dimensions"
        end if
        
        ! Check third plot dimensions  
        if (size(fig%plots(3)%z_grid, 1) /= 5 .or. size(fig%plots(3)%z_grid, 2) /= 2) then
            error stop "Third surface should have correct z_grid dimensions"
        end if
        
    end subroutine test_surface_valid_scenarios_work

end program test_surface_plot_validation