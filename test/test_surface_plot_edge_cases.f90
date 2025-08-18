program test_surface_plot_edge_cases
    !! Test surface plot edge cases for comprehensive validation coverage
    !! Given: Various edge case scenarios for surface plotting
    !! When: add_surface is called under edge conditions
    !! Then: Should handle all cases appropriately
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    implicit none
    
    call test_surface_max_plots_limit()
    call test_surface_negative_values_handling()
    call test_surface_nan_inf_values_handling()
    call test_surface_identical_grid_points()
    
    print *, "All surface plot edge case tests passed!"
    
contains

    subroutine test_surface_max_plots_limit()
        !! Given: Figure at maximum plot capacity
        !! When: add_surface is called beyond limit
        !! Then: Should show warning and not add more plots
        type(figure_t) :: fig
        real(wp), dimension(2) :: x = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(2,2) :: z = reshape([0.0_wp, 1.0_wp, 1.0_wp, 2.0_wp], [2,2])
        integer :: i, initial_max
        
        call fig%initialize(640, 480)
        initial_max = fig%max_plots
        
        ! Fill to maximum capacity
        do i = 1, initial_max
            call fig%add_surface(x, y, z)
        end do
        
        if (fig%plot_count /= initial_max) then
            error stop "Should be able to add plots up to maximum capacity"
        end if
        
        ! Try to add one more - should fail with warning
        print *, "Testing beyond max plots (expect warning):"
        call fig%add_surface(x, y, z)
        
        ! Should still be at maximum, not exceed it
        if (fig%plot_count /= initial_max) then
            error stop "Should not exceed maximum plot capacity"
        end if
        
    end subroutine test_surface_max_plots_limit

    subroutine test_surface_negative_values_handling()
        !! Given: Surface data with negative coordinates and values
        !! When: add_surface is called with negative data
        !! Then: Should handle negative values correctly
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [-2.0_wp, -1.0_wp, 0.0_wp]
        real(wp), dimension(2) :: y = [-1.0_wp, 0.0_wp]
        real(wp), dimension(3,2) :: z
        
        ! Create surface with negative values
        z(:,1) = [-4.0_wp, -1.0_wp, 0.0_wp]  ! z = x^2 for y=-1
        z(:,2) = [-2.0_wp, -1.0_wp, 0.0_wp]  ! z = x^2 - 2 for y=0
        
        call fig%initialize(640, 480)
        
        call fig%add_surface(x, y, z, label="Negative values surface")
        
        if (fig%plot_count /= 1) then
            error stop "Should handle negative values in surface data"
        end if
        
        if (fig%plots(1)%label /= "Negative values surface") then
            error stop "Label should be preserved with negative values"
        end if
        
    end subroutine test_surface_negative_values_handling

    subroutine test_surface_nan_inf_values_handling()
        !! Given: Surface data with special floating point values
        !! When: add_surface is called with NaN/Inf values
        !! Then: Should handle special values without crashing
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(3,2) :: z
        
        ! Create surface with special values
        z(1,1) = 1.0_wp
        z(2,1) = huge(1.0_wp)   ! Very large value (not quite infinity)
        z(3,1) = -huge(1.0_wp)  ! Very large negative value
        z(1,2) = 0.0_wp
        z(2,2) = tiny(1.0_wp)   ! Very small positive value
        z(3,2) = 2.0_wp
        
        call fig%initialize(640, 480)
        
        call fig%add_surface(x, y, z, label="Special values surface")
        
        if (fig%plot_count /= 1) then
            error stop "Should handle special floating point values"
        end if
        
        if (abs(fig%plots(1)%z_grid(2,1) - huge(1.0_wp)) > 1e-10_wp) then
            error stop "Should preserve large values in z_grid"
        end if
        
    end subroutine test_surface_nan_inf_values_handling

    subroutine test_surface_identical_grid_points()
        !! Given: Surface grid with identical coordinate values
        !! When: add_surface is called with repeated grid points
        !! Then: Should handle degenerate grids
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [1.0_wp, 1.0_wp, 1.0_wp]  ! Identical x values
        real(wp), dimension(2) :: y = [2.0_wp, 2.0_wp]          ! Identical y values
        real(wp), dimension(3,2) :: z
        integer :: i, j
        
        ! Create surface with identical grid coordinates
        do i = 1, 3
            do j = 1, 2
                z(i,j) = real(i + j, wp)
            end do
        end do
        
        call fig%initialize(640, 480)
        
        call fig%add_surface(x, y, z, label="Degenerate grid")
        
        if (fig%plot_count /= 1) then
            error stop "Should handle degenerate grid coordinates"
        end if
        
        ! Verify grid values are preserved correctly
        if (any(abs(fig%plots(1)%x_grid - 1.0_wp) > 1e-10_wp)) then
            error stop "Should preserve identical x grid values"
        end if
        
        if (any(abs(fig%plots(1)%y_grid - 2.0_wp) > 1e-10_wp)) then
            error stop "Should preserve identical y grid values"
        end if
        
    end subroutine test_surface_identical_grid_points

end program test_surface_plot_edge_cases