program test_add_surface_api
    !! Test add_surface API behavior and grid validation
    !! Following TDD: Write test first, then implementation
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    implicit none
    
    call test_add_surface_basic()
    call test_add_surface_with_label()
    call test_surface_grid_validation()
    call test_surface_data_storage()
    
    print *, "All add_surface API tests passed!"
    
contains

    subroutine test_add_surface_basic()
        !! Test basic add_surface functionality
        type(figure_t) :: fig
        real(wp), dimension(4) :: x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), dimension(3) :: y = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(4,3) :: z
        integer :: i, j
        
        ! Create test surface data (paraboloid)
        do i = 1, 4
            do j = 1, 3
                z(i,j) = x(i)**2 + y(j)**2
            end do
        end do
        
        call fig%initialize(640, 480)
        
        ! Add surface plot
        call fig%add_surface(x, y, z)
        
        ! Assert
        if (fig%plot_count /= 1) then
            error stop "add_surface should increment plot count"
        end if
        
        ! Check that it's a surface plot (will need to add surface detection)
        if (.not. allocated(fig%plots(1)%z_grid)) then
            error stop "Surface plot should have z_grid allocated"
        end if
        
        if (.not. fig%plots(1)%is_3d()) then
            error stop "Surface plot should be detected as 3D"
        end if
        
    end subroutine test_add_surface_basic
    
    subroutine test_add_surface_with_label()
        !! Test add_surface with label parameter
        type(figure_t) :: fig
        real(wp), dimension(2) :: x = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(2,2) :: z = reshape([0.0_wp, 1.0_wp, 1.0_wp, 2.0_wp], [2,2])
        
        call fig%initialize(640, 480)
        
        ! Add with label
        call fig%add_surface(x, y, z, label="Test surface")
        
        ! Assert
        if (fig%plots(1)%label /= "Test surface") then
            error stop "Label not set correctly"
        end if
        
    end subroutine test_add_surface_with_label
    
    subroutine test_surface_grid_validation()
        !! Test that surface grid dimensions are validated
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(2,2) :: z_wrong = reshape([0.0_wp, 1.0_wp, 1.0_wp, 2.0_wp], [2,2])
        real(wp), dimension(3,2) :: z_correct
        
        call fig%initialize(640, 480)
        
        ! This should fail validation (wrong dimensions)
        call fig%add_surface(x, y, z_wrong)
        
        ! Should not add plot due to dimension mismatch
        if (fig%plot_count /= 0) then
            error stop "Should not add surface with mismatched dimensions"
        end if
        
        ! This should succeed
        z_correct = 0.0_wp
        call fig%add_surface(x, y, z_correct)
        
        if (fig%plot_count /= 1) then
            error stop "Should add surface with correct dimensions"
        end if
        
    end subroutine test_surface_grid_validation
    
    subroutine test_surface_data_storage()
        !! Test that surface data is stored correctly
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [0.0_wp, 0.5_wp, 1.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(3,2) :: z
        
        ! Create simple test data
        z(:,1) = [0.0_wp, 0.25_wp, 1.0_wp]
        z(:,2) = [1.0_wp, 1.25_wp, 2.0_wp]
        
        call fig%initialize(640, 480)
        call fig%add_surface(x, y, z)
        
        ! Assert data storage
        if (.not. allocated(fig%plots(1)%x_grid)) then
            error stop "x_grid should be allocated"
        end if
        
        if (.not. allocated(fig%plots(1)%y_grid)) then
            error stop "y_grid should be allocated"
        end if
        
        if (.not. allocated(fig%plots(1)%z_grid)) then
            error stop "z_grid should be allocated"
        end if
        
        if (size(fig%plots(1)%x_grid) /= 3) then
            error stop "x_grid size incorrect"
        end if
        
        if (size(fig%plots(1)%z_grid, 1) /= 3 .or. size(fig%plots(1)%z_grid, 2) /= 2) then
            error stop "z_grid dimensions incorrect"
        end if
        
        if (abs(fig%plots(1)%z_grid(2,1) - 0.25_wp) > 1e-10_wp) then
            error stop "z_grid values not stored correctly"
        end if
        
    end subroutine test_surface_data_storage

end program test_add_surface_api