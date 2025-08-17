program test_gltf_savefig
    !! Test savefig functionality for GLTF export
    !! Following TDD: Write test first, then implementation
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    implicit none
    
    call test_savefig_gltf_3d_line()
    call test_savefig_gltf_surface()
    call test_mixed_2d_3d_export()
    call test_gltf_file_structure()
    
    print *, "All GLTF savefig tests passed!"
    
contains

    subroutine test_savefig_gltf_3d_line()
        !! Test saving 3D line plot as GLTF
        type(figure_t) :: fig
        real(wp), dimension(5) :: x = [0.0_wp, 1.0_wp, 2.0_wp, 3.0_wp, 4.0_wp]
        real(wp), dimension(5) :: y = [0.0_wp, 1.0_wp, 0.0_wp, -1.0_wp, 0.0_wp]
        real(wp), dimension(5) :: z = [0.0_wp, 0.5_wp, 1.0_wp, 0.5_wp, 0.0_wp]
        character(len=256) :: filename
        logical :: file_exists
        
        filename = "test_3d_line.gltf"
        
        ! Create figure with 3D data
        call fig%initialize(640, 480)
        call fig%add_3d_plot(x, y, z, label="Test 3D line")
        
        ! Save as GLTF
        call fig%savefig(filename)
        
        ! Check file exists
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            error stop "GLTF file not created"
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // trim(filename))
        
    end subroutine test_savefig_gltf_3d_line
    
    subroutine test_savefig_gltf_surface()
        !! Test saving surface plot as GLTF
        type(figure_t) :: fig
        real(wp), dimension(3) :: x_grid = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(2) :: y_grid = [0.0_wp, 1.0_wp]
        real(wp), dimension(3,2) :: z_grid
        character(len=256) :: filename
        logical :: file_exists
        
        filename = "test_surface.gltf"
        
        ! Create surface data
        z_grid(:,1) = [0.0_wp, 1.0_wp, 0.0_wp]
        z_grid(:,2) = [1.0_wp, 2.0_wp, 1.0_wp]
        
        call fig%initialize(640, 480)
        call fig%add_surface(x_grid, y_grid, z_grid, label="Test surface")
        
        ! Save as GLTF
        call fig%savefig(filename)
        
        ! Check file exists
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            error stop "GLTF file not created for surface"
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // trim(filename))
        
    end subroutine test_savefig_gltf_surface
    
    subroutine test_mixed_2d_3d_export()
        !! Test that mixed 2D/3D plots trigger GLTF export
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(3) :: y = [0.0_wp, 1.0_wp, 0.0_wp]
        real(wp), dimension(3) :: z = [0.0_wp, 0.5_wp, 1.0_wp]
        character(len=256) :: filename
        logical :: file_exists
        
        filename = "test_mixed.gltf"
        
        call fig%initialize(640, 480)
        
        ! Add 2D plot
        call fig%add_plot(x, y, label="2D plot")
        
        ! Add 3D plot
        call fig%add_3d_plot(x, y, z, label="3D plot")
        
        ! Save - should detect 3D and use GLTF
        call fig%savefig(filename)
        
        inquire(file=filename, exist=file_exists)
        if (.not. file_exists) then
            error stop "GLTF not created for mixed 2D/3D"
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // trim(filename))
        
    end subroutine test_mixed_2d_3d_export
    
    subroutine test_gltf_file_structure()
        !! Test that GLTF file contains valid JSON structure
        type(figure_t) :: fig
        real(wp), dimension(2) :: x = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: z = [0.0_wp, 1.0_wp]
        character(len=256) :: filename
        character(len=1024) :: file_content
        integer :: unit, iostat
        
        filename = "test_structure.gltf"
        
        call fig%initialize(640, 480)
        call fig%add_3d_plot(x, y, z)
        call fig%savefig(filename)
        
        ! Read file content
        open(newunit=unit, file=filename, status='old', action='read')
        read(unit, '(A)', iostat=iostat) file_content
        close(unit)
        
        ! Check for GLTF structure
        if (index(file_content, '"asset"') == 0) then
            error stop "GLTF missing asset property"
        end if
        
        if (index(file_content, '"version"') == 0) then
            error stop "GLTF missing version"
        end if
        
        ! Clean up
        call execute_command_line('rm -f ' // trim(filename))
        
    end subroutine test_gltf_file_structure

end program test_gltf_savefig