program test_gltf_mesh_generation
    !! Test GLTF mesh generation for 3D plots
    !! Following TDD: Write test first, then implementation
    
    use iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_security, only: safe_remove_file
    implicit none
    
    ! Running GLTF mesh generation tests
    
    call test_gltf_contains_mesh_data()
    call test_gltf_contains_accessor_data()
    call test_gltf_contains_buffer_data()
    call test_gltf_scene_structure()
    
    print *, "All GLTF mesh generation tests passed!"
    
contains

    subroutine test_gltf_contains_mesh_data()
        !! Test that GLTF file contains mesh data
        type(figure_t) :: fig
        real(wp), dimension(3) :: x = [0.0_wp, 1.0_wp, 2.0_wp]
        real(wp), dimension(3) :: y = [0.0_wp, 1.0_wp, 0.0_wp]
        real(wp), dimension(3) :: z = [0.0_wp, 0.5_wp, 1.0_wp]
        character(len=256) :: filename
        character(len=65536) :: file_content
        integer :: unit, iostat
        
        filename = "test_mesh_data.gltf"
        
        call fig%initialize(640, 480)
        call fig%add_3d_plot(x, y, z, label="Test line")
        call fig%savefig(filename)
        
        ! Read entire file
        open(newunit=unit, file=filename, status='old', action='read')
        read(unit, '(A)', iostat=iostat) file_content
        close(unit)
        
        ! Check for mesh structure
        if (index(file_content, '"meshes"') == 0) then
            error stop "GLTF missing meshes array"
        end if
        
        if (index(file_content, '"primitives"') == 0) then
            error stop "GLTF missing primitives in mesh"
        end if
        
        ! Clean up
        block
        logical :: remove_success
        call safe_remove_file(filename, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(filename)
                end if
    end block
        
    end subroutine test_gltf_contains_mesh_data
    
    subroutine test_gltf_contains_accessor_data()
        !! Test that GLTF contains accessor definitions
        type(figure_t) :: fig
        real(wp), dimension(2) :: x = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: z = [0.0_wp, 1.0_wp]
        character(len=256) :: filename
        character(len=65536) :: file_content
        integer :: unit
        
        filename = "test_accessor.gltf"
        
        call fig%initialize(640, 480)
        call fig%add_3d_plot(x, y, z)
        call fig%savefig(filename)
        
        open(newunit=unit, file=filename, status='old', action='read')
        read(unit, '(A)') file_content
        close(unit)
        
        ! Check for accessor structure
        if (index(file_content, '"accessors"') == 0) then
            error stop "GLTF missing accessors array"
        end if
        
        if (index(file_content, '"bufferView"') == 0) then
            error stop "GLTF missing bufferView in accessor"
        end if
        
        if (index(file_content, '"componentType"') == 0) then
            error stop "GLTF missing componentType"
        end if
        
        ! Clean up
        block
        logical :: remove_success
        call safe_remove_file(filename, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(filename)
                end if
    end block
        
    end subroutine test_gltf_contains_accessor_data
    
    subroutine test_gltf_contains_buffer_data()
        !! Test that GLTF contains buffer definitions
        type(figure_t) :: fig
        real(wp), dimension(2) :: x = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: z = [0.0_wp, 1.0_wp]
        character(len=256) :: filename
        character(len=65536) :: file_content
        integer :: unit
        
        filename = "test_buffer.gltf"
        
        call fig%initialize(640, 480)
        call fig%add_3d_plot(x, y, z)
        call fig%savefig(filename)
        
        open(newunit=unit, file=filename, status='old', action='read')
        read(unit, '(A)') file_content
        close(unit)
        
        ! Check for buffer structure
        if (index(file_content, '"buffers"') == 0) then
            error stop "GLTF missing buffers array"
        end if
        
        if (index(file_content, '"bufferViews"') == 0) then
            error stop "GLTF missing bufferViews array"
        end if
        
        if (index(file_content, '"uri"') == 0) then
            error stop "GLTF missing buffer URI"
        end if
        
        ! Clean up
        block
        logical :: remove_success
        call safe_remove_file(filename, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(filename)
                end if
    end block
        
    end subroutine test_gltf_contains_buffer_data
    
    subroutine test_gltf_scene_structure()
        !! Test GLTF scene and node structure
        type(figure_t) :: fig
        real(wp), dimension(2) :: x = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: y = [0.0_wp, 1.0_wp]
        real(wp), dimension(2) :: z = [0.0_wp, 1.0_wp]
        character(len=256) :: filename
        character(len=65536) :: file_content
        integer :: unit
        
        filename = "test_scene.gltf"
        
        call fig%initialize(640, 480)
        call fig%add_3d_plot(x, y, z)
        call fig%savefig(filename)
        
        open(newunit=unit, file=filename, status='old', action='read')
        read(unit, '(A)') file_content
        close(unit)
        
        ! Check for scene structure
        if (index(file_content, '"scene"') == 0) then
            error stop "GLTF missing default scene"
        end if
        
        if (index(file_content, '"scenes"') == 0) then
            error stop "GLTF missing scenes array"
        end if
        
        if (index(file_content, '"nodes"') == 0) then
            error stop "GLTF missing nodes array"
        end if
        
        ! Clean up
        block
        logical :: remove_success
        call safe_remove_file(filename, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(filename)
                end if
    end block
        
    end subroutine test_gltf_scene_structure

end program test_gltf_mesh_generation