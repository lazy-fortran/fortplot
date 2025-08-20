program test_gltf_buffer_packing
    !! Test GLTF buffer packing for vertex and index data
    !! Following TDD: Write test first, then implementation
    
    use iso_fortran_env, only: wp => real64, real32, int16
    use fortplot_gltf_buffer, only: pack_vertex_data, pack_index_data, &
                                   create_accessor, create_buffer_view
    use fortplot_gltf_base, only: gltf_accessor_t, gltf_buffer_view_t
    implicit none
    
    ! Running GLTF buffer packing tests
    
    call test_vertex_data_packing()
    call test_index_data_packing()
    call test_accessor_creation()
    call test_buffer_view_creation()
    
    print *, "All GLTF buffer packing tests passed!"
    
contains

    subroutine test_vertex_data_packing()
        !! Test packing 3D vertex data into byte buffer
        real(wp), dimension(3) :: x = [1.0_wp, 2.0_wp, 3.0_wp]
        real(wp), dimension(3) :: y = [4.0_wp, 5.0_wp, 6.0_wp]
        real(wp), dimension(3) :: z = [7.0_wp, 8.0_wp, 9.0_wp]
        integer(1), allocatable :: buffer(:)
        integer :: expected_size
        real(wp) :: test_value
        
        ! Act - Pack vertex data
        call pack_vertex_data(x, y, z, buffer)
        
        ! Assert
        ! 3 vertices * 3 components * 4 bytes (real32) = 36 bytes
        expected_size = 3 * 3 * 4
        if (size(buffer) /= expected_size) then
            error stop "Buffer size incorrect"
        end if
        
        ! Check first vertex x-component (bytes 0-3)
        test_value = real(transfer(buffer(1:4), 1.0_real32), wp)
        if (abs(test_value - 1.0_wp) > 1e-6_wp) then
            error stop "First vertex X component incorrect"
        end if
        
    end subroutine test_vertex_data_packing
    
    subroutine test_index_data_packing()
        !! Test packing triangle indices into byte buffer
        integer, dimension(6) :: indices = [0, 1, 2, 2, 3, 0]  ! Two triangles
        integer(1), allocatable :: buffer(:)
        integer :: expected_size
        integer(2) :: test_index  ! uint16
        
        ! Act - Pack index data
        call pack_index_data(indices, buffer)
        
        ! Assert
        ! 6 indices * 2 bytes (uint16) = 12 bytes
        expected_size = 6 * 2
        if (size(buffer) /= expected_size) then
            error stop "Index buffer size incorrect"
        end if
        
        ! Check first index (bytes 0-1)
        test_index = transfer(buffer(1:2), 0_int16)
        if (test_index /= 0) then
            error stop "First index incorrect"
        end if
        
    end subroutine test_index_data_packing
    
    subroutine test_accessor_creation()
        !! Test creation of GLTF accessor metadata
        type(gltf_accessor_t) :: accessor
        integer :: buffer_view_idx = 0
        integer :: count = 10
        real(wp), dimension(3) :: min_vals = [-1.0_wp, -2.0_wp, -3.0_wp]
        real(wp), dimension(3) :: max_vals = [1.0_wp, 2.0_wp, 3.0_wp]
        
        ! Act
        call create_accessor(accessor, buffer_view_idx, count, &
                           "VEC3", "FLOAT", min_vals, max_vals)
        
        ! Assert
        if (accessor%buffer_view /= buffer_view_idx) then
            error stop "Buffer view index not set"
        end if
        
        if (accessor%count /= count) then
            error stop "Count not set"
        end if
        
        if (accessor%type /= "VEC3") then
            error stop "Type not set correctly"
        end if
        
        if (accessor%component_type /= 5126) then  ! FLOAT = 5126
            error stop "Component type not set correctly"
        end if
        
    end subroutine test_accessor_creation
    
    subroutine test_buffer_view_creation()
        !! Test creation of GLTF buffer view metadata
        type(gltf_buffer_view_t) :: buffer_view
        integer :: buffer_idx = 0
        integer :: byte_offset = 100
        integer :: byte_length = 256
        
        ! Act
        call create_buffer_view(buffer_view, buffer_idx, &
                               byte_offset, byte_length)
        
        ! Assert
        if (buffer_view%buffer /= buffer_idx) then
            error stop "Buffer index not set"
        end if
        
        if (buffer_view%byte_offset /= byte_offset) then
            error stop "Byte offset not set"
        end if
        
        if (buffer_view%byte_length /= byte_length) then
            error stop "Byte length not set"
        end if
        
    end subroutine test_buffer_view_creation

end program test_gltf_buffer_packing