module fortplot_gltf_buffer
    !! Module for packing data into GLTF binary buffers
    !! Following SRP - each routine handles specific buffer operation
    !! Following KISS - straightforward byte packing
    
    use iso_fortran_env, only: wp => real64, int16, real32
    use fortplot_gltf_base, only: gltf_accessor_t, gltf_buffer_view_t, &
                                 GLTF_COMPONENT_TYPE_FLOAT, &
                                 GLTF_COMPONENT_TYPE_UNSIGNED_SHORT
    implicit none
    
    private
    public :: pack_vertex_data
    public :: pack_index_data
    public :: create_accessor
    public :: create_buffer_view
    
contains

    subroutine pack_vertex_data(x, y, z, buffer)
        !! Pack 3D vertex data into byte buffer
        !! Following KISS - direct packing as float32
        real(wp), intent(in) :: x(:), y(:), z(:)
        integer(1), allocatable, intent(out) :: buffer(:)
        
        integer :: n_vertices, buffer_size, i, offset
        real(real32) :: x32, y32, z32
        
        n_vertices = size(x)
        buffer_size = n_vertices * 3 * 4  ! 3 components * 4 bytes each
        
        allocate(buffer(buffer_size))
        
        ! Pack vertices interleaved: x,y,z,x,y,z,...
        offset = 1
        do i = 1, n_vertices
            ! Convert to single precision
            x32 = real(x(i), real32)
            y32 = real(y(i), real32)
            z32 = real(z(i), real32)
            
            ! Pack bytes
            buffer(offset:offset+3) = transfer(x32, buffer(1:4))
            offset = offset + 4
            buffer(offset:offset+3) = transfer(y32, buffer(1:4))
            offset = offset + 4
            buffer(offset:offset+3) = transfer(z32, buffer(1:4))
            offset = offset + 4
        end do
        
    end subroutine pack_vertex_data
    
    subroutine pack_index_data(indices, buffer)
        !! Pack triangle indices into byte buffer
        !! Following KISS - pack as uint16
        integer, intent(in) :: indices(:)
        integer(1), allocatable, intent(out) :: buffer(:)
        
        integer :: n_indices, buffer_size, i, offset
        integer(int16) :: idx16
        
        n_indices = size(indices)
        buffer_size = n_indices * 2  ! 2 bytes per uint16
        
        allocate(buffer(buffer_size))
        
        ! Pack indices
        offset = 1
        do i = 1, n_indices
            idx16 = int(indices(i), int16)
            buffer(offset:offset+1) = transfer(idx16, buffer(1:2))
            offset = offset + 2
        end do
        
    end subroutine pack_index_data
    
    subroutine create_accessor(accessor, buffer_view_idx, count, &
                              type_str, component_str, min_vals, max_vals)
        !! Create GLTF accessor metadata
        !! Following SRP - only creates accessor structure
        type(gltf_accessor_t), intent(out) :: accessor
        integer, intent(in) :: buffer_view_idx, count
        character(len=*), intent(in) :: type_str, component_str
        real(wp), intent(in), optional :: min_vals(:), max_vals(:)
        
        accessor%buffer_view = buffer_view_idx
        accessor%count = count
        accessor%type = type_str
        
        ! Set component type based on string
        select case(component_str)
        case("FLOAT")
            accessor%component_type = GLTF_COMPONENT_TYPE_FLOAT
        case("UNSIGNED_SHORT")
            accessor%component_type = GLTF_COMPONENT_TYPE_UNSIGNED_SHORT
        case default
            accessor%component_type = GLTF_COMPONENT_TYPE_FLOAT
        end select
        
        ! Set min/max if provided
        if (present(min_vals) .and. present(max_vals)) then
            allocate(accessor%min(size(min_vals)))
            allocate(accessor%max(size(max_vals)))
            accessor%min = min_vals
            accessor%max = max_vals
        end if
        
    end subroutine create_accessor
    
    subroutine create_buffer_view(buffer_view, buffer_idx, &
                                 byte_offset, byte_length)
        !! Create GLTF buffer view metadata
        !! Following SRP - only creates buffer view structure
        type(gltf_buffer_view_t), intent(out) :: buffer_view
        integer, intent(in) :: buffer_idx, byte_offset, byte_length
        
        buffer_view%buffer = buffer_idx
        buffer_view%byte_offset = byte_offset
        buffer_view%byte_length = byte_length
        
    end subroutine create_buffer_view

end module fortplot_gltf_buffer