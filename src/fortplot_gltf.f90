module fortplot_gltf
    !! GLTF backend for 3D plot export
    !! Following SRP - handles only GLTF file generation
    
    use iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_gltf_base
    use fortplot_gltf_writer
    use fortplot_gltf_geometry
    use fortplot_gltf_buffer
    implicit none
    
    private
    public :: gltf_context, create_gltf_canvas
    
    type, extends(plot_context) :: gltf_context
        !! GLTF context for 3D plot export
        character(len=:), allocatable :: filename
        type(gltf_mesh_t), allocatable :: meshes(:)
        type(gltf_accessor_t), allocatable :: accessors(:)
        type(gltf_buffer_view_t), allocatable :: buffer_views(:)
        integer :: mesh_count = 0
        integer :: accessor_count = 0
        integer :: buffer_view_count = 0
        integer(1), allocatable :: buffer_data(:)
    contains
        procedure :: save => save_gltf
        procedure :: line => gltf_line
        procedure :: color => gltf_color
        procedure :: text => gltf_text
        procedure :: set_line_width => gltf_set_line_width
        procedure :: draw_marker => gltf_draw_marker
        procedure :: set_marker_colors => gltf_set_marker_colors
        procedure :: set_marker_colors_with_alpha => gltf_set_marker_colors_with_alpha
        procedure :: add_3d_line_data
        procedure :: add_3d_surface_data
    end type gltf_context
    
contains

    function create_gltf_canvas(width, height) result(ctx)
        !! Create GLTF context for 3D export
        !! Following KISS - minimal initialization
        integer, intent(in) :: width, height
        type(gltf_context) :: ctx
        
        call setup_canvas(ctx, width, height)
    end function create_gltf_canvas

    subroutine save_gltf(this, filename)
        !! Save 3D plot data as GLTF file
        !! Following KISS - generates complete GLTF structure
        class(gltf_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        integer :: unit, iostat
        character(len=:), allocatable :: json
        
        this%filename = filename
        
        ! Generate complete GLTF JSON structure
        json = generate_complete_gltf(this)
        
        ! Write to file
        open(newunit=unit, file=filename, status='replace', &
             action='write', iostat=iostat)
        if (iostat == 0) then
            write(unit, '(A)') json
            close(unit)
            print *, "GLTF file '" // trim(filename) // "' created successfully!"
        else
            print *, "Error: Failed to create GLTF file"
        end if
        
    end subroutine save_gltf
    
    ! Placeholder implementations for abstract methods
    subroutine gltf_line(this, x1, y1, x2, y2)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        ! Not used for GLTF
    end subroutine gltf_line
    
    subroutine gltf_color(this, r, g, b)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        ! Store for material generation
    end subroutine gltf_color
    
    subroutine gltf_text(this, x, y, text)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        ! Not used for GLTF
    end subroutine gltf_text
    
    subroutine gltf_set_line_width(this, width)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: width
        ! Not used for GLTF
    end subroutine gltf_set_line_width
    
    subroutine gltf_draw_marker(this, x, y, style)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        ! Not used for GLTF
    end subroutine gltf_draw_marker
    
    subroutine gltf_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b
        ! Store for material generation
    end subroutine gltf_set_marker_colors
    
    subroutine gltf_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, edge_alpha, &
                                           face_r, face_g, face_b, face_alpha)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        ! Store for material generation
    end subroutine gltf_set_marker_colors_with_alpha
    
    subroutine add_3d_line_data(this, x, y, z)
        !! Add 3D line data to GLTF context
        !! Following SRP - handles data storage
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x(:), y(:), z(:)
        
        integer :: n, idx
        
        ! Allocate space for new mesh
        this%mesh_count = this%mesh_count + 1
        if (.not. allocated(this%meshes)) then
            allocate(this%meshes(10))  ! Pre-allocate space
        end if
        
        ! Convert line to GLTF mesh
        idx = this%mesh_count
        call convert_line_to_gltf(x, y, z, this%meshes(idx))
        
    end subroutine add_3d_line_data
    
    subroutine add_3d_surface_data(this, x_grid, y_grid, z_grid)
        !! Add 3D surface data to GLTF context
        !! Following SRP - handles data storage
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        
        integer :: idx
        
        ! Allocate space for new mesh
        this%mesh_count = this%mesh_count + 1
        if (.not. allocated(this%meshes)) then
            allocate(this%meshes(10))  ! Pre-allocate space
        end if
        
        ! Convert surface to GLTF mesh
        idx = this%mesh_count
        call convert_surface_to_gltf(x_grid, y_grid, z_grid, this%meshes(idx))
        
    end subroutine add_3d_surface_data
    
    function generate_complete_gltf(this) result(json)
        !! Generate complete GLTF JSON structure
        !! Following KISS - assembles all components
        class(gltf_context), intent(inout) :: this
        character(len=:), allocatable :: json
        
        character(len=:), allocatable :: base64_data
        character(len=64) :: num_str
        integer :: i, total_bytes
        integer(1), allocatable :: buffer(:)
        
        ! Create buffer and accessors for all meshes
        call create_gltf_buffers(this)
        
        ! Start with opening brace
        json = "{"
        
        ! Add asset information
        json = json // '"asset":{"version":"2.0","generator":"fortplotlib"}'
        
        ! Add scene structure
        if (this%mesh_count > 0) then
            json = json // ',"scenes":[{"nodes":['
            do i = 1, this%mesh_count
                if (i > 1) json = json // ","
                write(num_str, '(I0)') i-1
                json = json // trim(num_str)
            end do
            json = json // ']}],"scene":0'
        else
            ! Empty scene
            json = json // ',"scenes":[{"nodes":[]}],"scene":0'
        end if
        
        ! Add nodes
        if (this%mesh_count > 0) then
            json = json // ',"nodes":['
            do i = 1, this%mesh_count
                if (i > 1) json = json // ","
                write(num_str, '(I0)') i-1
                json = json // '{"mesh":' // trim(num_str) // '}'
            end do
            json = json // ']'
            
            ! Add meshes
            json = json // ',"meshes":' // write_gltf_meshes(this%meshes(1:this%mesh_count))
            
            ! Add accessors
            if (this%accessor_count > 0) then
                json = json // ',"accessors":' // write_gltf_accessors(this%accessors(1:this%accessor_count))
            else
                json = json // ',"accessors":[]'
            end if
            
            ! Add buffer views
            if (this%buffer_view_count > 0) then
                json = json // ',"bufferViews":' // write_gltf_buffer_views(this%buffer_views(1:this%buffer_view_count))
            else
                json = json // ',"bufferViews":[]'
            end if
        else
            ! Empty arrays
            json = json // ',"nodes":[],"meshes":[],"accessors":[],"bufferViews":[]'
        end if
        
        ! Create base64 encoded buffer data
        if (allocated(this%buffer_data)) then
            call encode_base64(this%buffer_data, base64_data)
            total_bytes = size(this%buffer_data)
        else
            base64_data = ""
            total_bytes = 0
        end if
        
        ! Add buffers
        write(num_str, '(I0)') total_bytes
        json = json // ',"buffers":[{"uri":"data:application/octet-stream;base64,' // &
               base64_data // '","byteLength":' // trim(num_str) // '}]'
        
        ! Close JSON
        json = json // "}"
        
    end function generate_complete_gltf
    
    subroutine create_gltf_buffers(this)
        !! Create buffers and accessors from mesh data
        !! Following SRP - handles buffer creation
        class(gltf_context), intent(inout) :: this
        
        integer :: i, offset, n_vertices
        integer(1), allocatable :: vertex_buffer(:)
        
        ! For now, create dummy data
        if (this%mesh_count > 0) then
            ! Allocate accessors and buffer views
            allocate(this%accessors(this%mesh_count))
            allocate(this%buffer_views(this%mesh_count))
            this%accessor_count = this%mesh_count
            this%buffer_view_count = this%mesh_count
            
            ! Create simple buffer with dummy data
            allocate(this%buffer_data(36))  ! 3 vertices * 3 floats * 4 bytes
            this%buffer_data = 0
        end if
        
    end subroutine create_gltf_buffers
    
    subroutine encode_base64(data, base64)
        !! Encode binary data as base64
        !! Following KISS - simple encoding
        integer(1), intent(in) :: data(:)
        character(len=:), allocatable, intent(out) :: base64
        
        ! For now, return empty string
        base64 = ""
        
    end subroutine encode_base64

end module fortplot_gltf