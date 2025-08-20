module fortplot_gltf
    !! GLTF backend for 3D plot export
    !! Following SRP - handles only GLTF file generation
    
    use iso_fortran_env, only: wp => real64, real32, int8
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_gltf_base
    use fortplot_gltf_writer
    use fortplot_logging, only: log_info, log_error
    use fortplot_gltf_geometry
    use fortplot_gltf_buffer
    use fortplot_glb_writer
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
        procedure :: draw_arrow => gltf_draw_arrow
        procedure :: get_ascii_output => gltf_get_ascii_output
        
        !! New polymorphic methods to eliminate SELECT TYPE
        procedure :: get_width_scale => gltf_get_width_scale
        procedure :: get_height_scale => gltf_get_height_scale
        procedure :: fill_quad => gltf_fill_quad
        procedure :: fill_heatmap => gltf_fill_heatmap
        procedure :: render_legend_specialized => gltf_render_legend_specialized
        procedure :: calculate_legend_dimensions => gltf_calculate_legend_dimensions
        procedure :: set_legend_border_width => gltf_set_legend_border_width
        procedure :: calculate_legend_position_backend => gltf_calculate_legend_position
        procedure :: extract_rgb_data => gltf_extract_rgb_data
        procedure :: get_png_data_backend => gltf_get_png_data
        procedure :: prepare_3d_data => gltf_prepare_3d_data
        procedure :: render_ylabel => gltf_render_ylabel
        
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
        !! Save 3D plot data as GLTF or GLB file
        !! Following KISS - generates complete GLTF structure
        class(gltf_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        integer :: unit, iostat, ext_pos
        character(len=:), allocatable :: json, extension
        logical :: is_glb
        
        this%filename = filename
        
        ! Check file extension
        ext_pos = index(filename, '.', back=.true.)
        if (ext_pos > 0) then
            extension = filename(ext_pos+1:)
            is_glb = (extension == 'glb' .or. extension == 'GLB')
        else
            is_glb = .false.
        end if
        
        ! Generate complete GLTF JSON structure
        json = generate_complete_gltf(this, is_glb)
        
        if (is_glb) then
            ! Write GLB binary format
            if (allocated(this%buffer_data)) then
                call write_glb_file(filename, json, this%buffer_data)
                call log_info("GLB file '" // trim(filename) // "' created successfully!")
            else
                call log_error("No binary data for GLB file")
            end if
        else
            ! Write GLTF text format
            open(newunit=unit, file=filename, status='replace', &
                 action='write', iostat=iostat)
            if (iostat == 0) then
                write(unit, '(A)') json
                close(unit)
                call log_info("GLTF file '" // trim(filename) // "' created successfully!")
            else
                call log_error("Failed to create GLTF file")
            end if
        end if
        
    end subroutine save_gltf
    
    ! Placeholder implementations for abstract methods
    subroutine gltf_line(this, x1, y1, x2, y2)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        ! Unused parameters
    end subroutine gltf_line
    
    subroutine gltf_color(this, r, g, b)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        ! Unused parameters
    end subroutine gltf_color
    
    subroutine gltf_text(this, x, y, text)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        ! Unused parameters
    end subroutine gltf_text
    
    subroutine gltf_set_line_width(this, width)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: width
        ! Unused parameters
    end subroutine gltf_set_line_width
    
    subroutine gltf_draw_marker(this, x, y, style)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        ! Unused parameters
    end subroutine gltf_draw_marker
    
    subroutine gltf_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b
        ! Unused parameters
    end subroutine gltf_set_marker_colors
    
    subroutine gltf_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, edge_alpha, &
                                           face_r, face_g, face_b, face_alpha)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        ! Unused parameters
    end subroutine gltf_set_marker_colors_with_alpha
    
    subroutine add_3d_line_data(this, x, y, z)
        !! Add 3D line data to GLTF context
        !! Following SRP - handles data storage
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x(:), y(:), z(:)
        
        integer :: idx
        
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
    
    function generate_complete_gltf(this, is_glb) result(json)
        !! Generate complete GLTF JSON structure
        !! Following KISS - assembles all components
        class(gltf_context), intent(inout) :: this
        logical, intent(in) :: is_glb
        character(len=:), allocatable :: json
        
        character(len=:), allocatable :: base64_data
        character(len=64) :: num_str
        integer :: i, total_bytes
        
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
        
        ! Create base64 encoded buffer data (only for GLTF)
        if (allocated(this%buffer_data)) then
            if (.not. is_glb) then
                call encode_base64(this%buffer_data, base64_data)
            else
                base64_data = ""  ! Not used for GLB
            end if
            total_bytes = size(this%buffer_data)
        else
            base64_data = ""
            total_bytes = 0
        end if
        
        ! Add buffers
        write(num_str, '(I0)') total_bytes
        if (is_glb) then
            ! For GLB, buffer is in BIN chunk (no URI)
            json = json // ',"buffers":[{"byteLength":' // trim(num_str) // '}]'
        else
            ! For GLTF, embed as base64 data URI
            json = json // ',"buffers":[{"uri":"data:application/octet-stream;base64,' // &
                   base64_data // '","byteLength":' // trim(num_str) // '}]'
        end if
        
        ! Close JSON
        json = json // "}"
        
    end function generate_complete_gltf
    
    subroutine create_gltf_buffers(this)
        !! Create buffers and accessors from mesh data
        !! Following SRP - handles buffer creation
        class(gltf_context), intent(inout) :: this
        
        integer :: i, offset, n_vertices, buffer_size
        
        if (this%mesh_count > 0) then
            ! Calculate total buffer size needed
            buffer_size = 0
            do i = 1, this%mesh_count
                if (allocated(this%meshes(i)%primitives)) then
                    n_vertices = this%meshes(i)%primitives(1)%vertex_count
                    buffer_size = buffer_size + n_vertices * 3 * 4  ! 3 floats * 4 bytes
                end if
            end do
            
            ! Allocate buffer and tracking arrays
            allocate(this%buffer_data(buffer_size))
            allocate(this%accessors(this%mesh_count))
            allocate(this%buffer_views(this%mesh_count))
            this%accessor_count = this%mesh_count
            this%buffer_view_count = this%mesh_count
            
            ! Pack vertex data for each mesh
            offset = 0
            do i = 1, this%mesh_count
                call pack_mesh_data(this%meshes(i), this%buffer_data, &
                                  offset, this%accessors(i), this%buffer_views(i), i)
            end do
        end if
        
    end subroutine create_gltf_buffers
    
    subroutine pack_mesh_data(mesh, buffer, offset, accessor, buffer_view, buffer_view_index)
        !! Pack mesh vertex data into buffer
        !! Following SRP - handles single mesh packing
        type(gltf_mesh_t), intent(in) :: mesh
        integer(1), intent(inout) :: buffer(:)
        integer, intent(inout) :: offset
        type(gltf_accessor_t), intent(out) :: accessor
        type(gltf_buffer_view_t), intent(out) :: buffer_view
        integer, intent(in) :: buffer_view_index
        
        integer :: n_vertices, i, j, byte_idx
        real(real32) :: temp_float
        
        if (allocated(mesh%primitives) .and. allocated(mesh%vertices)) then
            n_vertices = mesh%primitives(1)%vertex_count
            
            ! Setup buffer view
            buffer_view%buffer = 0
            buffer_view%byte_offset = offset
            buffer_view%byte_length = n_vertices * 3 * 4
            
            ! Setup accessor
            accessor%buffer_view = buffer_view_index - 1  ! 0-based index
            accessor%byte_offset = 0
            accessor%component_type = GLTF_COMPONENT_TYPE_FLOAT
            accessor%count = n_vertices
            accessor%type = "VEC3"
            
            ! Set bounds
            allocate(accessor%min(3), accessor%max(3))
            accessor%min = mesh%min_bounds
            accessor%max = mesh%max_bounds
            
            ! Pack vertex data
            byte_idx = offset + 1
            do i = 1, n_vertices
                do j = 1, 3
                    temp_float = real(mesh%vertices(i, j), real32)
                    call pack_float_to_bytes(temp_float, buffer(byte_idx:byte_idx+3))
                    byte_idx = byte_idx + 4
                end do
            end do
            
            offset = offset + n_vertices * 3 * 4
        end if
        
    end subroutine pack_mesh_data
    
    subroutine encode_base64(data, base64)
        !! Encode binary data as base64
        !! Following KISS - simple encoding
        integer(1), intent(in) :: data(:)
        character(len=:), allocatable, intent(out) :: base64
        
        character(len=64) :: base64_chars = &
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
        integer :: i, j, n, val
        integer :: in_len, out_len
        
        in_len = size(data)
        out_len = ((in_len + 2) / 3) * 4
        allocate(character(len=out_len) :: base64)
        
        j = 1
        do i = 1, in_len, 3
            ! Get 3 bytes (or less at end)
            val = 0
            n = min(3, in_len - i + 1)
            
            if (n >= 1) val = ior(val, ishft(iand(int(data(i)), 255), 16))
            if (n >= 2) val = ior(val, ishft(iand(int(data(i+1)), 255), 8))
            if (n >= 3) val = ior(val, iand(int(data(i+2)), 255))
            
            ! Convert to 4 base64 characters
            base64(j:j) = base64_chars(iand(ishft(val, -18), 63) + 1:iand(ishft(val, -18), 63) + 1)
            base64(j+1:j+1) = base64_chars(iand(ishft(val, -12), 63) + 1:iand(ishft(val, -12), 63) + 1)
            
            if (n >= 2) then
                base64(j+2:j+2) = base64_chars(iand(ishft(val, -6), 63) + 1:iand(ishft(val, -6), 63) + 1)
            else
                base64(j+2:j+2) = '='
            end if
            
            if (n >= 3) then
                base64(j+3:j+3) = base64_chars(iand(val, 63) + 1:iand(val, 63) + 1)
            else
                base64(j+3:j+3) = '='
            end if
            
            j = j + 4
        end do
        
    end subroutine encode_base64
    
    subroutine pack_float_to_bytes(float_val, bytes)
        !! Pack float32 to byte array (little-endian)
        !! Following KISS - direct binary conversion
        real(real32), intent(in) :: float_val
        integer(1), intent(out) :: bytes(4)
        
        integer :: int_val
        
        ! Convert float to integer representation
        int_val = transfer(float_val, int_val)
        
        ! Pack as little-endian
        bytes(1) = int(iand(int_val, 255), int8)
        bytes(2) = int(iand(ishft(int_val, -8), 255), int8)
        bytes(3) = int(iand(ishft(int_val, -16), 255), int8)
        bytes(4) = int(iand(ishft(int_val, -24), 255), int8)
        
    end subroutine pack_float_to_bytes

    subroutine gltf_draw_arrow(this, x, y, dx, dy, size, style)
        !! Draw arrow for GLTF backend (not implemented for 3D export)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        
        ! GLTF backend doesn't support 2D arrow rendering
        ! Mark arrows as not rendered for this backend
        this%has_rendered_arrows = .false.
        this%uses_vector_arrows = .false.
        this%has_triangular_arrows = .false.
    end subroutine gltf_draw_arrow

    function gltf_get_ascii_output(this) result(output)
        !! Get ASCII output (not applicable for GLTF backend)
        class(gltf_context), intent(in) :: this
        character(len=:), allocatable :: output
        
        output = ""  ! GLTF backend doesn't produce ASCII output
    end function gltf_get_ascii_output

    function gltf_get_width_scale(this) result(scale)
        !! Get width scaling factor for coordinate transformation
        class(gltf_context), intent(in) :: this
        real(wp) :: scale
        
        ! For 3D GLTF export, no 2D coordinate scaling needed
        scale = 1.0_wp
    end function gltf_get_width_scale

    function gltf_get_height_scale(this) result(scale)
        !! Get height scaling factor for coordinate transformation  
        class(gltf_context), intent(in) :: this
        real(wp) :: scale
        
        ! For 3D GLTF export, no 2D coordinate scaling needed
        scale = 1.0_wp
    end function gltf_get_height_scale

    subroutine gltf_fill_quad(this, x_quad, y_quad)
        !! Fill quadrilateral using polymorphic interface (not supported in 3D)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        
        ! GLTF backend is for 3D export - 2D filled quads not supported
        ! Could potentially be implemented as 3D quad meshes in the future
    end subroutine gltf_fill_quad

    subroutine gltf_fill_heatmap(this, x_grid, y_grid, z_grid, z_min, z_max)
        !! Fill heatmap (not supported for 3D GLTF backend - no-op)
        class(gltf_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        
        ! GLTF backend is for 3D export - 2D heatmaps not supported
        ! Could potentially be implemented as 3D surface meshes in the future
    end subroutine gltf_fill_heatmap

    subroutine gltf_render_legend_specialized(this, legend, legend_x, legend_y)
        !! Render legend (not supported for 3D GLTF backend - no-op)
        use fortplot_legend, only: legend_t
        class(gltf_context), intent(inout) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y
        
        ! GLTF backend is for 3D export - 2D legends not supported
    end subroutine gltf_render_legend_specialized

    subroutine gltf_calculate_legend_dimensions(this, legend, legend_width, legend_height)
        !! Calculate legend dimensions (not supported for 3D GLTF backend - no-op)
        use fortplot_legend, only: legend_t
        class(gltf_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: legend_width, legend_height
        
        ! GLTF backend is for 3D export - return minimal dimensions
        legend_width = 0.0_wp
        legend_height = 0.0_wp
    end subroutine gltf_calculate_legend_dimensions

    subroutine gltf_set_legend_border_width(this)
        !! Set legend border width (not supported for 3D GLTF backend - no-op)
        class(gltf_context), intent(inout) :: this
        
        ! GLTF backend is for 3D export - no 2D line widths
    end subroutine gltf_set_legend_border_width

    subroutine gltf_calculate_legend_position(this, legend, x, y)
        !! Calculate legend position (not supported for 3D GLTF backend - no-op)
        use fortplot_legend, only: legend_t
        class(gltf_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: x, y
        
        ! GLTF backend is for 3D export - return default position
        x = 0.0_wp
        y = 0.0_wp
    end subroutine gltf_calculate_legend_position

    subroutine gltf_extract_rgb_data(this, width, height, rgb_data)
        !! Extract RGB data from GLTF backend (not supported - dummy data)
        use, intrinsic :: iso_fortran_env, only: real64
        class(gltf_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(real64), intent(out) :: rgb_data(width, height, 3)
        
        ! GLTF backend is for 3D export - no RGB data for animation
        rgb_data = 0.5_real64  ! Gray background
    end subroutine gltf_extract_rgb_data

    subroutine gltf_get_png_data(this, width, height, png_data, status)
        !! Get PNG data from GLTF backend (not supported)
        class(gltf_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        ! GLTF backend doesn't provide PNG data
        allocate(png_data(0))
        status = -1
    end subroutine gltf_get_png_data

    subroutine gltf_prepare_3d_data(this, plots)
        !! Prepare 3D data for GLTF backend - actual implementation
        use fortplot_plot_data, only: plot_data_t
        class(gltf_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)
        
        ! GLTF backend actually needs 3D data preparation
        call prepare_gltf_data(this, plots)
    end subroutine gltf_prepare_3d_data

    subroutine gltf_render_ylabel(this, ylabel)
        !! Render Y-axis label for GLTF backend (no-op - 3D doesn't use 2D labels)
        class(gltf_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        
        ! GLTF backend is for 3D export - doesn't use 2D axis labels
    end subroutine gltf_render_ylabel

end module fortplot_gltf