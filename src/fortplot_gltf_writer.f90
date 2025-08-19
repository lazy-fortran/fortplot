module fortplot_gltf_writer
    !! Module for writing GLTF JSON text format
    !! Following SRP - each routine has single responsibility
    !! Following KISS - simple string concatenation, no complex JSON libraries
    
    use iso_fortran_env, only: wp => real64
    use fortplot_gltf_base, only: gltf_asset_t, gltf_mesh_t, gltf_accessor_t, &
                                  gltf_buffer_view_t, GLTF_VERSION
    implicit none
    
    private
    public :: write_gltf_header
    public :: write_gltf_asset
    public :: write_gltf_meshes
    public :: write_gltf_accessors  
    public :: write_gltf_buffer_views
    
contains

    function write_gltf_header() result(json)
        !! Write minimal GLTF header with asset info
        !! Returns complete minimal GLTF structure
        character(len=:), allocatable :: json
        type(gltf_asset_t) :: asset
        
        ! Set up minimal asset info
        asset%version = GLTF_VERSION
        asset%generator = "fortplotlib"
        
        ! Build minimal GLTF structure
        json = '{'
        json = json // '"asset":' // write_gltf_asset(asset)
        json = json // '}'
        
    end function write_gltf_header
    
    function write_gltf_asset(asset) result(json)
        !! Convert asset type to JSON string
        !! SRP: Only handles asset serialization
        type(gltf_asset_t), intent(in) :: asset
        character(len=:), allocatable :: json
        
        json = '{'
        json = json // '"version":"' // asset%version // '"'
        
        if (allocated(asset%generator)) then
            json = json // ',"generator":"' // asset%generator // '"'
        end if
        
        if (allocated(asset%copyright)) then
            json = json // ',"copyright":"' // asset%copyright // '"'
        end if
        
        json = json // '}'
        
    end function write_gltf_asset
    
    function write_gltf_meshes(meshes) result(json)
        !! Write meshes array to JSON
        !! Following KISS - simple JSON generation
        type(gltf_mesh_t), intent(in) :: meshes(:)
        character(len=:), allocatable :: json
        character(len=32) :: num_str
        integer :: i
        
        json = "["
        
        do i = 1, size(meshes)
            if (i > 1) json = json // ","
            json = json // '{"primitives":['
            
            ! For now, simple primitive
            json = json // '{"attributes":{"POSITION":' 
            write(num_str, '(I0)') i-1
            json = json // trim(num_str) // '},"mode":4}'
            
            json = json // ']}'
        end do
        
        json = json // "]"
        
    end function write_gltf_meshes
    
    function write_gltf_accessors(accessors) result(json)
        !! Write accessors array to JSON
        !! Following KISS - simple JSON generation
        type(gltf_accessor_t), intent(in) :: accessors(:)
        character(len=:), allocatable :: json
        character(len=32) :: num_str, float_str
        integer :: i, j
        
        json = "["
        
        do i = 1, size(accessors)
            if (i > 1) json = json // ","
            
            json = json // '{"bufferView":' 
            write(num_str, '(I0)') accessors(i)%buffer_view
            json = json // trim(num_str)
            
            json = json // ',"componentType":'
            write(num_str, '(I0)') accessors(i)%component_type
            json = json // trim(num_str)
            
            json = json // ',"count":'
            write(num_str, '(I0)') accessors(i)%count
            json = json // trim(num_str)
            
            json = json // ',"type":"' // trim(accessors(i)%type) // '"'
            
            ! Add bounds if present
            if (allocated(accessors(i)%min)) then
                json = json // ',"min":['
                do j = 1, size(accessors(i)%min)
                    if (j > 1) json = json // ","
                    write(float_str, '(F0.6)') accessors(i)%min(j)
                    json = json // trim(adjustl(float_str))
                end do
                json = json // ']'
                
                json = json // ',"max":['
                do j = 1, size(accessors(i)%max)
                    if (j > 1) json = json // ","
                    write(float_str, '(F0.6)') accessors(i)%max(j)
                    json = json // trim(adjustl(float_str))
                end do
                json = json // ']'
            end if
            
            json = json // '}'
        end do
        
        json = json // "]"
        
    end function write_gltf_accessors
    
    function write_gltf_buffer_views(buffer_views) result(json)
        !! Write buffer views array to JSON
        !! Following KISS - simple JSON generation
        type(gltf_buffer_view_t), intent(in) :: buffer_views(:)
        character(len=:), allocatable :: json
        character(len=32) :: num_str
        integer :: i
        
        json = "["
        
        do i = 1, size(buffer_views)
            if (i > 1) json = json // ","
            
            json = json // '{"buffer":'
            write(num_str, '(I0)') buffer_views(i)%buffer
            json = json // trim(num_str)
            
            json = json // ',"byteLength":'
            write(num_str, '(I0)') buffer_views(i)%byte_length
            json = json // trim(num_str)
            
            json = json // ',"byteOffset":'
            write(num_str, '(I0)') buffer_views(i)%byte_offset
            json = json // trim(num_str) // '}'
        end do
        
        json = json // "]"
        
    end function write_gltf_buffer_views

end module fortplot_gltf_writer