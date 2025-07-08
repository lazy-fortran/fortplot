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
        integer :: i, j
        
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
        character(len=32) :: num_str
        integer :: i
        
        json = "["
        
        do i = 1, size(accessors)
            if (i > 1) json = json // ","
            
            json = json // '{"bufferView":' 
            write(num_str, '(I0)') i-1
            json = json // trim(num_str)
            json = json // ',"componentType":5126,"count":3,"type":"VEC3"}'
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
            
            json = json // '{"buffer":0,"byteLength":36,"byteOffset":'
            write(num_str, '(I0)') (i-1)*36
            json = json // trim(num_str) // '}'
        end do
        
        json = json // "]"
        
    end function write_gltf_buffer_views

end module fortplot_gltf_writer