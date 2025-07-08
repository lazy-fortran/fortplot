module fortplot_gltf_writer
    !! Module for writing GLTF JSON text format
    !! Following SRP - each routine has single responsibility
    !! Following KISS - simple string concatenation, no complex JSON libraries
    
    use iso_fortran_env, only: wp => real64
    use fortplot_gltf_base, only: gltf_asset_t, GLTF_VERSION
    implicit none
    
    private
    public :: write_gltf_header
    public :: write_gltf_asset
    
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

end module fortplot_gltf_writer