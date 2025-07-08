module fortplot_gltf_base
    !! Base module for GLTF constants and types
    !! Following DRY principle - all GLTF constants in one place
    !! Following KISS principle - simple, clear types
    
    use iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: gltf_asset_t
    public :: GLTF_VERSION
    
    ! GLTF 2.0 constants
    character(len=*), parameter :: GLTF_VERSION = "2.0"
    
    type :: gltf_asset_t
        !! GLTF asset information
        character(len=:), allocatable :: version
        character(len=:), allocatable :: generator
        character(len=:), allocatable :: copyright
    end type gltf_asset_t
    
end module fortplot_gltf_base