module fortplot_gltf_base
    !! Base module for GLTF constants and types
    !! Following DRY principle - all GLTF constants in one place
    !! Following KISS principle - simple, clear types
    
    use iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: gltf_asset_t, gltf_mesh_t, gltf_primitive_t
    public :: gltf_accessor_t, gltf_buffer_view_t
    public :: GLTF_VERSION
    public :: GLTF_MODE_TRIANGLES, GLTF_MODE_LINE_STRIP
    public :: GLTF_COMPONENT_TYPE_FLOAT, GLTF_COMPONENT_TYPE_UNSIGNED_SHORT
    
    ! GLTF 2.0 constants
    character(len=*), parameter :: GLTF_VERSION = "2.0"
    integer, parameter :: GLTF_MODE_TRIANGLES = 4
    integer, parameter :: GLTF_MODE_LINE_STRIP = 3
    integer, parameter :: GLTF_COMPONENT_TYPE_FLOAT = 5126
    integer, parameter :: GLTF_COMPONENT_TYPE_UNSIGNED_SHORT = 5123
    
    type :: gltf_asset_t
        !! GLTF asset information
        character(len=:), allocatable :: version
        character(len=:), allocatable :: generator
        character(len=:), allocatable :: copyright
    end type gltf_asset_t
    
    type :: gltf_primitive_t
        !! GLTF mesh primitive
        integer :: mode = GLTF_MODE_TRIANGLES
        integer :: position_accessor = -1
        integer :: normal_accessor = -1
        integer :: indices_accessor = -1
        integer :: vertex_count = 0
        integer :: index_count = 0
    end type gltf_primitive_t
    
    type :: gltf_mesh_t
        !! GLTF mesh containing primitives
        type(gltf_primitive_t), allocatable :: primitives(:)
        integer :: primitive_count = 0
        real(wp), dimension(3) :: min_bounds = 0.0_wp
        real(wp), dimension(3) :: max_bounds = 0.0_wp
        real(wp), allocatable :: vertices(:,:)  ! n_vertices x 3
    end type gltf_mesh_t
    
    type :: gltf_accessor_t
        !! GLTF accessor for buffer data
        integer :: buffer_view = -1
        integer :: byte_offset = 0
        integer :: component_type = GLTF_COMPONENT_TYPE_FLOAT
        integer :: count = 0
        character(len=16) :: type = "SCALAR"
        real(wp), allocatable :: min(:), max(:)
    end type gltf_accessor_t
    
    type :: gltf_buffer_view_t
        !! GLTF buffer view
        integer :: buffer = 0
        integer :: byte_offset = 0
        integer :: byte_length = 0
        integer :: byte_stride = 0
    end type gltf_buffer_view_t
    
end module fortplot_gltf_base