module fortplot_markers
    !! Shared marker utilities following DRY principles
    !! Eliminates code duplication between PNG and PDF backends
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_CROSS
    
    ! Marker style constants
    character(len=*), parameter :: MARKER_CIRCLE = 'o'
    character(len=*), parameter :: MARKER_SQUARE = 's' 
    character(len=*), parameter :: MARKER_DIAMOND = 'D'
    character(len=*), parameter :: MARKER_CROSS = 'x'
    
    ! Marker size constants - centralized for consistency
    real(wp), parameter :: SIZE_CIRCLE = 5.0_wp
    real(wp), parameter :: SIZE_SQUARE = 6.0_wp
    real(wp), parameter :: SIZE_DIAMOND = 6.0_wp
    real(wp), parameter :: SIZE_CROSS = 5.0_wp

contains

    pure function get_marker_size(style) result(size)
        !! Get standardized marker size for given style
        !! Eliminates magic number duplication across backends
        character(len=*), intent(in) :: style
        real(wp) :: size
        
        select case (trim(style))
        case (MARKER_CIRCLE)
            size = SIZE_CIRCLE
        case (MARKER_SQUARE)
            size = SIZE_SQUARE
        case (MARKER_DIAMOND)
            size = SIZE_DIAMOND
        case (MARKER_CROSS)
            size = SIZE_CROSS
        case default
            size = SIZE_CIRCLE  ! Default fallback
        end select
    end function get_marker_size

end module fortplot_markers