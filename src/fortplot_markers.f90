module fortplot_markers
    !! Shared marker utilities following DRY principles
    !! Eliminates code duplication between PNG and PDF backends
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: get_marker_size, validate_marker_style, get_default_marker
    public :: MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_CROSS, MARKER_PLUS, MARKER_STAR
    public :: MARKER_TRIANGLE_UP, MARKER_TRIANGLE_DOWN, MARKER_PENTAGON, MARKER_HEXAGON
    public :: MARKER_DIAMOND_SMALL
    
    ! Marker style constants - pyplot compatible
    character(len=*), parameter :: MARKER_CIRCLE = 'o'
    character(len=*), parameter :: MARKER_SQUARE = 's' 
    character(len=*), parameter :: MARKER_DIAMOND = 'D'
    character(len=*), parameter :: MARKER_DIAMOND_SMALL = 'd'
    character(len=*), parameter :: MARKER_CROSS = 'x'
    character(len=*), parameter :: MARKER_PLUS = '+'
    character(len=*), parameter :: MARKER_STAR = '*'
    character(len=*), parameter :: MARKER_TRIANGLE_UP = '^'
    character(len=*), parameter :: MARKER_TRIANGLE_DOWN = 'v'
    character(len=*), parameter :: MARKER_PENTAGON = 'p'
    character(len=*), parameter :: MARKER_HEXAGON = 'h'
    
    ! Marker size constants - centralized for consistency
    real(wp), parameter :: SIZE_CIRCLE = 5.0_wp
    real(wp), parameter :: SIZE_SQUARE = 6.0_wp
    real(wp), parameter :: SIZE_DIAMOND = 6.0_wp
    real(wp), parameter :: SIZE_CROSS = 5.0_wp
    real(wp), parameter :: SIZE_PLUS = 5.0_wp
    real(wp), parameter :: SIZE_STAR = 7.0_wp
    real(wp), parameter :: SIZE_TRIANGLE = 6.0_wp
    real(wp), parameter :: SIZE_PENTAGON = 7.0_wp
    real(wp), parameter :: SIZE_HEXAGON = 7.0_wp

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
        case (MARKER_DIAMOND, MARKER_DIAMOND_SMALL)
            size = SIZE_DIAMOND
        case (MARKER_CROSS)
            size = SIZE_CROSS
        case (MARKER_PLUS)
            size = SIZE_PLUS
        case (MARKER_STAR)
            size = SIZE_STAR
        case (MARKER_TRIANGLE_UP, MARKER_TRIANGLE_DOWN)
            size = SIZE_TRIANGLE
        case (MARKER_PENTAGON)
            size = SIZE_PENTAGON
        case (MARKER_HEXAGON)
            size = SIZE_HEXAGON
        case default
            size = SIZE_CIRCLE  ! Default fallback
        end select
    end function get_marker_size

    pure function validate_marker_style(style) result(is_valid)
        !! Validate if marker style is supported
        character(len=*), intent(in) :: style
        logical :: is_valid
        
        select case (trim(style))
        case (MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_DIAMOND_SMALL, MARKER_CROSS, &
              MARKER_PLUS, MARKER_STAR, MARKER_TRIANGLE_UP, MARKER_TRIANGLE_DOWN, &
              MARKER_PENTAGON, MARKER_HEXAGON)
            is_valid = .true.
        case default
            is_valid = .false.
        end select
    end function validate_marker_style
    
    pure function get_default_marker() result(marker)
        !! Get default marker style
        character(len=1) :: marker
        marker = MARKER_CIRCLE
    end function get_default_marker

end module fortplot_markers