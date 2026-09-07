module fortplot_markers
    !! Shared marker utilities following DRY principles
    !! Eliminates code duplication between PNG and PDF backends
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: get_marker_size, validate_marker_style, get_default_marker
    public :: marker_size_scale, DEFAULT_SCATTER_AREA
    public :: MARKER_POINT, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND
    public :: MARKER_CROSS, MARKER_PLUS, MARKER_STAR
    public :: MARKER_TRIANGLE_UP, MARKER_TRIANGLE_DOWN, MARKER_PENTAGON, MARKER_HEXAGON
    public :: MARKER_DIAMOND_SMALL, MARKER_TRIANGLE_LEFT, MARKER_TRIANGLE_RIGHT
    
    ! Marker style constants - pyplot compatible
    character(len=*), parameter :: MARKER_POINT = '.'
    character(len=*), parameter :: MARKER_CIRCLE = 'o'
    character(len=*), parameter :: MARKER_SQUARE = 's' 
    character(len=*), parameter :: MARKER_DIAMOND = 'D'
    character(len=*), parameter :: MARKER_DIAMOND_SMALL = 'd'
    character(len=*), parameter :: MARKER_CROSS = 'x'
    character(len=*), parameter :: MARKER_PLUS = '+'
    character(len=*), parameter :: MARKER_STAR = '*'
    character(len=*), parameter :: MARKER_TRIANGLE_UP = '^'
    character(len=*), parameter :: MARKER_TRIANGLE_DOWN = 'v'
    character(len=*), parameter :: MARKER_TRIANGLE_LEFT = '<'
    character(len=*), parameter :: MARKER_TRIANGLE_RIGHT = '>'
    character(len=*), parameter :: MARKER_PENTAGON = 'p'
    character(len=*), parameter :: MARKER_HEXAGON = 'h'
    
    ! Marker dimensions at 100 dpi for Matplotlib's default 6-point marker.
    ! Circle uses a radius, square a side, diamond a diagonal, and crosses an
    ! extent. Explicit scatter areas scale these dimensions by sqrt(s / 36).
    real(wp), parameter :: SIZE_CIRCLE = 3.0_wp*100.0_wp/72.0_wp
    real(wp), parameter :: SIZE_POINT = 0.5_wp*SIZE_CIRCLE
    real(wp), parameter :: SIZE_SQUARE = 2.0_wp*SIZE_CIRCLE
    real(wp), parameter :: SIZE_DIAMOND = sqrt(2.0_wp)*SIZE_SQUARE
    real(wp), parameter :: SIZE_CROSS = SIZE_SQUARE
    real(wp), parameter :: SIZE_PLUS = SIZE_SQUARE
    real(wp), parameter :: SIZE_STAR = SIZE_SQUARE
    real(wp), parameter :: SIZE_TRIANGLE = SIZE_SQUARE
    real(wp), parameter :: SIZE_PENTAGON = SIZE_SQUARE
    real(wp), parameter :: SIZE_HEXAGON = SIZE_SQUARE
    real(wp), parameter :: DEFAULT_SCATTER_AREA = 36.0_wp

contains

    pure function marker_size_scale(area) result(scale)
        !! Linear radius scale factor for a matplotlib scatter area `s`.
        !! s is an area (points^2); radius scales with sqrt(s). Normalized so
        !! that the default 36 points squared returns one.
        real(wp), intent(in) :: area
        real(wp) :: scale

        if (area <= 0.0_wp) then
            scale = 0.0_wp
        else
            scale = sqrt(area/DEFAULT_SCATTER_AREA)
        end if
    end function marker_size_scale

    pure function get_marker_size(style) result(size)
        !! Get standardized marker size for given style
        !! Eliminates magic number duplication across backends
        character(len=*), intent(in) :: style
        real(wp) :: size
        
        select case (trim(style))
        case (MARKER_POINT)
            size = SIZE_POINT
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
        case (MARKER_TRIANGLE_UP, MARKER_TRIANGLE_DOWN, &
              MARKER_TRIANGLE_LEFT, MARKER_TRIANGLE_RIGHT)
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
        case (MARKER_POINT, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, &
              MARKER_DIAMOND_SMALL, MARKER_CROSS, &
              MARKER_PLUS, MARKER_STAR, MARKER_TRIANGLE_UP, MARKER_TRIANGLE_DOWN, &
              MARKER_PENTAGON, MARKER_HEXAGON, &
              MARKER_TRIANGLE_LEFT, MARKER_TRIANGLE_RIGHT)
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
