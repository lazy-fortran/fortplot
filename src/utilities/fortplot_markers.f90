module fortplot_markers
    !! Shared marker utilities following DRY principles
    !! Eliminates code duplication between PNG and PDF backends
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: get_marker_size, validate_marker_style, get_default_marker
    public :: marker_size_scale, DEFAULT_SCATTER_AREA
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
    
    ! Marker size constants - centralized for consistency. The circle value is
    ! the fill radius; the other shapes are sized so the RENDERED filled-pixel
    ! areas match matplotlib's default scatter, where a single size s renders the
    ! square and diamond slightly larger than the circle and roughly equal to one
    ! another (square ~1.17x circle area, diamond ~1.23x circle area). The factors
    ! below are calibrated against measured rasterized areas (see
    ! test/marker/test_marker_equal_area.f90), not ideal geometry: the circle
    ! antialiasing window and the supersampled quad fill use the same tight edge
    ! convention, so equal-geometry constants would render unequal areas. For the
    ! raster backend get_marker_size is interpreted as: circle -> radius, square
    ! -> full side, diamond -> full diagonal, cross/plus -> full extent.
    real(wp), parameter :: SIZE_CIRCLE = 3.7_wp
    real(wp), parameter :: SIZE_SQUARE = 1.911_wp*SIZE_CIRCLE   ! calibrated side
    real(wp), parameter :: SIZE_DIAMOND = 2.744_wp*SIZE_CIRCLE  ! calibrated diagonal
    real(wp), parameter :: SIZE_CROSS = 2.0_wp*SIZE_CIRCLE                 ! extent = diameter
    real(wp), parameter :: SIZE_PLUS = 2.0_wp*SIZE_CIRCLE                  ! extent = diameter
    real(wp), parameter :: SIZE_STAR = 2.0_wp*SIZE_CIRCLE
    real(wp), parameter :: SIZE_TRIANGLE = 2.0_wp*SIZE_CIRCLE
    real(wp), parameter :: SIZE_PENTAGON = 2.0_wp*SIZE_CIRCLE
    real(wp), parameter :: SIZE_HEXAGON = 2.0_wp*SIZE_CIRCLE

    ! matplotlib's default scatter area s (points^2). The fixed marker sizes
    ! above reproduce this area, so an explicit s == DEFAULT_SCATTER_AREA leaves
    ! the rendered size unchanged. Marker radius scales with sqrt(s).
    real(wp), parameter :: DEFAULT_SCATTER_AREA = 20.0_wp

contains

    pure function marker_size_scale(area) result(scale)
        !! Linear radius scale factor for a matplotlib scatter area `s`.
        !! s is an area (points^2); radius scales with sqrt(s). Normalized so
        !! that area == DEFAULT_SCATTER_AREA returns 1 (today's default size).
        real(wp), intent(in) :: area
        real(wp) :: scale

        if (area <= 0.0_wp) then
            scale = 1.0_wp
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