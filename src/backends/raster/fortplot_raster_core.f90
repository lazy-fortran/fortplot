module fortplot_raster_core
    !! Core raster image management and basic operations
    !! Extracted from fortplot_raster.f90 for size reduction (SRP compliance)
    use iso_c_binding
    use fortplot_constants, only: SOLID_LINE_PATTERN_LENGTH
    use fortplot_raster_drawing, only: color_to_byte
    use fortplot_raster_line_styles, only: set_raster_line_style
    use fortplot_bitmap, only: initialize_white_background
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_image_t, create_raster_image, destroy_raster_image

    type :: raster_image_t
        integer(1), allocatable :: image_data(:)
        integer :: width, height
        real(wp) :: current_r = 0.0_wp, current_g = 0.0_wp, current_b = 0.0_wp
        real(wp) :: current_line_width = 1.0_wp
        ! Line style pattern support
        character(len=10) :: line_style = '-'
        real(wp) :: line_pattern(20)
        integer :: pattern_size = 1
        real(wp) :: pattern_length = SOLID_LINE_PATTERN_LENGTH
        real(wp) :: pattern_distance = 0.0_wp
        ! Marker colors - separate edge and face colors with alpha
        real(wp) :: marker_edge_r = 0.0_wp, marker_edge_g = 0.0_wp, marker_edge_b = 0.0_wp, marker_edge_alpha = 1.0_wp
        real(wp) :: marker_face_r = 1.0_wp, marker_face_g = 0.0_wp, marker_face_b = 0.0_wp, marker_face_alpha = 1.0_wp
    contains
        procedure :: set_color => raster_set_color
        procedure :: get_color_bytes => raster_get_color_bytes
        procedure :: set_line_style => raster_set_line_style
    end type raster_image_t

contains

    function create_raster_image(width, height) result(image)
        integer, intent(in) :: width, height
        type(raster_image_t) :: image

        image%width = width
        image%height = height
        allocate(image%image_data(width * height * 3))
        call initialize_white_background(image%image_data, width, height)
        
        ! Initialize line style to solid
        call image%set_line_style('-')
    end function create_raster_image

    subroutine destroy_raster_image(image)
        type(raster_image_t), intent(inout) :: image
        if (allocated(image%image_data)) deallocate(image%image_data)
    end subroutine destroy_raster_image

    subroutine raster_set_color(this, r, g, b)
        class(raster_image_t), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        this%current_r = r
        this%current_g = g
        this%current_b = b
    end subroutine raster_set_color

    subroutine raster_get_color_bytes(this, r, g, b)
        class(raster_image_t), intent(in) :: this
        integer(1), intent(out) :: r, g, b

        r = color_to_byte(this%current_r)
        g = color_to_byte(this%current_g)
        b = color_to_byte(this%current_b)
    end subroutine raster_get_color_bytes

    subroutine raster_set_line_style(this, style)
        !! Set line style pattern for raster image
        class(raster_image_t), intent(inout) :: this
        character(len=*), intent(in) :: style
        
        call set_raster_line_style(style, this%line_style, this%line_pattern, &
                                  this%pattern_size, this%pattern_length, this%pattern_distance)
    end subroutine raster_set_line_style

end module fortplot_raster_core