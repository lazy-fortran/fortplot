module fortplot_annotations
    !! Text annotation system for fortplot (Issue #55)
    !! 
    !! Provides comprehensive text annotation functionality with support for:
    !! - Multiple coordinate systems (data, figure, axis)
    !! - Typography control (font size, alignment, rotation)
    !! - Background boxes and arrow annotations
    !! - Backend-independent rendering
    !!
    !! Follows SOLID principles with clean separation of concerns
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
    use fortplot_scales, only: transform_x_coordinate, transform_y_coordinate
    use fortplot_text, only: calculate_text_width, calculate_text_height
    use fortplot_logging, only: log_warning, log_error
    
    implicit none
    private
    
    ! Coordinate system constants
    integer, parameter, public :: COORD_DATA = 1
    integer, parameter, public :: COORD_FIGURE = 2  
    integer, parameter, public :: COORD_AXIS = 3
    
    ! Simple color type for background boxes (lightweight version)
    type :: annotation_color_t
        real(wp) :: r = 0.0_wp
        real(wp) :: g = 0.0_wp  
        real(wp) :: b = 0.0_wp
        real(wp) :: a = 1.0_wp  ! Alpha channel
    end type annotation_color_t
    
    ! Text annotation type
    type, public :: text_annotation_t
        ! Content and position
        character(len=256) :: text = ""
        real(wp) :: x = 0.0_wp
        real(wp) :: y = 0.0_wp
        integer :: coord_type = COORD_DATA
        
        ! Typography properties
        real(wp) :: font_size = 12.0_wp
        real(wp) :: rotation = 0.0_wp
        character(len=16) :: alignment = 'left'
        
        ! Background box properties
        logical :: has_bbox = .false.
        type(annotation_color_t) :: bbox_color = annotation_color_t(1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp) ! White
        
        ! Arrow annotation properties  
        logical :: has_arrow = .false.
        real(wp) :: xytext_x = 0.0_wp
        real(wp) :: xytext_y = 0.0_wp
        integer :: xytext_coord_type = COORD_DATA
    end type text_annotation_t
    
    ! Public interface
    public :: create_text_annotation, destroy_text_annotation
    public :: transform_annotation_coordinates, transform_annotation_coordinates_log
    public :: calculate_aligned_position, calculate_rotated_bounds
    public :: is_annotation_visible
    public :: validate_annotation_coordinates, validate_annotation_parameters
    
    ! Overloaded coordinate transformation interface
    interface transform_annotation_coordinates
        module procedure transform_annotation_coordinates_4arg
        module procedure transform_annotation_coordinates_5arg
    end interface transform_annotation_coordinates
    
contains

    function create_text_annotation(text, x, y, coord_type) result(annotation)
        !! Create a text annotation with specified properties
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: x, y
        integer, intent(in), optional :: coord_type
        type(text_annotation_t) :: annotation
        
        annotation%text = text
        annotation%x = x
        annotation%y = y
        annotation%coord_type = COORD_DATA
        if (present(coord_type)) annotation%coord_type = coord_type
        
        ! Set default typography properties
        annotation%font_size = 12.0_wp
        annotation%rotation = 0.0_wp
        annotation%alignment = 'left'
        annotation%has_bbox = .false.
        annotation%has_arrow = .false.
    end function create_text_annotation

    subroutine destroy_text_annotation(annotation)
        !! Clean up text annotation resources
        type(text_annotation_t), intent(inout) :: annotation
        
        ! For now, simple reset - future versions may have dynamic allocations
        annotation%text = ""
        annotation%x = 0.0_wp
        annotation%y = 0.0_wp
    end subroutine destroy_text_annotation

    subroutine transform_annotation_coordinates_4arg(annotation, area_or_size, pixel_x, pixel_y)
        !! 4-argument coordinate transformation (figure or axis coordinates)
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: area_or_size(:)
        real(wp), intent(out) :: pixel_x, pixel_y
        
        select case (annotation%coord_type)
        case (COORD_FIGURE)
            call transform_figure_coordinates(annotation, area_or_size, pixel_x, pixel_y)
        case (COORD_AXIS)
            call transform_axis_coordinates(annotation, area_or_size, pixel_x, pixel_y)
        case default
            ! Default to axis coordinates for 4-argument calls
            call transform_axis_coordinates(annotation, area_or_size, pixel_x, pixel_y)
        end select
    end subroutine transform_annotation_coordinates_4arg

    subroutine transform_annotation_coordinates_5arg(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        !! 5-argument coordinate transformation (data coordinates)
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: plot_area(:)
        real(wp), intent(in) :: data_bounds(:)
        real(wp), intent(out) :: pixel_x, pixel_y
        
        call transform_data_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
    end subroutine transform_annotation_coordinates_5arg

    subroutine transform_data_coordinates(annotation, plot_area, data_bounds, pixel_x, pixel_y)
        !! Transform data coordinates to pixel coordinates
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: plot_area(4)  ! x, y, width, height
        real(wp), intent(in) :: data_bounds(4)  ! xmin, xmax, ymin, ymax
        real(wp), intent(out) :: pixel_x, pixel_y
        
        real(wp) :: x_range, y_range, x_norm, y_norm
        
        ! Calculate normalized coordinates (0-1)
        x_range = data_bounds(2) - data_bounds(1)
        y_range = data_bounds(4) - data_bounds(3)
        
        if (x_range > 0.0_wp) then
            x_norm = (annotation%x - data_bounds(1)) / x_range
        else
            x_norm = 0.5_wp  ! Center if no range
        end if
        
        if (y_range > 0.0_wp) then
            y_norm = (annotation%y - data_bounds(3)) / y_range
        else
            y_norm = 0.5_wp  ! Center if no range
        end if
        
        ! Map to pixel coordinates
        pixel_x = plot_area(1) + x_norm * plot_area(3)
        pixel_y = plot_area(2) + y_norm * plot_area(4)
    end subroutine transform_data_coordinates

    subroutine transform_figure_coordinates(annotation, figure_size, pixel_x, pixel_y)
        !! Transform figure coordinates (0-1 normalized) to pixel coordinates
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: figure_size(2)  ! width, height
        real(wp), intent(out) :: pixel_x, pixel_y
        
        pixel_x = annotation%x * figure_size(1)
        pixel_y = annotation%y * figure_size(2)
    end subroutine transform_figure_coordinates

    subroutine transform_axis_coordinates(annotation, plot_area, pixel_x, pixel_y)
        !! Transform axis coordinates (0-1 normalized to plot area) to pixel coordinates
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: plot_area(4)  ! x, y, width, height
        real(wp), intent(out) :: pixel_x, pixel_y
        
        pixel_x = plot_area(1) + annotation%x * plot_area(3)
        pixel_y = plot_area(2) + annotation%y * plot_area(4)
    end subroutine transform_axis_coordinates

    subroutine transform_annotation_coordinates_log(annotation, plot_area, data_bounds, &
                                                   log_scale_x, log_scale_y, pixel_x, pixel_y)
        !! Transform annotation coordinates with logarithmic scaling support
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: plot_area(4)
        real(wp), intent(in) :: data_bounds(4)
        logical, intent(in) :: log_scale_x, log_scale_y
        real(wp), intent(out) :: pixel_x, pixel_y
        
        real(wp) :: x_norm, y_norm
        real(wp) :: log_xmin, log_xmax, log_ymin, log_ymax
        
        if (annotation%coord_type /= COORD_DATA) then
            ! For non-data coordinates, use regular transformation
            call transform_annotation_coordinates(annotation, plot_area, &
                                                data_bounds, pixel_x, pixel_y)
            return
        end if
        
        ! Handle logarithmic X transformation
        if (log_scale_x) then
            log_xmin = log10(max(data_bounds(1), 1.0e-10_wp))
            log_xmax = log10(max(data_bounds(2), 1.0e-10_wp))
            x_norm = (log10(max(annotation%x, 1.0e-10_wp)) - log_xmin) / (log_xmax - log_xmin)
        else
            x_norm = (annotation%x - data_bounds(1)) / (data_bounds(2) - data_bounds(1))
        end if
        
        ! Handle logarithmic Y transformation
        if (log_scale_y) then
            log_ymin = log10(max(data_bounds(3), 1.0e-10_wp))
            log_ymax = log10(max(data_bounds(4), 1.0e-10_wp))
            y_norm = (log10(max(annotation%y, 1.0e-10_wp)) - log_ymin) / (log_ymax - log_ymin)
        else
            y_norm = (annotation%y - data_bounds(3)) / (data_bounds(4) - data_bounds(3))
        end if
        
        ! Map to pixel coordinates
        pixel_x = plot_area(1) + x_norm * plot_area(3)
        pixel_y = plot_area(2) + y_norm * plot_area(4)
    end subroutine transform_annotation_coordinates_log

    subroutine calculate_aligned_position(annotation, text_width, text_height, &
                                        adjusted_x, adjusted_y)
        !! Calculate aligned text position based on alignment settings
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: text_width, text_height
        real(wp), intent(out) :: adjusted_x, adjusted_y
        
        adjusted_x = annotation%x
        adjusted_y = annotation%y
        
        ! Horizontal alignment
        select case (trim(annotation%alignment))
        case ('center')
            adjusted_x = annotation%x - text_width / 2.0_wp
        case ('right')
            adjusted_x = annotation%x - text_width
        case ('left')
            ! No adjustment needed
        end select
        
        ! Vertical alignment (simple baseline positioning for now)
        ! Future enhancement: support 'top', 'center', 'bottom', 'baseline'
    end subroutine calculate_aligned_position

    subroutine calculate_rotated_bounds(annotation, bounds)
        !! Calculate bounding box for rotated text
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(out) :: bounds(4)  ! xmin, xmax, ymin, ymax
        
        real(wp) :: text_width, text_height
        real(wp) :: cos_angle, sin_angle, angle_rad
        real(wp) :: corner_x(4), corner_y(4)
        
        ! Calculate text dimensions (basic approximation for now)
        text_width = real(calculate_text_width(annotation%text), wp)
        text_height = real(calculate_text_height(annotation%text), wp)
        
        ! Convert rotation to radians
        angle_rad = annotation%rotation * 3.14159265359_wp / 180.0_wp
        cos_angle = cos(angle_rad)
        sin_angle = sin(angle_rad)
        
        ! Calculate rotated corners
        corner_x(1) = 0.0_wp
        corner_y(1) = 0.0_wp
        
        corner_x(2) = text_width * cos_angle
        corner_y(2) = text_width * sin_angle
        
        corner_x(3) = text_width * cos_angle - text_height * sin_angle
        corner_y(3) = text_width * sin_angle + text_height * cos_angle
        
        corner_x(4) = -text_height * sin_angle
        corner_y(4) = text_height * cos_angle
        
        ! Find bounding box
        bounds(1) = minval(corner_x) + annotation%x  ! xmin
        bounds(2) = maxval(corner_x) + annotation%x  ! xmax
        bounds(3) = minval(corner_y) + annotation%y  ! ymin
        bounds(4) = maxval(corner_y) + annotation%y  ! ymax
    end subroutine calculate_rotated_bounds

    function is_annotation_visible(annotation, plot_area) result(visible)
        !! Check if annotation is visible within plot area
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: plot_area(4)
        logical :: visible
        
        real(wp) :: pixel_x, pixel_y
        
        ! For simplicity, check if center point is within reasonable bounds
        ! More sophisticated implementation would check full text bounds
        
        if (annotation%coord_type == COORD_AXIS) then
            call transform_axis_coordinates(annotation, plot_area, pixel_x, pixel_y)
        else
            ! For other coordinate types, assume visible for now
            visible = .true.
            return
        end if
        
        visible = (pixel_x >= plot_area(1) - 50.0_wp .and. &
                  pixel_x <= plot_area(1) + plot_area(3) + 50.0_wp .and. &
                  pixel_y >= plot_area(2) - 50.0_wp .and. &
                  pixel_y <= plot_area(2) + plot_area(4) + 50.0_wp)
    end function is_annotation_visible

    subroutine validate_annotation_coordinates(annotation, valid, error_message)
        !! Validate annotation coordinate values
        type(text_annotation_t), intent(in) :: annotation
        logical, intent(out) :: valid
        character(len=256), intent(out) :: error_message
        
        valid = .true.
        error_message = ""
        
        ! Check for NaN coordinates
        if (ieee_is_nan(annotation%x)) then
            valid = .false.
            error_message = "X coordinate is NaN"
            return
        end if
        
        if (ieee_is_nan(annotation%y)) then
            valid = .false.
            error_message = "Y coordinate is NaN"
            return
        end if
        
        ! Check for infinite coordinates
        if (.not. ieee_is_finite(annotation%x)) then
            valid = .false.
            error_message = "X coordinate is infinite"
            return
        end if
        
        if (.not. ieee_is_finite(annotation%y)) then
            valid = .false.
            error_message = "Y coordinate is infinite"
            return
        end if
        
        ! Validate coordinate type
        if (annotation%coord_type < COORD_DATA .or. annotation%coord_type > COORD_AXIS) then
            valid = .false.
            error_message = "Invalid coordinate type"
            return
        end if
        
        ! For figure coordinates, check 0-1 range (with tolerance for extrapolation)
        if (annotation%coord_type == COORD_FIGURE) then
            if (annotation%x < -0.5_wp .or. annotation%x > 1.5_wp .or. &
                annotation%y < -0.5_wp .or. annotation%y > 1.5_wp) then
                call log_warning("Figure coordinates outside normal 0-1 range")
            end if
        end if
    end subroutine validate_annotation_coordinates

    subroutine validate_annotation_parameters(annotation, valid, error_message)
        !! Validate annotation parameter values
        type(text_annotation_t), intent(in) :: annotation
        logical, intent(out) :: valid
        character(len=256), intent(out) :: error_message
        
        valid = .true.
        error_message = ""
        
        ! Check font size
        if (annotation%font_size <= 0.0_wp) then
            valid = .false.
            error_message = "Font size must be positive"
            return
        end if
        
        if (annotation%font_size > 200.0_wp) then
            valid = .false.
            error_message = "Font size too large (>200)"
            return
        end if
        
        ! Check text content
        if (len_trim(annotation%text) == 0) then
            valid = .false.
            error_message = "Text content cannot be empty"
            return
        end if
        
        ! Check rotation angle (normalize if needed)
        if (ieee_is_nan(annotation%rotation)) then
            valid = .false.
            error_message = "Rotation angle is NaN"
            return
        end if
        
        ! Check alignment
        select case (trim(annotation%alignment))
        case ('left', 'center', 'right')
            ! Valid alignments
        case default
            valid = .false.
            error_message = "Invalid alignment: " // trim(annotation%alignment)
            return
        end select
    end subroutine validate_annotation_parameters

end module fortplot_annotations