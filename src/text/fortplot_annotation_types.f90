module fortplot_annotation_types
    !! Type definitions and basic operations for text annotations
    !! 
    !! Provides:
    !! - Annotation type definitions with all properties
    !! - Coordinate system constants
    !! - Creation and destruction functions
    !! - Basic validation functions
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
    use fortplot_logging, only: log_warning
    implicit none
    
    private
    
    ! Coordinate system constants
    integer, parameter, public :: COORD_DATA = 1
    integer, parameter, public :: COORD_FIGURE = 2  
    integer, parameter, public :: COORD_AXIS = 3
    
    ! Simple color type for background boxes (lightweight version)
    type, public :: annotation_color_t
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
        character(len=16) :: ha = 'left'     ! Horizontal alignment
        character(len=16) :: va = 'bottom'   ! Vertical alignment
        character(len=64) :: font_family = 'DejaVu Sans'
        character(len=16) :: weight = 'normal'  ! Font weight
        character(len=16) :: style = 'normal'   ! Font style
        
        ! Color properties
        real(wp) :: color(3) = [0.0_wp, 0.0_wp, 0.0_wp]  ! RGB text color (black)
        real(wp) :: alpha = 1.0_wp  ! Text transparency
        
        ! Background box properties
        logical :: has_bbox = .false.
        logical :: bbox = .false.  ! Alias for has_bbox
        type(annotation_color_t) :: bbox_color = annotation_color_t(1.0_wp, 1.0_wp, 1.0_wp, 1.0_wp) ! White
        
        ! Arrow annotation properties  
        logical :: has_arrow = .false.
        real(wp) :: arrow_x = 0.0_wp  ! Arrow target x
        real(wp) :: arrow_y = 0.0_wp  ! Arrow target y
        integer :: arrow_coord_type = COORD_DATA  ! Arrow target coordinate type
        real(wp) :: xytext_x = 0.0_wp
        real(wp) :: xytext_y = 0.0_wp
        integer :: xytext_coord_type = COORD_DATA
        character(len=64) :: arrowstyle = ''  ! Arrow properties string
    end type text_annotation_t
    
    ! Public interface
    public :: create_text_annotation, destroy_text_annotation
    public :: validate_annotation_coordinates, validate_annotation_parameters
    public :: validate_annotation

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
        annotation%ha = 'left'
        annotation%va = 'bottom'
        annotation%weight = 'normal'
        annotation%style = 'normal'
        
        ! Set default color properties
        annotation%color = [0.0_wp, 0.0_wp, 0.0_wp]  ! Black
        annotation%alpha = 1.0_wp
        
        ! Set default box and arrow properties
        annotation%has_bbox = .false.
        annotation%bbox = .false.
        annotation%has_arrow = .false.
        annotation%arrow_x = 0.0_wp
        annotation%arrow_y = 0.0_wp
        annotation%arrow_coord_type = COORD_DATA
        annotation%arrowstyle = ''
    end function create_text_annotation

    subroutine destroy_text_annotation(annotation)
        !! Clean up text annotation resources
        type(text_annotation_t), intent(inout) :: annotation
        
        ! For now, simple reset - future versions may have dynamic allocations
        annotation%text = ""
        annotation%x = 0.0_wp
        annotation%y = 0.0_wp
    end subroutine destroy_text_annotation

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
            error_message = "Font size must be positive (font parameter invalid)"
            return
        end if
        
        if (annotation%font_size > 200.0_wp) then
            valid = .false.
            error_message = "Font size too large (>200, font parameter invalid)"
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
        
        ! Large rotation angles are automatically normalized to 0-360 range
        ! This is valid behavior, not an error
        
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

    subroutine validate_annotation(annotation, valid, error_message)
        !! Comprehensive annotation validation combining coordinate and parameter checks
        type(text_annotation_t), intent(in) :: annotation
        logical, intent(out) :: valid
        character(len=*), intent(out) :: error_message
        
        logical :: coord_valid, param_valid
        character(len=256) :: coord_error, param_error
        
        ! Check coordinates first
        call validate_annotation_coordinates(annotation, coord_valid, coord_error)
        
        ! Check parameters
        call validate_annotation_parameters(annotation, param_valid, param_error)
        
        ! Combine results
        if (.not. coord_valid .and. .not. param_valid) then
            valid = .false.
            error_message = "Coordinate error: " // trim(coord_error) // &
                          "; Parameter error: " // trim(param_error)
        else if (.not. coord_valid) then
            valid = .false.
            error_message = "Coordinate error: " // trim(coord_error)
        else if (.not. param_valid) then
            valid = .false.
            error_message = "Parameter error: " // trim(param_error)
        else
            valid = .true.
            error_message = ""
        end if
    end subroutine validate_annotation

end module fortplot_annotation_types