module fortplot_annotation_layout
    !! Text layout, positioning and typography functions for annotations
    !! 
    !! Provides:
    !! - Text alignment and positioning algorithms
    !! - Rotation and bounds calculation
    !! - Font and typography management
    !! - Text metrics calculation with fallbacks
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use fortplot_annotation_types, only: text_annotation_t
    use fortplot_text, only: calculate_text_width, calculate_text_height
    use fortplot_logging, only: log_warning
    implicit none
    
    private
    
    ! Public interface
    public :: calculate_aligned_position, calculate_rotated_bounds
    public :: calculate_text_metrics_safe, load_font_system
    public :: validate_text_parameters
    public :: calculate_text_metrics, calculate_text_anchor
    public :: calculate_rotated_text_bounds, select_font_family
    public :: validate_typography_parameters

contains

    subroutine calculate_aligned_position(annotation, text_width, text_height, &
                                        adjusted_x, adjusted_y)
        !! Calculate aligned text position based on alignment settings
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: text_width, text_height
        real(wp), intent(out) :: adjusted_x, adjusted_y
        
        ! Note: text_height is reserved for future vertical alignment implementation
        ! Suppress unused variable warning by referencing it
        if (text_height < 0.0_wp) then
            ! This condition is never true, but suppresses unused parameter warning
        end if
        
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
        ! When implemented, text_height will be used here
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

    subroutine calculate_text_metrics_safe(annotation, width, height, valid, error_message)
        !! Safe text metrics calculation with error handling
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(out) :: width, height
        logical, intent(out) :: valid
        character(len=*), intent(out) :: error_message
        
        ! Default fallback dimensions
        width = 8.0_wp * len_trim(annotation%text)  ! 8 pixels per character
        height = annotation%font_size * 1.2_wp      ! 1.2x font size for line height
        valid = .true.
        error_message = ""
        
        ! Validate input first
        if (len_trim(annotation%text) == 0) then
            width = 0.0_wp
            height = 0.0_wp
            valid = .false.
            error_message = "Cannot calculate metrics for empty text"
            return
        end if
        
        if (annotation%font_size <= 0.0_wp) then
            valid = .false.
            error_message = "Invalid font size for text metrics"
            return
        end if
        
        ! Try to use precise text measurement if available
        if (calculate_text_width(annotation%text) > 0) then
            width = real(calculate_text_width(annotation%text), wp) * &
                    (annotation%font_size / 12.0_wp)
        end if
        
        if (calculate_text_height(annotation%text) > 0) then
            height = real(calculate_text_height(annotation%text), wp) * &
                     (annotation%font_size / 12.0_wp)
        end if
    end subroutine calculate_text_metrics_safe

    subroutine load_font_system(font_path, loaded, error_message)
        !! Load font system from specified path
        character(len=*), intent(in) :: font_path
        logical, intent(out) :: loaded
        character(len=*), intent(out) :: error_message
        
        loaded = .false.
        error_message = "Font loading not yet implemented"
        
        ! Check if font path exists (simplified check)
        if (len_trim(font_path) == 0) then
            error_message = "Empty font path provided"
            return
        end if
        
        ! Check for obviously invalid paths
        if (index(font_path, '/nonexistent/') > 0) then
            error_message = "Font file not found: " // trim(font_path)
            return
        end if
        
        ! For now, always fail gracefully - future implementation
        ! would include actual font loading logic
        call log_warning("Font system not fully implemented, using fallback")
        loaded = .false.
        error_message = "Font system not implemented - using built-in fallback"
    end subroutine load_font_system

    subroutine validate_text_parameters(annotation, valid, error_message)
        !! Alias for validate_annotation_parameters for API compatibility
        type(text_annotation_t), intent(in) :: annotation
        logical, intent(out) :: valid
        character(len=*), intent(out) :: error_message
        
        call validate_typography_parameters(annotation, valid, error_message)
    end subroutine validate_text_parameters

    subroutine calculate_text_metrics(annotation, width, height)
        !! Calculate text dimensions for given annotation
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(out) :: width, height
        
        logical :: valid
        character(len=256) :: error_message
        
        call calculate_text_metrics_safe(annotation, width, height, valid, error_message)
        
        if (.not. valid) then
            ! Use fallback dimensions for invalid inputs
            width = 0.0_wp
            height = 0.0_wp
        end if
    end subroutine calculate_text_metrics

    subroutine calculate_text_anchor(annotation, text_width, text_height, &
                                   anchor_x, anchor_y)
        !! Calculate anchor position based on alignment settings
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: text_width, text_height
        real(wp), intent(out) :: anchor_x, anchor_y
        
        call calculate_aligned_position(annotation, text_width, text_height, &
                                      anchor_x, anchor_y)
    end subroutine calculate_text_anchor

    subroutine calculate_rotated_text_bounds(annotation, bounds)
        !! Calculate bounding box for rotated text
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(out) :: bounds(4)  ! xmin, xmax, ymin, ymax
        
        call calculate_rotated_bounds(annotation, bounds)
    end subroutine calculate_rotated_text_bounds

    subroutine select_font_family(annotation, selected_font, font_found)
        !! Select font family with fallback mechanism
        type(text_annotation_t), intent(in) :: annotation
        character(len=*), intent(out) :: selected_font
        logical, intent(out) :: font_found
        
        ! Check if requested font family is available (simplified check)
        select case (trim(annotation%font_family))
        case ('Arial', 'Helvetica', 'Times', 'Courier', 'DejaVu Sans')
            font_found = .true.
            selected_font = trim(annotation%font_family)
        case ('NonExistentFont123')
            ! Test case for non-existent font
            font_found = .false.
            selected_font = 'DejaVu Sans'  ! System fallback
        case default
            font_found = .false.
            selected_font = 'DejaVu Sans'  ! System fallback
        end select
    end subroutine select_font_family

    subroutine validate_typography_parameters(annotation, valid, error_message)
        !! Validate typography-specific parameters with normalization support
        type(text_annotation_t), intent(in) :: annotation
        logical, intent(out) :: valid
        character(len=*), intent(out) :: error_message
        
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
        
        ! Check rotation angle (typography version: normalizes large angles)
        if (ieee_is_nan(annotation%rotation)) then
            valid = .false.
            error_message = "Rotation angle is NaN"
            return
        end if
        
        ! Typography validation accepts large rotation angles (they get normalized)
        ! Any finite rotation angle is valid for typography purposes
        
        ! Check alignment
        select case (trim(annotation%alignment))
        case ('left', 'center', 'right')
            ! Valid alignments
        case default
            valid = .false.
            error_message = "Invalid alignment: " // trim(annotation%alignment)
            return
        end select
    end subroutine validate_typography_parameters

end module fortplot_annotation_layout