module fortplot_plot_annotations
    !! Plot annotation operations module
    !! 
    !! This module handles text and arrow annotations for plots including
    !! coordinate system specification and styling options.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_annotations, only: text_annotation_t, COORD_DATA, COORD_FIGURE, COORD_AXIS, &
        validate_annotation
    use fortplot_logging, only: log_warning

    implicit none

    private
    public :: add_text_annotation
    public :: add_arrow_annotation

contains

    subroutine add_text_annotation(self, x, y, text, coord_type, font_size, rotation, &
                                  ha, va, bbox, color, alpha, weight, style)
        !! Add text annotation to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        character(len=*), intent(in), optional :: coord_type
        real(wp), intent(in), optional :: font_size
        real(wp), intent(in), optional :: rotation
        character(len=*), intent(in), optional :: ha, va
        logical, intent(in), optional :: bbox
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: alpha
        character(len=*), intent(in), optional :: weight, style
        
        type(text_annotation_t) :: annotation
        logical :: valid_annotation
        character(len=256) :: error_message
        
        ! Set annotation properties
        annotation%x = x
        annotation%y = y
        annotation%text = text
        
        if (present(coord_type)) then
            select case (coord_type)
            case ('data')
                annotation%coord_type = COORD_DATA
            case ('figure')
                annotation%coord_type = COORD_FIGURE
            case ('axes')
                annotation%coord_type = COORD_AXIS
            case default
                annotation%coord_type = COORD_DATA
            end select
        else
            annotation%coord_type = COORD_DATA
        end if
        
        if (present(font_size)) annotation%font_size = font_size
        if (present(rotation)) annotation%rotation = rotation
        if (present(ha)) annotation%ha = ha
        if (present(va)) annotation%va = va
        if (present(bbox)) then
            annotation%bbox = bbox
            annotation%has_bbox = bbox
        end if
        if (present(color)) annotation%color = color
        if (present(alpha)) annotation%alpha = alpha
        if (present(weight)) annotation%weight = weight
        if (present(style)) annotation%style = style
        
        ! Validate annotation at creation time (Issue #870: prevent duplicate warnings)
        call validate_annotation(annotation, valid_annotation, error_message)
        annotation%validated = .true.
        annotation%valid = valid_annotation
        
        ! Only add valid annotations to prevent rendering issues
        if (valid_annotation) then
            ! Add annotation to figure
            if (.not. allocated(self%annotations)) then
                allocate(self%annotations(self%max_annotations))
            end if
            
            self%annotation_count = self%annotation_count + 1
            self%annotations(self%annotation_count) = annotation
        else
            ! Issue warning once at creation time, not during rendering
            call log_warning("Skipping invalid annotation: " // trim(error_message))
        end if
    end subroutine add_text_annotation

    subroutine add_arrow_annotation(self, text, xy, xytext, xy_coord_type, xytext_coord_type, &
                                   arrowprops, font_size, color, alpha)
        !! Add arrow annotation to figure
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: xy(2)  ! Arrow point
        real(wp), intent(in) :: xytext(2)  ! Text location
        character(len=*), intent(in), optional :: xy_coord_type
        character(len=*), intent(in), optional :: xytext_coord_type
        character(len=*), intent(in), optional :: arrowprops  ! JSON-like string
        real(wp), intent(in), optional :: font_size
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: alpha
        
        type(text_annotation_t) :: annotation
        logical :: valid_annotation
        character(len=256) :: error_message
        
        ! Set annotation properties
        annotation%x = xytext(1)
        annotation%y = xytext(2)
        annotation%text = text
        annotation%arrow_x = xy(1)
        annotation%arrow_y = xy(2)
        annotation%has_arrow = .true.
        
        if (present(xytext_coord_type)) then
            select case (xytext_coord_type)
            case ('data')
                annotation%coord_type = COORD_DATA
            case ('figure')
                annotation%coord_type = COORD_FIGURE
            case ('axes')
                annotation%coord_type = COORD_AXIS
            case default
                annotation%coord_type = COORD_DATA
            end select
        else
            annotation%coord_type = COORD_DATA
        end if
        
        if (present(xy_coord_type)) then
            select case (xy_coord_type)
            case ('data')
                annotation%arrow_coord_type = COORD_DATA
            case ('figure')
                annotation%arrow_coord_type = COORD_FIGURE
            case ('axes')
                annotation%arrow_coord_type = COORD_AXIS
            case default
                annotation%arrow_coord_type = COORD_DATA
            end select
        else
            annotation%arrow_coord_type = COORD_DATA
        end if
        
        if (present(arrowprops)) annotation%arrowstyle = arrowprops
        if (present(font_size)) annotation%font_size = font_size
        if (present(color)) annotation%color = color
        if (present(alpha)) annotation%alpha = alpha
        
        ! Validate annotation at creation time (Issue #870: prevent duplicate warnings)
        call validate_annotation(annotation, valid_annotation, error_message)
        annotation%validated = .true.
        annotation%valid = valid_annotation
        
        ! Only add valid annotations to prevent rendering issues
        if (valid_annotation) then
            ! Add annotation to figure
            if (.not. allocated(self%annotations)) then
                allocate(self%annotations(self%max_annotations))
            end if
            
            self%annotation_count = self%annotation_count + 1
            self%annotations(self%annotation_count) = annotation
        else
            ! Issue warning once at creation time, not during rendering
            call log_warning("Skipping invalid annotation: " // trim(error_message))
        end if
    end subroutine add_arrow_annotation

end module fortplot_plot_annotations