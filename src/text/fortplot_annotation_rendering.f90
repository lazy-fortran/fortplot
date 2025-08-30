module fortplot_annotation_rendering
    !! Annotation rendering dispatch system for fortplot
    !! 
    !! This module provides the missing bridge between stored annotations
    !! and backend-specific rendering methods. It processes annotations
    !! from the figure state and dispatches them to appropriate backends.
    !! 
    !! CRITICAL: This module REUSES existing backend infrastructure:
    !! - ASCII backend: Uses text_element_t and add_text_element
    !! - Raster backend: Uses raster_draw_text directly  
    !! - PDF backend: Uses pdf text rendering methods
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_annotations, only: text_annotation_t, COORD_DATA, COORD_FIGURE, COORD_AXIS
    use fortplot_logging, only: log_info, log_warning
    implicit none
    
    private
    public :: render_figure_annotations

contains

    subroutine render_figure_annotations(backend, annotations, annotation_count, &
                                        x_min, x_max, y_min, y_max, &
                                        width, height, &
                                        margin_left, margin_right, &
                                        margin_bottom, margin_top)
        !! Render all annotations for the current figure
        !! 
        !! This is the main entry point called from figure_render() that processes
        !! all stored annotations and dispatches them to the appropriate backend.
        !! Uses existing backend text rendering infrastructure.
        
        class(plot_context), intent(inout) :: backend
        type(text_annotation_t), intent(in) :: annotations(:)
        integer, intent(in) :: annotation_count
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        
        integer :: i
        real(wp) :: render_x, render_y
        logical :: valid_annotation
        character(len=256) :: error_message
        
        ! Early exit if no annotations
        if (annotation_count == 0) return
        
        call log_info("Rendering annotations: processing " // &
                     trim(adjustl(int_to_char(annotation_count))) // " annotations")
        
        ! Process each annotation
        do i = 1, annotation_count
            ! Validate annotation before rendering
            call validate_annotation_for_rendering(annotations(i), valid_annotation, error_message)
            if (.not. valid_annotation) then
                call log_warning("Skipping invalid annotation " // &
                               trim(adjustl(int_to_char(i))) // ": " // trim(error_message))
                cycle
            end if
            
            ! Transform coordinates to rendering coordinates
            call transform_annotation_to_rendering_coords(annotations(i), &
                                                         x_min, x_max, y_min, y_max, &
                                                         width, height, &
                                                         margin_left, margin_right, &
                                                         margin_bottom, margin_top, &
                                                         render_x, render_y)
            
            ! Set annotation color
            call backend%color(annotations(i)%color(1), &
                              annotations(i)%color(2), &
                              annotations(i)%color(3))
            
            ! Render the annotation text using existing backend method
            call backend%text(render_x, render_y, trim(annotations(i)%text))
            
            ! Render arrow if present (simplified implementation)
            if (annotations(i)%has_arrow) then
                call render_annotation_arrow(backend, annotations(i), &
                                           x_min, x_max, y_min, y_max, &
                                           width, height, &
                                           margin_left, margin_right, &
                                           margin_bottom, margin_top)
            end if
        end do
        
        call log_info("Annotation rendering completed successfully")
    end subroutine render_figure_annotations

    subroutine validate_annotation_for_rendering(annotation, valid, error_message)
        !! Validate annotation for rendering (reuses existing validation)
        use fortplot_annotation_types, only: validate_annotation
        type(text_annotation_t), intent(in) :: annotation
        logical, intent(out) :: valid
        character(len=256), intent(out) :: error_message
        
        ! Use existing validation from annotation_types
        call validate_annotation(annotation, valid, error_message)
        
        ! Additional rendering-specific checks
        if (valid .and. len_trim(annotation%text) == 0) then
            valid = .false.
            error_message = "Empty text content"
        end if
    end subroutine validate_annotation_for_rendering

    subroutine transform_annotation_to_rendering_coords(annotation, &
                                                       x_min, x_max, y_min, y_max, &
                                                       width, height, &
                                                       margin_left, margin_right, &
                                                       margin_bottom, margin_top, &
                                                       render_x, render_y)
        !! Transform annotation coordinates to backend rendering coordinates
        !! Uses existing coordinate transformation infrastructure
        
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        real(wp), intent(out) :: render_x, render_y
        
        ! Use existing coordinate transformation system
        select case (annotation%coord_type)
        case (COORD_DATA)
            ! Data coordinates: transform to plot area
            ! Margins are fractional (0.15 = 15% of figure width/height)
            render_x = margin_left * real(width, wp) + &
                      (annotation%x - x_min) / (x_max - x_min) * &
                      real(width, wp) * (1.0_wp - margin_left - margin_right)
            render_y = margin_bottom * real(height, wp) + &
                      (annotation%y - y_min) / (y_max - y_min) * &
                      real(height, wp) * (1.0_wp - margin_bottom - margin_top)
                      
        case (COORD_FIGURE)
            ! Figure coordinates: 0-1 relative to entire figure
            render_x = annotation%x * real(width, wp)
            render_y = annotation%y * real(height, wp)
            
        case (COORD_AXIS)
            ! Axis coordinates: 0-1 relative to plot area  
            render_x = margin_left * real(width, wp) + &
                      annotation%x * real(width, wp) * (1.0_wp - margin_left - margin_right)
            render_y = margin_bottom * real(height, wp) + &
                      annotation%y * real(height, wp) * (1.0_wp - margin_bottom - margin_top)
            
        case default
            ! Default to data coordinates
            render_x = annotation%x
            render_y = annotation%y
        end select
    end subroutine transform_annotation_to_rendering_coords

    subroutine render_annotation_arrow(backend, annotation, &
                                      x_min, x_max, y_min, y_max, &
                                      width, height, &
                                      margin_left, margin_right, &
                                      margin_bottom, margin_top)
        !! Render arrow for annotation (simplified implementation)
        class(plot_context), intent(inout) :: backend
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top
        
        real(wp) :: arrow_start_x, arrow_start_y, arrow_end_x, arrow_end_y
        
        ! Transform arrow target coordinates
        select case (annotation%arrow_coord_type)
        case (COORD_DATA)
            arrow_end_x = margin_left * real(width, wp) + &
                         (annotation%arrow_x - x_min) / (x_max - x_min) * &
                         real(width, wp) * (1.0_wp - margin_left - margin_right)
            arrow_end_y = margin_bottom * real(height, wp) + &
                         (annotation%arrow_y - y_min) / (y_max - y_min) * &
                         real(height, wp) * (1.0_wp - margin_bottom - margin_top)
        case (COORD_FIGURE)
            arrow_end_x = annotation%arrow_x * real(width, wp)
            arrow_end_y = annotation%arrow_y * real(height, wp)
        case (COORD_AXIS)
            arrow_end_x = margin_left * real(width, wp) + &
                         annotation%arrow_x * real(width, wp) * (1.0_wp - margin_left - margin_right)
            arrow_end_y = margin_bottom * real(height, wp) + &
                         annotation%arrow_y * real(height, wp) * (1.0_wp - margin_bottom - margin_top)
        case default
            arrow_end_x = annotation%arrow_x
            arrow_end_y = annotation%arrow_y
        end select
        
        ! Transform text position coordinates  
        select case (annotation%xytext_coord_type)
        case (COORD_DATA)
            arrow_start_x = margin_left * real(width, wp) + &
                           (annotation%xytext_x - x_min) / (x_max - x_min) * &
                           real(width, wp) * (1.0_wp - margin_left - margin_right)
            arrow_start_y = margin_bottom * real(height, wp) + &
                           (annotation%xytext_y - y_min) / (y_max - y_min) * &
                           real(height, wp) * (1.0_wp - margin_bottom - margin_top)
        case (COORD_FIGURE)
            arrow_start_x = annotation%xytext_x * real(width, wp)
            arrow_start_y = annotation%xytext_y * real(height, wp)
        case (COORD_AXIS)
            arrow_start_x = margin_left * real(width, wp) + &
                           annotation%xytext_x * real(width, wp) * (1.0_wp - margin_left - margin_right)
            arrow_start_y = margin_bottom * real(height, wp) + &
                           annotation%xytext_y * real(height, wp) * (1.0_wp - margin_bottom - margin_top)
        case default
            arrow_start_x = annotation%xytext_x
            arrow_start_y = annotation%xytext_y
        end select
        
        ! Draw simple arrow line using existing line method
        call backend%line(arrow_start_x, arrow_start_y, arrow_end_x, arrow_end_y)
    end subroutine render_annotation_arrow

    pure function int_to_char(num) result(str)
        !! Simple integer to character conversion
        integer, intent(in) :: num
        character(len=12) :: str
        write(str, '(I0)') num
    end function int_to_char

    ! Import validation from existing annotation_types module  
    ! (Interface removed - direct module use via use statement)

end module fortplot_annotation_rendering