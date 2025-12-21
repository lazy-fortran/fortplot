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
    use fortplot_annotations, only: text_annotation_t, COORD_DATA, &
                                    COORD_FIGURE, COORD_AXIS
    use fortplot_logging, only: log_info, log_warning
    implicit none

    private
    public :: render_figure_annotations

contains

    subroutine render_figure_annotations(backend, annotations, annotation_count, &
                                         x_min, x_max, y_min, y_max, &
                                         width, height, dpi, &
                                         margin_left, margin_right, &
                                         margin_bottom, margin_top)
        !! Render all annotations for the current figure
        !!
        !! This is the main entry point called from figure_render() that processes
        !! all stored annotations and dispatches them to the appropriate backend.
        !! Uses existing backend text rendering infrastructure.

        use fortplot_ascii, only: ascii_context
        use fortplot_pdf, only: pdf_context
        use fortplot_raster, only: raster_context

        class(plot_context), intent(inout) :: backend
        type(text_annotation_t), intent(in) :: annotations(:)
        integer, intent(in) :: annotation_count
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: width, height
        real(wp), intent(in) :: dpi
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top

        integer :: i
        logical :: valid_annotation
        character(len=256) :: error_message
        integer :: canvas_width, canvas_height

        ! Early exit if no annotations
        if (annotation_count == 0) return

        canvas_width = backend%width
        canvas_height = backend%height
        associate (dw => width, dh => height)
        end associate

        call log_info("Rendering annotations: processing "// &
                      trim(adjustl(int_to_char(annotation_count)))//" annotations")

        ! Process each annotation
        do i = 1, annotation_count
            ! Skip re-validation if already validated at creation time.
            ! Issue #870: prevent duplicate warnings.
            if (annotations(i)%validated) then
                ! Use stored validation result to skip already-invalid annotations
                if (.not. annotations(i)%valid) then
                    cycle  ! Already warned at creation time, skip silently
                end if
                valid_annotation = .true.
                ! Skip validation for already-validated annotations
            else
                ! Fallback validation for annotations created without validation
                call validate_annotation_for_rendering(annotations(i), &
                                                       valid_annotation, error_message)
                if (.not. valid_annotation) then
                    call log_warning("Skipping invalid annotation "// &
                                     trim(adjustl(int_to_char(i)))//": "// &
                                     trim(error_message))
                    cycle
                end if
            end if

            select type (bk => backend)
            class is (raster_context)
                call render_annotation_text_raster(bk, annotations(i), dpi)
            class is (pdf_context)
                call render_annotation_text_pdf(bk, annotations(i))
            class is (ascii_context)
                call render_annotation_text_ascii(bk, annotations(i), x_min, x_max, &
                                                  y_min, y_max, canvas_width, &
                                                  canvas_height, margin_left, &
                                                  margin_right, margin_bottom, &
                                                  margin_top)
            class default
                call backend%color(annotations(i)%color(1), &
                                   annotations(i)%color(2), &
                                   annotations(i)%color(3))
                call backend%text(annotations(i)%x, annotations(i)%y, &
                                  trim(annotations(i)%text))
            end select

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

    subroutine render_annotation_text_raster(backend, annotation, dpi)
        use fortplot_annotations, only: COORD_DATA, COORD_FIGURE, COORD_AXIS
        use fortplot_raster, only: raster_context
        class(raster_context), intent(inout) :: backend
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: dpi

        real(wp) :: x_px, y_px
        real(wp) :: x_data, y_data
        real(wp) :: pixel_height
        character(len=16) :: ha, va

        ha = trim(annotation%ha)
        if (len_trim(ha) == 0) ha = 'left'
        va = trim(annotation%va)
        if (len_trim(va) == 0) va = 'bottom'

        pixel_height = max(1.0_wp, annotation%font_size*dpi/72.0_wp)

        select case (annotation%coord_type)
        case (COORD_DATA)
            x_data = annotation%x
            y_data = annotation%y
            x_px = (x_data - backend%x_min)/(backend%x_max - backend%x_min)* &
                   real(backend%plot_area%width, wp) + real(backend%plot_area%left, wp)
            y_px = real(backend%plot_area%bottom + backend%plot_area%height, wp) - &
                   (y_data - backend%y_min)/(backend%y_max - backend%y_min)* &
                   real(backend%plot_area%height, wp)
        case (COORD_AXIS)
            x_px = real(backend%plot_area%left, wp) + annotation%x* &
                   real(backend%plot_area%width, wp)
            y_px = real(backend%plot_area%bottom, wp) + (1.0_wp - annotation%y)* &
                   real(backend%plot_area%height, wp)
        case (COORD_FIGURE)
            x_px = annotation%x*real(backend%width, wp)
            y_px = (1.0_wp - annotation%y)*real(backend%height, wp)
        case default
            x_px = annotation%x
            y_px = annotation%y
        end select

        call backend%draw_text_styled(x_px, y_px, trim(annotation%text), pixel_height, &
                                      annotation%rotation, ha, va, &
                                      annotation%has_bbox, annotation%color)
    end subroutine render_annotation_text_raster

    subroutine render_annotation_text_pdf(backend, annotation)
        use fortplot_annotations, only: COORD_DATA, COORD_FIGURE, COORD_AXIS
        use fortplot_pdf, only: pdf_context
        class(pdf_context), intent(inout) :: backend
        type(text_annotation_t), intent(in) :: annotation

        real(wp) :: x_pt, y_pt
        real(wp) :: x_data, y_data
        character(len=16) :: ha, va

        ha = trim(annotation%ha)
        if (len_trim(ha) == 0) ha = 'left'
        va = trim(annotation%va)
        if (len_trim(va) == 0) va = 'bottom'

        select case (annotation%coord_type)
        case (COORD_DATA)
            x_data = annotation%x
            y_data = annotation%y
            x_pt = real(backend%plot_area%left, wp) + &
                   (x_data - backend%x_min)/(backend%x_max - backend%x_min)* &
                   real(backend%plot_area%width, wp)
            y_pt = real(backend%plot_area%bottom, wp) + &
                   (y_data - backend%y_min)/(backend%y_max - backend%y_min)* &
                   real(backend%plot_area%height, wp)
        case (COORD_AXIS)
            x_pt = real(backend%plot_area%left, wp) + annotation%x* &
                   real(backend%plot_area%width, wp)
            y_pt = real(backend%plot_area%bottom, wp) + annotation%y* &
                   real(backend%plot_area%height, wp)
        case (COORD_FIGURE)
            x_pt = annotation%x*real(backend%width, wp)
            y_pt = annotation%y*real(backend%height, wp)
        case default
            x_pt = annotation%x
            y_pt = annotation%y
        end select

        call backend%draw_text_styled(x_pt, y_pt, trim(annotation%text), &
                                      annotation%font_size, annotation%rotation, &
                                      ha, va, annotation%has_bbox, annotation%color)
    end subroutine render_annotation_text_pdf

    subroutine render_annotation_text_ascii(backend, annotation, x_min, x_max, y_min, &
                                            y_max, width, height, margin_left, &
                                            margin_right, margin_bottom, margin_top)
        use fortplot_ascii, only: ascii_context
        class(ascii_context), intent(inout) :: backend
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top

        real(wp) :: render_x, render_y

        call transform_annotation_to_rendering_coords(annotation, x_min, x_max, &
                                                      y_min, y_max, width, height, &
                                                      margin_left, margin_right, &
                                                      margin_bottom, margin_top, &
                                                      render_x, render_y)
        call backend%text(render_x, render_y, trim(annotation%text))
    end subroutine render_annotation_text_ascii

    pure subroutine map_annotation_to_data_coords(annotation, x_min, x_max, y_min, &
                                                  y_max, x, y)
        type(text_annotation_t), intent(in) :: annotation
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(out) :: x, y

        select case (annotation%coord_type)
        case (COORD_DATA)
            x = annotation%x
            y = annotation%y
        case (COORD_FIGURE, COORD_AXIS)
            x = x_min + annotation%x*(x_max - x_min)
            y = y_min + annotation%y*(y_max - y_min)
        case default
            x = annotation%x
            y = annotation%y
        end select
    end subroutine map_annotation_to_data_coords

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

        ! Use coordinate transformation system for ASCII canvas coordinates.
        select case (annotation%coord_type)
        case (COORD_DATA)
            ! Data coordinates: transform to plot area
            ! Margins are fractional (0.15 = 15% of figure width/height)
            render_x = margin_left*real(width, wp) + &
                       (annotation%x - x_min)/(x_max - x_min)* &
                       real(width, wp)*(1.0_wp - margin_left - margin_right)
            render_y = margin_bottom*real(height, wp) + &
                       (annotation%y - y_min)/(y_max - y_min)* &
                       real(height, wp)*(1.0_wp - margin_bottom - margin_top)

        case (COORD_FIGURE)
            ! Figure coordinates: 0-1 relative to entire figure
            render_x = annotation%x*real(width, wp)
            render_y = annotation%y*real(height, wp)

        case (COORD_AXIS)
            ! Axis coordinates: 0-1 relative to plot area
            render_x = margin_left*real(width, wp) + &
                       annotation%x*real(width, wp)*(1.0_wp - margin_left - &
                                                     margin_right)
            render_y = margin_bottom*real(height, wp) + &
                       annotation%y*real(height, wp)*(1.0_wp - margin_bottom - &
                                                      margin_top)

        case default
            ! Default to data coordinates
            render_x = annotation%x
            render_y = annotation%y
        end select

        ! ASCII canvas has row 1 at top, so invert y (figure/axis coords use y up).
        render_y = real(height, wp) - render_y + 1.0_wp
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

        associate (dw => width, dh => height, &
                   dml => margin_left, dmr => margin_right, &
                   dmb => margin_bottom, dmt => margin_top)
        end associate

        call map_xy_to_data_coords(annotation%arrow_coord_type, annotation%arrow_x, &
                                   annotation%arrow_y, x_min, x_max, y_min, y_max, &
                                   arrow_end_x, arrow_end_y)
        call map_xy_to_data_coords(annotation%coord_type, annotation%x, annotation%y, &
                                   x_min, x_max, y_min, y_max, arrow_start_x, &
                                   arrow_start_y)

        call backend%color(annotation%color(1), annotation%color(2), &
                           annotation%color(3))

        ! Draw arrow shaft and head (Matplotlib-style)
        call backend%line(arrow_start_x, arrow_start_y, arrow_end_x, arrow_end_y)
        call backend%draw_arrow(arrow_end_x, arrow_end_y, arrow_end_x - arrow_start_x, &
                                arrow_end_y - arrow_start_y, 1.0_wp, &
                                trim(annotation%arrowstyle))
    end subroutine render_annotation_arrow

    pure subroutine map_xy_to_data_coords(coord_type, x_in, y_in, x_min, x_max, y_min, &
                                          y_max, x, y)
        integer, intent(in) :: coord_type
        real(wp), intent(in) :: x_in, y_in
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(out) :: x, y

        select case (coord_type)
        case (COORD_DATA)
            x = x_in
            y = y_in
        case (COORD_FIGURE, COORD_AXIS)
            x = x_min + x_in*(x_max - x_min)
            y = y_min + y_in*(y_max - y_min)
        case default
            x = x_in
            y = y_in
        end select
    end subroutine map_xy_to_data_coords

    pure function int_to_char(num) result(str)
        !! Simple integer to character conversion
        integer, intent(in) :: num
        character(len=12) :: str
        write (str, '(I0)') num
    end function int_to_char

    ! Import validation from existing annotation_types module
    ! (Interface removed - direct module use via use statement)

end module fortplot_annotation_rendering
