module fortplot_text_stub
    !! Text and annotation functions implementation
    !! Provides pyplot-style text() and annotate() functions that integrate
    !! with the global figure and annotation system
    
    use iso_fortran_env, only: wp => real64
    use fortplot_global, only: global_figure
    use fortplot_plot_annotations, only: add_text_annotation, add_arrow_annotation
    use fortplot_annotations, only: COORD_DATA, COORD_FIGURE, COORD_AXIS
    use fortplot_logging, only: log_error
    
    implicit none
    private
    
    public :: text, annotate
    
contains

    subroutine text(x, y, text_content, coord_type, font_size, rotation, alignment, has_bbox, ha)
        !! Add text annotation to the current figure
        !! 
        !! Parameters:
        !!   x, y: Position coordinates
        !!   text_content: Text to display
        !!   coord_type: Coordinate system (COORD_DATA, COORD_FIGURE, COORD_AXIS)
        !!   font_size: Text size in points
        !!   rotation: Rotation angle in degrees
        !!   alignment: Text alignment (left, center, right)
        !!   has_bbox: Add background box
        !!   ha: Horizontal alignment alias
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text_content
        integer, intent(in), optional :: coord_type
        character(len=*), intent(in), optional :: alignment, ha
        real(wp), intent(in), optional :: font_size, rotation
        logical, intent(in), optional :: has_bbox
        
        character(len=16) :: coord_type_str
        
        ! Check if global figure is allocated
        if (.not. allocated(global_figure)) then
            call log_error("text: No current figure - call figure() first")
            return
        end if
        
        ! Convert coordinate type to string
        coord_type_str = 'data'
        if (present(coord_type)) then
            select case (coord_type)
            case (COORD_DATA)
                coord_type_str = 'data'
            case (COORD_FIGURE)
                coord_type_str = 'figure'
            case (COORD_AXIS)
                coord_type_str = 'axes'
            end select
        end if
        
        ! Determine horizontal alignment
        block
            character(len=:), allocatable :: ha_str
            if (present(ha)) then
                ha_str = ha
            else if (present(alignment)) then
                ha_str = alignment
            else
                ha_str = ''
            end if
            
            ! Add text annotation to current figure
            call add_text_annotation(global_figure, x, y, text_content, &
                                    coord_type=coord_type_str, &
                                    font_size=font_size, &
                                    rotation=rotation, &
                                    ha=ha_str, &
                                    bbox=has_bbox)
        end block
    end subroutine text

    subroutine annotate(text_content, xy, xytext, xy_coord_type, xytext_coord_type, &
                       arrow_style, arrow_color, font_size, has_bbox, alignment, ha)
        !! Add arrow annotation to the current figure
        !!
        !! Parameters:
        !!   text_content: Text to display
        !!   xy: Arrow target coordinates [x, y]
        !!   xytext: Text position coordinates [x, y] (optional, defaults to xy)
        !!   xy_coord_type: Coordinate system for arrow target
        !!   xytext_coord_type: Coordinate system for text position
        !!   arrow_style: Arrow styling properties
        !!   arrow_color: Arrow color specification
        !!   font_size: Text size in points
        !!   has_bbox: Add background box
        !!   alignment: Text alignment
        !!   ha: Horizontal alignment alias
        character(len=*), intent(in) :: text_content
        real(wp), dimension(2), intent(in) :: xy
        real(wp), dimension(2), intent(in), optional :: xytext
        integer, intent(in), optional :: xy_coord_type, xytext_coord_type
        character(len=*), intent(in), optional :: arrow_style, arrow_color, alignment, ha
        real(wp), intent(in), optional :: font_size
        logical, intent(in), optional :: has_bbox
        
        character(len=16) :: xy_coord_str, xytext_coord_str
        character(len=1) :: dummy_c
        logical :: dummy_l
        real(wp), dimension(2) :: text_pos
        ! Quiet unused optionals to keep stub warning-free
        if (present(arrow_color)) dummy_c = arrow_color(1:1)
        if (present(alignment)) dummy_c = alignment(1:1)
        if (present(ha)) dummy_c = ha(1:1)
        if (present(has_bbox)) dummy_l = has_bbox
        
        ! Check if global figure is allocated
        if (.not. allocated(global_figure)) then
            call log_error("annotate: No current figure - call figure() first")
            return
        end if
        
        ! Set text position (default to xy if not specified)
        text_pos = xy
        if (present(xytext)) text_pos = xytext
        
        ! Convert coordinate types to strings
        xy_coord_str = 'data'
        if (present(xy_coord_type)) then
            select case (xy_coord_type)
            case (COORD_DATA)
                xy_coord_str = 'data'
            case (COORD_FIGURE)
                xy_coord_str = 'figure'
            case (COORD_AXIS)
                xy_coord_str = 'axes'
            end select
        end if
        
        xytext_coord_str = xy_coord_str  ! Default to same as xy
        if (present(xytext_coord_type)) then
            select case (xytext_coord_type)
            case (COORD_DATA)
                xytext_coord_str = 'data'
            case (COORD_FIGURE)
                xytext_coord_str = 'figure'
            case (COORD_AXIS)
                xytext_coord_str = 'axes'
            end select
        end if
        
        ! Add arrow annotation to current figure
        call add_arrow_annotation(global_figure, text_content, xy, text_pos, &
                                 xy_coord_type=xy_coord_str, &
                                 xytext_coord_type=xytext_coord_str, &
                                 arrowprops=arrow_style, &
                                 font_size=font_size)
    end subroutine annotate

end module fortplot_text_stub
