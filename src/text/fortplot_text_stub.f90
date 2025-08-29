module fortplot_text_stub
    !! Stub implementation for text and annotation functions
    !! These are placeholders for compatibility until full implementation
    !! See issue #491 for implementation roadmap
    
    use iso_fortran_env, only: wp => real64
    use fortplot_logging, only: log_warning
    
    implicit none
    private
    
    public :: text, annotate
    
contains

    subroutine text(x, y, text_content, coord_type, font_size, rotation, alignment, has_bbox, ha)
        !! Stub for text annotation
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text_content
        integer, intent(in), optional :: coord_type
        character(len=*), intent(in), optional :: alignment, ha
        real(wp), intent(in), optional :: font_size, rotation
        logical, intent(in), optional :: has_bbox
        
        call log_warning("text: Text annotations not yet implemented (see issue #491)")
    end subroutine text

    subroutine annotate(text_content, xy, xytext, xy_coord_type, xytext_coord_type, &
                       arrow_style, arrow_color, font_size, has_bbox, alignment, ha)
        !! Stub for annotation with arrow
        character(len=*), intent(in) :: text_content
        real(wp), dimension(2), intent(in) :: xy
        real(wp), dimension(2), intent(in), optional :: xytext
        integer, intent(in), optional :: xy_coord_type, xytext_coord_type
        character(len=*), intent(in), optional :: arrow_style, arrow_color, alignment, ha
        real(wp), intent(in), optional :: font_size
        logical, intent(in), optional :: has_bbox
        
        call log_warning("annotate: Annotations not yet implemented (see issue #491)")
    end subroutine annotate

end module fortplot_text_stub