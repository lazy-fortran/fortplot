module fortplot_text
    !! Main text module that provides backend-agnostic font and layout services
    use fortplot_text_fonts, only: init_text_system, cleanup_text_system, get_font_metrics
    use fortplot_text_fonts, only: get_font_ascent_ratio, find_font_by_name, find_any_available_font
    use fortplot_text_layout, only: calculate_text_width, calculate_text_height
    use fortplot_text_layout, only: calculate_text_descent
    use fortplot_text_layout, only: calculate_text_width_with_size
    use fortplot_text_layout, only: TITLE_FONT_SIZE, LABEL_FONT_SIZE, TICK_FONT_SIZE
    implicit none
    
    private

    ! Re-export public interface from sub-modules
    public :: init_text_system, cleanup_text_system, calculate_text_width, calculate_text_height
    public :: get_font_metrics, calculate_text_descent
    public :: get_font_ascent_ratio, find_font_by_name, find_any_available_font
    public :: calculate_text_width_with_size
    public :: TITLE_FONT_SIZE, LABEL_FONT_SIZE, TICK_FONT_SIZE

end module fortplot_text
