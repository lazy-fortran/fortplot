module fortplot_pdf_text
    !! Aggregates PDF text rendering interfaces

    use fortplot_pdf_text_escape, only: escape_pdf_string, unicode_to_symbol_char, &
        unicode_codepoint_to_pdf_escape
    use fortplot_pdf_text_render, only: draw_pdf_text, draw_pdf_text_direct, &
        draw_pdf_text_bold
    use fortplot_pdf_text_render, only: draw_mixed_font_text
    use fortplot_pdf_text_render, only: draw_rotated_mixed_font_text
    use fortplot_pdf_mathtext_render, only: draw_pdf_mathtext
    use fortplot_pdf_text_metrics, only: estimate_pdf_text_width
    implicit none
    private

    public :: draw_pdf_text
    public :: draw_pdf_text_direct
    public :: draw_pdf_text_bold
    public :: draw_mixed_font_text
    public :: draw_rotated_mixed_font_text
    public :: escape_pdf_string
    public :: unicode_to_symbol_char
    public :: unicode_codepoint_to_pdf_escape
    public :: draw_pdf_mathtext
    public :: estimate_pdf_text_width

end module fortplot_pdf_text
