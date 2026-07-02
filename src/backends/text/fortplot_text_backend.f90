module fortplot_text_backend
    !! Text-family names for the character-cell plotting backend.
    !!
    !! Preferred public API for the backend that renders plots as text. The
    !! ASCII charset is its current default; future modes (Unicode, braille,
    !! ANSI color) share this family. fortplot_ascii stays the concrete
    !! implementation and a working compatibility API, so both
    !! ``use fortplot_text_backend`` and ``use fortplot_ascii`` compile. The
    !! name fortplot_text is already the backend-agnostic font/layout module,
    !! so the backend family uses fortplot_text_backend.
    use fortplot_ascii, only: text_context => ascii_context, &
        create_text_canvas => create_ascii_canvas
    use fortplot_legend_drawing, only: render_text_legend => render_ascii_legend, &
        backend_is_text => backend_is_ascii
    implicit none

    private
    public :: text_context, create_text_canvas
    public :: render_text_legend, backend_is_text
end module fortplot_text_backend
