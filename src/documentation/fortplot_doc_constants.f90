module fortplot_doc_constants
    !! Constants for documentation generation.
    !!
    !! Shared between fortplot_documentation, fortplot_doc_processing,
    !! and fortplot_doc_output to avoid circular dependencies.

    implicit none
    private

    public :: PATH_MAX_LEN, FILENAME_MAX_LEN, LINE_MAX_LEN
    public :: MAX_EXAMPLES, MAX_MEDIA_FILES
    public :: VIDEO_WIDTH, VIDEO_HEIGHT
    public :: GITHUB_BASE_URL
    public :: OUTPUT_BASE_DIR
    public :: EXAMPLES_INDEX_PATH
    public :: INDEX_START_MARKER, INDEX_END_MARKER
    public :: FALLBACK_COUNT, FALLBACK_EXAMPLES

    ! String length constants
    integer, parameter :: PATH_MAX_LEN = 256
    integer, parameter :: FILENAME_MAX_LEN = 256
    integer, parameter :: LINE_MAX_LEN = 1024

    ! Array size constants
    integer, parameter :: MAX_EXAMPLES = 64
    integer, parameter :: MAX_MEDIA_FILES = 32

    ! Video dimensions
    integer, parameter :: VIDEO_WIDTH = 800
    integer, parameter :: VIDEO_HEIGHT = 600

    ! URL constants
    character(len=*), parameter :: GITHUB_BASE_URL = &
        'https://github.com/lazy-fortran/fortplot/blob/main/'

    ! Processing constants
    character(len=*), parameter :: OUTPUT_BASE_DIR = 'output/example/fortran/'
    character(len=*), parameter :: EXAMPLES_INDEX_PATH = 'doc/examples/index.md'
    character(len=*), parameter :: INDEX_START_MARKER = '<!-- AUTO_EXAMPLES_START -->'
    character(len=*), parameter :: INDEX_END_MARKER   = '<!-- AUTO_EXAMPLES_END -->'

    integer, parameter :: FALLBACK_COUNT = 16
    character(len=32), parameter :: FALLBACK_EXAMPLES(FALLBACK_COUNT) = [ &
        "basic_plots         ", "line_styles         ", &
        "marker_demo         ", "format_string_demo  ", &
        "contour_demo        ", "pcolormesh_demo     ", &
        "streamplot_demo     ", "ascii_heatmap       ", &
        "scale_examples      ", "legend_demo         ", &
        "legend_box_demo     ", "unicode_demo        ", &
        "show_viewer_demo    ", "raster_backend_demo ", &
        "pdf_backend_demo    ", "text_backend_demo   " ]

end module fortplot_doc_constants
