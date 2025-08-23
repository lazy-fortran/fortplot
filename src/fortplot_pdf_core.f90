module fortplot_pdf_core
    !! Core PDF types and basic operations
    !! Provides fundamental PDF context and stream management
    
    use iso_fortran_env, only: wp => real64
    implicit none
    private

    ! Public types
    public :: pdf_context_core
    public :: pdf_font_t
    
    ! Public procedures
    public :: create_pdf_canvas_core
    public :: initialize_pdf_stream
    public :: finalize_pdf_stream
    public :: normalize_to_pdf_coords

    ! PDF-specific constants
    real(wp), parameter, public :: PDF_WIDTH = 595.0_wp   ! US Letter width in points
    real(wp), parameter, public :: PDF_HEIGHT = 842.0_wp  ! A4 height in points  
    real(wp), parameter, public :: PDF_MARGIN = 50.0_wp   ! Margin in points
    real(wp), parameter, public :: PDF_PLOT_WIDTH = PDF_WIDTH - 2.0_wp * PDF_MARGIN
    real(wp), parameter, public :: PDF_PLOT_HEIGHT = PDF_HEIGHT - 2.0_wp * PDF_MARGIN
    real(wp), parameter, public :: PDF_TICK_SIZE = 5.0_wp
    real(wp), parameter, public :: PDF_TITLE_SIZE = 14.0_wp
    real(wp), parameter, public :: PDF_LABEL_SIZE = 12.0_wp
    real(wp), parameter, public :: PDF_TICK_LABEL_SIZE = 10.0_wp
    real(wp), parameter, public :: PDF_FONT_SIZE = 10.0_wp
    
    type :: pdf_font_t
        integer :: helvetica_obj = 5
        integer :: symbol_obj = 6
    contains
        procedure :: get_helvetica_obj
        procedure :: get_symbol_obj
    end type pdf_font_t

    type :: pdf_context_core
        character(len=:), allocatable :: stream_data
        real(wp) :: width
        real(wp) :: height
        real(wp) :: current_line_width = 1.0_wp
        type(pdf_font_t) :: fonts
    contains
        procedure :: set_color => set_pdf_color
        procedure :: set_line_width => set_pdf_line_width
    end type pdf_context_core

contains

    function create_pdf_canvas_core(width, height) result(ctx)
        real(wp), intent(in) :: width, height
        type(pdf_context_core) :: ctx
        
        ctx%width = width
        ctx%height = height
        ctx%stream_data = ""
        ctx%current_line_width = 1.0_wp
        
        call initialize_pdf_stream(ctx)
    end function create_pdf_canvas_core

    subroutine initialize_pdf_stream(ctx)
        type(pdf_context_core), intent(inout) :: ctx
        
        ! Initialize stream with save state
        ctx%stream_data = ctx%stream_data // "q" // new_line('a')
        ! Set initial line width
        ctx%stream_data = ctx%stream_data // "1 w" // new_line('a')
        ! Set line join and cap styles
        ctx%stream_data = ctx%stream_data // "1 J 1 j" // new_line('a')
    end subroutine initialize_pdf_stream

    subroutine set_pdf_color(this, r, g, b)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        character(len=64) :: color_cmd
        
        write(color_cmd, '(F0.3, 1X, F0.3, 1X, F0.3, " RG")') r, g, b
        this%stream_data = this%stream_data // trim(adjustl(color_cmd)) // new_line('a')
    end subroutine set_pdf_color

    subroutine set_pdf_line_width(this, width)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: width
        character(len=32) :: width_cmd
        
        this%current_line_width = width
        write(width_cmd, '(F0.3, " w")') width
        this%stream_data = this%stream_data // trim(adjustl(width_cmd)) // new_line('a')
    end subroutine set_pdf_line_width

    subroutine normalize_to_pdf_coords(ctx, x, y, pdf_x, pdf_y)
        !! Convert normalized coordinates (0-1) to PDF coordinates
        type(pdf_context_core), intent(in) :: ctx
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: pdf_x, pdf_y
        
        ! Convert from normalized (0-1) to PDF coordinates
        ! PDF origin is bottom-left, with margins
        pdf_x = PDF_MARGIN + x * PDF_PLOT_WIDTH
        pdf_y = PDF_MARGIN + y * PDF_PLOT_HEIGHT
    end subroutine normalize_to_pdf_coords

    subroutine finalize_pdf_stream(ctx)
        type(pdf_context_core), intent(inout) :: ctx
        
        ! Restore graphics state
        ctx%stream_data = ctx%stream_data // "Q" // new_line('a')
    end subroutine finalize_pdf_stream

    integer function get_helvetica_obj(this) result(obj)
        class(pdf_font_t), intent(in) :: this
        obj = this%helvetica_obj
    end function get_helvetica_obj

    integer function get_symbol_obj(this) result(obj)
        class(pdf_font_t), intent(in) :: this
        obj = this%symbol_obj
    end function get_symbol_obj

end module fortplot_pdf_core