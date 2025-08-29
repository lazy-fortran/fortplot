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

    ! PDF-specific constants (margins and sizes)
    real(wp), parameter, public :: PDF_MARGIN = 50.0_wp   ! Margin in points
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
        use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        character(len=64) :: color_cmd
        real(wp) :: safe_r, safe_g, safe_b
        
        ! Validate and clamp RGB values to prevent PDF format errors
        safe_r = r
        safe_g = g  
        safe_b = b
        
        ! Handle NaN and infinity
        if (.not. ieee_is_finite(safe_r)) safe_r = 0.0_wp
        if (.not. ieee_is_finite(safe_g)) safe_g = 0.0_wp
        if (.not. ieee_is_finite(safe_b)) safe_b = 0.0_wp
        
        ! Clamp to [0,1] range
        safe_r = max(0.0_wp, min(1.0_wp, safe_r))
        safe_g = max(0.0_wp, min(1.0_wp, safe_g))
        safe_b = max(0.0_wp, min(1.0_wp, safe_b))
        
        write(color_cmd, '(F0.3, 1X, F0.3, 1X, F0.3, " RG")') safe_r, safe_g, safe_b
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