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
        integer :: extgstate_count = 0
        integer, allocatable :: extgstate_stroke_milli(:)
        integer, allocatable :: extgstate_fill_milli(:)
        ! Optional single image XObject support
        logical :: has_image = .false.
        integer :: image_width = 0
        integer :: image_height = 0
        character(len=:), allocatable :: image_data
    contains
        procedure :: set_color => set_pdf_color
        procedure :: set_line_width => set_pdf_line_width
        procedure :: set_image => set_pdf_image
        procedure :: register_extgstate => register_pdf_extgstate
    end type pdf_context_core

contains

    function create_pdf_canvas_core(width, height) result(ctx)
        real(wp), intent(in) :: width, height
        type(pdf_context_core) :: ctx

        ctx%width = width
        ctx%height = height
        ctx%stream_data = ""
        ctx%current_line_width = 1.0_wp
        ctx%extgstate_count = 0

        call initialize_pdf_stream(ctx)
    end function create_pdf_canvas_core

    subroutine initialize_pdf_stream(ctx)
        type(pdf_context_core), intent(inout) :: ctx

        ! Initialize stream with save state
        ctx%stream_data = ctx%stream_data//"q"//new_line('a')
        ! Set initial line width
        ctx%stream_data = ctx%stream_data//"1 w"//new_line('a')
        ! Set line join and cap styles
        ctx%stream_data = ctx%stream_data//"1 J 1 j"//new_line('a')
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

        ! Set both stroking (RG) and non-stroking (rg) colors.
        write (color_cmd, '(F0.3, 1X, F0.3, 1X, F0.3, " RG")') safe_r, safe_g, safe_b
        this%stream_data = this%stream_data//trim(adjustl(color_cmd))//new_line('a')
        write (color_cmd, '(F0.3, 1X, F0.3, 1X, F0.3, " rg")') safe_r, safe_g, safe_b
        this%stream_data = this%stream_data//trim(adjustl(color_cmd))//new_line('a')
    end subroutine set_pdf_color

    subroutine set_pdf_line_width(this, width)
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: width
        character(len=32) :: width_cmd

        this%current_line_width = width
        write (width_cmd, '(F0.3, " w")') width
        this%stream_data = this%stream_data//trim(adjustl(width_cmd))//new_line('a')
    end subroutine set_pdf_line_width

    subroutine finalize_pdf_stream(ctx)
        type(pdf_context_core), intent(inout) :: ctx

        ! Restore graphics state
        ctx%stream_data = ctx%stream_data//"Q"//new_line('a')
    end subroutine finalize_pdf_stream

    subroutine set_pdf_image(this, width_px, height_px, data)
        class(pdf_context_core), intent(inout) :: this
        integer, intent(in) :: width_px, height_px
        character(len=*), intent(in) :: data
        this%has_image = .true.
        this%image_width = width_px
        this%image_height = height_px
        this%image_data = data
    end subroutine set_pdf_image

    function register_pdf_extgstate(this, stroke_alpha, fill_alpha) result(name)
        use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
        class(pdf_context_core), intent(inout) :: this
        real(wp), intent(in) :: stroke_alpha, fill_alpha
        character(len=:), allocatable :: name

        integer :: stroke_milli, fill_milli
        integer :: i

        stroke_milli = quantize_alpha(stroke_alpha)
        fill_milli = quantize_alpha(fill_alpha)

        if (stroke_milli == 1000 .and. fill_milli == 1000) then
            allocate (character(len=0) :: name)
            return
        end if

        if (.not. ieee_is_finite(stroke_alpha) .or. .not. &
            ieee_is_finite(fill_alpha)) then
            allocate (character(len=0) :: name)
            return
        end if

        do i = 1, this%extgstate_count
            if (this%extgstate_stroke_milli(i) == stroke_milli .and. &
                this%extgstate_fill_milli(i) == fill_milli) then
                name = gstate_name(i)
                return
            end if
        end do

        call append_gstate(this, stroke_milli, fill_milli)
        name = gstate_name(this%extgstate_count)
    end function register_pdf_extgstate

    integer function quantize_alpha(alpha) result(alpha_milli)
        real(wp), intent(in) :: alpha
        real(wp) :: alpha_clamped

        if (alpha /= alpha) then
            alpha_milli = 1000
            return
        end if

        alpha_clamped = max(0.0_wp, min(1.0_wp, alpha))
        alpha_milli = int(nint(alpha_clamped*1000.0_wp))
    end function quantize_alpha

    subroutine append_gstate(this, stroke_milli, fill_milli)
        class(pdf_context_core), intent(inout) :: this
        integer, intent(in) :: stroke_milli, fill_milli

        integer, allocatable :: new_stroke(:), new_fill(:)
        integer :: n_old, n_new

        n_old = this%extgstate_count
        n_new = n_old+1

        allocate (new_stroke(n_new), new_fill(n_new))
        if (n_old > 0) then
            new_stroke(1:n_old) = this%extgstate_stroke_milli
            new_fill(1:n_old) = this%extgstate_fill_milli
        end if
        new_stroke(n_new) = stroke_milli
        new_fill(n_new) = fill_milli

        call move_alloc(new_stroke, this%extgstate_stroke_milli)
        call move_alloc(new_fill, this%extgstate_fill_milli)
        this%extgstate_count = n_new
    end subroutine append_gstate

    function gstate_name(idx) result(name)
        integer, intent(in) :: idx
        character(len=32) :: name

        write (name, '("GS", I0)') idx
    end function gstate_name

    integer function get_helvetica_obj(this) result(obj)
        class(pdf_font_t), intent(in) :: this
        obj = this%helvetica_obj
    end function get_helvetica_obj

    integer function get_symbol_obj(this) result(obj)
        class(pdf_font_t), intent(in) :: this
        obj = this%symbol_obj
    end function get_symbol_obj

end module fortplot_pdf_core
