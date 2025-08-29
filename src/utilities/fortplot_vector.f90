module fortplot_vector
    !! Shared vector graphics functionality for backends like PDF, SVG, etc.
    !! Extracted from PDF backend to provide reusable vector drawing primitives
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: vector_graphics_state, vector_stream_writer
    
    type :: vector_graphics_state
        !! Encapsulates vector graphics state to provide clean API
        real(wp) :: line_width = 1.0_wp
        real(wp) :: stroke_r = 0.0_wp, stroke_g = 0.0_wp, stroke_b = 1.0_wp
        real(wp) :: fill_r = 0.0_wp, fill_g = 0.0_wp, fill_b = 0.0_wp
    end type vector_graphics_state
    
    type, abstract :: vector_stream_writer
        !! Abstract interface for vector stream writers (PDF, SVG, etc.)
        character(len=:), allocatable :: content_stream
        type(vector_graphics_state) :: current_state
    contains
        procedure(write_command_interface), deferred :: write_command
        procedure(write_move_interface), deferred :: write_move
        procedure(write_line_interface), deferred :: write_line
        procedure(write_stroke_interface), deferred :: write_stroke
        procedure(write_color_interface), deferred :: write_color
        procedure(write_line_width_interface), deferred :: write_line_width
        procedure(save_state_interface), deferred :: save_state
        procedure(restore_state_interface), deferred :: restore_state
        procedure :: initialize_stream => initialize_vector_stream
        procedure :: add_to_stream => add_to_vector_stream
        procedure :: draw_vector_line => draw_generic_vector_line
        procedure :: set_vector_color => set_generic_vector_color
        procedure :: set_vector_line_width => set_generic_vector_line_width
    end type vector_stream_writer
    
    abstract interface
        subroutine write_command_interface(this, command)
            import :: vector_stream_writer
            class(vector_stream_writer), intent(inout) :: this
            character(len=*), intent(in) :: command
        end subroutine
        
        subroutine write_move_interface(this, x, y)
            import :: vector_stream_writer, wp
            class(vector_stream_writer), intent(inout) :: this
            real(wp), intent(in) :: x, y
        end subroutine
        
        subroutine write_line_interface(this, x, y)
            import :: vector_stream_writer, wp
            class(vector_stream_writer), intent(inout) :: this
            real(wp), intent(in) :: x, y
        end subroutine
        
        subroutine write_stroke_interface(this)
            import :: vector_stream_writer
            class(vector_stream_writer), intent(inout) :: this
        end subroutine
        
        subroutine write_color_interface(this, r, g, b)
            import :: vector_stream_writer, wp
            class(vector_stream_writer), intent(inout) :: this
            real(wp), intent(in) :: r, g, b
        end subroutine
        
        subroutine write_line_width_interface(this, width)
            import :: vector_stream_writer, wp
            class(vector_stream_writer), intent(inout) :: this
            real(wp), intent(in) :: width
        end subroutine
        
        subroutine save_state_interface(this)
            import :: vector_stream_writer
            class(vector_stream_writer), intent(inout) :: this
        end subroutine
        
        subroutine restore_state_interface(this)
            import :: vector_stream_writer
            class(vector_stream_writer), intent(inout) :: this
        end subroutine
    end interface
    
contains

    subroutine initialize_vector_stream(this)
        class(vector_stream_writer), intent(inout) :: this
        
        this%content_stream = ""
        ! Initialize graphics state
        this%current_state%line_width = 1.0_wp
        this%current_state%stroke_r = 0.0_wp
        this%current_state%stroke_g = 0.0_wp
        this%current_state%stroke_b = 1.0_wp
    end subroutine initialize_vector_stream
    
    subroutine add_to_vector_stream(this, command)
        class(vector_stream_writer), intent(inout) :: this
        character(len=*), intent(in) :: command
        
        if (allocated(this%content_stream)) then
            this%content_stream = this%content_stream // command // char(10)
        else
            this%content_stream = command // char(10)
        end if
    end subroutine add_to_vector_stream
    
    subroutine draw_generic_vector_line(this, x1, y1, x2, y2)
        class(vector_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        
        call this%write_move(x1, y1)
        call this%write_line(x2, y2)
        call this%write_stroke()
    end subroutine draw_generic_vector_line
    
    subroutine set_generic_vector_color(this, r, g, b)
        class(vector_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        
        ! Only update state if color actually changed (avoid redundant operations)
        if (abs(this%current_state%stroke_r - r) > 1e-6_wp .or. &
            abs(this%current_state%stroke_g - g) > 1e-6_wp .or. &
            abs(this%current_state%stroke_b - b) > 1e-6_wp) then
            
            this%current_state%stroke_r = r
            this%current_state%stroke_g = g  
            this%current_state%stroke_b = b
            
            call this%write_color(r, g, b)
        end if
    end subroutine set_generic_vector_color
    
    subroutine set_generic_vector_line_width(this, width)
        class(vector_stream_writer), intent(inout) :: this
        real(wp), intent(in) :: width
        
        if (abs(width - this%current_state%line_width) > 1e-6_wp) then
            this%current_state%line_width = width
            call this%write_line_width(width)
        end if
    end subroutine set_generic_vector_line_width

end module fortplot_vector