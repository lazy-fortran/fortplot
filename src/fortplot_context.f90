module fortplot_context
    !! Abstract base class for plotting backends
    !!
    !! This module defines the common interface that all plotting backends
    !! (PNG, PDF, ASCII) must implement. Provides polymorphic interface
    !! for unified plotting operations across different output formats.
    !!
    !! Author: fortplotlib contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: plot_context, setup_canvas
    type, abstract :: plot_context
        integer :: width, height
        real(wp) :: x_min, x_max, y_min, y_max
    contains
        procedure(line_interface), deferred :: line
        procedure(color_interface), deferred :: color
        procedure(text_interface), deferred :: text
        procedure(save_interface), deferred :: save
    end type plot_context
    
    abstract interface
        subroutine line_interface(this, x1, y1, x2, y2)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: x1, y1, x2, y2
        end subroutine line_interface
        
        subroutine color_interface(this, r, g, b)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: r, g, b
        end subroutine color_interface
        
        subroutine text_interface(this, x, y, text)
            import :: plot_context, wp
            class(plot_context), intent(inout) :: this
            real(wp), intent(in) :: x, y
            character(len=*), intent(in) :: text
        end subroutine text_interface
        
        subroutine save_interface(this, filename)
            import :: plot_context
            class(plot_context), intent(inout) :: this
            character(len=*), intent(in) :: filename
        end subroutine save_interface
    end interface

contains

    subroutine setup_canvas(ctx, width, height)
        class(plot_context), intent(inout) :: ctx
        integer, intent(in) :: width, height
        
        ctx%width = width
        ctx%height = height
        ctx%x_min = -1.0_wp
        ctx%x_max = 1.0_wp
        ctx%y_min = -1.0_wp
        ctx%y_max = 1.0_wp
    end subroutine setup_canvas

end module fortplot_context