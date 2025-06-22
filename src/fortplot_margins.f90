module fortplot_margins
    !! Common margin and axis functionality for plotting backends
    !! 
    !! This module provides shared functionality for calculating margins,
    !! plot areas, and drawing axes across different backends (PNG, PDF, etc.)
    !! Follows DRY principle by centralizing common plotting layout logic.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: plot_margins_t, calculate_plot_area, draw_basic_axes_frame, get_axis_tick_positions
    
    ! Standard matplotlib-style margins
    type :: plot_margins_t
        real(wp) :: left = 0.15_wp     ! 15% left margin
        real(wp) :: right = 0.05_wp    ! 5% right margin  
        real(wp) :: bottom = 0.15_wp   ! 15% bottom margin
        real(wp) :: top = 0.05_wp      ! 5% top margin
    end type plot_margins_t
    
    ! Plot area geometry
    type :: plot_area_t
        integer :: left, bottom, width, height
    end type plot_area_t
    
    public :: plot_area_t
    
contains

    subroutine calculate_plot_area(canvas_width, canvas_height, margins, plot_area)
        !! Calculate plot area based on canvas size and margins
        integer, intent(in) :: canvas_width, canvas_height
        type(plot_margins_t), intent(in) :: margins
        type(plot_area_t), intent(out) :: plot_area
        
        plot_area%left = int(margins%left * real(canvas_width, wp)) + 1
        plot_area%bottom = int(margins%top * real(canvas_height, wp)) + 1  ! For image coords (Y=0 at top)
        plot_area%width = canvas_width - int((margins%left + margins%right) * real(canvas_width, wp))
        plot_area%height = canvas_height - int((margins%bottom + margins%top) * real(canvas_height, wp))
    end subroutine calculate_plot_area

    subroutine get_axis_tick_positions(plot_area, num_ticks_x, num_ticks_y, &
                                      x_positions, y_positions, actual_num_x, actual_num_y)
        !! Generate tick mark positions for basic axes
        type(plot_area_t), intent(in) :: plot_area
        integer, intent(in) :: num_ticks_x, num_ticks_y
        real(wp), intent(out) :: x_positions(:), y_positions(:)
        integer, intent(out) :: actual_num_x, actual_num_y
        
        real(wp) :: tick_spacing_x, tick_spacing_y
        integer :: i
        
        actual_num_x = min(num_ticks_x, size(x_positions))
        actual_num_y = min(num_ticks_y, size(y_positions))
        
        if (actual_num_x > 1) then
            tick_spacing_x = real(plot_area%width, wp) / real(actual_num_x - 1, wp)
            do i = 1, actual_num_x
                x_positions(i) = real(plot_area%left, wp) + real(i - 1, wp) * tick_spacing_x
            end do
        end if
        
        if (actual_num_y > 1) then
            tick_spacing_y = real(plot_area%height, wp) / real(actual_num_y - 1, wp)
            do i = 1, actual_num_y
                y_positions(i) = real(plot_area%bottom, wp) + real(i - 1, wp) * tick_spacing_y
            end do
        end if
    end subroutine get_axis_tick_positions

    subroutine draw_basic_axes_frame(plot_area, draw_line_proc)
        !! Draw basic rectangular axes frame
        !! Uses a procedure pointer to be backend-agnostic
        type(plot_area_t), intent(in) :: plot_area
        interface
            subroutine draw_line_proc(x1, y1, x2, y2)
                import :: wp
                real(wp), intent(in) :: x1, y1, x2, y2
            end subroutine
        end interface
        
        real(wp) :: left, right, top, bottom
        
        left = real(plot_area%left, wp)
        right = real(plot_area%left + plot_area%width, wp) 
        top = real(plot_area%bottom, wp)
        bottom = real(plot_area%bottom + plot_area%height, wp)
        
        ! Draw frame (rectangle)
        call draw_line_proc(left, top, right, top)        ! Top edge
        call draw_line_proc(left, bottom, right, bottom)  ! Bottom edge  
        call draw_line_proc(left, top, left, bottom)      ! Left edge
        call draw_line_proc(right, top, right, bottom)    ! Right edge
    end subroutine draw_basic_axes_frame

end module fortplot_margins