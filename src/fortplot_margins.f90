module fortplot_margins
    !! Common margin and axis functionality for plotting backends
    !! 
    !! This module provides shared functionality for calculating margins,
    !! plot areas, and drawing axes across different backends (PNG, PDF, etc.)
    !! Follows DRY principle by centralizing common plotting layout logic.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_layout, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_ticks, only: calculate_tick_labels, format_tick_value, calculate_nice_axis_limits
    use fortplot_ticks, only: calculate_tick_labels_log, calculate_tick_labels_symlog
    implicit none
    
    private
    public :: plot_margins_t, plot_area_t, calculate_plot_area, draw_basic_axes_frame, get_axis_tick_positions
    
contains


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