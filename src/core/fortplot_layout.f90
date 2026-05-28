module fortplot_layout
    !! Layout and margin calculations for plotting backends
    !! 
    !! This module handles plot area geometry and margin calculations,
    !! following Single Responsibility Principle by focusing solely
    !! on layout-related functionality.
    !! 
    !! Author: fortplot contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: plot_margins_t, plot_area_t, calculate_plot_area
    
    ! Matplotlib default margins (exact values from matplotlibrc)
    type :: plot_margins_t
        real(wp) :: left = 0.125_wp    ! 12.5% left margin 
        real(wp) :: right = 0.9_wp     ! 90% right edge (not margin!)
        real(wp) :: bottom = 0.11_wp   ! 11% bottom margin
        real(wp) :: top = 0.88_wp      ! 88% top edge (not margin!)
    end type plot_margins_t
    
    ! Plot area geometry
    type :: plot_area_t
        integer :: left, bottom, width, height
    end type plot_area_t
    
contains

    subroutine calculate_plot_area(canvas_width, canvas_height, margins, plot_area)
        !! Calculate plot area based on canvas size and margins (matplotlib-compatible)
        !! Note: left/bottom are margins, right/top are edge positions
        integer, intent(in) :: canvas_width, canvas_height
        type(plot_margins_t), intent(in) :: margins
        type(plot_area_t), intent(out) :: plot_area
        
        integer :: right_edge, top_edge

        ! Round each edge to the nearest pixel, matching matplotlib's axes bbox
        ! rounding. ceiling()/floor() biased the top edge up by one pixel
        ! (427.2 -> 428 instead of 427) for the default 640x480 canvas.
        plot_area%left = nint(margins%left * real(canvas_width, wp))
        right_edge = nint(margins%right * real(canvas_width, wp))
        plot_area%width = right_edge - plot_area%left

        ! Image coordinates have Y=0 at the top, so the edges are flipped.
        plot_area%bottom = nint((1.0_wp - margins%top) * real(canvas_height, wp))
        top_edge = nint((1.0_wp - margins%bottom) * real(canvas_height, wp))
        plot_area%height = top_edge - plot_area%bottom
    end subroutine calculate_plot_area

end module fortplot_layout