module fortplot_layout
    !! Layout and margin calculations for plotting backends
    !! 
    !! This module handles plot area geometry and margin calculations,
    !! following Single Responsibility Principle by focusing solely
    !! on layout-related functionality.
    !! 
    !! Author: fortplotlib contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: plot_margins_t, plot_area_t, calculate_plot_area
    
    ! Matplotlib-style margins (better balanced for title and label space)
    type :: plot_margins_t
        real(wp) :: left = 0.125_wp    ! 12.5% left margin (space for y-labels)
        real(wp) :: right = 0.05_wp    ! 5% right margin  
        real(wp) :: bottom = 0.11_wp   ! 11% bottom margin (space for x-labels)
        real(wp) :: top = 0.095_wp     ! 9.5% top margin (space for title)
    end type plot_margins_t
    
    ! Plot area geometry
    type :: plot_area_t
        integer :: left, bottom, width, height
    end type plot_area_t
    
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

end module fortplot_layout