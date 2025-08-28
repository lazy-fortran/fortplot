module fortplot_figure_compatibility
    !! Backward compatibility methods for figure_t
    !! 
    !! This module provides backward compatibility methods that were previously
    !! part of fortplot_figure_core but extracted to reduce file size below
    !! QADS compliance limits (<500 lines target, <1000 lines hard limit).
    !!
    !! Single Responsibility: Maintain backward compatibility with animation
    !! and other modules that depend on legacy figure_t interfaces.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_figure_initialization
    use fortplot_figure_accessors
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: get_figure_width_compat, get_figure_height_compat
    public :: get_figure_rendered_compat, set_figure_rendered_compat
    public :: get_figure_plot_count_compat
    public :: setup_png_backend_for_animation_compat
    public :: extract_rgb_data_for_animation_compat
    public :: extract_png_data_for_animation_compat
    public :: backend_color_compat, backend_line_compat, backend_associated_compat
    public :: get_figure_x_min_compat, get_figure_x_max_compat
    public :: get_figure_y_min_compat, get_figure_y_max_compat

contains

    function get_figure_width_compat(state) result(width)
        !! Get figure width (compatibility wrapper)
        type(figure_state_t), intent(in) :: state
        integer :: width
        width = get_figure_width(state)
    end function get_figure_width_compat
    
    function get_figure_height_compat(state) result(height)
        !! Get figure height (compatibility wrapper)
        type(figure_state_t), intent(in) :: state
        integer :: height
        height = get_figure_height(state)
    end function get_figure_height_compat
    
    function get_figure_rendered_compat(state) result(rendered)
        !! Get rendered state (compatibility wrapper)
        type(figure_state_t), intent(in) :: state
        logical :: rendered
        rendered = get_figure_rendered(state)
    end function get_figure_rendered_compat
    
    subroutine set_figure_rendered_compat(state, rendered)
        !! Set rendered state (compatibility wrapper)
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: rendered
        call set_figure_rendered(state, rendered)
    end subroutine set_figure_rendered_compat
    
    function get_figure_plot_count_compat(state) result(plot_count)
        !! Get number of plots (compatibility wrapper)
        type(figure_state_t), intent(in) :: state
        integer :: plot_count
        plot_count = get_figure_plot_count(state)
    end function get_figure_plot_count_compat
    
    subroutine setup_png_backend_for_animation_compat(state)
        !! Setup PNG backend for animation (compatibility wrapper)
        type(figure_state_t), intent(inout) :: state
        call setup_png_for_animation(state)
    end subroutine setup_png_backend_for_animation_compat
    
    subroutine extract_rgb_data_for_animation_compat(state, rgb_data)
        !! Extract RGB data for animation (compatibility wrapper)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(out) :: rgb_data(:,:,:)
        call extract_rgb_for_animation(state, rgb_data)
    end subroutine extract_rgb_data_for_animation_compat
    
    subroutine extract_png_data_for_animation_compat(state, png_data, status)
        !! Extract PNG data for animation (compatibility wrapper)
        type(figure_state_t), intent(inout) :: state
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        call extract_png_for_animation(state, png_data, status)
    end subroutine extract_png_data_for_animation_compat
    
    subroutine backend_color_compat(state, r, g, b)
        !! Set backend color (compatibility wrapper)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: r, g, b
        call set_backend_color(state, r, g, b)
    end subroutine backend_color_compat
    
    function backend_associated_compat(state) result(is_associated)
        !! Check if backend is allocated (compatibility wrapper)
        type(figure_state_t), intent(in) :: state
        logical :: is_associated
        is_associated = is_backend_associated(state)
    end function backend_associated_compat
    
    subroutine backend_line_compat(state, x1, y1, x2, y2)
        !! Draw line using backend (compatibility wrapper)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x1, y1, x2, y2
        call draw_backend_line(state, x1, y1, x2, y2)
    end subroutine backend_line_compat
    
    function get_figure_x_min_compat(state) result(x_min)
        !! Get x minimum value (compatibility wrapper)
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_min
        x_min = get_figure_x_min(state)
    end function get_figure_x_min_compat
    
    function get_figure_x_max_compat(state) result(x_max)
        !! Get x maximum value (compatibility wrapper)
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_max
        x_max = get_figure_x_max(state)
    end function get_figure_x_max_compat
    
    function get_figure_y_min_compat(state) result(y_min)
        !! Get y minimum value (compatibility wrapper)
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_min
        y_min = get_figure_y_min(state)
    end function get_figure_y_min_compat
    
    function get_figure_y_max_compat(state) result(y_max)
        !! Get y maximum value (compatibility wrapper)
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_max
        y_max = get_figure_y_max(state)
    end function get_figure_y_max_compat

end module fortplot_figure_compatibility