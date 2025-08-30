module fortplot_figure_core_compat
    !! Figure backward compatibility and animation support module
    !! 
    !! This module contains backward compatibility and animation methods
    !! extracted from fortplot_figure_core for architectural compliance
    !!
    !! ARCHITECTURAL REFACTORING (Issue #678):
    !! - Focused module for backward compatibility operations
    !! - Single Responsibility Principle compliance
    !! - Clean separation from core plotting functionality

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_compatibility
    use fortplot_figure_accessors
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_core_io, only: render_figure_impl
    implicit none

    private
    public :: get_width_figure, get_height_figure, get_rendered_figure, set_rendered_figure
    public :: get_plot_count_figure, setup_png_backend_for_animation_figure
    public :: extract_rgb_data_for_animation_figure, extract_png_data_for_animation_figure
    public :: backend_color_figure, backend_associated_figure, backend_line_figure
    public :: get_x_min_figure, get_x_max_figure, get_y_min_figure, get_y_max_figure

contains

    function get_width_figure(state) result(width)
        !! Get figure width
        type(figure_state_t), intent(in) :: state
        integer :: width
        width = get_figure_width_compat(state)
    end function get_width_figure
    
    function get_height_figure(state) result(height)
        !! Get figure height
        type(figure_state_t), intent(in) :: state
        integer :: height
        height = get_figure_height_compat(state)
    end function get_height_figure
    
    function get_rendered_figure(state) result(rendered)
        !! Get rendered state
        type(figure_state_t), intent(in) :: state
        logical :: rendered
        rendered = get_figure_rendered_compat(state)
    end function get_rendered_figure
    
    subroutine set_rendered_figure(state, rendered)
        !! Set rendered state
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: rendered
        call set_figure_rendered_compat(state, rendered)
    end subroutine set_rendered_figure
    
    function get_plot_count_figure(state) result(plot_count)
        !! Get number of plots
        type(figure_state_t), intent(in) :: state
        integer :: plot_count
        plot_count = get_figure_plot_count_compat(state)
    end function get_plot_count_figure
    
    subroutine setup_png_backend_for_animation_figure(state)
        !! Setup PNG backend for animation (temporary method)
        type(figure_state_t), intent(inout) :: state
        call setup_png_backend_for_animation_compat(state)
    end subroutine setup_png_backend_for_animation_figure
    
    subroutine extract_rgb_data_for_animation_figure(state, plots, plot_count, rgb_data)
        !! Extract RGB data for animation
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        real(wp), intent(out) :: rgb_data(:,:,:)
        
        
        if (.not. state%rendered) then
            call render_figure_impl(state, plots, plot_count)
        end if
        
        call extract_rgb_data_for_animation_compat(state, rgb_data)
    end subroutine extract_rgb_data_for_animation_figure
    
    subroutine extract_png_data_for_animation_figure(state, plots, plot_count, png_data, status)
        !! Extract PNG data for animation
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        
        if (.not. state%rendered) then
            call render_figure_impl(state, plots, plot_count)
        end if
        
        call extract_png_data_for_animation_compat(state, png_data, status)
    end subroutine extract_png_data_for_animation_figure
    
    subroutine backend_color_figure(state, r, g, b)
        !! Set backend color
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: r, g, b
        call backend_color_compat(state, r, g, b)
    end subroutine backend_color_figure
    
    function backend_associated_figure(state) result(is_associated)
        !! Check if backend is allocated
        type(figure_state_t), intent(in) :: state
        logical :: is_associated
        is_associated = backend_associated_compat(state)
    end function backend_associated_figure
    
    subroutine backend_line_figure(state, x1, y1, x2, y2)
        !! Draw line using backend
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x1, y1, x2, y2
        call backend_line_compat(state, x1, y1, x2, y2)
    end subroutine backend_line_figure
    
    function get_x_min_figure(state) result(x_min)
        !! Get x minimum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_min
        x_min = get_figure_x_min_compat(state)
    end function get_x_min_figure
    
    function get_x_max_figure(state) result(x_max)
        !! Get x maximum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_max
        x_max = get_figure_x_max_compat(state)
    end function get_x_max_figure
    
    function get_y_min_figure(state) result(y_min)
        !! Get y minimum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_min
        y_min = get_figure_y_min_compat(state)
    end function get_y_min_figure
    
    function get_y_max_figure(state) result(y_max)
        !! Get y maximum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_max
        y_max = get_figure_y_max_compat(state)
    end function get_y_max_figure

end module fortplot_figure_core_compat