module fortplot_figure_accessors
    !! Accessor functions for figure_t
    !! 
    !! Provides getter and setter functions extracted from fortplot_figure_core
    !! to meet QADS size limits.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_figure_initialization, only: figure_state_t, setup_figure_backend
    use fortplot_plot_data, only: plot_data_t
    implicit none
    
    private
    public :: get_figure_width, get_figure_height
    public :: get_figure_rendered, set_figure_rendered
    public :: get_figure_plot_count, get_figure_plots
    public :: get_figure_x_min, get_figure_x_max
    public :: get_figure_y_min, get_figure_y_max
    public :: setup_png_for_animation
    public :: extract_rgb_for_animation
    public :: extract_png_for_animation
    public :: set_backend_color
    public :: is_backend_associated
    public :: draw_backend_line
    
contains
    
    function get_figure_width(state) result(width)
        !! Get figure width from state
        type(figure_state_t), intent(in) :: state
        integer :: width
        width = state%width
    end function get_figure_width
    
    function get_figure_height(state) result(height)
        !! Get figure height from state
        type(figure_state_t), intent(in) :: state
        integer :: height
        height = state%height
    end function get_figure_height
    
    function get_figure_rendered(state) result(rendered)
        !! Get rendered state
        type(figure_state_t), intent(in) :: state
        logical :: rendered
        rendered = state%rendered
    end function get_figure_rendered
    
    subroutine set_figure_rendered(state, rendered)
        !! Set rendered state
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: rendered
        state%rendered = rendered
    end subroutine set_figure_rendered
    
    function get_figure_plot_count(state) result(plot_count)
        !! Get number of plots from state
        type(figure_state_t), intent(in) :: state
        integer :: plot_count
        plot_count = state%plot_count
    end function get_figure_plot_count
    
    function get_figure_plots(plots) result(plots_ptr)
        !! Get pointer to plots array
        type(plot_data_t), target, intent(in) :: plots(:)
        type(plot_data_t), pointer :: plots_ptr(:)
        plots_ptr => plots
    end function get_figure_plots
    
    function get_figure_x_min(state) result(x_min)
        !! Get x minimum value from state
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_min
        x_min = state%x_min
    end function get_figure_x_min
    
    function get_figure_x_max(state) result(x_max)
        !! Get x maximum value from state
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_max
        x_max = state%x_max
    end function get_figure_x_max
    
    function get_figure_y_min(state) result(y_min)
        !! Get y minimum value from state
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_min
        y_min = state%y_min
    end function get_figure_y_min
    
    function get_figure_y_max(state) result(y_max)
        !! Get y maximum value from state
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_max
        y_max = state%y_max
    end function get_figure_y_max
    
    subroutine setup_png_for_animation(state)
        !! Setup PNG backend for animation
        type(figure_state_t), intent(inout) :: state
        
        call setup_figure_backend(state, 'png')
        state%rendered = .false.
    end subroutine setup_png_for_animation
    
    subroutine extract_rgb_for_animation(state, rgb_data)
        !! Extract RGB data for animation
        type(figure_state_t), intent(in) :: state
        real(wp), intent(out) :: rgb_data(:,:,:)
        
        if (allocated(state%backend)) then
            call state%backend%extract_rgb_data(state%width, state%height, rgb_data)
        end if
    end subroutine extract_rgb_for_animation
    
    subroutine extract_png_for_animation(state, png_data, status)
        !! Extract PNG data for animation
        type(figure_state_t), intent(in) :: state
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        if (allocated(state%backend)) then
            call state%backend%get_png_data_backend(state%width, state%height, png_data, status)
        end if
    end subroutine extract_png_for_animation
    
    subroutine set_backend_color(state, r, g, b)
        !! Set backend color
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: r, g, b
        
        if (allocated(state%backend)) then
            call state%backend%color(r, g, b)
        end if
    end subroutine set_backend_color
    
    function is_backend_associated(state) result(is_associated)
        !! Check if backend is allocated
        type(figure_state_t), intent(in) :: state
        logical :: is_associated
        
        is_associated = allocated(state%backend)
    end function is_backend_associated
    
    subroutine draw_backend_line(state, x1, y1, x2, y2)
        !! Draw line using backend
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x1, y1, x2, y2
        
        if (allocated(state%backend)) then
            call state%backend%line(x1, y1, x2, y2)
        end if
    end subroutine draw_backend_line
    
end module fortplot_figure_accessors