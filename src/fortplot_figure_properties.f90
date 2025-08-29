module fortplot_figure_properties
    !! Figure property accessors and getters/setters
    !! Extracted from fortplot_figure_core.f90 for size reduction (SRP compliance)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_compatibility, only: get_figure_width_compat, get_figure_height_compat, &
                                            get_figure_rendered_compat, set_figure_rendered_compat, &
                                            get_figure_plot_count_compat, get_figure_x_min_compat, &
                                            get_figure_x_max_compat, get_figure_y_min_compat, &
                                            get_figure_y_max_compat, backend_line_compat, &
                                            backend_associated_compat, backend_color_compat
    use fortplot_figure_initialization, only: figure_state_t
    implicit none

    private
    public :: get_figure_width_property, get_figure_height_property
    public :: get_figure_rendered_property, set_figure_rendered_property
    public :: get_figure_plot_count_property, get_figure_plots_property
    public :: get_figure_x_min_property, get_figure_x_max_property
    public :: get_figure_y_min_property, get_figure_y_max_property
    public :: figure_backend_color_property, figure_backend_associated_property
    public :: figure_backend_line_property

contains

    function get_figure_width_property(state) result(width)
        !! Get figure width
        type(figure_state_t), intent(in) :: state
        integer :: width
        width = get_figure_width_compat(state)
    end function get_figure_width_property
    
    function get_figure_height_property(state) result(height)
        !! Get figure height
        type(figure_state_t), intent(in) :: state
        integer :: height
        height = get_figure_height_compat(state)
    end function get_figure_height_property
    
    function get_figure_rendered_property(state) result(rendered)
        !! Get rendered state
        type(figure_state_t), intent(in) :: state
        logical :: rendered
        rendered = get_figure_rendered_compat(state)
    end function get_figure_rendered_property
    
    subroutine set_figure_rendered_property(state, rendered)
        !! Set rendered state
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: rendered
        call set_figure_rendered_compat(state, rendered)
    end subroutine set_figure_rendered_property
    
    function get_figure_plot_count_property(state) result(plot_count)
        !! Get number of plots
        type(figure_state_t), intent(in) :: state
        integer :: plot_count
        plot_count = get_figure_plot_count_compat(state)
    end function get_figure_plot_count_property
    
    function get_figure_plots_property(plots) result(plots_ptr)
        !! Get pointer to plots array  
        type(plot_data_t), intent(in), target :: plots(:)
        type(plot_data_t), pointer :: plots_ptr(:)
        plots_ptr => plots  ! Keep direct access for efficiency
    end function get_figure_plots_property
    
    function get_figure_x_min_property(state) result(x_min)
        !! Get x minimum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_min
        x_min = get_figure_x_min_compat(state)
    end function get_figure_x_min_property
    
    function get_figure_x_max_property(state) result(x_max)
        !! Get x maximum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_max
        x_max = get_figure_x_max_compat(state)
    end function get_figure_x_max_property
    
    function get_figure_y_min_property(state) result(y_min)
        !! Get y minimum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_min
        y_min = get_figure_y_min_compat(state)
    end function get_figure_y_min_property
    
    function get_figure_y_max_property(state) result(y_max)
        !! Get y maximum value
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_max
        y_max = get_figure_y_max_compat(state)
    end function get_figure_y_max_property

    subroutine figure_backend_color_property(state, r, g, b)
        !! Set color using backend
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: r, g, b
        call backend_color_compat(state, r, g, b)
    end subroutine figure_backend_color_property
    
    function figure_backend_associated_property(state) result(is_associated)
        !! Check if backend is associated
        type(figure_state_t), intent(in) :: state
        logical :: is_associated
        is_associated = backend_associated_compat(state)
    end function figure_backend_associated_property
    
    subroutine figure_backend_line_property(state, x1, y1, x2, y2)
        !! Draw line using backend
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x1, y1, x2, y2
        call backend_line_compat(state, x1, y1, x2, y2)
    end subroutine figure_backend_line_property

end module fortplot_figure_properties