module fortplot_figure_properties_new
    !! Figure property management module for fortplot_figure_core
    !! 
    !! This module handles property access, data ranges, and backend interface
    !! for figure objects. Extracted from fortplot_figure_core.f90 for better
    !! organization following Single Responsibility Principle.
    !!
    !! Responsibilities:
    !! - Property getters/setters (width, height, rendered, plot counts)
    !! - Data range management (x_min, x_max, y_min, y_max)
    !! - Backend interface methods (color, line, association)
    !! - Figure state property access

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_ranges, only: update_figure_data_ranges_pcolormesh, update_figure_data_ranges_boxplot
    use fortplot_figure_compatibility, only: get_figure_width_compat, get_figure_height_compat, &
                                             get_figure_rendered_compat, set_figure_rendered_compat, &
                                             get_figure_plot_count_compat, get_figure_x_min_compat, &
                                             get_figure_x_max_compat, get_figure_y_min_compat, &
                                             get_figure_y_max_compat, backend_line_compat, &
                                             backend_associated_compat, backend_color_compat
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges
    implicit none

    private
    public :: figure_get_width, figure_get_height, figure_get_rendered, figure_set_rendered
    public :: figure_get_plot_count, figure_get_plots, figure_backend_color, figure_backend_associated
    public :: figure_backend_line, figure_get_x_min, figure_get_x_max, figure_get_y_min, figure_get_y_max
    public :: figure_update_data_ranges_pcolormesh, figure_update_data_ranges_boxplot, figure_update_data_ranges

contains

    function figure_get_width(state) result(width)
        !! Get figure width property
        type(figure_state_t), intent(in) :: state
        integer :: width
        width = get_figure_width_compat(state)
    end function figure_get_width
    
    function figure_get_height(state) result(height)
        !! Get figure height property
        type(figure_state_t), intent(in) :: state
        integer :: height
        height = get_figure_height_compat(state)
    end function figure_get_height
    
    function figure_get_rendered(state) result(rendered)
        !! Get figure rendered property
        type(figure_state_t), intent(in) :: state
        logical :: rendered
        rendered = get_figure_rendered_compat(state)
    end function figure_get_rendered
    
    subroutine figure_set_rendered(state, rendered)
        !! Set figure rendered property
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: rendered
        call set_figure_rendered_compat(state, rendered)
    end subroutine figure_set_rendered
    
    function figure_get_plot_count(state) result(plot_count)
        !! Get figure plot count property
        type(figure_state_t), intent(in) :: state
        integer :: plot_count
        plot_count = get_figure_plot_count_compat(state)
    end function figure_get_plot_count
    
    function figure_get_plots(plots) result(plots_ptr)
        !! Get figure plots property
        type(plot_data_t), intent(in), target :: plots(:)
        type(plot_data_t), pointer :: plots_ptr(:)
        plots_ptr => plots
    end function figure_get_plots
    
    subroutine figure_backend_color(state, r, g, b)
        !! Set backend color property
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: r, g, b
        call backend_color_compat(state, r, g, b)
    end subroutine figure_backend_color
    
    function figure_backend_associated(state) result(is_associated)
        !! Get backend association property
        type(figure_state_t), intent(in) :: state
        logical :: is_associated
        is_associated = backend_associated_compat(state)
    end function figure_backend_associated
    
    subroutine figure_backend_line(state, x1, y1, x2, y2)
        !! Draw line using backend property
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x1, y1, x2, y2
        call backend_line_compat(state, x1, y1, x2, y2)
    end subroutine figure_backend_line
    
    function figure_get_x_min(state) result(x_min)
        !! Get x minimum property
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_min
        x_min = get_figure_x_min_compat(state)
    end function figure_get_x_min
    
    function figure_get_x_max(state) result(x_max)
        !! Get x maximum property
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_max
        x_max = get_figure_x_max_compat(state)
    end function figure_get_x_max
    
    function figure_get_y_min(state) result(y_min)
        !! Get y minimum property
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_min
        y_min = get_figure_y_min_compat(state)
    end function figure_get_y_min
    
    function figure_get_y_max(state) result(y_max)
        !! Get y maximum property
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_max
        y_max = get_figure_y_max_compat(state)
    end function figure_get_y_max

    subroutine figure_update_data_ranges_pcolormesh(plots, plot_count, &
                                                   xlim_set, ylim_set, &
                                                   x_min, x_max, y_min, y_max)
        !! Update data ranges after adding pcolormesh plot - delegate to ranges module
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        
        call update_figure_data_ranges_pcolormesh(plots, plot_count, &
                                                 xlim_set, ylim_set, &
                                                 x_min, x_max, y_min, y_max)
    end subroutine figure_update_data_ranges_pcolormesh

    subroutine figure_update_data_ranges_boxplot(data, position, &
                                                x_min, x_max, y_min, y_max, &
                                                xlim_set, ylim_set)
        !! Update data ranges after adding boxplot - delegate to ranges module
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        logical, intent(in) :: xlim_set, ylim_set
        
        call update_figure_data_ranges_boxplot(data, position, &
                                               x_min, x_max, y_min, y_max, &
                                               xlim_set, ylim_set)
    end subroutine figure_update_data_ranges_boxplot

    subroutine figure_update_data_ranges(plots, plot_count, &
                                        xlim_set, ylim_set, &
                                        x_min, x_max, y_min, y_max, &
                                        x_min_transformed, x_max_transformed, &
                                        y_min_transformed, y_max_transformed, &
                                        xscale, yscale, symlog_threshold)
        !! Update data ranges based on current plot
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in) :: xlim_set, ylim_set
        real(wp), intent(inout) :: x_min, x_max, y_min, y_max
        real(wp), intent(inout) :: x_min_transformed, x_max_transformed
        real(wp), intent(inout) :: y_min_transformed, y_max_transformed
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        
        call calculate_figure_data_ranges(plots, plot_count, &
                                        xlim_set, ylim_set, &
                                        x_min, x_max, y_min, y_max, &
                                        x_min_transformed, x_max_transformed, &
                                        y_min_transformed, y_max_transformed, &
                                        xscale, yscale, symlog_threshold)
    end subroutine figure_update_data_ranges

end module fortplot_figure_properties_new
