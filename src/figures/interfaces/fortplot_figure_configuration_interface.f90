module fortplot_figure_configuration_interface
    !! Interface aggregator for all configuration operations
    !! 
    !! This module provides a focused interface that aggregates all configuration
    !! functionality from specialized modules. This reduces coupling by providing
    !! a single dependency for core modules instead of multiple direct dependencies.
    !!
    !! ARCHITECTURE: Interface Segregation Pattern
    !! - Aggregates configuration operations from 3 focused modules
    !! - Reduces coupling from 19 dependencies to 3 in core modules  
    !! - Maintains clean separation between configuration types
    !! - Preserves all public APIs for backward compatibility

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_figure_initialization, only: figure_state_t
    
    ! Import focused configuration modules (≤3 dependencies)
    use fortplot_figure_core_config, only: grid_figure, set_xlabel_figure, &
                                           set_ylabel_figure, set_title_figure, &
                                           set_xscale_figure, set_yscale_figure, &
                                           set_xlim_figure, set_ylim_figure, &
                                           set_line_width_figure
    use fortplot_figure_properties_new
    use fortplot_figure_grid, only: render_grid_lines

    implicit none
    private

    ! Re-export all configuration interfaces with proper signatures
    public :: grid_figure, set_xlabel_figure, set_ylabel_figure, set_title_figure
    public :: set_xscale_figure, set_yscale_figure, set_xlim_figure, set_ylim_figure
    public :: set_line_width_figure, render_grid_lines
    
    ! Re-export properties interfaces
    public :: figure_get_width, figure_get_height, figure_get_rendered, figure_set_rendered
    public :: figure_get_plot_count, figure_get_plots, figure_backend_color, figure_backend_associated
    public :: figure_backend_line, figure_get_x_min, figure_get_x_max, figure_get_y_min, figure_get_y_max
    public :: figure_update_data_ranges_pcolormesh, figure_update_data_ranges_boxplot, figure_update_data_ranges

end module fortplot_figure_configuration_interface