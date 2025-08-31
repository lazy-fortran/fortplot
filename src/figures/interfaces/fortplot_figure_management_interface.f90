module fortplot_figure_management_interface
    !! Interface aggregator for all management operations
    !! 
    !! This module provides a focused interface that aggregates all management
    !! functionality from specialized modules. This reduces coupling by providing
    !! a single dependency for core modules instead of multiple direct dependencies.
    !!
    !! ARCHITECTURE: Interface Segregation Pattern
    !! - Aggregates management operations from 3 focused modules
    !! - Reduces coupling from 19 dependencies to 3 in core modules  
    !! - Maintains clean separation between management types
    !! - Preserves all public APIs for backward compatibility

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t, initialize_figure_state
    
    ! Import focused management modules (≤3 dependencies)
    use fortplot_figure_management
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges, &
                                                  render_figure_to_backend
    use fortplot_figure_plot_management, only: update_plot_ydata, setup_figure_legend

    implicit none
    private

    ! Re-export all management interfaces with proper signatures
    public :: initialize_figure_state
    public :: calculate_figure_data_ranges, render_figure_to_backend
    public :: update_plot_ydata, setup_figure_legend
    
    ! Management operation interfaces from figure_management
    public :: figure_initialize, figure_destroy, figure_savefig, figure_savefig_with_status
    public :: figure_show, figure_clear_streamlines, figure_setup_png_backend_for_animation
    public :: figure_extract_rgb_data_for_animation, figure_extract_png_data_for_animation
    public :: figure_subplots, figure_subplot_plot, figure_subplot_plot_count
    public :: figure_subplot_set_title, figure_subplot_set_xlabel, figure_subplot_set_ylabel
    public :: figure_subplot_title

end module fortplot_figure_management_interface