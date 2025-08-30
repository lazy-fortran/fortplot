module fortplot_figure_comprehensive_operations
    !! Comprehensive operations interface for fortplot figure core
    !! 
    !! This module provides a single comprehensive interface that aggregates
    !! ALL figure operations needed by the core module. This reduces coupling
    !! from 19 direct dependencies to 1 comprehensive interface.
    !!
    !! ARCHITECTURE: Facade Pattern
    !! - Single entry point for all figure operations
    !! - Encapsulates complex subsystem interactions
    !! - Reduces coupling while maintaining functionality
    !! - Provides clean separation between core and implementation modules

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    
    ! Import ALL necessary modules (facade pattern allows more dependencies)
    use fortplot_figure_operations
    use fortplot_figure_management
    use fortplot_figure_properties_new
    use fortplot_figure_core_ranges, only: update_data_ranges_figure, update_data_ranges_pcolormesh_figure

    implicit none
    private

    ! Re-export ALL operations needed by core module
    ! Operations module functions (exact names used by core)
    public :: figure_add_plot_operation, figure_add_contour_operation, figure_add_contour_filled_operation
    public :: figure_add_pcolormesh_operation, figure_streamplot_operation, figure_hist_operation
    public :: figure_boxplot_operation, figure_scatter_operation, figure_set_xlabel_operation
    public :: figure_set_ylabel_operation, figure_set_title_operation, figure_set_xscale_operation
    public :: figure_set_yscale_operation, figure_set_xlim_operation, figure_set_ylim_operation
    public :: figure_set_line_width_operation, figure_set_ydata_operation, figure_legend_operation
    public :: figure_grid_operation, figure_render
    
    ! Management module functions
    public :: figure_initialize, figure_destroy, figure_clear, figure_savefig, figure_savefig_with_status
    public :: figure_show, figure_clear_streamlines
    public :: figure_subplots, figure_subplot_plot, figure_subplot_plot_count
    public :: figure_subplot_set_title, figure_subplot_set_xlabel, figure_subplot_set_ylabel
    public :: figure_subplot_title
    public :: figure_setup_png_backend_for_animation
    public :: figure_extract_rgb_data_for_animation, figure_extract_png_data_for_animation
    
    ! Properties module functions  
    public :: figure_get_width, figure_get_height, figure_get_rendered, figure_set_rendered
    public :: figure_get_plot_count, figure_get_plots
    public :: figure_get_x_min, figure_get_x_max, figure_get_y_min, figure_get_y_max
    public :: figure_backend_color, figure_backend_associated, figure_backend_line
    
    ! Additional operations needed by core
    public :: update_data_ranges_figure, update_data_ranges_pcolormesh_figure

end module fortplot_figure_comprehensive_operations