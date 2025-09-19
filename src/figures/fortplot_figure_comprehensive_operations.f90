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
    use fortplot_figure_core_operations
    use fortplot_figure_core_config
    use fortplot_figure_core_advanced
    use fortplot_figure_core_accessors
    use fortplot_figure_core_utils

    implicit none
    private

    ! Re-export ALL operations needed by core module
    ! Operations module functions (exact names used by core)
    public :: figure_add_plot_operation, figure_add_contour_operation, figure_add_contour_filled_operation
    public :: figure_add_pcolormesh_operation, figure_add_fill_between_operation
    public :: figure_streamplot_operation, figure_hist_operation
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
    public :: core_clear, core_clear_streamlines, core_destroy
    
    ! Properties module functions  
    public :: figure_get_width, figure_get_height, figure_get_rendered, figure_set_rendered
    public :: figure_get_plot_count, figure_get_plots
    public :: figure_get_x_min, figure_get_x_max, figure_get_y_min, figure_get_y_max
    public :: figure_backend_color, figure_backend_associated, figure_backend_line, figure_backend_arrow
    
    ! Additional operations needed by core
    public :: update_data_ranges_figure, update_data_ranges_pcolormesh_figure
    
    ! Core operations extracted from main module
    public :: core_initialize, core_add_plot, core_add_contour, core_add_contour_filled
    public :: core_add_pcolormesh, core_add_fill_between
    public :: core_streamplot, core_savefig, core_savefig_with_status
    public :: core_show
    
    ! Configuration operations from core_config module
    public :: core_set_xlabel, core_set_ylabel, core_set_title
    public :: core_set_xscale, core_set_yscale, core_set_xlim, core_set_ylim
    public :: core_set_line_width, core_grid
    
    ! Advanced plotting operations from core_advanced module
    public :: core_scatter, core_hist, core_boxplot
    
    ! Property accessor operations from core_accessors module
    public :: core_get_width, core_get_height, core_get_rendered, core_set_rendered
    public :: core_get_plot_count, core_get_plots, core_get_x_min, core_get_x_max
    public :: core_get_y_min, core_get_y_max, core_backend_color, core_backend_associated
    public :: core_backend_line, core_setup_png_backend_for_animation, core_backend_arrow
    public :: core_extract_rgb_data_for_animation, core_extract_png_data_for_animation
    
    ! Utility operations from core_utils module
    public :: core_set_ydata, core_figure_legend

end module fortplot_figure_comprehensive_operations
