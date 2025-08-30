module fortplot_figure_plotting_interface
    !! Interface aggregator for all plotting operations
    !! 
    !! This module provides a focused interface that aggregates all plotting
    !! functionality from specialized modules. This reduces coupling by providing
    !! a single dependency for core modules instead of multiple direct dependencies.
    !!
    !! ARCHITECTURE: Interface Segregation Pattern
    !! - Aggregates plotting operations from 3 focused modules
    !! - Reduces coupling from 19 dependencies to 3 in core modules  
    !! - Maintains clean separation between plotting types
    !! - Preserves all public APIs for backward compatibility

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    
    ! Import focused basic plotting modules (≤3 dependencies)
    use fortplot_figure_plots, only: figure_add_plot, figure_add_contour, &
                                     figure_add_contour_filled, figure_add_pcolormesh
    use fortplot_figure_scatter, only: add_scatter_plot
    ! Note: Advanced plots (boxplot, streamlines, histogram) moved to advanced_plotting_interface

    implicit none
    private

    ! Re-export basic plotting interfaces with proper signatures
    public :: figure_add_plot, figure_add_contour, figure_add_contour_filled
    public :: figure_add_pcolormesh, add_scatter_plot

end module fortplot_figure_plotting_interface