module fortplot_figure_advanced_plotting_interface
    !! Interface aggregator for advanced plotting operations
    !! 
    !! This module provides a focused interface for advanced plotting functionality
    !! including boxplots, streamlines, and specialized plot types. Split from the
    !! main plotting interface to maintain ≤3 dependencies per module.
    !!
    !! ARCHITECTURE: Interface Segregation Pattern
    !! - Aggregates advanced plotting operations from 3 focused modules
    !! - Maintains ≤3 dependency limit per interface module
    !! - Separates basic plots from advanced visualizations
    !! - Preserves all public APIs for backward compatibility

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    
    ! Import focused advanced plotting modules (≤3 dependencies)
    use fortplot_figure_boxplot, only: add_boxplot, update_boxplot_ranges
    use fortplot_figure_streamlines, only: streamplot_figure, clear_streamline_data
    use fortplot_figure_histogram, only: hist_figure

    implicit none
    private

    ! Re-export all advanced plotting interfaces with proper signatures
    public :: add_boxplot, update_boxplot_ranges
    public :: streamplot_figure, clear_streamline_data
    public :: hist_figure

end module fortplot_figure_advanced_plotting_interface