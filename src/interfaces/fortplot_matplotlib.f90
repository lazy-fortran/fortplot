module fortplot_matplotlib
    !! Matplotlib-compatible API wrapper for fortplot
    !! Main facade module that re-exports all matplotlib-style functions
    !!
    !! This module provides a unified interface to matplotlib-style plotting
    !! by combining functionality from specialized submodules:
    !! - fortplot_matplotlib_plotting: Basic plots (plot, scatter, bar, etc.)
    !! - fortplot_matplotlib_contour: Field plots (contour, pcolormesh, etc.)
    !! - fortplot_matplotlib_axes: Axis operations (labels, limits, scales)
    !! - fortplot_matplotlib_io: Figure management (save, show, etc.)
    
    use fortplot_matplotlib_plotting, only: &
        plot, scatter, errorbar, boxplot, &
        bar, barh, hist, histogram, &
        add_plot, add_errorbar, add_scatter, add_3d_plot
    
    use fortplot_matplotlib_contour, only: &
        contour, contour_filled, pcolormesh, streamplot, &
        add_contour, add_contour_filled, add_pcolormesh, add_surface
    
    use fortplot_matplotlib_axes, only: &
        xlabel, ylabel, title, legend, grid, &
        xlim, ylim, set_xscale, set_yscale, &
        set_line_width, set_ydata
    
    use fortplot_matplotlib_io, only: &
        figure, subplot, savefig, savefig_with_status, &
        show, show_viewer, get_global_figure, ensure_global_figure_initialized
    
    use fortplot_text_stub, only: &
        text, annotate
    
    implicit none
    private
    
    ! Re-export all matplotlib-style functions from submodules
    ! Plotting functions
    public :: plot, scatter, errorbar, boxplot
    public :: bar, barh, hist, histogram
    public :: add_plot, add_errorbar, add_scatter, add_3d_plot
    
    ! Contour and field functions
    public :: contour, contour_filled, pcolormesh, streamplot
    public :: add_contour, add_contour_filled, add_pcolormesh, add_surface
    
    ! Axis and annotation functions
    public :: xlabel, ylabel, title, legend, grid
    public :: xlim, ylim, set_xscale, set_yscale
    public :: set_line_width, set_ydata
    public :: text, annotate
    
    ! Figure management functions
    public :: figure, subplot
    public :: savefig, savefig_with_status
    public :: show, show_viewer
    
    ! Testing support
    public :: get_global_figure, ensure_global_figure_initialized
    
end module fortplot_matplotlib
