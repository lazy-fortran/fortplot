module fortplot_matplotlib
    !! Matplotlib-compatible API wrapper for fortplot
    !! Main facade module that re-exports all matplotlib-style functions
    !!
    !! This module provides a unified interface to matplotlib-style plotting
    !! by re-exporting the consolidated implementation from
    !! `fortplot_matplotlib_advanced`. Public API remains stable while
    !! internal modules are simplified.

    use fortplot_matplotlib_advanced, only: &
        plot, scatter, errorbar, boxplot, &
        bar, barh, hist, histogram, &
        add_plot, add_errorbar, add_scatter, add_3d_plot, &
        imshow, pie, polar, step, stem, &
        fill, fill_between, twinx, twiny, &
        contour, contour_filled, pcolormesh, streamplot, quiver, &
        add_contour, add_contour_filled, add_pcolormesh, add_surface, &
        add_quiver, colorbar, &
        xlabel, ylabel, title, suptitle, legend, grid, &
        xlim, ylim, set_xscale, set_yscale, &
        set_line_width, set_ydata, use_axis, get_active_axis, minorticks_on, axis, &
        tight_layout, &
        figure, subplot, subplots, subplots_grid, savefig, savefig_with_status, &
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
    public :: imshow, pie, polar, step, stem
    public :: fill, fill_between, twinx, twiny

    ! Contour and field functions
    public :: contour, contour_filled, pcolormesh, streamplot, quiver
    public :: add_contour, add_contour_filled, add_pcolormesh, add_surface
    public :: add_quiver, colorbar

    ! Axis and annotation functions
    public :: xlabel, ylabel, title, suptitle, legend, grid
    public :: xlim, ylim, set_xscale, set_yscale
    public :: set_line_width, set_ydata, use_axis, get_active_axis, minorticks_on, axis
    public :: tight_layout
    public :: text, annotate

    ! Figure management functions
    public :: figure, subplot, subplots, subplots_grid
    public :: savefig, savefig_with_status
    public :: show, show_viewer

    ! Testing support
    public :: get_global_figure, ensure_global_figure_initialized

end module fortplot_matplotlib
