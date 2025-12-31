module fortplot_matplotlib_advanced
    !! Slim matplotlib-compatible facade that aggregates specialised wrappers

    use fortplot_matplotlib_plot_wrappers, only: &
        plot, scatter, errorbar, boxplot, bar, barh, hist, histogram, add_plot, &
        add_errorbar, add_scatter, add_3d_plot
    use fortplot_matplotlib_field_wrappers, only: &
        contour, contour_filled, pcolormesh, streamplot, quiver, add_quiver, &
        add_contour, add_contour_filled, add_pcolormesh, add_surface
    use fortplot_matplotlib_axes, only: &
        xlabel, ylabel, title, suptitle, legend, grid, xlim, ylim, set_xscale, &
        set_yscale, set_line_width, set_ydata, use_axis, get_active_axis
    use fortplot_matplotlib_session, only: &
        ensure_global_figure_initialized, get_global_figure, figure, subplot, &
        subplots, subplots_grid, savefig, savefig_with_status, show_data, &
        show_figure, show_viewer
    use fortplot_matplotlib_colorbar, only: colorbar
    use fortplot_matplotlib_plots_new, only: &
        imshow, pie, polar, step, stem, fill, fill_between, twinx, twiny

    implicit none
    private

    public :: plot, scatter, errorbar, boxplot
    public :: bar, barh, hist, histogram
    public :: add_plot, add_errorbar, add_scatter, add_3d_plot
    public :: imshow, pie, polar, step, stem
    public :: fill, fill_between, twinx, twiny

    public :: contour, contour_filled, pcolormesh, streamplot, quiver
    public :: add_contour, add_contour_filled, add_pcolormesh, add_surface
    public :: add_quiver

    public :: xlabel, ylabel, title, suptitle, legend, grid
    public :: xlim, ylim, set_xscale, set_yscale
    public :: set_line_width, set_ydata, use_axis, get_active_axis
    public :: colorbar

    public :: figure, subplot, subplots, subplots_grid
    public :: savefig, savefig_with_status
    public :: show, show_viewer
    public :: ensure_global_figure_initialized, get_global_figure

    interface show
        module procedure show_data, show_figure
    end interface show

end module fortplot_matplotlib_advanced
