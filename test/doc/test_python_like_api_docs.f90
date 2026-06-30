program test_python_like_api_docs
    implicit none

    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine figure(num, figsize, dpi)', &
                          [character(len=24) :: 'Parameters', 'num', 'figsize', 'dpi'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine subplot(nrows, ncols, index)', &
                          [character(len=24) :: 'Parameters', 'nrows', 'ncols', 'index'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine subplots(nrows, ncols, axes, sharex, sharey)', &
                          [character(len=24) :: 'Parameters', 'axes', 'sharex', 'sharey'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine savefig(filename, dpi, transparent, bbox_inches)', &
                          [character(len=24) :: 'Parameters', 'filename', 'bbox_inches'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine show_data(x, y, label, title_text, xlabel_text, ylabel_text, blocking)', &
                          [character(len=24) :: 'Parameters', 'x', 'blocking'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine show_figure(blocking)', &
                          [character(len=24) :: 'Parameters', 'blocking'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine show_viewer(blocking)', &
                          [character(len=24) :: 'Parameters', 'blocking'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine ion()', &
                          [character(len=24) :: 'interactive mode'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine ioff()', &
                          [character(len=24) :: 'Disable interactive mode'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine draw()', &
                          [character(len=24) :: 'ASCII output'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine ensure_fig_init()', &
                          [character(len=24) :: 'global figure exists'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine ensure_global_figure_initialized()', &
                          [character(len=24) :: 'singleton is ready'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'function get_global_figure() result(global_fig)', &
                          [character(len=24) :: 'global figure pointer'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'function subplots_grid(nrows, ncols) result(axes)', &
                          [character(len=24) :: 'Returns', 'axes'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_session.f90', &
                          'subroutine savefig_with_status(filename, status, dpi, transparent, bbox_inches)', &
                          [character(len=24) :: 'Parameters', 'status', 'bbox_inches'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine xlabel(label_text)', &
                          [character(len=24) :: 'Parameters', 'label_text'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine ylabel(label_text)', &
                          [character(len=24) :: 'Parameters', 'label_text'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine title(title_text)', &
                          [character(len=24) :: 'Parameters', 'title_text'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine legend(loc, box, fontsize, position)', &
                          [character(len=24) :: 'Parameters', 'loc', 'position'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine grid(visible, which, axis, alpha, linestyle, enabled)', &
                          [character(len=24) :: 'Parameters', 'visible', 'enabled'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine set_xscale(scale, linthresh, threshold, base, linscale)', &
                          [character(len=24) :: 'Parameters', 'scale', 'linthresh', 'threshold'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine set_yscale(scale, linthresh, threshold, base, linscale)', &
                          [character(len=24) :: 'Parameters', 'scale', 'linthresh', 'threshold'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine xlim(xmin, xmax)', &
                          [character(len=24) :: 'Parameters', 'xmin', 'xmax'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine suptitle(title_text, fontsize)', &
                          [character(len=24) :: 'Parameters', 'title_text', 'fontsize'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine ylim(ymin, ymax)', &
                          [character(len=24) :: 'Parameters', 'ymin', 'ymax'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine view_init(elev, azim, dist)', &
                          [character(len=24) :: 'Parameters', 'elev', 'azim'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine xscale(scale, linthresh, threshold, base, linscale)', &
                          [character(len=24) :: 'Parameters', 'scale', 'threshold'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine yscale(scale, linthresh, threshold, base, linscale)', &
                          [character(len=24) :: 'Parameters', 'scale', 'threshold'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine set_line_width(width)', &
                          [character(len=24) :: 'Parameters', 'width'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine set_ydata(ydata)', &
                          [character(len=24) :: 'Parameters', 'ydata'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine use_axis(axis_name)', &
                          [character(len=24) :: 'Parameters', 'axis_name'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine minorticks_on()', &
                          [character(len=24) :: 'minor ticks'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine tight_layout(pad, w_pad, h_pad)', &
                          [character(len=24) :: 'Parameters', 'pad', 'w_pad'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine axhline(y, xmin, xmax, color, linestyle, linewidth, label)', &
                          [character(len=24) :: 'Parameters', 'y', 'linewidth'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine axvline(x, ymin, ymax, color, linestyle, linewidth, label)', &
                          [character(len=24) :: 'Parameters', 'x', 'linewidth'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine hlines(y, xmin, xmax, colors, linestyles, linewidth, label)', &
                          [character(len=24) :: 'Parameters', 'y', 'linewidth'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine vlines(x, ymin, ymax, colors, linestyles, linewidth, label)', &
                          [character(len=24) :: 'Parameters', 'x', 'linewidth'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine set_xticks(positions, labels)', &
                          [character(len=24) :: 'Parameters', 'positions', 'labels'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'subroutine set_yticks(positions, labels)', &
                          [character(len=24) :: 'Parameters', 'positions', 'labels'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_axes.f90', &
                          'function get_active_axis() result(axis_name)', &
                          [character(len=24) :: 'Returns', 'axis_name'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine plot(x, y, label, linestyle, color, linewidth, marker, markersize, alpha)', &
                          [character(len=24) :: 'Parameters', 'x', 'markersize', 'alpha'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine bar_rgb(x, height, width, bottom, label, color, edgecolor, align, alpha)', &
                          [character(len=24) :: 'Parameters', 'x', 'edgecolor', 'alpha'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine bar_string(x, height, color, width, bottom, label, edgecolor, align, alpha)', &
                          [character(len=24) :: 'Parameters', 'height', 'color'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine barh_rgb(y, width, height, left, label, color, edgecolor, align, alpha)', &
                          [character(len=24) :: 'Parameters', 'y', 'left'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine barh_string(y, width, color, height, left, label, edgecolor, align, alpha)', &
                          [character(len=24) :: 'Parameters', 'y', 'color'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine bar_rgb_edgecolor(x, height, color, edgecolor, width, bottom, label, align, alpha)', &
                          [character(len=24) :: 'Parameters', 'edgecolor', 'alpha'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine barh_rgb_edgecolor(y, width, color, edgecolor, height, left, label, align, alpha)', &
                          [character(len=24) :: 'Parameters', 'edgecolor', 'alpha'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine bar_rgb_array(x, height, color_per_bar, edgecolor_per_bar, width, bottom, label, align, alpha)', &
                          [character(len=24) :: 'Parameters', 'color_per_bar', 'edgecolor_per_bar'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine barh_rgb_array(y, width, color_per_bar, edgecolor_per_bar, height, left, label, align, alpha)', &
                          [character(len=24) :: 'Parameters', 'color_per_bar', 'edgecolor_per_bar'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine boxplot_string(data, position, width, label, show_outliers, horizontal, color)', &
                          [character(len=24) :: 'Parameters', 'data', 'horizontal', 'color'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine boxplot_rgb(data, position, width, label, show_outliers, horizontal, color)', &
                          [character(len=24) :: 'Parameters', 'data', 'horizontal', 'color'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine add_plot_rgb(x, y, color, label, linestyle, alpha)', &
                          [character(len=24) :: 'Parameters', 'x', 'color'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine add_plot_string(x, y, color, label, linestyle, alpha)', &
                          [character(len=24) :: 'Parameters', 'x', 'color'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine add_3d_plot_rgb(x, y, z, label, linestyle, color, linewidth, marker, &', &
                          [character(len=24) :: 'Parameters', 'z', 'markersize'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plot_wrappers.f90', &
                          'subroutine add_3d_plot_string(x, y, z, label, linestyle, color, linewidth, marker, &', &
                          [character(len=24) :: 'Parameters', 'x', 'markersize', 'color'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_colorbar.f90', &
                          'subroutine colorbar(label, location, fraction, pad, shrink, plot_index, &', &
                          [character(len=24) :: 'Parameters', 'label', 'plot_index', 'ticks'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_mesh_wrappers.f90', &
                          'subroutine pcolormesh(x, y, z, shading, cmap, show_colorbar, label, &', &
                          [character(len=24) :: 'Parameters', 'x', 'show_colorbar', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_mesh_wrappers.f90', &
                          'subroutine add_pcolormesh(x, y, z, shading, cmap, show_colorbar, label, &', &
                          [character(len=24) :: 'Parameters', 'x', 'show_colorbar', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_mesh_wrappers.f90', &
                          'subroutine add_surface(x, y, z, cmap, show_colorbar, alpha, edgecolor, &', &
                          [character(len=24) :: 'Parameters', 'z', 'edgecolor', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_field_wrappers.f90', &
                          'subroutine contour(x, y, z, levels, cmap, label, colormap)', &
                          [character(len=24) :: 'Parameters', 'label', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_field_wrappers.f90', &
                          'subroutine contour_filled(x, y, z, levels, cmap, show_colorbar, label, &', &
                          [character(len=24) :: 'Parameters', 'z', 'show_colorbar', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_field_wrappers.f90', &
                          'subroutine contourf(x, y, z, levels, cmap, show_colorbar, label, colormap)', &
                          [character(len=24) :: 'Parameters', 'show_colorbar', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_field_wrappers.f90', &
                          'subroutine add_contour(x, y, z, levels, cmap, label, colormap)', &
                          [character(len=24) :: 'Parameters', 'label', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_field_wrappers.f90', &
                          'subroutine add_contour_filled(x, y, z, levels, cmap, show_colorbar, label, &', &
                          [character(len=24) :: 'Parameters', 'show_colorbar', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_field_wrappers.f90', &
                          'subroutine add_contourf(x, y, z, levels, cmap, show_colorbar, label, colormap)', &
                          [character(len=24) :: 'Parameters', 'show_colorbar', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_vector_wrappers.f90', &
                          'subroutine streamplot(x, y, u, v, density, linewidth, color, &', &
                          [character(len=24) :: 'Parameters', 'u', 'arrowsize', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_vector_wrappers.f90', &
                          'subroutine quiver_rgb(x, y, u, v, scale, color, width, headwidth, &', &
                          [character(len=24) :: 'Parameters', 'scale', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_vector_wrappers.f90', &
                          'subroutine quiver_string(x, y, u, v, color, scale, width, headwidth, &', &
                          [character(len=24) :: 'Parameters', 'color', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_vector_wrappers.f90', &
                          'subroutine add_quiver_rgb(x, y, u, v, scale, color, width, headwidth, &', &
                          [character(len=24) :: 'Parameters', 'scale', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_vector_wrappers.f90', &
                          'subroutine add_quiver_string(x, y, u, v, color, scale, width, headwidth, &', &
                          [character(len=24) :: 'Parameters', 'color', 'colormap'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_errorbar.f90', &
                          'subroutine errorbar_rgb(x, y, xerr, yerr, fmt, label, capsize, linestyle, &', &
                          [character(len=24) :: 'Parameters', 'xerr', 'yerr'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_errorbar.f90', &
                          'subroutine errorbar_string(x, y, color, xerr, yerr, fmt, label, capsize, &', &
                          [character(len=24) :: 'Parameters', 'color', 'xerr'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_errorbar.f90', &
                          'subroutine errorbar_rgb_icap(x, y, capsize, xerr, yerr, fmt, label, linestyle, &', &
                          [character(len=24) :: 'Parameters', 'capsize', 'xerr'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_errorbar.f90', &
                          'subroutine errorbar_string_icap(x, y, color, capsize, xerr, yerr, fmt, label, &', &
                          [character(len=24) :: 'Parameters', 'color', 'capsize', 'xerr'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_errorbar.f90', &
                          'subroutine add_errorbar_rgb(x, y, xerr, yerr, fmt, label, capsize, &', &
                          [character(len=24) :: 'Object-oriented alias', 'errorbar_rgb'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_errorbar.f90', &
                          'subroutine add_errorbar_string(x, y, color, xerr, yerr, fmt, label, capsize, &', &
                          [character(len=24) :: 'Object-oriented alias', 'errorbar_string'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_errorbar.f90', &
                          'subroutine add_errorbar_rgb_icap(x, y, capsize, xerr, yerr, fmt, label, &', &
                          [character(len=24) :: 'Integer-capsize overload', 'add_errorbar_rgb'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_errorbar.f90', &
                          'subroutine add_errorbar_string_icap(x, y, color, capsize, xerr, yerr, fmt, &', &
                          [character(len=24) :: 'Integer-capsize overload', 'add_errorbar_string'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_scatter.f90', &
                          'subroutine scatter_rgb(x, y, s, c, label, marker, markersize, color, &', &
                          [character(len=24) :: 'Parameters', 'x', 'markersize'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_scatter.f90', &
                          'subroutine scatter_string(x, y, c, label, marker, markersize, color, &', &
                          [character(len=24) :: 'Parameters', 'x', 'markersize', 'linewidths_scalar'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_scatter.f90', &
                          'subroutine add_scatter_2d_rgb(x, y, markersize, s, c, label, marker, color, &', &
                          [character(len=24) :: 'Parameters', 'x', 'markersize'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_scatter.f90', &
                          'subroutine add_scatter_2d_string(x, y, color, c, label, marker, markersize, &', &
                          [character(len=24) :: 'Parameters', 'x', 'markersize'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_scatter.f90', &
                          'subroutine add_scatter_3d_rgb(x, y, z, s, c, label, marker, markersize, &', &
                          [character(len=24) :: 'Parameters', 'z', 'markersize'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_scatter.f90', &
                          'subroutine add_scatter_3d_string(x, y, z, color, s, c, label, marker, &', &
                          [character(len=24) :: 'Parameters', 'z', 'markersize'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_hist_wrappers.f90', &
                          'subroutine hist_rgb(data, bins, range, density, weights, cumulative, &', &
                          [character(len=24) :: 'Parameters', 'data', 'alpha'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_hist_wrappers.f90', &
                          'subroutine hist_string(data, color, bins, range, density, weights, &', &
                          [character(len=24) :: 'Parameters', 'data', 'color'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_hist_wrappers.f90', &
                          'subroutine histogram_rgb(data, bins, range, density, weights, cumulative, &', &
                          [character(len=24) :: 'Parameters', 'data', 'alpha'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_hist_wrappers.f90', &
                          'subroutine histogram_string(data, color, bins, range, density, weights, &', &
                          [character(len=24) :: 'Parameters', 'data', 'orientation', 'alpha'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine imshow(z, cmap, alpha, vmin, vmax, origin, extent, interpolation, aspect)', &
                          [character(len=24) :: 'Parameters', 'z', 'cmap', 'aspect'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine pie(values, labels, colors, explode, autopct, startangle)', &
                          [character(len=24) :: 'Parameters', 'values', 'labels'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine polar_string(theta, r, fmt, label, linestyle, marker, color)', &
                          [character(len=24) :: 'Parameters', 'theta', 'r'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine polar_rgb(theta, r, color, fmt, label, linestyle, marker)', &
                          [character(len=24) :: 'Parameters', 'theta', 'r'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine step_string(x, y, where, label, linestyle, color, linewidth)', &
                          [character(len=24) :: 'Parameters', 'where', 'linewidth'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine step_rgb(x, y, color, where, label, linestyle, linewidth)', &
                          [character(len=24) :: 'Parameters', 'where', 'linewidth'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine stem(x, y, linefmt, markerfmt, basefmt, label, bottom)', &
                          [character(len=24) :: 'Parameters', 'linefmt', 'bottom'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine fill_string(x, y, color, alpha, step)', &
                          [character(len=24) :: 'Parameters', 'y', 'step'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine fill_rgb(x, y, color, alpha, step)', &
                          [character(len=24) :: 'Parameters', 'y', 'step'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine fill_default(x, y, alpha, step)', &
                          [character(len=24) :: 'Parameters', 'alpha'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine fill_between_string(x, y1, y2, where, color, alpha, interpolate, step)', &
                          [character(len=24) :: 'Parameters', 'y1', 'color', 'step'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine fill_between_rgb(x, y1, y2, where, color, alpha, interpolate, step)', &
                          [character(len=24) :: 'Parameters', 'y1', 'step'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine twinx()', &
                          [character(len=24) :: 'Parameters', 'None.'])
    call assert_doc_block('src/interfaces/fortplot_matplotlib_plots.f90', &
                          'subroutine twiny()', &
                          [character(len=24) :: 'Parameters', 'None.'])

    print *, 'All python-like API doc tests passed.'

contains

    subroutine assert_doc_block(path, signature, patterns)
        character(len=*), intent(in) :: path
        character(len=*), intent(in) :: signature
        character(len=*), dimension(:), intent(in) :: patterns

        integer, parameter :: max_lines = 4096
        character(len=1024) :: lines(max_lines)
        character(len=1024) :: line
        integer :: unit, ios, count, i, j
        logical :: found_signature, found_pattern

        count = 0
        open(newunit=unit, file=path, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Cannot open ', trim(path)
            stop 1
        end if

        do
            read(unit, '(A)', iostat=ios) line
            if (ios /= 0) exit
            count = count + 1
            if (count > max_lines) then
                print *, 'Too many lines in ', trim(path)
                close(unit)
                stop 1
            end if
            lines(count) = line
        end do
        close(unit)

        found_signature = .false.
        do i = 1, count
            if (index(lines(i), signature) > 0) then
                found_signature = .true.
                if (.not. signature_has_doc_block(lines, i)) then
                    print *, 'Missing doc block in ', trim(path), ': ', trim(signature)
                    stop 1
                end if
                do j = 1, size(patterns)
                    found_pattern = block_has_pattern(lines, count, i, patterns(j))
                    if (.not. found_pattern) then
                        print *, 'Missing doc text in ', trim(path), ': ', trim(patterns(j))
                        stop 1
                    end if
                end do
                exit
            end if
        end do

        if (.not. found_signature) then
            print *, 'Missing signature in ', trim(path), ': ', trim(signature)
            stop 1
        end if
    end subroutine assert_doc_block

    logical function signature_has_doc_block(lines, signature_line)
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: signature_line
        integer :: i

        signature_has_doc_block = .false.
        do i = signature_line + 1, min(size(lines), signature_line + 8)
            if (index(lines(i), '!!') > 0) then
                signature_has_doc_block = .true.
                return
            end if
        end do
    end function signature_has_doc_block

    logical function block_has_pattern(lines, count, signature_line, pattern)
        character(len=*), intent(in) :: lines(:)
        integer, intent(in) :: count, signature_line
        character(len=*), intent(in) :: pattern
        integer :: first_line, last_line, i

        first_line = signature_line + 1
        last_line = min(count, signature_line + 32)
        block_has_pattern = .false.
        do i = first_line, last_line
            if (index(lines(i), trim(pattern)) > 0) then
                block_has_pattern = .true.
                return
            end if
        end do
    end function block_has_pattern

end program test_python_like_api_docs
