title: fortplot Documentation
---

# fortplot Documentation

Modern Fortran plotting for scientific workflows. Start here for the essential
resources, then dive into focused guides as needed.

## Quick Links

- [Quick Start](https://github.com/lazy-fortran/fortplot#usage) - install and
  produce your first plot in minutes
- [Setup Matrix](https://github.com/lazy-fortran/fortplot#setup) - fpm, CMake,
  and Python packaging instructions

## Examples Gallery

Browse runnable examples from the repository. Each page documents the key ideas
and links directly to the source code.

### Curated Highlights
- [Basic Plots](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/basic_plots)
- [3D Plotting](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/3d_plotting)
- [Contours](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/contour_demo)
- [Animations](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/animation)
- [Unicode Support](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/unicode_demo)

### Full Listing (generated)
<!-- AUTO_EXAMPLES_START -->

- [3D Plotting](./examples/3d_plotting.html) - 3D plotting (lines and surfaces) with axes, ticks, and labels.
- [Animation](./examples/animation.html) - Generate an MP4 animation from a sequence of frames.
- [Annotation Demo](./examples/annotation_demo.html) - Add text annotations in data coordinates.
- [Ascii Heatmap](./examples/ascii_heatmap.html) - Render a heatmap to terminal-friendly ASCII output.
- [Bar Chart Demo](./examples/bar_chart_demo.html) - Demonstrates grouped bar charts (vertical and horizontal) via both the stateful API and `figure_t`.
- [Basic Plots](./examples/basic_plots.html) - Basic line plots and saving outputs (PNG, PDF, ASCII).
- [Boxplot Demo](./examples/boxplot_demo.html) - Demonstrates box-and-whisker plots for statistical data visualization.
- [Colored Contours](./examples/colored_contours.html) - Contour plots with filled regions and colormaps.
- [Contour Demo](./examples/contour_demo.html) - Contour line plots from gridded data.
- [Contour Filled Demo](./examples/contour_filled_demo.html) - Filled contour plots, including colormaps and labeled contour levels.
- [Disconnected Lines](./examples/disconnected_lines.html) - Line plots with gaps created by NaN separators.
- [Dpi Demo](./examples/dpi_demo.html) - Control output DPI when saving figures for consistent sizing across formats.
- [Errorbar Demo](./examples/errorbar_demo.html) - Demonstrates error bar plotting with both symmetric and asymmetric errors for scientific data visualization.
- [Fill Between Demo](./examples/fill_between_demo.html) - Create filled regions using both the stateful API and `figure_t`.
- [Format String Demo](./examples/format_string_demo.html) - Matplotlib-style format strings for color, marker, and line style.
- [Grid Demo](./examples/grid_demo.html) - Demonstrates grid functionality for enhanced plot readability.
- [Legend Demo](./examples/legend_demo.html) - Legends, labels, and legend placement.
- [Line Styles](./examples/line_styles.html) - Line styles, dash patterns, and markers.
- [Marker Demo](./examples/marker_demo.html) - Marker styles and marker sizing.
- [Mathtext Demo](./examples/mathtext_demo.html) - Render math text and symbols in labels and titles.
- [Pcolormesh Demo](./examples/pcolormesh_demo.html) - Pcolormesh heatmaps with colormaps and shading.
- [Pcolormesh Negative](./examples/pcolormesh_negative.html) - Pcolormesh with negative-valued data, including colormap handling across zero.
- [Pie Chart Demo](./examples/pie_chart_demo.html) - Build pie charts using both the stateful API and `figure_t` (exploded wedges, `autopct`, and start angles).
- [Polar Demo](./examples/polar_demo.html) - Demonstrates fortplot's polar plotting API with custom colors, linestyles, and markers.
- [Scale Examples](./examples/scale_examples.html) - Linear, log, and symlog axis scales.
- [Scatter Demo](./examples/scatter_demo.html) - Demonstrates enhanced scatter plotting with color mapping, variable marker sizes, and bubble charts.
- [Show Viewer Demo](./examples/show_viewer_demo.html) - Open saved plots with a system viewer.
- [Smart Show Demo](./examples/smart_show_demo.html) - Choose a suitable backend/viewer based on the environment.
- [Streamplot Demo](./examples/streamplot_demo.html) - Streamplots for 2D vector fields.
- [Subplot Demo](./examples/subplot_demo.html) - Demonstration of subplot functionality using the stateful API.
- [Twin Axes Demo](./examples/twin_axes_demo.html) - Demonstrates `twinx` and `twiny` for multiple axes on one figure.
- [Unicode Demo](./examples/unicode_demo.html) - Unicode symbols in labels and titles.

<!-- AUTO_EXAMPLES_END -->

## User Guides

- [Scatter Plot Guide](scatter_plot_guide.md) - sizes, colours, and styling in a
  single reference
- [Surface Plot Guide](surface_plot_guide.md) - 3D surfaces and shading
- [Testing Guide](testing_guide.md) - local and CI workflows
- [Unicode Support](unicode_support.md) - math text and symbol rendering
- [Warning System](warning_system.md) - suppressing or surfacing diagnostics

## Platform Notes

- [Windows FFmpeg Setup](windows_ffmpeg_setup.md) - enable animation exports on
  Windows runners
- [Windows CI Performance](windows_ci_performance.md) - keep pipelines under
  target durations
- [Security](security.md) - hardening measures for sandboxed environments
- [User Compilation Guide](user_compilation_guide.md) - integrate fortplot into
  downstream projects

## Architecture & Design

- [Module Architecture](module_architecture.md)
- [Figure Management](design/figure_management.md)
- [Backends](design/backends.md)
- [Axes Layout](design/axes_layout.md)
- [Streamplot](design/streamplot.md)
- [Styling](design/styling.md)

## API Reference

- [Core Interface](https://github.com/lazy-fortran/fortplot/blob/main/src/fortplot.f90)
- [Plot Type Internals](design/basic_plots.md)
- [FORD Documentation](https://lazy-fortran.github.io/fortplot/)

## Project Resources

- [GitHub Repository](https://github.com/lazy-fortran/fortplot)
- [Issue Tracker](https://github.com/lazy-fortran/fortplot/issues)
