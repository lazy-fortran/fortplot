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

### Full Listing (auto-generated)
<!-- AUTO_EXAMPLES_START -->

- [3D Plotting](./examples/3d_plotting.html) - This comprehensive example demonstrates all 3D plotting capabilities in fortplot, consolidating features from...
- [Animation](./examples/animation.html) - Documentation for this example is auto-generated.
- [Annotation Demo](./examples/annotation_demo.html) - Documentation for this example is auto-generated.
- [Ascii Heatmap](./examples/ascii_heatmap.html) - Documentation for this example is auto-generated.
- [Bar Chart Demo](./examples/bar_chart_demo.html) - Grouped bar charts for categorical comparisons using both the pyplot-style
- [Basic Plots](./examples/basic_plots.html) - Documentation for this example is auto-generated.
- [Boxplot Demo](./examples/boxplot_demo.html) - Demonstrates box-and-whisker plots for statistical data visualization.
- [Colored Contours](./examples/colored_contours.html) - Documentation for this example is auto-generated.
- [Contour Demo](./examples/contour_demo.html) - Documentation for this example is auto-generated.
- [Contour Filled Demo](./examples/contour_filled_demo.html) - Documentation for this example is auto-generated.
- [Disconnected Lines](./examples/disconnected_lines.html) - This example demonstrates how to create plots with disconnected line segments by using NaN (Not-a-Number) values as...
- [Errorbar Demo](./examples/errorbar_demo.html) - Demonstrates error bar plotting with both symmetric and asymmetric errors for scientific data visualization.
- [Fill Between Demo](./examples/fill_between_demo.html) - Demonstrates how to create filled regions using both the stateful and object
- [Format String Demo](./examples/format_string_demo.html) - Documentation for this example is auto-generated.
- [Grid Demo](./examples/grid_demo.html) - Demonstrates grid functionality for enhanced plot readability.
- [Legend Demo](./examples/legend_demo.html) - Documentation for this example is auto-generated.
- [Line Styles](./examples/line_styles.html) - Documentation for this example is auto-generated.
- [Marker Demo](./examples/marker_demo.html) - Documentation for this example is auto-generated.
- [Mathtext Demo](./examples/mathtext_demo.html) - Documentation for this example is auto-generated.
- [Pcolormesh Demo](./examples/pcolormesh_demo.html) - Documentation for this example is auto-generated.
- [Pcolormesh Negative](./examples/pcolormesh_negative.html) - Documentation for this example is auto-generated.
- [Pie Chart Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/pie_chart_demo) - Documentation pending; browse the source tree.
- [Polar Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/polar_demo) - Documentation pending; browse the source tree.
- [Scale Examples](./examples/scale_examples.html) - Documentation for this example is auto-generated.
- [Scatter Demo](./examples/scatter_demo.html) - Demonstrates enhanced scatter plotting with color mapping, variable marker sizes, and bubble charts.
- [Show Viewer Demo](./examples/show_viewer_demo.html) - Documentation for this example is auto-generated.
- [Smart Show Demo](./examples/smart_show_demo.html) - Documentation for this example is auto-generated.
- [Streamplot Demo](./examples/streamplot_demo.html) - Documentation for this example is auto-generated.
- [Subplot Demo](./examples/subplot_demo.html) - Demonstration of subplot functionality using the stateful API.
- [Twin Axes Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/twin_axes_demo) - Documentation pending; browse the source tree.
- [Unicode Demo](./examples/unicode_demo.html) - Documentation for this example is auto-generated.

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
