title: fortplot Documentation
---

# fortplot Documentation

Modern Fortran plotting for scientific workflows. Start here for the essential
resources, then dive into focused guides as needed.

## Quick Links

- [Quick Start](https://github.com/lazy-fortran/fortplot#usage) – install and
  produce your first plot in minutes
- [Setup Matrix](https://github.com/lazy-fortran/fortplot#setup) – fpm, CMake,
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

- [3D Plotting](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/3d_plotting) - Documentation pending; browse the source tree.
- [Animation](./examples/animation.html) - This example demonstrates creating animated plots and saving to video files.
- [Annotation Demo](./examples/annotation_demo.html) - See documentation page for details.
- [Ascii Heatmap](./examples/ascii_heatmap.html) - This example demonstrates terminal-based heatmap visualization using ASCII characters.
- [Basic Plots](./examples/basic_plots.html) - This example demonstrates the fundamental plotting capabilities of fortplotlib using both the simple functional API...
- [Boxplot Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/boxplot_demo) - Documentation pending; browse the source tree.
- [Colored Contours](./examples/colored_contours.html) - This example shows filled (colored) contour plots with customizable colormaps for visualizing 2D scalar fields.
- [Contour Demo](./examples/contour_demo.html) - This example demonstrates line contour plotting (level lines) with custom levels and a mixed plot combining contours...
- [Contour Filled Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/contour_filled_demo) - Documentation pending; browse the source tree.
- [Disconnected Lines](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/disconnected_lines) - Documentation pending; browse the source tree.
- [Errorbar Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/errorbar_demo) - Documentation pending; browse the source tree.
- [Fill Between Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/fill_between_demo) - Documentation pending; browse the source tree.
- [Format String Demo](./examples/format_string_demo.html) - This example demonstrates matplotlib-style format strings for quick and intuitive plot styling.
- [Grid Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/grid_demo) - Documentation pending; browse the source tree.
- [Legend Demo](./examples/legend_demo.html) - This example demonstrates legend placement and customization options.
- [Line Styles](./examples/line_styles.html) - This example demonstrates all available line styles in fortplotlib, showing how to customize the appearance of...
- [Marker Demo](./examples/marker_demo.html) - This example showcases various marker types and scatter plot capabilities in fortplotlib.
- [Mathtext Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/mathtext_demo) - Documentation pending; browse the source tree.
- [Pcolormesh Demo](./examples/pcolormesh_demo.html) - This example demonstrates pseudocolor plots for efficient 2D data visualization.
- [Pcolormesh Negative](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/pcolormesh_negative) - Documentation pending; browse the source tree.
- [Scale Examples](./examples/scale_examples.html) - This example demonstrates different axis scaling options including logarithmic and symmetric logarithmic (symlog)...
- [Scatter Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/scatter_demo) - Documentation pending; browse the source tree.
- [Show Viewer Demo](./examples/show_viewer_demo.html) - This example demonstrates using the built-in viewer for interactive display.
- [Smart Show Demo](./examples/smart_show_demo.html) - This example demonstrates intelligent display mode selection based on environment.
- [Streamplot Demo](./examples/streamplot_demo.html) - This example shows vector field visualization using streamlines.
- [Subplot Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/subplot_demo) - Documentation pending; browse the source tree.
- [Unicode Demo](./examples/unicode_demo.html) - This example demonstrates mathematical symbols and Unicode support in plots.

<!-- AUTO_EXAMPLES_END -->

## User Guides

- [Scatter Plot Guide](scatter_plot_guide.md) – sizes, colours, and styling in a
  single reference
- [Surface Plot Guide](surface_plot_guide.md) – 3D surfaces and shading
- [Testing Guide](testing_guide.md) – local and CI workflows
- [Unicode Support](unicode_support.md) – math text and symbol rendering
- [Warning System](warning_system.md) – suppressing or surfacing diagnostics

## Platform Notes

- [Windows FFmpeg Setup](windows_ffmpeg_setup.md) – enable animation exports on
  Windows runners
- [Windows CI Performance](windows_ci_performance.md) – keep pipelines under
  target durations
- [Security](security.md) – hardening measures for sandboxed environments
- [User Compilation Guide](user_compilation_guide.md) – integrate fortplot into
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
