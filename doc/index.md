title: fortplot Documentation
---

# fortplot Documentation

Modern Fortran plotting library inspired by matplotlib. No external dependencies, multiple output formats, and Python compatibility.

## Getting Started

### [Quick Start Guide](https://github.com/lazy-fortran/fortplot#usage)
Basic usage patterns for both stateful and object-oriented APIs.

### [Setup Instructions](https://github.com/lazy-fortran/fortplot#setup)
Installation with fpm, CMake, or pip for Python projects.

## Examples Gallery

### [Fortran Examples](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran)
Complete working examples with source code and generated plots.

**Featured Examples:**
- [Basic Plots](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/basic_plots) - Line plots with both APIs
- [3D Plotting](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/3d_plotting) - Surface and 3D line plots
- [Contours](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/contour_demo) - Contour and filled contour plots
- [Animations](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/animation) - Animated plots with FFmpeg
- [Unicode Support](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/unicode_demo) - Greek letters and LaTeX math

**All Examples (auto-generated):**
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

## Technical Guides

### Development
- [Testing Guide](testing_guide.md) - Testing procedures and warning control
- [Module Architecture](module_architecture.md) - Code organization and refactoring
- [OO vs Stateful Compatibility](oo_stateful_compatibility.md) - Migration and argument parity
- [Security](security.md) - Security features and compliance

### Platform-Specific
- [Windows CI Performance](windows_ci_performance.md) - Windows optimization
- [Windows FFmpeg Setup](windows_ffmpeg_setup.md) - Animation dependencies
- [Warning System](warning_system.md) - Environment variable control
- [Unicode Support](unicode_support.md) - Text rendering and symbols

### Plot Types
- [Surface Plot Guide](surface_plot_guide.md) - 3D surface plotting
- [Scatter Plot Guide](scatter_plot_guide.md) - Basic scatter plots
- [Scatter Tutorial](scatter_tutorial.md) - Interactive scatter tutorial
- [Scatter Advanced](scatter_advanced.md) - Advanced scatter features
- [MPEG Validation](mpeg_validation.md) - Animation format validation

## API Reference

### [Core Interface](https://github.com/lazy-fortran/fortplot/blob/main/src/fortplot.f90)
Main module with stateful plotting functions.

### [Figure Management](design/figure_management.md)
Object-oriented figure handling and subplot systems.

### [Backends](design/backends.md)
PNG, PDF, and ASCII output backends.

### [Plot Types](design/basic_plots.md)
Line plots, scatter, contours, surfaces, and animations.

## Design Documentation

- [Axes Layout](design/axes_layout.md) - Coordinate systems and transformations
- [Basic Plots](design/basic_plots.md) - Line plot implementation
- [Backends](design/backends.md) - Output format architecture
- [Contour](design/contour.md) - Contour plot algorithms
- [Figure Management](design/figure_management.md) - Figure and subplot system
- [Streamplot](design/streamplot.md) - Vector field visualization
- [Styling](design/styling.md) - Colors, markers, and formatting

## External Resources

- [GitHub Repository](https://github.com/lazy-fortran/fortplot)
- [API Documentation](https://lazy-fortran.github.io/fortplot/) - FORD-generated docs
- [Issue Tracker](https://github.com/lazy-fortran/fortplot/issues)
