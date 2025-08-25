title: fortplot Documentation
---

# fortplot Documentation

Modern Fortran plotting library inspired by matplotlib. No external dependencies, multiple output formats, and Python compatibility.

## Getting Started

### [Quick Start Guide](../README.md#usage)
Basic usage patterns for both stateful and object-oriented APIs.

### [Setup Instructions](../README.md#setup)
Installation with fpm, CMake, or pip for Python projects.

## Examples Gallery

### [Fortran Examples](../example/fortran/README.md)
Complete working examples with source code and generated plots.

**Featured Examples:**
- [Basic Plots](../example/fortran/basic_plots/README.md) - Line plots with both APIs
- [3D Plotting](../example/fortran/3d_plotting/README.md) - Surface and 3D line plots
- [Contours](../example/fortran/contour_demo/README.md) - Contour and filled contour plots
- [Animations](../example/fortran/animation/README.md) - Animated plots with FFmpeg
- [Unicode Support](../example/fortran/unicode_demo/README.md) - Greek letters and LaTeX math

**All Examples:**
- [Animation](../example/fortran/animation/README.md) - Creating MP4 animations
- [Annotations](../example/fortran/annotation_demo/README.md) - Text and arrow annotations
- [ASCII Heatmaps](../example/fortran/ascii_heatmap/README.md) - Terminal visualization
- [Basic Plots](../example/fortran/basic_plots/README.md) - Fundamental plotting
- [Colored Contours](../example/fortran/colored_contours/README.md) - Filled contour plots
- [Contour Demo](../example/fortran/contour_demo/README.md) - Contour line plots
- [Disconnected Lines](../example/fortran/disconnected_lines/README.md) - Multi-segment plots
- [Format Strings](../example/fortran/format_string_demo/README.md) - Matplotlib-style strings
- [Legend Demo](../example/fortran/legend_demo/README.md) - Legend positioning
- [Line Styles](../example/fortran/line_styles/README.md) - Line and marker styles
- [Marker Demo](../example/fortran/marker_demo/README.md) - Scatter plot markers
- [Pcolormesh](../example/fortran/pcolormesh_demo/README.md) - 2D color mesh plots
- [Scale Examples](../example/fortran/scale_examples/README.md) - Log and symlog scales
- [Show Viewer](../example/fortran/show_viewer_demo/README.md) - Interactive display
- [Smart Show](../example/fortran/smart_show_demo/README.md) - Intelligent display
- [Stateful Streamplot](../example/fortran/stateful_streamplot/README.md) - Vector field evolution
- [Streamplot](../example/fortran/streamplot_demo/README.md) - Vector field visualization
- [Subplot Demo](../example/fortran/subplot_demo/README.md) - Multiple plot grids
- [Unicode Demo](../example/fortran/unicode_demo/README.md) - Mathematical symbols

## Technical Guides

### Development
- [Testing Guide](testing_guide.md) - Testing procedures and warning control
- [Module Architecture](module_architecture.md) - Code organization and refactoring
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

### [Core Interface](../src/fortplot.f90)
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
