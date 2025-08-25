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

**All Examples:**
- [Animation](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/animation) - Creating MP4 animations
- [Annotations](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/annotation_demo) - Text and arrow annotations
- [ASCII Heatmaps](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/ascii_heatmap) - Terminal visualization
- [Basic Plots](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/basic_plots) - Fundamental plotting
- [Boxplot Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/boxplot_demo) - Box and whisker plots
- [Colored Contours](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/colored_contours) - Filled contour plots
- [Contour Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/contour_demo) - Contour line plots
- [Disconnected Lines](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/disconnected_lines) - Multi-segment plots
- [Errorbar Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/errorbar_demo) - Error bars and uncertainty
- [Format Strings](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/format_string_demo) - Matplotlib-style strings
- [Grid Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/grid_demo) - Grid line control
- [Legend Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/legend_demo) - Legend positioning
- [Line Styles](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/line_styles) - Line and marker styles
- [Marker Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/marker_demo) - Scatter plot markers
- [Pcolormesh](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/pcolormesh_demo) - 2D color mesh plots
- [Scale Examples](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/scale_examples) - Log and symlog scales
- [Scatter Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/scatter_demo) - Scatter plot basics
- [Show Viewer](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/show_viewer_demo) - Interactive display
- [Smart Show](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/smart_show_demo) - Intelligent display
- [Stateful Streamplot](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/stateful_streamplot) - Vector field evolution
- [Streamplot](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/streamplot_demo) - Vector field visualization
- [Subplot Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/subplot_demo) - Multiple plot grids
- [Unicode Demo](https://github.com/lazy-fortran/fortplot/tree/main/example/fortran/unicode_demo) - Mathematical symbols

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
