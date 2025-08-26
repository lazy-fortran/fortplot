module fortplot
    !! Top-level public interface for fortplot - Modern Fortran plotting library
    !!
    !! This module serves as a thin facade that re-exports the complete fortplot API.
    !! It delegates all functionality to specialized modules while maintaining full
    !! backward compatibility with existing code.
    !!
    !! = Architecture =
    !! This module is organized as a facade pattern, delegating to:
    !! - fortplot_matplotlib: Matplotlib-compatible pyplot-style functions
    !! - fortplot_global: Global figure instance management
    !! - fortplot_figure_core: Core figure type and constants
    !! - fortplot_animation: Animation framework
    !! - fortplot_validation: Testing utilities
    !! - fortplot_colors: Color handling
    !! - fortplot_contour_regions: Contour extraction
    !! - fortplot_logging: Logging functionality
    !!
    !! = Key Types =
    !! - figure_t: Main plotting canvas for creating and managing plots
    !! - animation_t: Animation framework for dynamic visualizations  
    !! - color_t: Advanced color handling with matplotlib syntax support
    !! - validation_result_t: Testing utilities for plot output validation
    !!
    !! = Core Plot Types =
    !! - Line plots: plot(), add_plot() - Basic line and scatter plotting
    !! - Contour plots: contour(), contour_filled() - 2D field visualization
    !! - Heat maps: pcolormesh() - Pseudocolor mesh plots
    !! - Vector fields: streamplot() - Flow visualization with streamlines
    !! - Statistical: hist(), boxplot(), errorbar() - Data distribution plots
    !! - 3D plots: add_3d_plot(), add_surface() - Three-dimensional visualization
    !!
    !! = Output Backends =
    !! - PNG: High-quality raster graphics for publications
    !! - PDF: Vector graphics for scalable documents
    !! - ASCII: Terminal-based plots for remote/headless environments
    !!
    !! = Quick Start Examples =
    !!   ! Simple line plot
    !!   use fortplot
    !!   call plot(x_data, y_data, label="measurements")
    !!   call show()
    !!
    !!   ! Advanced figure with multiple plots
    !!   type(figure_t) :: fig
    !!   call fig%initialize(800, 600)
    !!   call figure_add_plot(fig, x, y, label="data", linestyle='b-o')
    !!   call figure_add_contour(fig, x_grid, y_grid, z_field)
    !!   call figure_legend(fig, )
    !!   call figure_savefig(fig, 'results.pdf')
    !!
    !! Author: fortplot contributors

    use iso_fortran_env, only: wp => real64
    
    ! Core types and constants
    use fortplot_figure_core, only: figure_t
    use fortplot_constants, only: EPSILON_COMPARE, EPSILON_GEOMETRY, &
                                  XLABEL_VERTICAL_OFFSET, YLABEL_HORIZONTAL_OFFSET, &
                                  TICK_MARK_LENGTH, TITLE_VERTICAL_OFFSET
    
    ! Animation functionality
    use fortplot_animation, only: animation_t, FuncAnimation
    
    ! Logging functionality
    use fortplot_logging, only: set_log_level, &
                                LOG_LEVEL_SILENT, LOG_LEVEL_ERROR, &
                                LOG_LEVEL_WARNING, LOG_LEVEL_INFO, LOG_LEVEL_DEBUG
    
    ! Validation functionality
    use fortplot_validation, only: validation_result_t, validate_file_exists, validate_file_size, &
                                   validate_png_format, validate_pdf_format, validate_ascii_format, &
                                   compare_with_baseline
    
    ! Color functionality
    use fortplot_colors, only: color_t, parse_color, parse_color_rgba, is_valid_color, &
                               validate_color_for_backend, clear_color_cache
    
    ! Contour extraction functionality
    use fortplot_contour_regions, only: contour_region_t, contour_polygon_t, extract_contour_regions
    
    ! Annotation functionality with coordinate constants
    use fortplot_annotations, only: COORD_DATA, COORD_FIGURE, COORD_AXIS
    
    ! Matplotlib-compatible API (all pyplot-style functions)
    use fortplot_matplotlib, only: plot, contour, contour_filled, pcolormesh, streamplot, &
                                   hist, histogram, scatter, errorbar, boxplot, &
                                   bar, barh, text, annotate, &
                                   xlabel, ylabel, title, legend, &
                                   savefig, figure, subplot, &
                                   add_plot, add_contour, add_contour_filled, add_pcolormesh, add_errorbar, &
                                   add_3d_plot, add_surface, add_scatter, &
                                   set_xscale, set_yscale, xlim, ylim, &
                                   set_line_width, set_ydata, &
                                   show, show_viewer, &
                                   ensure_global_figure_initialized, get_global_figure

    implicit none
    private

    ! =========================================================================
    ! PUBLIC INTERFACE RE-EXPORTS
    ! =========================================================================
    
    ! Core types and constants
    public :: figure_t, wp
    
    ! Numerical constants
    public :: EPSILON_COMPARE, EPSILON_GEOMETRY, &
              XLABEL_VERTICAL_OFFSET, YLABEL_HORIZONTAL_OFFSET, TICK_MARK_LENGTH, &
              TITLE_VERTICAL_OFFSET
    
    ! Coordinate system constants for annotations
    public :: COORD_DATA, COORD_FIGURE, COORD_AXIS
    
    ! Matplotlib-compatible plotting functions
    public :: plot, contour, contour_filled, pcolormesh, streamplot
    public :: hist, histogram, scatter, errorbar, boxplot
    public :: bar, barh
    public :: text, annotate
    
    ! Figure management and configuration
    public :: figure, subplot
    public :: xlabel, ylabel, title, legend
    public :: xlim, ylim, set_xscale, set_yscale
    public :: set_line_width, set_ydata
    public :: savefig
    
    ! Extended plotting functions
    public :: add_plot, add_contour, add_contour_filled, add_pcolormesh
    public :: add_errorbar, add_3d_plot, add_surface, add_scatter
    
    ! Display functions
    public :: show, show_viewer
    
    ! Global figure access
    public :: ensure_global_figure_initialized, get_global_figure
    
    ! Animation interface
    public :: animation_t, FuncAnimation
    
    ! Logging interface
    public :: set_log_level
    public :: LOG_LEVEL_SILENT, LOG_LEVEL_ERROR
    public :: LOG_LEVEL_WARNING, LOG_LEVEL_INFO, LOG_LEVEL_DEBUG
    
    ! Validation interface
    public :: validation_result_t
    public :: validate_file_exists, validate_file_size
    public :: validate_png_format, validate_pdf_format, validate_ascii_format
    public :: compare_with_baseline
    
    ! Color interface
    public :: color_t
    public :: parse_color, parse_color_rgba, is_valid_color
    public :: validate_color_for_backend, clear_color_cache
    
    ! Contour region extraction interface
    public :: contour_region_t, contour_polygon_t, extract_contour_regions

    ! =========================================================================
    ! STYLE CONSTANTS (matplotlib-compatible)
    ! =========================================================================
    
    ! Line style constants
    
    !! Solid line style for continuous data visualization
    !! Usage: call plot(x, y, linestyle=LINESTYLE_SOLID)
    !! Visual: ————————————————————
    character(len=*), parameter, public :: LINESTYLE_SOLID = '-'
    
    !! Dashed line style for highlighting trends or secondary data
    !! Usage: call plot(x, y, linestyle=LINESTYLE_DASHED)
    !! Visual: ---- ---- ---- ----
    character(len=*), parameter, public :: LINESTYLE_DASHED = '--'
    
    !! Dotted line style for reference lines or uncertainty bounds
    !! Usage: call plot(x, y, linestyle=LINESTYLE_DOTTED)
    !! Visual: ••••••••••••••••••••
    character(len=*), parameter, public :: LINESTYLE_DOTTED = ':'
    
    !! Dash-dot line style for mixed emphasis visualization
    !! Usage: call plot(x, y, linestyle=LINESTYLE_DASHDOT)
    !! Visual: ——•——•——•——•
    character(len=*), parameter, public :: LINESTYLE_DASHDOT = '-.'
    
    !! No line style - displays only markers without connecting lines
    !! Usage: call plot(x, y, linestyle=LINESTYLE_NONE)
    !! Visual: • • • • (markers only)
    character(len=*), parameter, public :: LINESTYLE_NONE = 'None'

    ! Marker style constants
    
    !! Circular markers for standard data point visualization
    !! Usage: call scatter(x, y, marker=MARKER_CIRCLE)
    !! Visual: ● (filled circle)
    character(len=*), parameter, public :: MARKER_CIRCLE = 'o'
    
    !! Cross-shaped markers for outliers or special data points
    !! Usage: call scatter(x, y, marker=MARKER_CROSS)  
    !! Visual: ✕ (diagonal cross)
    character(len=*), parameter, public :: MARKER_CROSS = 'x'
    
    !! Square markers for categorical or discrete data visualization
    !! Usage: call scatter(x, y, marker=MARKER_SQUARE)
    !! Visual: ■ (filled square)
    character(len=*), parameter, public :: MARKER_SQUARE = 's'
    
    !! Diamond-shaped markers for highlighting key data points
    !! Usage: call scatter(x, y, marker=MARKER_DIAMOND)
    !! Visual: ♦ (filled diamond)
    character(len=*), parameter, public :: MARKER_DIAMOND = 'D'
    
    !! Plus-sign markers for positive values or additive data
    !! Usage: call scatter(x, y, marker=MARKER_PLUS)
    !! Visual: ＋ (orthogonal plus)
    character(len=*), parameter, public :: MARKER_PLUS = '+'
    
    !! Star-shaped markers for exceptional or peak values
    !! Usage: call scatter(x, y, marker=MARKER_STAR)
    !! Visual: ★ (filled star)
    character(len=*), parameter, public :: MARKER_STAR = '*'
    
    !! Upward triangle markers for increasing trends or maxima
    !! Usage: call scatter(x, y, marker=MARKER_TRIANGLE_UP)
    !! Visual: ▲ (filled upward triangle)
    character(len=*), parameter, public :: MARKER_TRIANGLE_UP = '^'
    
    !! Downward triangle markers for decreasing trends or minima
    !! Usage: call scatter(x, y, marker=MARKER_TRIANGLE_DOWN)
    !! Visual: ▼ (filled downward triangle)
    character(len=*), parameter, public :: MARKER_TRIANGLE_DOWN = 'v'
    
    !! Pentagon-shaped markers for specialized scientific data
    !! Usage: call scatter(x, y, marker=MARKER_PENTAGON)
    !! Visual: ⬟ (filled pentagon)
    character(len=*), parameter, public :: MARKER_PENTAGON = 'p'
    
    !! Hexagon-shaped markers for crystallographic or geometric data
    !! Usage: call scatter(x, y, marker=MARKER_HEXAGON)
    !! Visual: ⬢ (filled hexagon)
    character(len=*), parameter, public :: MARKER_HEXAGON = 'h'

    ! This module is now a pure facade - no implementation code needed
    ! All functionality is delegated to the specialized modules above

end module fortplot