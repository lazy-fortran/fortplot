module fortplot
    !! Top-level public interface for fortplot - Modern Fortran plotting library
    !!
    !! This module provides a comprehensive, matplotlib-compatible API for creating 
    !! professional scientific visualizations with support for multiple plot types
    !! and output formats (PNG, PDF, ASCII).
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
    !! = Related Modules =
    !! - fortplot_figure: Core figure implementation and backend management
    !! - fortplot_colors: Advanced color parsing and matplotlib compatibility
    !! - fortplot_animation: Animation framework for dynamic plots
    !! - fortplot_validation: Automated testing utilities for plot output
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
    !!   call fig%add_plot(x, y, label="data", linestyle='b-o')
    !!   call fig%add_contour(x_grid, y_grid, z_field)
    !!   call fig%legend()
    !!   call fig%savefig('results.pdf')
    !!
    !! Author: fortplot contributors

    use iso_fortran_env, only: wp => real64
    use fortplot_figure, only: figure_t
    use fortplot_format_parser, only: parse_format_string, contains_format_chars
    use fortplot_animation, only: animation_t, FuncAnimation
    use fortplot_logging, only: set_log_level, log_error, log_warning, log_info, &
                                LOG_LEVEL_SILENT, LOG_LEVEL_ERROR, &
                                LOG_LEVEL_WARNING, LOG_LEVEL_INFO, LOG_LEVEL_DEBUG
    use fortplot_validation, only: validation_result_t, validate_file_exists, validate_file_size, &
                                   validate_png_format, validate_pdf_format, validate_ascii_format, &
                                   compare_with_baseline
    use fortplot_colors, only: color_t, parse_color, parse_color_rgba, is_valid_color, &
                               validate_color_for_backend, clear_color_cache
    use fortplot_figure_core, only: COORD_DATA, COORD_FIGURE, COORD_AXIS

    implicit none

    private

    ! Re-export public interface
    public :: figure_t, wp
    public :: plot, contour, contour_filled, pcolormesh, streamplot, errorbar, boxplot, show, show_viewer
    public :: hist, histogram, scatter
    public :: xlabel, ylabel, title, legend
    public :: savefig, figure, subplot
    public :: add_plot, add_contour, add_contour_filled, add_pcolormesh, add_errorbar
    public :: add_3d_plot, add_surface, add_scatter
    public :: set_xscale, set_yscale, xlim, ylim
    public :: set_line_width, set_ydata
    public :: bar, barh
    public :: text, annotate
    public :: COORD_DATA, COORD_FIGURE, COORD_AXIS
    public :: get_global_figure
    
    ! Animation interface
    public :: animation_t, FuncAnimation
    
    ! Logging interface
    public :: set_log_level, LOG_LEVEL_SILENT, LOG_LEVEL_ERROR, &
              LOG_LEVEL_WARNING, LOG_LEVEL_INFO, LOG_LEVEL_DEBUG
    
    ! Validation interface for functional output testing
    public :: validation_result_t, validate_file_exists, validate_file_size, &
              validate_png_format, validate_pdf_format, validate_ascii_format, &
              compare_with_baseline
              
    ! Color interface for matplotlib-compatible color syntax
    public :: color_t, parse_color, parse_color_rgba, is_valid_color, &
              validate_color_for_backend, clear_color_cache

    ! Line style constants (matplotlib-compatible)
    
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

    ! Marker style constants (matplotlib-compatible)
    
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


    !! Overloaded show interface for flexible plot display
    !! 
    !! Provides two distinct approaches for displaying plots:
    !! 
    !! = show_data =
    !! Direct data visualization - creates and displays a plot in one call
    !! Usage: call show(x_data, y_data, label="data", title_text="Results")
    !! Purpose: Quick visualization without explicit figure management
    !! 
    !! = show_figure = 
    !! Display current global figure - like matplotlib.pyplot.show()
    !! Usage: call show() ! Displays current figure contents
    !! Purpose: Display plots added via plot(), contour(), etc.
    !!
    !! Auto-selection: Interface automatically chooses the appropriate procedure
    !! based on the provided arguments - data arrays trigger show_data,
    !! no arguments or options-only trigger show_figure
    !!
    !! Cross-references: Used with plot(), add_plot(), savefig(), show_viewer()
    interface show
        module procedure show_data, show_figure
    end interface show

    ! Note: plot and add_plot now handle both separate args and format strings in single routines

    ! Global figure for simple API
    type(figure_t), save, target :: fig

contains

    subroutine plot(x, y, label, linestyle)
        !! Add a line plot to the global figure (pyplot-fortran compatible)
        !!
        !! Creates line plots with optional markers for data visualization.
        !! This is the primary interface for 2D line plotting.
        !!
        !! Arguments:
        !!   x, y: Data arrays for the line plot
        !!   label: Optional label for the plot
        !!   linestyle: Line style and markers ('b-o', 'r--', 'g:', 'ko', etc.)
        !!              Supports both pyplot-fortran and matplotlib format strings
        !!
        !! Cross-references: See add_plot() for explicit add operation, scatter() for point-only plots,
        !!                  show() for display, savefig() for output, errorbar() for uncertainty plots
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle

        call ensure_global_figure_initialized()
        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine plot

    subroutine contour(x, y, z, levels, label)
        !! Add a contour plot to the global figure (pyplot-style)
        !!
        !! Creates line contour plots for 2D scalar field visualization.
        !! Shows level curves (isolines) of constant values.
        !!
        !! Arguments:
        !!   x, y: Grid coordinate arrays
        !!   z: 2D data array for contouring
        !!   levels: Optional array of contour levels
        !!   label: Optional label for the plot
        !!
        !! Cross-references: See add_contour() for explicit add operation, contour_filled() for filled contours,
        !!                  pcolormesh() for grid visualization, streamplot() for vector fields
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label

        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine contour

    subroutine contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        !! Add a filled contour plot with color levels to the global figure
        !!
        !! Creates filled contour plots for 2D scalar field visualization with smooth
        !! color transitions between levels. Compatible with matplotlib contourf function.
        !!
        !! = Parameters =
        !! x : real(8), dimension(:), intent(in)
        !!     1D coordinate array for x-axis grid points (size: nx)
        !!     Must be monotonically increasing
        !!     
        !! y : real(8), dimension(:), intent(in)
        !!     1D coordinate array for y-axis grid points (size: ny) 
        !!     Must be monotonically increasing
        !!     
        !! z : real(8), dimension(:,:), intent(in)
        !!     2D scalar field data array (shape: ny × nx)
        !!     Contains the field values to be contoured
        !!     
        !! levels : real(8), dimension(:), intent(in), optional
        !!     Contour level values array (default: auto-generated 10 levels)
        !!     Must be monotonically increasing for proper rendering
        !!     
        !! colormap : character(len=*), intent(in), optional
        !!     Colormap name for color mapping (default: 'crest')
        !!     Valid options: 'viridis', 'plasma', 'inferno', 'magma', 'coolwarm',
        !!                   'RdYlBu', 'seismic', 'jet', 'hsv', 'rainbow',
        !!                   'crest', 'rocket', 'mako', 'flare', 'icefire'
        !!                   
        !! show_colorbar : logical, intent(in), optional
        !!     Display colorbar legend (default: .true.)
        !!     Set to .false. for overlay plots or custom legends
        !!     
        !! label : character(len=*), intent(in), optional
        !!     Plot label for legends (default: none)
        !!     Used when combining with other plot types
        !!
        !! = Usage Examples =
        !!   ! Basic filled contour with default settings
        !!   call contour_filled(x_grid, y_grid, temperature_field)
        !!   
        !!   ! Custom levels and colormap
        !!   real(8) :: custom_levels(5) = [0.0, 0.25, 0.5, 0.75, 1.0]
        !!   call contour_filled(x, y, data, levels=custom_levels, colormap='coolwarm')
        !!
        !! = Cross-references =
        !! See also: contour() for line contours, pcolormesh() for pixelated visualization
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar

        call fig%add_contour_filled(x, y, z, levels=levels, colormap=colormap, &
                                   show_colorbar=show_colorbar, label=label)
    end subroutine contour_filled

    subroutine pcolormesh(x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        !! Add a pcolormesh (pseudocolor mesh) plot to the global figure
        !!
        !! Creates a pseudocolor plot with rectangular grid cells, where each cell
        !! is colored according to a scalar value. Compatible with matplotlib pcolormesh.
        !!
        !! = Parameters =
        !! x : real(8), dimension(:), intent(in)
        !!     1D grid coordinate array for x-axis (size: nx or nx+1)
        !!     For regular grids: can be same size as c columns (nx) or cell edges (nx+1)
        !!     Must be monotonically increasing
        !!     
        !! y : real(8), dimension(:), intent(in)
        !!     1D grid coordinate array for y-axis (size: ny or ny+1)  
        !!     For regular grids: can be same size as c rows (ny) or cell edges (ny+1)
        !!     Must be monotonically increasing
        !!     
        !! c : real(8), dimension(:,:), intent(in)
        !!     2D color data array (shape: ny × nx)
        !!     Each element represents the scalar value for one grid cell
        !!     Values are mapped to colors using the specified colormap
        !!     
        !! colormap : character(len=*), intent(in), optional
        !!     Colormap name for color mapping (default: 'viridis')
        !!     Valid options: 'viridis', 'plasma', 'inferno', 'magma', 'coolwarm',
        !!                   'RdYlBu', 'seismic', 'jet', 'hot', 'bone', 'gray'
        !!                   
        !! vmin : real(8), intent(in), optional
        !!     Minimum value for color scale (default: min(c))
        !!     Values below vmin are clamped to the colormap minimum
        !!     
        !! vmax : real(8), intent(in), optional  
        !!     Maximum value for color scale (default: max(c))
        !!     Values above vmax are clamped to the colormap maximum
        !!     
        !! edgecolors : character(len=*), intent(in), optional
        !!     Cell edge color specification (default: 'none')
        !!     Valid options: 'none', 'black', 'white', 'gray', or any valid color
        !!     
        !! linewidths : real(8), intent(in), optional
        !!     Width of cell edge lines (default: 0.0, ignored if edgecolors='none')
        !!     Typical range: 0.1 to 2.0 for visible edges
        !!
        !! = Grid Requirements =
        !! - x and y must define a rectangular grid structure
        !! - For cell-centered data: size(x)=nx, size(y)=ny, size(c)=[ny,nx]
        !! - For edge-defined data: size(x)=nx+1, size(y)=ny+1, size(c)=[ny,nx]
        !! - Grid spacing can be non-uniform (stretched grids supported)
        !!
        !! = Usage Examples =
        !!   ! Simple heatmap with default settings
        !!   call pcolormesh(x_coords, y_coords, temperature_data)
        !!   
        !!   ! Custom color scale and edges
        !!   call pcolormesh(x, y, data, colormap='coolwarm', vmin=-1.0, vmax=1.0, &
        !!                  edgecolors='black', linewidths=0.5)
        !!
        !! = Cross-references =
        !! See also: contour_filled() for smooth contours, scatter() for point data
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: c
        character(len=*), intent(in), optional :: colormap
        real(8), intent(in), optional :: vmin, vmax
        character(len=*), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: linewidths

        call fig%add_pcolormesh(x, y, c, colormap=colormap, vmin=vmin, vmax=vmax, &
                               edgecolors=edgecolors, linewidths=linewidths)
    end subroutine pcolormesh

    subroutine streamplot(x, y, u, v, density, arrowsize, arrowstyle)
        !! Add a streamplot (vector field visualization) to the global figure with arrow support
        !!
        !! Creates streamlines that follow the direction of a 2D vector field,
        !! providing intuitive visualization of flow patterns, magnetic fields,
        !! or any other vector data. Compatible with matplotlib streamplot function.
        !!
        !! = Parameters =
        !! x : real(8), dimension(:), intent(in)
        !!     1D coordinate array for x-axis grid points (size: nx)
        !!     Must be monotonically increasing for proper interpolation
        !!     
        !! y : real(8), dimension(:), intent(in) 
        !!     1D coordinate array for y-axis grid points (size: ny)
        !!     Must be monotonically increasing for proper interpolation
        !!     
        !! u : real(8), dimension(:,:), intent(in)
        !!     2D velocity field x-component array (shape: ny × nx)
        !!     Represents horizontal velocity/vector component at each grid point
        !!     
        !! v : real(8), dimension(:,:), intent(in)
        !!     2D velocity field y-component array (shape: ny × nx) 
        !!     Represents vertical velocity/vector component at each grid point
        !!     
        !! density : real(8), intent(in), optional
        !!     Streamline density control parameter (default: 1.0_real64)
        !!     Valid range: 0.1 to 5.0 (higher = more streamlines)
        !!     Controls spacing between streamline seeds
        !!
        !! arrowsize : real(8), intent(in), optional
        !!     Arrow size scaling factor (default: 1.0_real64)
        !!     Controls the size of arrow heads along streamlines
        !!     Set to 0.0 to disable arrow rendering
        !!
        !! arrowstyle : character(len=*), intent(in), optional
        !!     Arrow style specification (default: '->')
        !!     Valid options: '->', '-', '<-', '<->'
        !!     Controls arrow head appearance
        !!
        !! = Usage Examples =
        !!   ! Basic streamplot with arrows
        !!   call streamplot(x_grid, y_grid, u_field, v_field, arrowsize=1.5_wp)
        !!   
        !!   ! Streamplot with custom arrow style
        !!   call streamplot(x, y, u, v, density=2.0_wp, arrowstyle='<->')
        !!   
        !!   ! Streamlines without arrows
        !!   call streamplot(x, y, u, v, arrowsize=0.0_wp)
        !!
        !! = Cross-references =
        !! See also: pcolormesh() for scalar field visualization, add_plot() for trajectory data
        real(wp), dimension(:), intent(in) :: x, y
        real(wp), dimension(:,:), intent(in) :: u, v
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: arrowsize
        character(len=*), intent(in), optional :: arrowstyle

        call fig%streamplot(x, y, u, v, density=density, arrowsize=arrowsize, arrowstyle=arrowstyle)
    end subroutine streamplot

    subroutine bar(x, heights, width, label, color)
        !! Add a vertical bar chart to the global figure (pyplot-style)
        !!
        !! Arguments:
        !!   x: X-axis positions for bars
        !!   heights: Heights of bars
        !!   width: Optional - width of bars (default: 0.8)
        !!   label: Optional - bar chart label for legend
        !!   color: Optional - bar color
        real(8), dimension(:), intent(in) :: x, heights
        real(8), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        real(8), dimension(3), intent(in), optional :: color

        call fig%bar(x, heights, width=width, label=label, color=color)
    end subroutine bar

    subroutine barh(y, widths, height, label, color)
        !! Add a horizontal bar chart to the global figure (pyplot-style)
        !!
        !! Arguments:
        !!   y: Y-axis positions for bars
        !!   widths: Widths of bars
        !!   height: Optional - height of bars (default: 0.8)
        !!   label: Optional - bar chart label for legend
        !!   color: Optional - bar color
        real(8), dimension(:), intent(in) :: y, widths
        real(8), intent(in), optional :: height
        character(len=*), intent(in), optional :: label
        real(8), dimension(3), intent(in), optional :: color

        call fig%barh(y, widths, height=height, label=label, color=color)
    end subroutine barh

    subroutine hist(data, bins, density, label, color)
        !! Add histogram plot to the global figure (pyplot-style)
        !!
        !! Creates a histogram from input data, compatible with matplotlib.pyplot.hist
        !!
        !! Arguments:
        !!   data: Input data array to create histogram from
        !!   bins: Optional - number of bins (integer, default: 10)
        !!   density: Optional - normalize to probability density (default: false)
        !!   label: Optional - histogram label for legend
        !!   color: Optional - histogram color as RGB values [0-1]
        !!
        !! Example:
        !!   ! Simple histogram
        !!   call hist(data_values, bins=20, label='Distribution')
        real(8), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(8), intent(in), optional :: color(3)

        call ensure_global_figure_initialized()
        call fig%hist(data, bins=bins, density=density, label=label, color=color)
    end subroutine hist

    subroutine histogram(data, bins, density, label, color)
        !! Add histogram plot to the global figure (pyplot-style)
        !!
        !! Alias for hist() subroutine - creates a histogram from input data
        !! Compatible with matplotlib.pyplot.histogram
        !!
        !! Arguments:
        !!   data: Input data array to create histogram from
        !!   bins: Optional - number of bins (integer, default: 10)
        !!   density: Optional - normalize to probability density (default: false)
        !!   label: Optional - histogram label for legend
        !!   color: Optional - histogram color as RGB values [0-1]
        !!
        !! Example:
        !!   ! Simple histogram
        !!   call histogram(data_values, bins=20, label='Distribution')
        real(8), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(8), intent(in), optional :: color(3)

        call ensure_global_figure_initialized()
        call fig%hist(data, bins=bins, density=density, label=label, color=color)
    end subroutine histogram

    subroutine boxplot(data, position, width, label, show_outliers, horizontal, color)
        !! Add a box plot to the global figure (matplotlib-style)
        !!
        !! Creates a box plot displaying the distribution of data through
        !! quartiles, median, and outliers. Compatible with matplotlib boxplot function.
        !!
        !! Arguments:
        !!   data: 1D array of data values for statistical analysis
        !!   position: Optional x-axis position for the box (default: 1.0)
        !!   width: Optional width of the box (default: 0.8)
        !!   label: Optional label for the plot
        !!   show_outliers: Optional flag to display outliers (default: true)
        !!   horizontal: Optional flag for horizontal orientation (default: false)
        !!   color: Optional RGB color array [0-1] for box coloring
        !!
        !! Example:
        !!   ! Simple box plot
        !!   call boxplot(measurement_data, label='Measurements')
        real(wp), dimension(:), intent(in) :: data
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        real(wp), intent(in), optional :: color(3)

        ! TODO: Implement boxplot method in figure_core
        call log_error("boxplot() not yet implemented - please use main branch for boxplot support")
    end subroutine boxplot

    subroutine show_data(x, y, label, title_text, xlabel_text, ylabel_text, blocking)
        !! Display a line plot in the terminal using ASCII graphics
        !! Uses the global figure initialized by figure() subroutine
        !!
        !! Arguments:
        !!   x, y: Data arrays for the line plot
        !!   label: Optional label for the plot
        !!   title_text: Optional plot title
        !!   xlabel_text, ylabel_text: Optional axis labels
        !!   blocking: Optional - if true, wait for user input after display (default: false)
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, title_text, xlabel_text, ylabel_text
        logical, intent(in), optional :: blocking

        call fig%initialize()

        if (present(title_text)) call fig%set_title(title_text)
        if (present(xlabel_text)) call fig%set_xlabel(xlabel_text)
        if (present(ylabel_text)) call fig%set_ylabel(ylabel_text)

        call fig%add_plot(x, y, label=label)
        call fig%show(blocking=blocking)
    end subroutine show_data

    subroutine show_figure(blocking)
        !! Display the global figure intelligently
        !! If GUI is available, opens in system viewer (like matplotlib.pyplot.show())
        !! Otherwise, falls back to ASCII terminal display
        !! Like pyplot's show() - displays current figure
        !! 
        !! Arguments:
        !!   blocking: Optional - if true, wait for user input after display (default: false)
        logical, intent(in), optional :: blocking
        
        if (is_gui_available()) then
            call show_viewer_implementation(blocking=blocking)
        else
            ! Fallback to ASCII display
            call fig%show(blocking=blocking)
        end if
    end subroutine show_figure

    subroutine figure(width, height)
        !! Initialize the global figure for simple API usage
        !!
        !! Arguments:
        !!   width, height: Optional figure dimensions (default: 640x480)
        integer, intent(in), optional :: width, height

        if (present(width) .and. present(height)) then
            call fig%initialize(width, height)
        else
            call fig%initialize()
        end if
    end subroutine figure
    
    subroutine subplot(rows, cols, index)
        !! Create a subplot grid and switch to specified subplot
        !!
        !! Arguments:
        !!   rows: Number of subplot rows
        !!   cols: Number of subplot columns
        !!   index: Subplot index (1-based, row-major order)
        integer, intent(in) :: rows, cols, index
        
        ! Ensure figure is initialized (may have been called after figure())
        if (.not. allocated(fig%backend)) then
            call fig%initialize()
        end if
        
        ! Ensure subplots are initialized if not already
        if (.not. allocated(fig%subplots)) then
            call fig%initialize_default_subplot()
        end if
        
        call fig%set_subplot(rows, cols, index)
    end subroutine subplot

    subroutine xlabel(text)
        !! Set x-axis label for the global figure
        character(len=*), intent(in) :: text
        call fig%set_xlabel(text)
    end subroutine xlabel

    subroutine ylabel(text)
        !! Set y-axis label for the global figure
        character(len=*), intent(in) :: text
        call fig%set_ylabel(text)
    end subroutine ylabel

    subroutine title(text)
        !! Set title for the global figure
        character(len=*), intent(in) :: text
        call fig%set_title(text)
    end subroutine title

    subroutine legend(location)
        !! Show legend for the global figure
        character(len=*), intent(in), optional :: location

        call fig%legend(location=location)
    end subroutine legend

    subroutine savefig(filename, blocking)
        !! Save global figure to file (backend determined by extension)
        !! 
        !! Arguments:
        !!   filename: Output filename (extension determines format)
        !!   blocking: Optional - if true, wait for user input after save (default: false)
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        call fig%savefig(filename, blocking=blocking)
    end subroutine savefig

    subroutine add_plot(x, y, label, linestyle)
        !! Add a line plot to the global figure (pyplot-fortran compatible)
        !!
        !! Explicit interface for adding line plots to existing figures.
        !! Provides the same functionality as plot() with clearer intent.
        !!
        !! Cross-references: See plot() for primary interface, scatter() for point plots,
        !!                  add_3d_plot() for three-dimensional lines
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle

        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine add_plot

    subroutine add_contour(x, y, z, levels, label)
        !! Add a contour plot to the global figure
        !!
        !! Explicit interface for adding contour plots to existing figures.
        !! Provides the same functionality as contour() with clearer intent.
        !!
        !! Cross-references: See contour() for primary interface, add_contour_filled() for filled contours,
        !!                  add_pcolormesh() for grid plots
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label
        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine add_contour

    subroutine add_contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        !! Add a filled contour plot with color levels to the global figure
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        call fig%add_contour_filled(x, y, z, levels=levels, colormap=colormap, &
                                   show_colorbar=show_colorbar, label=label)
    end subroutine add_contour_filled

    subroutine add_pcolormesh(x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        !! Add a pcolormesh plot to the global figure
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: c
        character(len=*), intent(in), optional :: colormap
        real(8), intent(in), optional :: vmin, vmax
        character(len=*), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: linewidths
        call ensure_global_figure_initialized()
        call fig%add_pcolormesh(x, y, c, colormap=colormap, vmin=vmin, vmax=vmax, &
                               edgecolors=edgecolors, linewidths=linewidths)
    end subroutine add_pcolormesh

    subroutine errorbar(x, y, xerr, yerr, xerr_lower, xerr_upper, yerr_lower, yerr_upper, &
                       capsize, elinewidth, label, linestyle, marker, color)
        !! Add error bar plot to the global figure (pyplot-style)
        !!
        !! Arguments:
        !!   x, y: Data arrays for the error bar plot
        !!   xerr, yerr: Optional symmetric error arrays
        !!   xerr_lower, xerr_upper: Optional asymmetric X error arrays
        !!   yerr_lower, yerr_upper: Optional asymmetric Y error arrays
        !!   capsize: Optional cap size for error bars (default: 5.0)
        !!   elinewidth: Optional error bar line width (default: 1.0)
        !!   label: Optional label for the plot
        !!   linestyle: Optional line style
        !!   marker: Optional marker style
        !!   color: Optional RGB color array
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: xerr, yerr
        real(8), dimension(:), intent(in), optional :: xerr_lower, xerr_upper
        real(8), dimension(:), intent(in), optional :: yerr_lower, yerr_upper
        real(8), intent(in), optional :: capsize, elinewidth
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(8), dimension(3), intent(in), optional :: color

        call fig%errorbar(x, y, xerr=xerr, yerr=yerr, xerr_lower=xerr_lower, xerr_upper=xerr_upper, &
                         yerr_lower=yerr_lower, yerr_upper=yerr_upper, capsize=capsize, &
                         elinewidth=elinewidth, label=label, linestyle=linestyle, &
                         marker=marker, color=color)
    end subroutine errorbar

    subroutine add_errorbar(x, y, xerr, yerr, xerr_lower, xerr_upper, yerr_lower, yerr_upper, &
                           capsize, elinewidth, label, linestyle, marker, color)
        !! Add error bar plot to the global figure
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: xerr, yerr
        real(8), dimension(:), intent(in), optional :: xerr_lower, xerr_upper
        real(8), dimension(:), intent(in), optional :: yerr_lower, yerr_upper
        real(8), intent(in), optional :: capsize, elinewidth
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(8), dimension(3), intent(in), optional :: color

        call fig%errorbar(x, y, xerr=xerr, yerr=yerr, xerr_lower=xerr_lower, xerr_upper=xerr_upper, &
                         yerr_lower=yerr_lower, yerr_upper=yerr_upper, capsize=capsize, &
                         elinewidth=elinewidth, label=label, linestyle=linestyle, &
                         marker=marker, color=color)
    end subroutine add_errorbar

    subroutine add_3d_plot(x, y, z, label, linestyle, markersize, linewidth)
        !! Add a 3D line plot to the global figure
        !! 
        !! Arguments:
        !!   x, y, z: Data arrays for the 3D line plot
        !!   label: Optional label for the plot
        !!   linestyle: Optional line style and markers
        !!   markersize: Optional marker size
        !!   linewidth: Optional line width
        real(8), dimension(:), intent(in) :: x, y, z
        character(len=*), intent(in), optional :: label, linestyle
        real(8), intent(in), optional :: markersize, linewidth
        call fig%add_3d_plot(x, y, z, label=label, linestyle=linestyle, &
                            markersize=markersize, linewidth=linewidth)
    end subroutine add_3d_plot

    subroutine add_surface(x, y, z, label)
        !! Add a surface plot to the global figure
        !! 
        !! Arguments:
        !!   x, y: Grid coordinate arrays (1D)
        !!   z: 2D data array defining surface heights
        !!   label: Optional label for the plot
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        character(len=*), intent(in), optional :: label
        call fig%add_surface(x, y, z, label=label)
    end subroutine add_surface

    subroutine set_xscale(scale, threshold)
        !! Set x-axis scale for the global figure
        character(len=*), intent(in) :: scale
        real(8), intent(in), optional :: threshold
        call fig%set_xscale(scale, threshold)
    end subroutine set_xscale

    subroutine set_yscale(scale, threshold)
        !! Set y-axis scale for the global figure
        character(len=*), intent(in) :: scale
        real(8), intent(in), optional :: threshold
        call fig%set_yscale(scale, threshold)
    end subroutine set_yscale

    subroutine xlim(xmin, xmax)
        !! Set x-axis limits for the global figure
        real(8), intent(in) :: xmin, xmax
        call fig%set_xlim(xmin, xmax)
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        !! Set y-axis limits for the global figure
        real(8), intent(in) :: ymin, ymax
        call fig%set_ylim(ymin, ymax)
    end subroutine ylim

    subroutine set_line_width(width)
        !! Set line width for subsequent plots in the global figure
        real(8), intent(in) :: width
        call fig%set_line_width(width)
    end subroutine set_line_width

    subroutine set_ydata(plot_index, y_new)
        !! Update y-data for an existing plot in the global figure
        !! Useful for animations and interactive updates
        integer, intent(in) :: plot_index
        real(8), dimension(:), intent(in) :: y_new
        call fig%set_ydata(plot_index, y_new)
    end subroutine set_ydata

    function is_gui_available() result(gui_available)
        !! Check if GUI environment is available for opening plots
        !! Secure version that only checks environment variables
        logical :: gui_available
        character(len=256) :: display_var, wayland_var, session_type
        integer :: status
        
        gui_available = .false.
        
#ifdef __linux__
        ! Check for Wayland (modern Linux GUI) - secure environment variable check
        call get_environment_variable('WAYLAND_DISPLAY', wayland_var, status=status)
        if (status == 0 .and. len_trim(wayland_var) > 0) then
            gui_available = .true.
            return
        end if
        
        ! Check session type
        call get_environment_variable('XDG_SESSION_TYPE', session_type, status=status)
        if (status == 0) then
            if (index(session_type, 'wayland') > 0 .or. index(session_type, 'x11') > 0) then
                gui_available = .true.
                return
            elseif (index(session_type, 'tty') > 0) then
                ! Explicitly no GUI for tty sessions
                return
            end if
        end if
        
        ! Check for X11 (traditional Linux GUI) - only environment variable
        call get_environment_variable('DISPLAY', display_var, status=status)
        if (status == 0 .and. len_trim(display_var) > 0) then
            gui_available = .true.
        end if
#elif defined(__APPLE__)
        ! On macOS, assume GUI is available (secure assumption)
        gui_available = .true.
#elif defined(_WIN32) || defined(_WIN64)
        ! Windows typically has GUI available  
        gui_available = .true.
#else
        ! For other Unix-like systems, check environment variables only
        call get_environment_variable('WAYLAND_DISPLAY', wayland_var, status=status)
        if (status == 0 .and. len_trim(wayland_var) > 0) then
            gui_available = .true.
            return
        end if
        
        call get_environment_variable('DISPLAY', display_var, status=status)
        if (status == 0 .and. len_trim(display_var) > 0) then
            gui_available = .true.
        end if
#endif
    end function is_gui_available

    subroutine show_viewer_implementation(blocking)
        !! Internal implementation for showing plot in system viewer
        !! Used by both show_viewer() and show_figure() when GUI is available
        !! 
        !! Arguments:
        !!   blocking: Optional - if true, wait for user input after display (default: false)
        use iso_fortran_env, only: int64
        use fortplot_security, only: safe_launch_viewer, safe_remove_file
        
        logical, intent(in), optional :: blocking
        logical :: do_block, success
        character(len=256) :: temp_filename
        character(len=32) :: timestamp
        integer :: stat
        integer(int64) :: time_val
        
        ! Default to non-blocking
        do_block = .false.
        if (present(blocking)) do_block = blocking
        
        ! Generate unique temporary filename with timestamp
        call system_clock(time_val)
        write(timestamp, '(I0)') time_val
        
        ! Use runtime detection for temporary directory
        call get_environment_variable('WINDIR', temp_filename, status=stat)
        if (stat == 0 .and. len_trim(temp_filename) > 0) then
            ! Windows detected - use current directory
            ! TEMPORARY FIX: Use PNG instead of PDF to avoid segfault
            temp_filename = 'fortplot_' // trim(timestamp) // '.png'
        else
            ! Unix-like system (Linux, macOS, etc.) - use /tmp
            ! TEMPORARY FIX: Use PNG instead of PDF to avoid segfault
            temp_filename = '/tmp/fortplot_' // trim(timestamp) // '.png'
        end if
        
        ! Save figure to temporary file
        call fig%savefig(temp_filename)
        
        ! Open with secure viewer launch
        call safe_launch_viewer(temp_filename, success)
        
        if (success) then
            stat = 0
        else
            stat = 1
        end if
        
        if (stat /= 0) then
            call log_warning('Failed to open plot viewer. Plot saved to: ' // trim(temp_filename))
            call log_info('Please open the file manually with your preferred PDF viewer.')
        else
            call log_info('Plot opened in default viewer. File: ' // trim(temp_filename))
        end if
            
        if (do_block) then
            call log_info('Press Enter to continue and clean up temporary file...')
            read(*,*)
            
            ! Clean up temporary file securely
            call safe_remove_file(temp_filename, success)
            if (.not. success) then
                call log_warning('Could not remove temporary file: ' // trim(temp_filename))
            end if
        else
            ! In non-blocking mode, just inform that file stays
            call log_info('Note: Temporary file will remain at: ' // trim(temp_filename))
        end if
    end subroutine show_viewer_implementation

    subroutine show_viewer(blocking)
        !! Display the current figure in the system's default viewer
        !! Similar to matplotlib.pyplot.show() - saves to temporary file and opens with system viewer
        !!
        !! Arguments:
        !!   blocking: Optional - if true, wait for user input after display (default: false)
        !!
        !! Supports:
        !!   - Linux: uses xdg-open
        !!   - macOS: uses open  
        !!   - Windows: uses start
        !!
        !! Usage:
        !!   call plot(x, y)
        !!   call show_viewer()  ! Opens plot in default PDF viewer
        logical, intent(in), optional :: blocking
        
        call ensure_global_figure_initialized()
        call show_viewer_implementation(blocking=blocking)
    end subroutine show_viewer

    subroutine scatter(x, y, s, c, label, marker, markersize, color, &
                      colormap, vmin, vmax, show_colorbar)
        !! Add enhanced scatter plot to the global figure (pyplot-style)
        !!
        !! Arguments:
        !!   x, y: Data arrays for the scatter plot
        !!   s: Optional size mapping array for bubble charts
        !!   c: Optional color mapping array for color-coded plots
        !!   label: Optional label for the plot
        !!   marker: Optional marker style ('o', 's', 'D', 'x', '+', '*', '^', 'v', 'p', 'h')
        !!   markersize: Optional default marker size when s not provided
        !!   color: Optional RGB color array [0-1] for uniform coloring
        !!   colormap: Optional colormap name ('viridis', 'plasma', 'inferno', 'coolwarm', etc.)
        !!   vmin, vmax: Optional color scale limits
        !!   show_colorbar: Optional flag to show colorbar for color mapping
        !!
        !! Examples:
        !!   ! Basic scatter plot
        !!   call scatter(x, y, label='Data Points')
        !!   
        !!   ! Bubble chart with size mapping
        !!   call scatter(x, y, s=sizes, label='Bubble Chart')
        !!   
        !!   ! Color-mapped scatter plot
        !!   call scatter(x, y, c=values, colormap='viridis', label='Color Mapped')
        !!   
        !!   ! Full featured scatter plot
        !!   call scatter(x, y, s=sizes, c=values, marker='s', colormap='plasma', &
        !!               vmin=0.0_real64, vmax=1.0_real64, label='Advanced Scatter')
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: s, c
        character(len=*), intent(in), optional :: label, marker, colormap
        real(8), intent(in), optional :: markersize, vmin, vmax
        real(8), dimension(3), intent(in), optional :: color
        logical, intent(in), optional :: show_colorbar

        call ensure_global_figure_initialized()
        call fig%add_scatter(x, y, s=s, c=c, label=label, marker=marker, &
                            markersize=markersize, color=color, colormap=colormap, &
                            vmin=vmin, vmax=vmax, show_colorbar=show_colorbar)
    end subroutine scatter

    subroutine add_scatter(x, y, s, c, label, marker, markersize, color, &
                          colormap, vmin, vmax, show_colorbar)
        !! Add enhanced scatter plot to the global figure (wrapper for consistency)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: s, c
        character(len=*), intent(in), optional :: label, marker, colormap
        real(8), intent(in), optional :: markersize, vmin, vmax
        real(8), dimension(3), intent(in), optional :: color
        logical, intent(in), optional :: show_colorbar

        call fig%add_scatter(x, y, s=s, c=c, label=label, marker=marker, &
                            markersize=markersize, color=color, colormap=colormap, &
                            vmin=vmin, vmax=vmax, show_colorbar=show_colorbar)
    end subroutine add_scatter

    subroutine text(x, y, text_content, coord_type, font_size, rotation, alignment, has_bbox)
        !! Add text annotation to the global figure (matplotlib-style)
        !!
        !! Arguments:
        !!   x, y: Position coordinates
        !!   text_content: Text content to display
        !!   coord_type: Optional coordinate system (COORD_DATA, COORD_FIGURE, COORD_AXIS)
        !!   font_size: Optional font size in points
        !!   rotation: Optional rotation angle in degrees
        !!   alignment: Optional text alignment ('left', 'center', 'right')
        !!   has_bbox: Optional background box flag
        real(8), intent(in) :: x, y
        character(len=*), intent(in) :: text_content
        integer, intent(in), optional :: coord_type
        real(8), intent(in), optional :: font_size, rotation
        character(len=*), intent(in), optional :: alignment
        logical, intent(in), optional :: has_bbox
        
        call ensure_global_figure_initialized()
        call fig%text(x, y, text_content, coord_type=coord_type, font_size=font_size, &
                     rotation=rotation, alignment=alignment, has_bbox=has_bbox)
    end subroutine text

    subroutine annotate(text_content, xy, xytext, xy_coord_type, xytext_coord_type, &
                       font_size, alignment, has_bbox)
        !! Add arrow annotation to the global figure (matplotlib-style)
        !!
        !! Arguments:
        !!   text_content: Text content to display
        !!   xy: Position coordinates of arrow tip [x, y]
        !!   xytext: Position coordinates of text [x, y]
        !!   xy_coord_type: Optional coordinate system for arrow tip
        !!   xytext_coord_type: Optional coordinate system for text
        !!   font_size: Optional font size in points
        !!   alignment: Optional text alignment
        !!   has_bbox: Optional background box flag
        character(len=*), intent(in) :: text_content
        real(8), intent(in) :: xy(2), xytext(2)
        integer, intent(in), optional :: xy_coord_type, xytext_coord_type
        real(8), intent(in), optional :: font_size
        character(len=*), intent(in), optional :: alignment
        logical, intent(in), optional :: has_bbox
        
        call ensure_global_figure_initialized()
        call fig%annotate(text_content, xy, xytext, xy_coord_type=xy_coord_type, &
                         xytext_coord_type=xytext_coord_type, font_size=font_size, &
                         alignment=alignment, has_bbox=has_bbox)
    end subroutine annotate

    subroutine ensure_global_figure_initialized()
        !! Ensure global figure is initialized before use (matplotlib compatibility)
        !! Auto-initializes with default dimensions if not already initialized
        if (.not. allocated(fig%backend)) then
            call fig%initialize()
        end if
    end subroutine ensure_global_figure_initialized

    function get_global_figure() result(global_fig)
        !! Get reference to the global figure for testing access to arrow data
        !! This allows tests to access fig%arrow_data without making fig public
        type(figure_t), pointer :: global_fig
        global_fig => fig
    end function get_global_figure

end module fortplot
