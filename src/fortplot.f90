module fortplot
    !! Top-level public interface for fortplotlib
    !!
    !! This module provides a clean, user-friendly API for creating scientific plots
    !! with support for line plots, contour plots, and multiple output formats.
    !!
    !! Quick Start:
    !!   use fortplot
    !!   type(figure_t) :: fig
    !!
    !!   call fig%initialize(640, 480)
    !!   call fig%add_plot(x, y, label="data")
    !!   call fig%savefig('output.png')
    !!
    !! Author: fortplotlib contributors

    use iso_fortran_env, only: wp => real64
    use fortplot_figure, only: figure_t
    use fortplot_format_parser, only: parse_format_string, contains_format_chars
    use fortplot_animation, only: animation_t, FuncAnimation

    implicit none

    private

    ! Re-export public interface
    public :: figure_t, wp
    public :: plot, contour, contour_filled, pcolormesh, streamplot, boxplot, show, show_viewer
    public :: xlabel, ylabel, title, legend
    public :: savefig, figure
    public :: add_plot, add_contour, add_contour_filled, add_pcolormesh
    public :: set_xscale, set_yscale, xlim, ylim
    public :: set_line_width, set_ydata
    
    ! Animation interface
    public :: animation_t, FuncAnimation

    ! Line style constants (pyplot-style)
    character(len=*), parameter, public :: LINESTYLE_SOLID = '-'
    character(len=*), parameter, public :: LINESTYLE_DASHED = '--'
    character(len=*), parameter, public :: LINESTYLE_DOTTED = ':'
    character(len=*), parameter, public :: LINESTYLE_DASHDOT = '-.'
    character(len=*), parameter, public :: LINESTYLE_NONE = 'None'

    ! Marker style constants (pyplot-style)
    character(len=*), parameter, public :: MARKER_CIRCLE = 'o'
    character(len=*), parameter, public :: MARKER_CROSS = 'x'
    character(len=*), parameter, public :: MARKER_SQUARE = 's'
    character(len=*), parameter, public :: MARKER_DIAMOND = 'D'
    character(len=*), parameter, public :: MARKER_PLUS = '+'
    character(len=*), parameter, public :: MARKER_STAR = '*'


    ! Interface for overloaded show routine
    interface show
        module procedure show_data, show_figure
    end interface show

    ! Note: plot and add_plot now handle both separate args and format strings in single routines

    ! Global figure for simple API
    type(figure_t), save :: fig

contains

    subroutine plot(x, y, label, linestyle)
        !! Add a line plot to the global figure (pyplot-fortran compatible)
        !!
        !! Arguments:
        !!   x, y: Data arrays for the line plot
        !!   label: Optional label for the plot
        !!   linestyle: Line style and markers ('b-o', 'r--', 'g:', 'ko', etc.)
        !!              Supports both pyplot-fortran and matplotlib format strings
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle

        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine plot

    subroutine contour(x, y, z, levels, label)
        !! Add a contour plot to the global figure (pyplot-style)
        !!
        !! Arguments:
        !!   x, y: Grid coordinate arrays
        !!   z: 2D data array for contouring
        !!   levels: Optional array of contour levels
        !!   label: Optional label for the plot
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: z
        real(8), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label

        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine contour

    subroutine contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        !! Add a filled contour plot with color levels to the global figure
        !!
        !! Arguments:
        !!   x, y: Grid coordinate arrays
        !!   z: 2D data array for contouring
        !!   levels: Optional array of contour levels
        !!   colormap: Optional colormap name ('crest' (default), 'viridis', 'plasma', 'rocket', 'mako', 'flare', etc.)
        !!   show_colorbar: Optional flag to show colorbar
        !!   label: Optional label for the plot
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
        !! Creates a pseudocolor plot with a non-regular rectangular grid.
        !! Compatible with matplotlib pcolormesh function.
        !!
        !! Arguments:
        !!   x, y: Grid coordinate arrays (1D for regular grid)
        !!   c: Color data array (2D)
        !!   colormap: Optional colormap name ('viridis', 'plasma', 'coolwarm', etc.)
        !!   vmin, vmax: Optional color scale limits
        !!   edgecolors: Optional edge color ('none', 'black', etc.)
        !!   linewidths: Optional edge line width
        !!
        !! Example:
        !!   ! Simple heatmap
        !!   call pcolormesh(x, y, temperature_data, colormap='viridis')
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: c
        character(len=*), intent(in), optional :: colormap
        real(8), intent(in), optional :: vmin, vmax
        character(len=*), intent(in), optional :: edgecolors
        real(8), intent(in), optional :: linewidths

        call fig%add_pcolormesh(x, y, c, colormap=colormap, vmin=vmin, vmax=vmax, &
                               edgecolors=edgecolors, linewidths=linewidths)
    end subroutine pcolormesh

    subroutine streamplot(x, y, u, v, density)
        !! Add a streamplot (vector field visualization) to the global figure
        !!
        !! Creates streamlines that follow the direction of a 2D vector field,
        !! providing intuitive visualization of flow patterns, magnetic fields,
        !! or any other vector data.
        !!
        !! Arguments:
        !!   x, y: 1D coordinate arrays defining the grid
        !!   u, v: 2D velocity/vector field components (u=x-component, v=y-component)
        !!   density: Optional streamline density parameter (default=1.0)
        !!            Higher values = more streamlines, lower values = fewer streamlines
        !!
        !! Example:
        !!   ! Circular flow field
        !!   call streamplot(x, y, -y_grid, x_grid, density=1.5_real64)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:,:), intent(in) :: u, v
        real(8), intent(in), optional :: density

        call fig%streamplot(x, y, u, v, density=density)
    end subroutine streamplot

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
        real(8), dimension(:), intent(in) :: data
        real(8), intent(in), optional :: position
        real(8), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        real(8), intent(in), optional :: color(3)

        call fig%boxplot(data, position=position, width=width, label=label, &
                        show_outliers=show_outliers, horizontal=horizontal, color=color)
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
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle

        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine add_plot

    subroutine add_contour(x, y, z, levels, label)
        !! Add a contour plot to the global figure
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
        call fig%add_pcolormesh(x, y, c, colormap=colormap, vmin=vmin, vmax=vmax, &
                               edgecolors=edgecolors, linewidths=linewidths)
    end subroutine add_pcolormesh

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
        !! Supports X11, Wayland, macOS, and Windows
        logical :: gui_available
        character(len=256) :: display_var, wayland_var, session_type
        character(len=512) :: test_command
        integer :: status, exit_stat
        
        gui_available = .false.
        
#ifdef __linux__
        ! First check if xdg-open is available (required for GUI)
        test_command = 'which xdg-open >/dev/null 2>&1'
        call execute_command_line(test_command, exitstat=exit_stat)
        if (exit_stat /= 0) then
            return  ! No xdg-open, definitely no GUI
        end if
        
        ! Check for Wayland (modern Linux GUI)
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
        
        ! Check for X11 (traditional Linux GUI)  
        call get_environment_variable('DISPLAY', display_var, status=status)
        if (status == 0 .and. len_trim(display_var) > 0) then
            ! Test if X11 display is actually accessible
            test_command = 'xset q >/dev/null 2>&1'
            call execute_command_line(test_command, exitstat=exit_stat)
            gui_available = (exit_stat == 0)
        end if
#elif defined(__APPLE__)
        ! On macOS, check if 'open' command exists (should always be there)
        test_command = 'which open >/dev/null 2>&1'
        call execute_command_line(test_command, exitstat=exit_stat)
        gui_available = (exit_stat == 0)
#elif defined(_WIN32) || defined(_WIN64)
        ! Windows typically has GUI available  
        gui_available = .true.
#else
        ! For other Unix-like systems, check both Wayland and X11
        test_command = 'which xdg-open >/dev/null 2>&1'
        call execute_command_line(test_command, exitstat=exit_stat)
        if (exit_stat /= 0) then
            return  ! No xdg-open
        end if
        
        ! Check Wayland first
        call get_environment_variable('WAYLAND_DISPLAY', wayland_var, status=status)
        if (status == 0 .and. len_trim(wayland_var) > 0) then
            gui_available = .true.
            return
        end if
        
        ! Then check X11
        call get_environment_variable('DISPLAY', display_var, status=status)
        if (status == 0 .and. len_trim(display_var) > 0) then
            test_command = 'xset q >/dev/null 2>&1'
            call execute_command_line(test_command, exitstat=exit_stat)
            gui_available = (exit_stat == 0)
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
        
        logical, intent(in), optional :: blocking
        logical :: do_block
        character(len=256) :: temp_filename
        character(len=512) :: command
        character(len=32) :: timestamp
        integer :: stat
        integer(int64) :: time_val
        
        ! Default to non-blocking
        do_block = .false.
        if (present(blocking)) do_block = blocking
        
        ! Generate unique temporary filename with timestamp
        call system_clock(time_val)
        write(timestamp, '(I0)') time_val
        
#ifdef __linux__
        temp_filename = '/tmp/fortplot_' // trim(timestamp) // '.pdf'
#elif defined(__APPLE__)
        temp_filename = '/tmp/fortplot_' // trim(timestamp) // '.pdf'
#elif defined(_WIN32) || defined(_WIN64)
        temp_filename = 'fortplot_' // trim(timestamp) // '.pdf'
#else
        temp_filename = 'fortplot_' // trim(timestamp) // '.pdf'
#endif
        
        ! Save figure to temporary file
        call fig%savefig(temp_filename)
        
        ! Open with system default viewer
#ifdef __linux__
        command = 'xdg-open "' // trim(temp_filename) // '" 2>/dev/null'
#elif defined(__APPLE__)
        command = 'open "' // trim(temp_filename) // '"'
#elif defined(_WIN32) || defined(_WIN64)
        command = 'start "" "' // trim(temp_filename) // '"'
#else
        ! Fallback - try xdg-open (most Unix-like systems)
        command = 'xdg-open "' // trim(temp_filename) // '" 2>/dev/null'
#endif
        
        ! Execute system command to open file
        call execute_command_line(command, wait=.false., exitstat=stat)
        
        if (stat /= 0) then
            print *, 'Warning: Failed to open plot viewer. Plot saved to: ', trim(temp_filename)
            print *, 'Please open the file manually with your preferred PDF viewer.'
        else
            print *, 'Plot opened in default viewer. File: ', trim(temp_filename)
            
            if (do_block) then
                print *, 'Press Enter to continue and clean up temporary file...'
                read(*,*)
                
                ! Clean up temporary file
#ifdef __linux__ 
                command = 'rm -f "' // trim(temp_filename) // '"'
#elif defined(__APPLE__)
                command = 'rm -f "' // trim(temp_filename) // '"'
#elif defined(_WIN32) || defined(_WIN64)
                command = 'del "' // trim(temp_filename) // '"'
#else
                command = 'rm -f "' // trim(temp_filename) // '"'
#endif
                call execute_command_line(command)
            else
                ! In non-blocking mode, just inform that file stays
                print *, 'Note: Temporary file will remain at: ', trim(temp_filename)
            end if
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
        
        call show_viewer_implementation(blocking=blocking)
    end subroutine show_viewer

end module fortplot
