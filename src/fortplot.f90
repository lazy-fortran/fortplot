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

    use fortplot_figure, only: figure_t
    use, intrinsic :: iso_fortran_env, only: wp => real64

    implicit none

    private

    ! Re-export public interface
    public :: figure_t, wp
    public :: plot, contour, contour_filled, show
    public :: xlabel, ylabel, title, legend
    public :: savefig, figure
    public :: add_plot, add_contour, add_contour_filled
    public :: set_xscale, set_yscale
    
    ! Line style constants (pyplot-style)
    character(len=*), parameter, public :: LINESTYLE_SOLID = '-'
    character(len=*), parameter, public :: LINESTYLE_DASHED = '--'
    character(len=*), parameter, public :: LINESTYLE_DOTTED = ':'
    character(len=*), parameter, public :: LINESTYLE_DASHDOT = '-.'
    character(len=*), parameter, public :: LINESTYLE_NONE = 'None'

    ! Marker style constants (pyplot-style)
    character(len=*), parameter, public :: MARKER_CIRCLE = 'o'
    character(len=*), parameter, public :: MARKER_DOT = '.'
    character(len=*), parameter, public :: MARKER_CROSS = 'x'
    character(len=*), parameter, public :: MARKER_PLUS = '+'
    character(len=*), parameter, public :: MARKER_STAR = '*'
    character(len=*), parameter, public :: MARKER_NONE = 'None'
    
    
    ! Interface for overloaded show routine
    interface show
        module procedure show_data, show_figure
    end interface show
    
    ! Global figure for simple API
    type(figure_t), save :: fig

contains

    subroutine plot(x, y, label, linestyle, marker)
        !! Add a line plot to the global figure (pyplot-style)
        !!
        !! Arguments:
        !!   x, y: Data arrays for the line plot
        !!   label: Optional label for the plot
        !!   linestyle: Optional line style ('-', '--', '-.', ':', 'None')
        !!   marker: Optional marker style ('o', '.', 'x', '+', '*', 'None')
        real(wp), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle, marker
        
        call fig%add_plot(x, y, label=label, linestyle=linestyle, marker=marker)
    end subroutine plot

    subroutine contour(x, y, z, levels, label)
        !! Add a contour plot to the global figure (pyplot-style)
        !!
        !! Arguments:
        !!   x, y: Grid coordinate arrays
        !!   z: 2D data array for contouring
        !!   levels: Optional array of contour levels
        !!   label: Optional label for the plot
        real(wp), dimension(:), intent(in) :: x, y
        real(wp), dimension(:,:), intent(in) :: z
        real(wp), dimension(:), intent(in), optional :: levels
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
        real(wp), dimension(:), intent(in) :: x, y
        real(wp), dimension(:,:), intent(in) :: z
        real(wp), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        call fig%add_contour_filled(x, y, z, levels=levels, colormap=colormap, &
                                   show_colorbar=show_colorbar, label=label)
    end subroutine contour_filled

    subroutine show_data(x, y, label, title_text, xlabel_text, ylabel_text)
        !! Display a line plot in the terminal using ASCII graphics
        !! Uses the global figure initialized by figure() subroutine
        !!
        !! Arguments:
        !!   x, y: Data arrays for the line plot
        !!   label: Optional label for the plot
        !!   title_text: Optional plot title
        !!   xlabel_text, ylabel_text: Optional axis labels
        real(wp), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, title_text, xlabel_text, ylabel_text
        
        call fig%initialize()
        
        if (present(title_text)) call fig%set_title(title_text)
        if (present(xlabel_text)) call fig%set_xlabel(xlabel_text)
        if (present(ylabel_text)) call fig%set_ylabel(ylabel_text)
        
        call fig%add_plot(x, y, label=label)
        call fig%show()
    end subroutine show_data

    subroutine show_figure()
        !! Display the global figure in terminal using ASCII graphics
        !! Like pyplot's show() - displays current figure
        call fig%show()
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

    subroutine savefig(filename)
        !! Save global figure to file (backend determined by extension)
        character(len=*), intent(in) :: filename
        call fig%savefig(filename)
    end subroutine savefig

    subroutine add_plot(x, y, label, linestyle, marker)
        !! Add a line plot to the global figure
        real(wp), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle, marker
        call fig%add_plot(x, y, label=label, linestyle=linestyle, marker=marker)
    end subroutine add_plot

    subroutine add_contour(x, y, z, levels, label)
        !! Add a contour plot to the global figure
        real(wp), dimension(:), intent(in) :: x, y
        real(wp), dimension(:,:), intent(in) :: z
        real(wp), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label
        call fig%add_contour(x, y, z, levels=levels, label=label)
    end subroutine add_contour

    subroutine add_contour_filled(x, y, z, levels, colormap, show_colorbar, label)
        !! Add a filled contour plot with color levels to the global figure
        real(wp), dimension(:), intent(in) :: x, y
        real(wp), dimension(:,:), intent(in) :: z
        real(wp), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        call fig%add_contour_filled(x, y, z, levels=levels, colormap=colormap, &
                                   show_colorbar=show_colorbar, label=label)
    end subroutine add_contour_filled

    subroutine set_xscale(scale, threshold)
        !! Set x-axis scale for the global figure
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call fig%set_xscale(scale, threshold)
    end subroutine set_xscale

    subroutine set_yscale(scale, threshold)
        !! Set y-axis scale for the global figure  
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call fig%set_yscale(scale, threshold)
    end subroutine set_yscale

end module fortplot