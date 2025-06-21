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

    ! Re-export public interface
    public :: figure_t, wp
    public :: plot, contour, show_plot
    public :: xlabel, ylabel, title
    public :: savefig

contains

    subroutine plot(x, y, filename, label, title_text, xlabel_text, ylabel_text)
        !! Create a simple line plot with optional customization
        !!
        !! Arguments:
        !!   x, y: Data arrays for the line plot
        !!   filename: Output filename (determines backend from extension)
        !!   label: Optional label for the plot
        !!   title_text: Optional plot title
        !!   xlabel_text, ylabel_text: Optional axis labels
        real(wp), dimension(:), intent(in) :: x, y
        character(len=*), intent(in) :: filename
        character(len=*), intent(in), optional :: label, title_text, xlabel_text, ylabel_text
        
        type(figure_t) :: fig
        
        call fig%initialize()
        
        if (present(title_text)) call fig%set_title(title_text)
        if (present(xlabel_text)) call fig%set_xlabel(xlabel_text)
        if (present(ylabel_text)) call fig%set_ylabel(ylabel_text)
        
        call fig%add_plot(x, y, label=label)
        call fig%savefig(filename)
    end subroutine plot

    subroutine contour(x, y, z, filename, levels, label, title_text, xlabel_text, ylabel_text)
        !! Create a contour plot with optional customization
        !!
        !! Arguments:
        !!   x, y: Grid coordinate arrays
        !!   z: 2D data array for contouring
        !!   filename: Output filename (determines backend from extension)
        !!   levels: Optional array of contour levels
        !!   label: Optional label for the plot
        !!   title_text: Optional plot title
        !!   xlabel_text, ylabel_text: Optional axis labels
        real(wp), dimension(:), intent(in) :: x, y
        real(wp), dimension(:,:), intent(in) :: z
        character(len=*), intent(in) :: filename
        real(wp), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: label, title_text, xlabel_text, ylabel_text
        
        type(figure_t) :: fig
        
        call fig%initialize()
        
        if (present(title_text)) call fig%set_title(title_text)
        if (present(xlabel_text)) call fig%set_xlabel(xlabel_text)
        if (present(ylabel_text)) call fig%set_ylabel(ylabel_text)
        
        call fig%add_contour(x, y, z, levels=levels, label=label)
        call fig%savefig(filename)
    end subroutine contour

    subroutine show_plot(x, y, label, title_text, xlabel_text, ylabel_text)
        !! Display a line plot in the terminal using ASCII graphics
        !!
        !! Arguments:
        !!   x, y: Data arrays for the line plot
        !!   label: Optional label for the plot
        !!   title_text: Optional plot title
        !!   xlabel_text, ylabel_text: Optional axis labels
        real(wp), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, title_text, xlabel_text, ylabel_text
        
        type(figure_t) :: fig
        
        call fig%initialize()
        
        if (present(title_text)) call fig%set_title(title_text)
        if (present(xlabel_text)) call fig%set_xlabel(xlabel_text)
        if (present(ylabel_text)) call fig%set_ylabel(ylabel_text)
        
        call fig%add_plot(x, y, label=label)
        call fig%show()
    end subroutine show_plot

    subroutine xlabel(fig, text)
        !! Set x-axis label for a figure
        type(figure_t), intent(inout) :: fig
        character(len=*), intent(in) :: text
        call fig%set_xlabel(text)
    end subroutine xlabel

    subroutine ylabel(fig, text)
        !! Set y-axis label for a figure
        type(figure_t), intent(inout) :: fig
        character(len=*), intent(in) :: text
        call fig%set_ylabel(text)
    end subroutine ylabel

    subroutine title(fig, text)
        !! Set title for a figure
        type(figure_t), intent(inout) :: fig
        character(len=*), intent(in) :: text
        call fig%set_title(text)
    end subroutine title

    subroutine savefig(fig, filename)
        !! Save figure to file (backend determined by extension)
        type(figure_t), intent(inout) :: fig
        character(len=*), intent(in) :: filename
        call fig%savefig(filename)
    end subroutine savefig

end module fortplot