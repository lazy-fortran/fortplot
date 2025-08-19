module fortplot_python_interface
    !! Python interface module for F2PY binding generation
    !!
    !! This module provides a clean interface for Python bindings using F2PY.
    !! It exposes the essential functions needed for simplified Python show() API.
    !!
    !! Key functions:
    !! - show_figure(blocking): Direct call to Fortran show with blocking parameter
    !! - show_viewer(blocking): Direct call to Fortran show_viewer with blocking parameter
    !! - All other plotting functions for complete API coverage

    use iso_fortran_env, only: wp => real64
    use fortplot, only: fortplot_show => show, fortplot_show_viewer => show_viewer, &
                        fortplot_figure => figure, fortplot_plot => plot, &
                        fortplot_savefig => savefig, fortplot_title => title, &
                        fortplot_xlabel => xlabel, fortplot_ylabel => ylabel, &
                        fortplot_contour => contour, fortplot_contour_filled => contour_filled, &
                        fortplot_pcolormesh => pcolormesh, fortplot_streamplot => streamplot, &
                        fortplot_legend => legend, fortplot_set_xscale => set_xscale, &
                        fortplot_set_yscale => set_yscale, fortplot_xlim => xlim, fortplot_ylim => ylim
    implicit none

    private
    
    ! Export functions for F2PY - use simple names for Python access
    public :: show_figure, show_viewer, figure, plot, savefig
    public :: title, xlabel, ylabel, contour, contour_filled
    public :: pcolormesh, streamplot, legend
    public :: set_xscale, set_yscale, xlim, ylim

contains

    subroutine show_figure(blocking)
        !! Python-accessible show_figure function with optional blocking
        !! This replaces the temp file + webbrowser approach with direct Fortran call
        !!
        !! Arguments:
        !!   blocking: Optional - if true, wait for user input after display (default: false)
        logical, intent(in), optional :: blocking
        
        call fortplot_show(blocking=blocking)
    end subroutine show_figure

    subroutine show_viewer(blocking)
        !! Python-accessible show_viewer function with optional blocking
        !! Forces display in system viewer regardless of GUI availability
        !!
        !! Arguments:
        !!   blocking: Optional - if true, wait for user input after display (default: false)
        logical, intent(in), optional :: blocking
        
        call fortplot_show_viewer(blocking=blocking)
    end subroutine show_viewer

    subroutine figure(width, height)
        !! Python-accessible figure initialization
        !! 
        !! Arguments:
        !!   width, height: Optional figure dimensions (default: 640x480)
        integer, intent(in), optional :: width, height
        
        call fortplot_figure(width, height)
    end subroutine figure

    subroutine plot(x, y, n, label, linestyle)
        !! Python-accessible plot function for line plots
        !!
        !! Arguments:
        !!   x, y: Data arrays for line plot
        !!   n: Array size
        !!   label: Optional label for legend
        !!   linestyle: Optional line style string
        integer, intent(in) :: n
        real(wp), dimension(n), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle
        
        call fortplot_plot(x, y, label=label, linestyle=linestyle)
    end subroutine plot

    subroutine savefig(filename)
        !! Python-accessible savefig function
        !!
        !! Arguments:
        !!   filename: Output filename (extension determines format)
        character(len=*), intent(in) :: filename
        
        call fortplot_savefig(filename)
    end subroutine savefig

    subroutine title(text)
        !! Python-accessible title function
        character(len=*), intent(in) :: text
        call fortplot_title(text)
    end subroutine title

    subroutine xlabel(text)
        !! Python-accessible xlabel function
        character(len=*), intent(in) :: text
        call fortplot_xlabel(text)
    end subroutine xlabel

    subroutine ylabel(text)
        !! Python-accessible ylabel function
        character(len=*), intent(in) :: text
        call fortplot_ylabel(text)
    end subroutine ylabel

    subroutine contour(x, y, z, nx, ny, levels, nlevels)
        !! Python-accessible contour function
        !!
        !! Arguments:
        !!   x, y: Grid coordinate arrays
        !!   z: 2D data array for contouring
        !!   nx, ny: Array dimensions
        !!   levels: Optional contour levels
        !!   nlevels: Number of levels
        integer, intent(in) :: nx, ny
        real(wp), dimension(nx), intent(in) :: x
        real(wp), dimension(ny), intent(in) :: y
        real(wp), dimension(nx, ny), intent(in) :: z
        integer, intent(in), optional :: nlevels
        real(wp), dimension(:), intent(in), optional :: levels
        
        if (present(levels) .and. present(nlevels)) then
            call fortplot_contour(x, y, z, levels=levels(1:nlevels))
        else
            call fortplot_contour(x, y, z)
        end if
    end subroutine contour

    subroutine contour_filled(x, y, z, nx, ny, levels, nlevels, colormap)
        !! Python-accessible filled contour function
        integer, intent(in) :: nx, ny
        real(wp), dimension(nx), intent(in) :: x
        real(wp), dimension(ny), intent(in) :: y
        real(wp), dimension(nx, ny), intent(in) :: z
        integer, intent(in), optional :: nlevels
        real(wp), dimension(:), intent(in), optional :: levels
        character(len=*), intent(in), optional :: colormap
        
        if (present(levels) .and. present(nlevels)) then
            call fortplot_contour_filled(x, y, z, levels=levels(1:nlevels), colormap=colormap)
        else
            call fortplot_contour_filled(x, y, z, colormap=colormap)
        end if
    end subroutine contour_filled

    subroutine pcolormesh(x, y, c, nx, ny, colormap, vmin, vmax, edgecolors, linewidths)
        !! Python-accessible pcolormesh function
        integer, intent(in) :: nx, ny
        real(wp), dimension(nx), intent(in) :: x
        real(wp), dimension(ny), intent(in) :: y
        real(wp), dimension(nx, ny), intent(in) :: c
        character(len=*), intent(in), optional :: colormap, edgecolors
        real(wp), intent(in), optional :: vmin, vmax, linewidths
        
        call fortplot_pcolormesh(x, y, c, colormap=colormap, vmin=vmin, vmax=vmax, &
                       edgecolors=edgecolors, linewidths=linewidths)
    end subroutine pcolormesh

    subroutine streamplot(x, y, u, v, nx, ny, density)
        !! Python-accessible streamplot function
        integer, intent(in) :: nx, ny
        real(wp), dimension(nx), intent(in) :: x
        real(wp), dimension(ny), intent(in) :: y
        real(wp), dimension(nx, ny), intent(in) :: u, v
        real(wp), intent(in), optional :: density
        
        call fortplot_streamplot(x, y, u, v, density=density)
    end subroutine streamplot

    subroutine legend()
        !! Python-accessible legend function
        call fortplot_legend()
    end subroutine legend

    subroutine set_xscale(scale)
        !! Python-accessible x-axis scale function
        character(len=*), intent(in) :: scale
        call fortplot_set_xscale(scale)
    end subroutine set_xscale

    subroutine set_yscale(scale)
        !! Python-accessible y-axis scale function
        character(len=*), intent(in) :: scale
        call fortplot_set_yscale(scale)
    end subroutine set_yscale

    subroutine xlim(xmin, xmax)
        !! Python-accessible x-axis limits function
        real(wp), intent(in) :: xmin, xmax
        call fortplot_xlim(xmin, xmax)
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        !! Python-accessible y-axis limits function
        real(wp), intent(in) :: ymin, ymax
        call fortplot_ylim(ymin, ymax)
    end subroutine ylim

end module fortplot_python_interface