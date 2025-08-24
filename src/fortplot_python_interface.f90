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
    use fortplot_matplotlib, only: mpl_show => show, mpl_show_viewer => show_viewer, &
                                  mpl_figure => figure, mpl_plot => plot, &
                                  mpl_savefig => savefig, mpl_title => title, &
                                  mpl_xlabel => xlabel, mpl_ylabel => ylabel, &
                                  mpl_contour => contour, mpl_contour_filled => contour_filled, &
                                  mpl_pcolormesh => pcolormesh, mpl_streamplot => streamplot, &
                                  mpl_legend => legend, mpl_set_xscale => set_xscale, &
                                  mpl_set_yscale => set_yscale, mpl_xlim => xlim, mpl_ylim => ylim, &
                                  mpl_scatter => scatter, mpl_histogram => histogram
    implicit none

    private
    
    ! Export functions for F2PY - use simple names for Python access
    public :: show_figure, show_viewer, figure, plot, savefig
    public :: title, xlabel, ylabel, contour, contour_filled
    public :: pcolormesh, streamplot, legend, scatter, histogram
    public :: set_xscale, set_yscale, xlim, ylim

contains

    subroutine show_figure(blocking)
        !! Python-accessible show_figure function with optional blocking
        !! This replaces the temp file + webbrowser approach with direct Fortran call
        !!
        !! Arguments:
        !!   blocking: Optional - if true, wait for user input after display (default: false)
        logical, intent(in), optional :: blocking
        
        call mpl_show(blocking=blocking)
    end subroutine show_figure

    subroutine show_viewer(blocking)
        !! Python-accessible show_viewer function with optional blocking
        !! Forces display in system viewer regardless of GUI availability
        !!
        !! Arguments:
        !!   blocking: Optional - if true, wait for user input after display (default: false)
        logical, intent(in), optional :: blocking
        
        call mpl_show_viewer(blocking=blocking)
    end subroutine show_viewer

    subroutine figure(width, height)
        !! Python-accessible figure initialization
        !! 
        !! Arguments:
        !!   width, height: Optional figure dimensions (default: 640x480)
        integer, intent(in), optional :: width, height
        
        real(8), dimension(2) :: figsize
        
        if (present(width) .and. present(height)) then
            figsize = [real(width, 8), real(height, 8)]
            call mpl_figure(figsize=figsize)
        else
            call mpl_figure()
        end if
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
        
        call mpl_plot(x, y, label=label, linestyle=linestyle)
    end subroutine plot

    subroutine savefig(filename)
        !! Python-accessible savefig function
        !!
        !! Arguments:
        !!   filename: Output filename (extension determines format)
        character(len=*), intent(in) :: filename
        
        call mpl_savefig(filename)
    end subroutine savefig

    subroutine title(text)
        !! Python-accessible title function
        character(len=*), intent(in) :: text
        call mpl_title(text)
    end subroutine title

    subroutine xlabel(text)
        !! Python-accessible xlabel function
        character(len=*), intent(in) :: text
        call mpl_xlabel(text)
    end subroutine xlabel

    subroutine ylabel(text)
        !! Python-accessible ylabel function
        character(len=*), intent(in) :: text
        call mpl_ylabel(text)
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
            call mpl_contour(x, y, z, levels=levels(1:nlevels))
        else
            call mpl_contour(x, y, z)
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
            call mpl_contour_filled(x, y, z, levels=levels(1:nlevels), colormap=colormap)
        else
            call mpl_contour_filled(x, y, z, colormap=colormap)
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
        
        call mpl_pcolormesh(x, y, c, colormap=colormap)
    end subroutine pcolormesh

    subroutine streamplot(x, y, u, v, nx, ny, density, arrowsize, arrowstyle)
        !! Python-accessible streamplot function with arrow support
        integer, intent(in) :: nx, ny
        real(wp), dimension(nx), intent(in) :: x
        real(wp), dimension(ny), intent(in) :: y
        real(wp), dimension(nx, ny), intent(in) :: u, v
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: arrowsize
        character(len=*), intent(in), optional :: arrowstyle
        
        call mpl_streamplot(x, y, u, v, density=density, arrow_scale=arrowsize)
    end subroutine streamplot

    subroutine legend()
        !! Python-accessible legend function
        call mpl_legend()
    end subroutine legend

    subroutine set_xscale(scale)
        !! Python-accessible x-axis scale function
        character(len=*), intent(in) :: scale
        call mpl_set_xscale(scale)
    end subroutine set_xscale

    subroutine set_yscale(scale)
        !! Python-accessible y-axis scale function
        character(len=*), intent(in) :: scale
        call mpl_set_yscale(scale)
    end subroutine set_yscale

    subroutine xlim(xmin, xmax)
        !! Python-accessible x-axis limits function
        real(wp), intent(in) :: xmin, xmax
        call mpl_xlim(xmin, xmax)
    end subroutine xlim

    subroutine ylim(ymin, ymax)
        !! Python-accessible y-axis limits function
        real(wp), intent(in) :: ymin, ymax
        call mpl_ylim(ymin, ymax)
    end subroutine ylim

    subroutine scatter(x, y, n, s, c, label, marker, markersize, color, &
                      colormap, vmin, vmax, show_colorbar)
        !! Python-accessible scatter plot function
        !!
        !! Arguments:
        !!   x, y: Data arrays for scatter plot points
        !!   n: Array size
        !!   s: Optional marker sizes
        !!   c: Optional color values
        !!   label: Optional label for legend
        !!   marker: Optional marker style
        !!   markersize: Optional marker size
        !!   color: Optional RGB color
        !!   colormap: Optional colormap name
        !!   vmin, vmax: Optional color scale limits
        !!   show_colorbar: Optional colorbar display flag
        integer, intent(in) :: n
        real(wp), dimension(n), intent(in) :: x, y
        real(wp), dimension(:), intent(in), optional :: s, c
        character(len=*), intent(in), optional :: label, marker, colormap
        real(wp), intent(in), optional :: markersize, vmin, vmax
        real(wp), dimension(3), intent(in), optional :: color
        logical, intent(in), optional :: show_colorbar
        
        call mpl_scatter(x, y, s=s, c=c, label=label, marker=marker, &
                        markersize=markersize, color=color, &
                        colormap=colormap, vmin=vmin, vmax=vmax, &
                        show_colorbar=show_colorbar)
    end subroutine scatter

    subroutine histogram(data, n, bins, density, label, color)
        !! Python-accessible histogram function
        !!
        !! Arguments:
        !!   data: Data array for histogram
        !!   n: Array size
        !!   bins: Optional number of bins
        !!   density: Optional density normalization
        !!   label: Optional label for legend
        !!   color: Optional RGB color
        integer, intent(in) :: n
        real(wp), dimension(n), intent(in) :: data
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), dimension(3), intent(in), optional :: color
        
        call mpl_histogram(data, bins=bins, density=density, label=label, color=color)
    end subroutine histogram

end module fortplot_python_interface