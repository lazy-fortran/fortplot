module fortplot_python_interface
    !! Unified Python interface module supporting both F2PY and bridge modes
    !!
    !! This module provides a complete interface for Python bindings that works with:
    !! 1. Direct F2PY compilation for high-performance native binding
    !! 2. Subprocess bridge mode for flexibility and ease of distribution
    !!
    !! All plotting functionality is exposed through a consistent API that works
    !! identically regardless of the binding method used.

    use iso_fortran_env, only: wp => real64, input_unit, output_unit, error_unit
    use fortplot_matplotlib, only: mpl_show => show, mpl_show_viewer => show_viewer, &
                                  mpl_figure => figure, mpl_plot => plot, &
                                  mpl_savefig => savefig, mpl_title => title, &
                                  mpl_xlabel => xlabel, mpl_ylabel => ylabel, &
                                  mpl_contour => contour, mpl_contour_filled => contour_filled, &
                                  mpl_pcolormesh => pcolormesh, mpl_streamplot => streamplot, &
                                  mpl_legend => legend, mpl_set_xscale => set_xscale, &
                                  mpl_set_yscale => set_yscale, mpl_xlim => xlim, mpl_ylim => ylim, &
                                  mpl_scatter => scatter, mpl_histogram => histogram, &
                                  mpl_hist => hist
    implicit none

    private
    
    ! Export functions for F2PY - use simple names for Python access
    public :: show_figure, show_viewer, figure, plot, savefig
    public :: title, xlabel, ylabel, contour, contour_filled
    public :: pcolormesh, streamplot, legend, scatter, histogram
    public :: set_xscale, set_yscale, xlim, ylim
    
    ! Bridge mode support - process commands from stdin
    public :: run_bridge_mode

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
        !!   width, height: Optional figure dimensions in pixels (default: 640x480)
        integer, intent(in), optional :: width, height
        
        real(8), dimension(2) :: figsize
        real(8), parameter :: DPI = 100.0d0
        
        if (present(width) .and. present(height)) then
            ! Convert pixel dimensions to inches for matplotlib compatibility
            ! Python passes pixels, but mpl_figure expects inches
            figsize = [real(width, 8) / DPI, real(height, 8) / DPI]
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

    subroutine run_bridge_mode()
        !! Run in bridge mode processing commands from stdin
        !! This provides an alternative to F2PY for Python integration
        !! Commands are read from stdin and executed in a loop
        
        character(len=256) :: command
        real(wp), allocatable :: x_data(:), y_data(:), data_array(:)
        integer :: ios
        
        ! Main command processing loop
        do
            read(input_unit, '(A)', iostat=ios) command
            if (ios /= 0) exit
            
            command = adjustl(trim(command))
            if (len_trim(command) == 0) cycle
            
            select case (trim(command))
            case ('FIGURE')
                call process_figure_cmd()
            case ('PLOT')
                call process_plot_cmd(x_data, y_data)
            case ('SCATTER')
                call process_scatter_cmd(x_data, y_data)
            case ('HISTOGRAM')
                call process_histogram_cmd(data_array)
            case ('TITLE')
                call process_title_cmd()
            case ('XLABEL')
                call process_xlabel_cmd()
            case ('YLABEL')
                call process_ylabel_cmd()
            case ('LEGEND')
                call mpl_legend()
            case ('SAVEFIG')
                call process_savefig_cmd()
            case ('SHOW')
                call process_show_cmd()
            case ('XLIM')
                call process_xlim_cmd()
            case ('YLIM')
                call process_ylim_cmd()
            case ('XSCALE')
                call process_xscale_cmd()
            case ('YSCALE')
                call process_yscale_cmd()
            case ('QUIT', 'EXIT')
                exit
            case default
                ! Unknown command, skip silently
                cycle
            end select
        end do
        
        ! Cleanup
        if (allocated(x_data)) deallocate(x_data)
        if (allocated(y_data)) deallocate(y_data)
        if (allocated(data_array)) deallocate(data_array)
        
    contains
        
        subroutine process_figure_cmd()
            integer :: width, height, ios
            read(input_unit, *, iostat=ios) width, height
            if (ios == 0) then
                call figure(width, height)
            else
                call mpl_figure()
            end if
        end subroutine
        
        subroutine process_plot_cmd(x_arr, y_arr)
            real(wp), allocatable, intent(inout) :: x_arr(:), y_arr(:)
            character(len=256) :: label_str, style_str
            integer :: n, i, ios
            
            read(input_unit, *, iostat=ios) n
            if (ios /= 0) return
            
            if (allocated(x_arr)) deallocate(x_arr)
            if (allocated(y_arr)) deallocate(y_arr)
            allocate(x_arr(n), y_arr(n))
            
            ! Read x values
            do i = 1, n
                read(input_unit, *, iostat=ios) x_arr(i)
                if (ios /= 0) return
            end do
            
            ! Read y values
            do i = 1, n
                read(input_unit, *, iostat=ios) y_arr(i)
                if (ios /= 0) return
            end do
            
            ! Read optional label
            read(input_unit, '(A)', iostat=ios) label_str
            if (ios /= 0) label_str = ''
            
            ! Read optional linestyle
            read(input_unit, '(A)', iostat=ios) style_str
            if (ios /= 0) style_str = '-'
            
            if (len_trim(label_str) > 0 .and. len_trim(style_str) > 0) then
                call plot(x_arr, y_arr, n, trim(label_str), trim(style_str))
            else if (len_trim(label_str) > 0) then
                call plot(x_arr, y_arr, n, label=trim(label_str))
            else
                call plot(x_arr, y_arr, n)
            end if
        end subroutine
        
        subroutine process_scatter_cmd(x_arr, y_arr)
            real(wp), allocatable, intent(inout) :: x_arr(:), y_arr(:)
            character(len=256) :: label_str
            integer :: n, i, ios
            
            read(input_unit, *, iostat=ios) n
            if (ios /= 0) return
            
            if (allocated(x_arr)) deallocate(x_arr)
            if (allocated(y_arr)) deallocate(y_arr)
            allocate(x_arr(n), y_arr(n))
            
            ! Read x values
            do i = 1, n
                read(input_unit, *, iostat=ios) x_arr(i)
                if (ios /= 0) return
            end do
            
            ! Read y values  
            do i = 1, n
                read(input_unit, *, iostat=ios) y_arr(i)
                if (ios /= 0) return
            end do
            
            ! Read optional label
            read(input_unit, '(A)', iostat=ios) label_str
            
            if (ios == 0 .and. len_trim(label_str) > 0) then
                call scatter(x_arr, y_arr, n, label=trim(label_str))
            else
                call scatter(x_arr, y_arr, n)
            end if
        end subroutine
        
        subroutine process_histogram_cmd(data_arr)
            real(wp), allocatable, intent(inout) :: data_arr(:)
            character(len=256) :: label_str
            integer :: n, i, bins_val, ios
            logical :: density_flag
            
            read(input_unit, *, iostat=ios) n
            if (ios /= 0) return
            
            if (allocated(data_arr)) deallocate(data_arr)
            allocate(data_arr(n))
            
            ! Read data values
            do i = 1, n
                read(input_unit, *, iostat=ios) data_arr(i)
                if (ios /= 0) return
            end do
            
            ! Read optional bins
            read(input_unit, *, iostat=ios) bins_val
            if (ios /= 0) bins_val = 0
            
            ! Read optional density flag
            read(input_unit, *, iostat=ios) density_flag
            if (ios /= 0) density_flag = .false.
            
            ! Read optional label
            read(input_unit, '(A)', iostat=ios) label_str
            
            if (bins_val > 0 .and. ios == 0 .and. len_trim(label_str) > 0) then
                call histogram(data_arr, n, bins_val, density_flag, trim(label_str))
            else if (ios == 0 .and. len_trim(label_str) > 0) then
                call histogram(data_arr, n, label=trim(label_str))
            else
                ! Use mpl_hist for basic histogram
                call mpl_hist(data_arr)
            end if
        end subroutine
        
        subroutine process_title_cmd()
            character(len=256) :: title_str
            integer :: ios
            read(input_unit, '(A)', iostat=ios) title_str
            if (ios == 0) call mpl_title(trim(title_str))
        end subroutine
        
        subroutine process_xlabel_cmd()
            character(len=256) :: xlabel_str
            integer :: ios
            read(input_unit, '(A)', iostat=ios) xlabel_str
            if (ios == 0) call mpl_xlabel(trim(xlabel_str))
        end subroutine
        
        subroutine process_ylabel_cmd()
            character(len=256) :: ylabel_str
            integer :: ios
            read(input_unit, '(A)', iostat=ios) ylabel_str
            if (ios == 0) call mpl_ylabel(trim(ylabel_str))
        end subroutine
        
        subroutine process_savefig_cmd()
            character(len=256) :: filename_str
            integer :: ios
            read(input_unit, '(A)', iostat=ios) filename_str
            if (ios == 0) call mpl_savefig(trim(filename_str))
        end subroutine
        
        subroutine process_show_cmd()
            logical :: blocking_flag
            integer :: ios
            character :: blocking_char
            read(input_unit, '(A1)', iostat=ios) blocking_char
            if (ios == 0) then
                blocking_flag = (blocking_char == 'T' .or. blocking_char == 't')
                call mpl_show(blocking=blocking_flag)
            else
                call mpl_show()
            end if
        end subroutine
        
        subroutine process_xlim_cmd()
            real(wp) :: xmin_val, xmax_val
            integer :: ios
            read(input_unit, *, iostat=ios) xmin_val, xmax_val
            if (ios == 0) call mpl_xlim(xmin_val, xmax_val)
        end subroutine
        
        subroutine process_ylim_cmd()
            real(wp) :: ymin_val, ymax_val
            integer :: ios
            read(input_unit, *, iostat=ios) ymin_val, ymax_val
            if (ios == 0) call mpl_ylim(ymin_val, ymax_val)
        end subroutine
        
        subroutine process_xscale_cmd()
            character(len=256) :: scale_str
            integer :: ios
            read(input_unit, '(A)', iostat=ios) scale_str
            if (ios == 0) call mpl_set_xscale(trim(scale_str))
        end subroutine
        
        subroutine process_yscale_cmd()
            character(len=256) :: scale_str
            integer :: ios
            read(input_unit, '(A)', iostat=ios) scale_str
            if (ios == 0) call mpl_set_yscale(trim(scale_str))
        end subroutine
        
    end subroutine run_bridge_mode

end module fortplot_python_interface