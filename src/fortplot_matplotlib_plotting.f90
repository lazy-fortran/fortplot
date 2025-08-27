module fortplot_matplotlib_plotting
    !! Plotting functions for matplotlib-compatible API
    !! Contains basic plotting operations (plot, scatter, bar, hist, etc.)
    
    use iso_fortran_env, only: wp => real64
    use fortplot_figure_core, only: figure_t
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error, log_warning, log_info
    
    implicit none
    private
    
    ! Export plotting functions
    public :: plot, scatter, errorbar, boxplot
    public :: bar, barh
    public :: hist, histogram
    public :: add_plot, add_errorbar, add_scatter
    public :: add_3d_plot
    
contains

    subroutine ensure_fig_init()
        !! Internal helper to ensure global figure is initialized
        if (.not. allocated(fig)) then
            allocate(figure_t :: fig)
        end if
        if (.not. fig%backend_associated()) then
            call fig%initialize()
        end if
    end subroutine ensure_fig_init

    subroutine plot(x, y, label, linestyle)
        !! Add a line plot to the global figure (pyplot-fortran compatible)
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle
        
        call ensure_fig_init()
        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine plot

    subroutine errorbar(x, y, xerr, yerr, fmt, label, capsize, linestyle, marker, color)
        !! Add an errorbar plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: xerr, yerr
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker, color
        real(8), intent(in), optional :: capsize
        
        call ensure_fig_init()
        ! Errorbar not fully implemented yet - use regular plot
        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine errorbar

    subroutine bar(x, height, width, bottom, label, color, edgecolor, align)
        !! Add a bar plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: x, height
        real(8), intent(in), optional :: width, bottom
        character(len=*), intent(in), optional :: label, color, edgecolor, align
        
        real(8) :: bar_width, bar_bottom
        character(len=32) :: bar_align
        
        call ensure_fig_init()
        
        bar_width = 0.8d0
        if (present(width)) bar_width = width
        
        bar_bottom = 0.0d0
        if (present(bottom)) bar_bottom = bottom
        
        bar_align = 'center'
        if (present(align)) bar_align = align
        
        ! Bar plot not yet implemented in figure_core - use line plot as placeholder
        call fig%add_plot(x, height, label=label)
    end subroutine bar

    subroutine barh(y, width, height, left, label, color, edgecolor, align)
        !! Add a horizontal bar plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: y, width
        real(8), intent(in), optional :: height, left
        character(len=*), intent(in), optional :: label, color, edgecolor, align
        
        real(8) :: bar_height, bar_left
        character(len=32) :: bar_align
        
        call ensure_fig_init()
        
        bar_height = 0.8d0
        if (present(height)) bar_height = height
        
        bar_left = 0.0d0
        if (present(left)) bar_left = left
        
        bar_align = 'center'
        if (present(align)) bar_align = align
        
        ! Horizontal bar plot not yet implemented in figure_core - use line plot as placeholder
        call fig%add_plot(width, y, label=label)
    end subroutine barh

    subroutine hist(data, bins, density, label, color)
        !! Add a histogram to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: data
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label, color
        
        integer :: num_bins
        logical :: use_density
        
        call ensure_fig_init()
        
        num_bins = 10
        if (present(bins)) num_bins = bins
        
        use_density = .false.
        if (present(density)) use_density = density
        
        ! Validate input
        if (size(data) == 0) then
            call log_error("hist: Empty data array provided")
            return
        end if
        
        if (num_bins <= 0) then
            call log_error("hist: Invalid number of bins")
            return
        end if
        
        ! Forward to figure method
        call fig%hist(data, bins=num_bins, density=use_density, &
                     label=label)
    end subroutine hist

    subroutine histogram(data, bins, density, label, color)
        !! Alias for hist to match numpy.histogram naming
        real(8), dimension(:), intent(in) :: data
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label, color
        
        call hist(data, bins, density, label, color)
    end subroutine histogram

    subroutine boxplot(data, position, width, label, show_outliers, horizontal, color)
        !! Add a box-and-whisker plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: data
        real(8), intent(in), optional :: position, width
        character(len=*), intent(in), optional :: label, color
        logical, intent(in), optional :: show_outliers, horizontal
        
        real(8) :: box_position, box_width
        logical :: outliers, horiz
        
        call ensure_fig_init()
        
        box_position = 1.0d0
        if (present(position)) box_position = position
        
        box_width = 0.5d0
        if (present(width)) box_width = width
        
        outliers = .true.
        if (present(show_outliers)) outliers = show_outliers
        
        horiz = .false.
        if (present(horizontal)) horiz = horizontal
        
        ! Forward to figure method
        call fig%boxplot(data, position=box_position, width=box_width, &
                        label=label, show_outliers=outliers, &
                        horizontal=horiz, color=color)
    end subroutine boxplot

    subroutine scatter(x, y, s, c, label, marker, markersize, color, &
                      alpha, edgecolors, linewidth)
        !! Add a scatter plot to the global figure (pyplot-style)
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: s, c
        character(len=*), intent(in), optional :: label, marker, color, edgecolors
        real(8), intent(in), optional :: markersize, alpha, linewidth
        
        call ensure_fig_init()
        
        ! Validate input arrays
        if (size(x) /= size(y)) then
            call log_error("scatter: x and y arrays must have same size")
            return
        end if
        
        if (present(s)) then
            if (size(s) /= size(x) .and. size(s) /= 1) then
                call log_error("scatter: size array must match data or be scalar")
                return
            end if
        end if
        
        ! Scatter plot not yet implemented in figure_core - use line plot as placeholder
        call fig%add_plot(x, y, label=label, linestyle='none')
    end subroutine scatter

    subroutine add_scatter(x, y, s, c, label, marker, markersize, color, &
                          alpha, edgecolors, linewidth)
        !! Direct interface to figure's add_scatter method for testing
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: s, c
        character(len=*), intent(in), optional :: label, marker, color, edgecolors
        real(8), intent(in), optional :: markersize, alpha, linewidth
        
        call ensure_fig_init()
        ! Scatter plot not yet implemented in figure_core - use line plot as placeholder
        call fig%add_plot(x, y, label=label, linestyle='none')
    end subroutine add_scatter

    subroutine add_plot(x, y, label, linestyle)
        !! Direct interface to figure's add_plot method for testing
        real(8), dimension(:), intent(in) :: x, y
        character(len=*), intent(in), optional :: label, linestyle
        
        call ensure_fig_init()
        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine add_plot

    subroutine add_errorbar(x, y, xerr, yerr, fmt, label, capsize, linestyle, marker, color)
        !! Add an errorbar plot with error bars
        !! Provides direct access to figure's add_errorbar method
        real(8), dimension(:), intent(in) :: x, y
        real(8), dimension(:), intent(in), optional :: xerr, yerr
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker, color
        real(8), intent(in), optional :: capsize
        
        real(8), allocatable :: xerr_local(:), yerr_local(:)
        character(len=64) :: fmt_local, label_local, linestyle_local
        character(len=64) :: marker_local, color_local
        real(8) :: capsize_local
        integer :: i, n
        
        call ensure_fig_init()
        
        n = size(x)
        
        ! Validate input arrays
        if (size(y) /= n) then
            call log_error("add_errorbar: x and y arrays must have same size")
            return
        end if
        
        ! Handle error arrays
        if (present(xerr)) then
            if (size(xerr) /= n .and. size(xerr) /= 1) then
                call log_error("add_errorbar: xerr must match data size or be scalar")
                return
            end if
            allocate(xerr_local(n))
            if (size(xerr) == 1) then
                xerr_local = xerr(1)
            else
                xerr_local = xerr
            end if
        end if
        
        if (present(yerr)) then
            if (size(yerr) /= n .and. size(yerr) /= 1) then
                call log_error("add_errorbar: yerr must match data size or be scalar")
                return
            end if
            allocate(yerr_local(n))
            if (size(yerr) == 1) then
                yerr_local = yerr(1)
            else
                yerr_local = yerr
            end if
        end if
        
        ! Set default values
        fmt_local = ''
        if (present(fmt)) fmt_local = fmt
        
        label_local = ''
        if (present(label)) label_local = label
        
        linestyle_local = '-'
        if (present(linestyle)) linestyle_local = linestyle
        
        marker_local = 'o'
        if (present(marker)) marker_local = marker
        
        color_local = 'auto'
        if (present(color)) color_local = color
        
        capsize_local = 3.0d0
        if (present(capsize)) capsize_local = capsize
        
        ! Errorbar plot not yet fully implemented in figure_core - use line plot as placeholder  
        call fig%add_plot(x, y, label=label_local, linestyle=linestyle_local)
    end subroutine add_errorbar

    subroutine add_3d_plot(x, y, z, label, linestyle, color, linewidth, marker, markersize)
        !! Add a 3D line plot with optional markers
        !! Provides direct access to figure's 3D plotting capabilities
        real(8), dimension(:), intent(in) :: x, y, z
        character(len=*), intent(in), optional :: label, linestyle, color, marker
        real(8), intent(in), optional :: linewidth, markersize
        
        character(len=64) :: label_local, linestyle_local, color_local, marker_local
        real(8) :: linewidth_local, markersize_local
        integer :: n
        
        call ensure_fig_init()
        
        ! Validate input arrays
        n = size(x)
        if (size(y) /= n .or. size(z) /= n) then
            call log_error("add_3d_plot: x, y, and z arrays must have same size")
            return
        end if
        
        if (n < 2) then
            call log_error("add_3d_plot: need at least 2 points for a line plot")
            return
        end if
        
        ! Set default values
        label_local = ''
        if (present(label)) label_local = label
        
        linestyle_local = '-'
        if (present(linestyle)) linestyle_local = linestyle
        
        color_local = 'auto'
        if (present(color)) color_local = color
        
        marker_local = 'none'
        if (present(marker)) marker_local = marker
        
        linewidth_local = 1.0d0
        if (present(linewidth)) linewidth_local = linewidth
        
        markersize_local = 6.0d0
        if (present(markersize)) markersize_local = markersize
        
        ! 3D plot not yet implemented in figure_core - use 2D projection as placeholder
        call fig%add_plot(x, y, label=label_local, linestyle=linestyle_local)
    end subroutine add_3d_plot

end module fortplot_matplotlib_plotting