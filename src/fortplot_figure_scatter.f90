module fortplot_figure_scatter
    !! Scatter plot implementation module
    !! 
    !! Provides efficient scatter plot functionality following SOLID principles.
    !! Uses single plot object for performance, not O(n) individual points.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_SCATTER
    use fortplot_logging, only: log_error, log_warning
    implicit none
    
    private
    public :: add_scatter_plot
    
contains
    
    subroutine add_scatter_plot(plots, plot_count, x, y, s, c, marker, &
                                markersize, color, colormap, alpha, &
                                edgecolor, facecolor, linewidth, &
                                vmin, vmax, label, show_colorbar, &
                                default_color)
        !! Add a single efficient scatter plot object
        !! Properly handles thousands of points with single plot object
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), c(:)
        character(len=*), intent(in), optional :: marker, colormap, label
        real(wp), intent(in), optional :: markersize, alpha, linewidth
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: color(3), edgecolor(3), facecolor(3)
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: default_color(3)
        
        type(plot_data_t), allocatable :: new_plots(:)
        integer :: n, i
        real(wp) :: actual_vmin, actual_vmax
        
        n = size(x)
        
        ! Validate inputs
        if (n == 0) then
            call log_warning("scatter: Empty arrays provided")
            return
        end if
        
        if (size(y) /= n) then
            call log_error("scatter: x and y arrays must have same size")
            return
        end if
        
        ! Resize plot array if needed
        if (plot_count >= size(plots)) then
            allocate(new_plots(size(plots) * 2))
            new_plots(1:plot_count) = plots(1:plot_count)
            call move_alloc(new_plots, plots)
        end if
        
        ! Create new scatter plot object (SINGLE object, not O(n))
        plot_count = plot_count + 1
        plots(plot_count)%plot_type = PLOT_TYPE_SCATTER
        
        ! Allocate and copy coordinate data
        allocate(plots(plot_count)%x(n), plots(plot_count)%y(n))
        plots(plot_count)%x = x
        plots(plot_count)%y = y
        
        ! Handle sizes (s parameter or markersize)
        call setup_scatter_sizes(plots(plot_count), n, s, markersize)
        
        ! Handle colors (c parameter or solid color)
        call setup_scatter_colors(plots(plot_count), n, c, color, &
                                 facecolor, default_color, vmin, vmax)
        
        ! Set colormap and colorbar options
        if (present(colormap)) then
            plots(plot_count)%scatter_colormap = colormap
        end if
        
        if (present(show_colorbar)) then
            plots(plot_count)%scatter_colorbar = show_colorbar
        else if (present(c)) then
            plots(plot_count)%scatter_colorbar = .true.  ! Default on if using color array
        end if
        
        ! Set marker style
        if (present(marker)) then
            plots(plot_count)%marker = marker
        else
            plots(plot_count)%marker = 'o'  ! Default circle
        end if
        
        ! Set label for legend
        if (present(label)) then
            plots(plot_count)%label = label
        else
            plots(plot_count)%label = ''
        end if
        
        ! Set linestyle to 'none' for scatter plots
        plots(plot_count)%linestyle = 'none'
        
    end subroutine add_scatter_plot
    
    subroutine setup_scatter_sizes(plot, n, s, markersize)
        !! Configure scatter plot marker sizes
        type(plot_data_t), intent(inout) :: plot
        integer, intent(in) :: n
        real(wp), intent(in), optional :: s(:), markersize
        
        if (present(s)) then
            if (size(s) == n) then
                allocate(plot%scatter_sizes(n))
                plot%scatter_sizes = s
            else if (size(s) == 1) then
                allocate(plot%scatter_sizes(n))
                plot%scatter_sizes = s(1)
            else
                call log_error("scatter: size array must match data or be scalar")
                plot%scatter_size_default = 20.0_wp
                return
            end if
        else if (present(markersize)) then
            plot%scatter_size_default = markersize
        else
            plot%scatter_size_default = 20.0_wp
        end if
    end subroutine setup_scatter_sizes
    
    subroutine setup_scatter_colors(plot, n, c, color, facecolor, &
                                   default_color, vmin, vmax)
        !! Configure scatter plot colors and color mapping
        type(plot_data_t), intent(inout) :: plot
        integer, intent(in) :: n
        real(wp), intent(in), optional :: c(:)
        real(wp), intent(in), optional :: color(3), facecolor(3), default_color(3)
        real(wp), intent(in), optional :: vmin, vmax
        
        real(wp) :: cmin, cmax
        
        if (present(c)) then
            if (size(c) == n) then
                allocate(plot%scatter_colors(n))
                plot%scatter_colors = c
                
                ! Set color scale range
                if (present(vmin) .and. present(vmax)) then
                    plot%scatter_vmin = vmin
                    plot%scatter_vmax = vmax
                    plot%scatter_vrange_set = .true.
                else
                    ! Auto-scale to data
                    cmin = minval(c)
                    cmax = maxval(c)
                    if (abs(cmax - cmin) < epsilon(1.0_wp)) then
                        plot%scatter_vmin = cmin - 0.5_wp
                        plot%scatter_vmax = cmax + 0.5_wp
                    else
                        plot%scatter_vmin = cmin
                        plot%scatter_vmax = cmax
                    end if
                    plot%scatter_vrange_set = .true.
                end if
            else if (size(c) == 1) then
                allocate(plot%scatter_colors(n))
                plot%scatter_colors = c(1)
                plot%scatter_vmin = c(1) - 0.5_wp
                plot%scatter_vmax = c(1) + 0.5_wp
                plot%scatter_vrange_set = .true.
            else
                call log_error("scatter: color array must match data or be scalar")
            end if
        else
            ! Use solid color
            if (present(color)) then
                plot%color = color
            else if (present(facecolor)) then
                plot%color = facecolor
            else if (present(default_color)) then
                plot%color = default_color
            else
                ! Default blue
                plot%color = [0.0_wp, 0.447_wp, 0.698_wp]
            end if
        end if
    end subroutine setup_scatter_colors
    
end module fortplot_figure_scatter