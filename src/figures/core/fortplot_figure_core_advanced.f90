module fortplot_figure_core_advanced
    !! Advanced plotting operations extracted from fortplot_figure_core
    !! 
    !! This module contains advanced plotting functionality like scatter plots,
    !! histograms, and statistical plots that were moved from the core module
    !! to maintain architectural compliance with size limits.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_operations
    use fortplot_figure_core_ranges, only: update_data_ranges_figure
    implicit none

    private
    public :: core_scatter, core_hist, core_boxplot

contains

    subroutine core_scatter(plots, state, plot_count, x, y, s, c, marker, markersize, &
                           color, colormap, alpha, edgecolor, facecolor, linewidth, &
                           vmin, vmax, label, show_colorbar, default_color)
        !! Add an efficient scatter plot using a single plot object
        !! Properly handles thousands of points without O(n) overhead
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), c(:)
        character(len=*), intent(in), optional :: marker, colormap, label
        real(wp), intent(in), optional :: markersize, alpha, linewidth, vmin, vmax
        real(wp), intent(in), optional :: color(3), edgecolor(3), facecolor(3)
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in) :: default_color(3)
        
        ! Delegate to efficient scatter implementation
        call figure_scatter_operation(plots, state%plot_count, &
                                     x, y, s, c, marker, markersize, color, &
                                     colormap, alpha, edgecolor, facecolor, &
                                     linewidth, vmin, vmax, label, show_colorbar, &
                                     default_color)
        
        ! Update figure state
        plot_count = state%plot_count
        
        ! Update data ranges
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_scatter

    subroutine core_hist(plots, state, plot_count, data, bins, density, label, color)
        !! Create a histogram plot
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        call figure_hist_operation(plots, state, plot_count, data, bins, density, label, color)
    end subroutine core_hist

    subroutine core_boxplot(plots, plot_count, data, position, width, label, &
                           show_outliers, horizontal, color, max_plots)
        !! Create a box plot
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        character(len=*), intent(in), optional :: color
        integer, intent(in) :: max_plots
        
        call figure_boxplot_operation(plots, plot_count, data, position, width, label, &
                                     show_outliers, horizontal, color, max_plots)
    end subroutine core_boxplot

end module fortplot_figure_core_advanced