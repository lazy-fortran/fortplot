module fortplot_figure_operations
    !! Figure core operations module for fortplot_figure_core
    !! 
    !! This module handles core plotting operations, rendering pipeline,
    !! and plot data management. Extracted from fortplot_figure_core.f90 
    !! for better organization following Single Responsibility Principle.
    !!
    !! Responsibilities:
    !! - Plot operations (add_plot, add_contour, add_pcolormesh, etc.)
    !! - Axis configuration (set_xlim, set_ylim, set_xlabel, etc.)
    !! - Grid configuration
    !! - Legend management
    !! - Rendering pipeline coordination

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_plot_data, only: plot_data_t, AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_legend, only: legend_t
    use fortplot_figure_plots, only: figure_add_plot, figure_add_contour, &
                                     figure_add_contour_filled, figure_add_surface, &
                                     figure_add_pcolormesh, figure_add_fill_between, &
                                     figure_add_pie
    use fortplot_figure_histogram, only: hist_figure
    use fortplot_figure_streamlines, only: streamplot_figure
    use fortplot_figure_boxplot, only: add_boxplot
    use fortplot_figure_scatter, only: add_scatter_plot
    use fortplot_figure_core_config, only: grid_figure, set_xlabel_figure, set_ylabel_figure, &
                                           set_title_figure, set_xscale_figure, set_yscale_figure, &
                                           set_xlim_figure, set_ylim_figure, set_line_width_figure
    use fortplot_figure_plot_management, only: update_plot_ydata, setup_figure_legend
    use fortplot_figure_render_engine, only: figure_render
    implicit none

    private
    public :: figure_add_plot_operation, figure_add_contour_operation, figure_add_contour_filled_operation
    public :: figure_add_surface_operation, figure_add_pcolormesh_operation, figure_add_fill_between_operation
    public :: figure_add_pie_operation
    public :: figure_streamplot_operation, figure_hist_operation
    public :: figure_boxplot_operation, figure_scatter_operation, figure_set_xlabel_operation
    public :: figure_set_ylabel_operation, figure_set_title_operation, figure_set_xscale_operation
    public :: figure_set_yscale_operation, figure_set_xlim_operation, figure_set_ylim_operation
    public :: figure_set_line_width_operation, figure_set_ydata_operation, figure_legend_operation
    public :: figure_grid_operation, figure_render

contains

    subroutine set_axis_for_latest_plot(state, plots)
        !! Assign the current active axis to the newest plot entry
        type(figure_state_t), intent(in) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer :: idx

        if (state%plot_count <= 0) return
        if (size(plots) == 0) return

        idx = state%plot_count
        if (idx > size(plots)) return

        plots(idx)%axis = state%active_axis
    end subroutine set_axis_for_latest_plot

    subroutine figure_add_plot_operation(plots, state, x, y, label, linestyle, color)
        !! Add a line plot to the figure
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        
        call figure_add_plot(plots, state, x, y, label, linestyle, color)
        call set_axis_for_latest_plot(state, plots)
    end subroutine figure_add_plot_operation

    subroutine figure_add_contour_operation(plots, state, x_grid, y_grid, z_grid, levels, label)
        !! Add a contour plot to the figure
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        call figure_add_contour(plots, state, x_grid, y_grid, z_grid, levels, label)
        call set_axis_for_latest_plot(state, plots)
    end subroutine figure_add_contour_operation

    subroutine figure_add_contour_filled_operation(plots, state, x_grid, y_grid, z_grid, &
                                                  levels, colormap, show_colorbar, label)
        !! Add a filled contour plot with color mapping
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar

        call figure_add_contour_filled(plots, state, x_grid, y_grid, z_grid, &
                                      levels, colormap, show_colorbar, label)
        call set_axis_for_latest_plot(state, plots)
    end subroutine figure_add_contour_filled_operation

    subroutine figure_add_surface_operation(plots, state, x_grid, y_grid, z_grid, label, &
                                            colormap, show_colorbar, alpha, edgecolor, linewidth)
        !! Add a 3D surface plot to the figure
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        character(len=*), intent(in), optional :: label, colormap
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        call figure_add_surface(plots, state, x_grid, y_grid, z_grid, label, colormap, &
                                show_colorbar, alpha, edgecolor, linewidth)
        call set_axis_for_latest_plot(state, plots)
    end subroutine figure_add_surface_operation

    subroutine figure_add_pcolormesh_operation(plots, state, x, y, c, colormap, &
                                              vmin, vmax, edgecolors, linewidths)
        !! Add a pcolormesh plot
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        
        call figure_add_pcolormesh(plots, state, x, y, c, colormap, &
                                  vmin, vmax, edgecolors, linewidths)
        call set_axis_for_latest_plot(state, plots)
    end subroutine figure_add_pcolormesh_operation

    subroutine figure_add_fill_between_operation(plots, state, x, upper, lower, mask, &
                                                color_string, alpha)
        !! Add an area fill between two curves
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: upper(:)
        real(wp), intent(in) :: lower(:)
        logical, intent(in), optional :: mask(:)
        character(len=*), intent(in), optional :: color_string
        real(wp), intent(in), optional :: alpha

        call figure_add_fill_between(plots, state, x, upper, lower, mask, color_string, alpha)
        call set_axis_for_latest_plot(state, plots)
    end subroutine figure_add_fill_between_operation

    subroutine figure_add_pie_operation(plots, state, values, labels, startangle, color_strings, explode, autopct)
        !! Add a pie chart to the figure
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:)
        real(wp), intent(in), optional :: startangle
        character(len=*), intent(in), optional :: color_strings(:)
        real(wp), intent(in), optional :: explode(:)
        character(len=*), intent(in), optional :: autopct

        call figure_add_pie(plots, state, values, labels, startangle, color_strings, explode, autopct)
        call set_axis_for_latest_plot(state, plots)
    end subroutine figure_add_pie_operation

    subroutine figure_streamplot_operation(plots, state, plot_count, x, y, u, v, &
                                          density, color)
        !! Add streamline plot to figure using basic algorithm
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        
        call streamplot_figure(plots, state, plot_count, x, y, u, v, &
                               density, color)
        call set_axis_for_latest_plot(state, plots)
    end subroutine figure_streamplot_operation

    subroutine figure_hist_operation(plots, state, plot_count, data, bins, density, label, color)
        !! Create a histogram plot
        type(plot_data_t), intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        call hist_figure(plots, state, plot_count, data, bins, density, label, color)
        call set_axis_for_latest_plot(state, plots)
    end subroutine figure_hist_operation

    subroutine figure_boxplot_operation(state, plots, plot_count, data, position, width, label, &
                                       show_outliers, horizontal, color, max_plots)
        !! Create a box plot
        type(figure_state_t), intent(in) :: state
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

        call add_boxplot(plots, plot_count, data, position, width, label, &
                         show_outliers, horizontal, color, max_plots)
        if (plot_count > 0 .and. size(plots) >= plot_count) then
            plots(plot_count)%axis = state%active_axis
        end if
    end subroutine figure_boxplot_operation

    subroutine figure_scatter_operation(state, plots, plot_count, x, y, s, c, marker, &
                                       markersize, color, colormap, vmin, vmax, &
                                       label, show_colorbar, default_color)
        !! Add an efficient scatter plot using a single plot object
        type(figure_state_t), intent(in) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), c(:)
        character(len=*), intent(in), optional :: marker, colormap, label
        real(wp), intent(in), optional :: markersize, vmin, vmax
        real(wp), intent(in), optional :: color(3)
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in) :: default_color(3)

        call add_scatter_plot(plots, plot_count, x, y, s, c, marker, markersize, &
                             color, colormap, vmin, vmax, label, show_colorbar, &
                             default_color)
        if (plot_count > 0 .and. size(plots) >= plot_count) then
            plots(plot_count)%axis = state%active_axis
        end if
    end subroutine figure_scatter_operation

    subroutine figure_set_xlabel_operation(state, xlabel_target, label)
        !! Set x-axis label
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: xlabel_target
        character(len=*), intent(in) :: label
        call set_xlabel_figure(state, xlabel_target, label)
    end subroutine figure_set_xlabel_operation

    subroutine figure_set_ylabel_operation(state, ylabel_target, label)
        !! Set y-axis label
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: ylabel_target
        character(len=*), intent(in) :: label
        call set_ylabel_figure(state, ylabel_target, label)
    end subroutine figure_set_ylabel_operation

    subroutine figure_set_title_operation(state, title_target, title)
        !! Set figure title
        type(figure_state_t), intent(inout) :: state
        character(len=:), allocatable, intent(inout) :: title_target
        character(len=*), intent(in) :: title
        call set_title_figure(state, title_target, title)
    end subroutine figure_set_title_operation

    subroutine figure_set_xscale_operation(state, scale, threshold)
        !! Set x-axis scale type
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        call set_xscale_figure(state, scale, threshold)
    end subroutine figure_set_xscale_operation

    subroutine figure_set_yscale_operation(state, scale, threshold)
        !! Set y-axis scale type
        type(figure_state_t), intent(inout) :: state
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        call set_yscale_figure(state, scale, threshold)
    end subroutine figure_set_yscale_operation

    subroutine figure_set_xlim_operation(state, x_min, x_max)
        !! Set x-axis limits
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_min, x_max
        
        call set_xlim_figure(state, x_min, x_max)
    end subroutine figure_set_xlim_operation

    subroutine figure_set_ylim_operation(state, y_min, y_max)
        !! Set y-axis limits
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: y_min, y_max
        
        call set_ylim_figure(state, y_min, y_max)
    end subroutine figure_set_ylim_operation

    subroutine figure_set_line_width_operation(state, width)
        !! Set line width for subsequent plots
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: width
        
        call set_line_width_figure(state, width)
    end subroutine figure_set_line_width_operation

    subroutine figure_set_ydata_operation(plots, plot_count, plot_index, y_new)
        !! Update y data for an existing plot
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        integer, intent(in) :: plot_index
        real(wp), intent(in) :: y_new(:)
        
        call update_plot_ydata(plots, plot_count, plot_index, y_new)
    end subroutine figure_set_ydata_operation

    subroutine figure_legend_operation(legend_data, show_legend, plots, plot_count, location, backend_name)
        !! Add legend to figure
        type(legend_t), intent(inout) :: legend_data
        logical, intent(inout) :: show_legend
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in), optional :: location
        character(len=*), intent(in) :: backend_name
        
        call setup_figure_legend(legend_data, show_legend, plots, plot_count, location)
    end subroutine figure_legend_operation

    subroutine figure_grid_operation(state, enabled, which, axis, alpha, linestyle)
        !! Enable/disable and configure grid lines
        type(figure_state_t), intent(inout) :: state
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha
        
        call grid_figure(state, enabled, which, axis, alpha, linestyle)
    end subroutine figure_grid_operation

end module fortplot_figure_operations
