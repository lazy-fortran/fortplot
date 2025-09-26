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
    use fortplot_figure_core_compat
    use fortplot_figure_core_config, only: grid_figure, set_xlabel_figure, set_ylabel_figure, &
                                           set_title_figure, set_xscale_figure, set_yscale_figure, &
                                           set_xlim_figure, set_ylim_figure, set_line_width_figure
    use fortplot_figure_plot_management, only: update_plot_ydata, setup_figure_legend
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges, &
                                                    setup_coordinate_system, &
                                                    render_figure_background, &
                                                    render_figure_axes, &
                                                    render_all_plots, &
                                                    render_figure_axes_labels_only
    use fortplot_figure_grid, only: render_grid_lines
    use fortplot_annotation_rendering, only: render_figure_annotations
    use fortplot_figure_aspect, only: contains_pie_plot, enforce_pie_axis_equal
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

    subroutine figure_legend_operation(legend_data, show_legend, plots, plot_count, location)
        !! Add legend to figure
        type(legend_t), intent(inout) :: legend_data
        logical, intent(inout) :: show_legend
        type(plot_data_t), intent(in) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in), optional :: location
        
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

    subroutine figure_render(state, plots, plot_count, annotations, annotation_count)
        !! Main rendering pipeline using focused modules
        !! Fixed Issue #432: Always render axes/labels even with no plot data
        !! Fixed Issue #844: ASCII annotation functionality
        use fortplot_annotations, only: text_annotation_t
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        type(text_annotation_t), intent(in), optional :: annotations(:)
        integer, intent(in), optional :: annotation_count
        real(wp) :: x_min_dummy, x_max_dummy
        real(wp) :: y_min_dummy, y_max_dummy
        real(wp) :: twinx_y_min, twinx_y_max
        real(wp) :: twinx_y_min_trans, twinx_y_max_trans
        real(wp) :: twiny_x_min, twiny_x_max
        real(wp) :: twiny_x_min_trans, twiny_x_max_trans

        ! Calculate final data ranges
        call calculate_figure_data_ranges(plots, plot_count, &
                                        state%xlim_set, state%ylim_set, &
                                        state%x_min, state%x_max, &
                                        state%y_min, state%y_max, &
                                        state%x_min_transformed, &
                                        state%x_max_transformed, &
                                        state%y_min_transformed, &
                                        state%y_max_transformed, &
                                        state%xscale, state%yscale, &
                                        state%symlog_threshold, axis_filter=AXIS_PRIMARY)

        if (state%has_twinx) then
            x_min_dummy = state%x_min
            x_max_dummy = state%x_max
            twinx_y_min = state%twinx_y_min
            twinx_y_max = state%twinx_y_max
            call calculate_figure_data_ranges(plots, plot_count, &
                                            xlim_set=.true., &
                                            ylim_set=state%twinx_ylim_set, &
                                            x_min=x_min_dummy, x_max=x_max_dummy, &
                                            y_min=twinx_y_min, y_max=twinx_y_max, &
                                            x_min_transformed=x_min_dummy, &
                                            x_max_transformed=x_max_dummy, &
                                            y_min_transformed=twinx_y_min_trans, &
                                            y_max_transformed=twinx_y_max_trans, &
                                            xscale=state%xscale, &
                                            yscale=state%twinx_yscale, &
                                            symlog_threshold=state%symlog_threshold, &
                                            axis_filter=AXIS_TWINX)
            state%twinx_y_min = twinx_y_min
            state%twinx_y_max = twinx_y_max
            state%twinx_y_min_transformed = twinx_y_min_trans
            state%twinx_y_max_transformed = twinx_y_max_trans
        end if

        if (state%has_twiny) then
            twiny_x_min = state%twiny_x_min
            twiny_x_max = state%twiny_x_max
            y_min_dummy = state%y_min
            y_max_dummy = state%y_max
            call calculate_figure_data_ranges(plots, plot_count, &
                                            xlim_set=state%twiny_xlim_set, &
                                            ylim_set=.true., &
                                            x_min=twiny_x_min, x_max=twiny_x_max, &
                                            y_min=y_min_dummy, y_max=y_max_dummy, &
                                            x_min_transformed=twiny_x_min_trans, &
                                            x_max_transformed=twiny_x_max_trans, &
                                            y_min_transformed=y_min_dummy, &
                                            y_max_transformed=y_max_dummy, &
                                            xscale=state%twiny_xscale, &
                                            yscale=state%yscale, &
                                            symlog_threshold=state%symlog_threshold, &
                                            axis_filter=AXIS_TWINY)
            state%twiny_x_min = twiny_x_min
            state%twiny_x_max = twiny_x_max
            state%twiny_x_min_transformed = twiny_x_min_trans
            state%twiny_x_max_transformed = twiny_x_max_trans
        end if

        if (contains_pie_plot(plots, plot_count)) then
            call enforce_pie_axis_equal(state)
        end if
        
        ! Setup coordinate system
        call setup_coordinate_system(state%backend, &
                                   state%x_min_transformed, state%x_max_transformed, &
                                   state%y_min_transformed, state%y_max_transformed)
        
        ! Render background
        call render_figure_background(state%backend)
        
        ! Render grid if enabled
        if (state%grid_enabled) then
            call render_grid_lines(state%backend, state%grid_enabled, &
                                  state%grid_which, state%grid_axis, &
                                  state%grid_alpha, state%width, state%height, &
                                  state%margin_left, state%margin_right, &
                                  state%margin_bottom, state%margin_top, &
                                  state%xscale, state%yscale, &
                                  state%symlog_threshold, state%x_min, state%x_max, &
                                  state%y_min, state%y_max, &
                                  state%x_min_transformed, state%x_max_transformed, &
                                  state%y_min_transformed, state%y_max_transformed, &
                                  state%grid_linestyle)
        end if
        
        ! Render axes
        call render_figure_axes(state%backend, state%xscale, state%yscale, &
                               state%symlog_threshold, state%x_min, state%x_max, &
                               state%y_min, state%y_max, state%title, &
                               state%xlabel, state%ylabel, plots, plot_count, &
                               has_twinx=state%has_twinx, twinx_y_min=state%twinx_y_min, &
                               twinx_y_max=state%twinx_y_max, twinx_ylabel=state%twinx_ylabel, &
                               twinx_yscale=state%twinx_yscale, has_twiny=state%has_twiny, &
                               twiny_x_min=state%twiny_x_min, twiny_x_max=state%twiny_x_max, &
                               twiny_xlabel=state%twiny_xlabel, twiny_xscale=state%twiny_xscale)
        
        ! Render all plots (only if there are plots to render)
        if (plot_count > 0) then
            call render_all_plots(state%backend, plots, plot_count, &
                                 state%x_min_transformed, state%x_max_transformed, &
                                 state%y_min_transformed, state%y_max_transformed, &
                                 state%xscale, state%yscale, state%symlog_threshold, &
                                 state%width, state%height, &
                                 state%margin_left, state%margin_right, &
                                 state%margin_bottom, state%margin_top, state=state)
        end if
        
        ! Render axis labels AFTER plots (for raster backends only to prevent overlap)
        call render_figure_axes_labels_only(state%backend, state%xscale, state%yscale, &
                                           state%symlog_threshold, state%x_min, state%x_max, &
                                           state%y_min, state%y_max, state%title, &
                                           state%xlabel, state%ylabel, plots, plot_count, &
                                           has_twinx=state%has_twinx, twinx_y_min=state%twinx_y_min, &
                                           twinx_y_max=state%twinx_y_max, twinx_ylabel=state%twinx_ylabel, &
                                           twinx_yscale=state%twinx_yscale, has_twiny=state%has_twiny, &
                                           twiny_x_min=state%twiny_x_min, twiny_x_max=state%twiny_x_max, &
                                           twiny_xlabel=state%twiny_xlabel, twiny_xscale=state%twiny_xscale)
        
        ! Render legend if requested
        if (state%show_legend .and. state%legend_data%num_entries > 0) then
            call state%legend_data%render(state%backend)
        end if
        
        ! Render annotations if any exist (Issue #844: ASCII annotation functionality)
        if (present(annotations) .and. present(annotation_count)) then
            if (annotation_count > 0) then
                call render_figure_annotations(state%backend, annotations, annotation_count, &
                                              state%x_min, state%x_max, &
                                              state%y_min, state%y_max, &
                                              state%width, state%height, &
                                              state%margin_left, state%margin_right, &
                                              state%margin_bottom, state%margin_top)
            end if
        end if
        
        state%rendered = .true.
    end subroutine figure_render

end module fortplot_figure_operations
