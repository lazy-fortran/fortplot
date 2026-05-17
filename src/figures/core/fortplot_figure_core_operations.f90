module fortplot_figure_core_operations
    !! Core operations implementations extracted from fortplot_figure_core
    !!
    !! This module contains the actual implementations of core figure operations
    !! that were previously inline procedures in the main core module.
    !!
    !! EXTRACTED OPERATIONS:
    !! - initialize: Figure initialization with backend setup
    !! - add_plot: Basic line plotting functionality
    !! - add_contour: Contour plot creation
    !! - add_contour_filled: Filled contour plots
    !! - add_pcolormesh: Pseudocolor mesh plotting
    !! - streamplot: Streamline visualization
    !! - savefig variants: File output operations
    !! - show: Figure display operations

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_annotations, only: text_annotation_t
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, subplot_data_t
    use fortplot_figure_initialization, only: figure_state_t, ensure_figure_storage
    use fortplot_figure_operations
    use fortplot_figure_management
    use fortplot_figure_core_ranges, only: update_data_ranges_figure, &
                                           update_data_ranges_pcolormesh_figure
    use fortplot_figure_quiver, only: quiver_figure
    implicit none

    private
    public :: core_initialize, core_add_plot, core_add_contour, core_add_contour_filled
    public :: core_add_surface, core_add_pcolormesh, core_add_fill_between, core_add_pie
    public :: core_streamplot, core_quiver, core_savefig, core_savefig_with_status
    public :: core_show

contains

    subroutine core_initialize(state, plots, streamlines, subplots_array, &
                               subplot_rows, &
                               subplot_cols, current_subplot, title, xlabel, ylabel, &
                               plot_count, width, height, backend, dpi)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        type(subplot_data_t), allocatable, intent(inout) :: subplots_array(:, :)
        integer, intent(inout) :: subplot_rows, subplot_cols, &
                                  current_subplot, plot_count
        character(len=:), allocatable, intent(inout) :: title, xlabel, ylabel
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        real(wp), intent(in), optional :: dpi

        call figure_initialize(state, plots, streamlines, subplots_array, &
                               subplot_rows, &
                               subplot_cols, current_subplot, title, xlabel, ylabel, &
                               plot_count, width, height, backend, dpi)
    end subroutine core_initialize

    subroutine core_add_plot(plots, state, x, y, label, linestyle, color, plot_count)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        integer, intent(inout) :: plot_count

        call ensure_figure_storage(plots, state)
        call figure_add_plot_operation(plots, state, x, y, label, linestyle, color)
        plot_count = state%plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_add_plot

    subroutine core_add_contour(plots, state, x_grid, y_grid, z_grid, levels, label, &
                                plot_count)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        integer, intent(inout) :: plot_count

        call ensure_figure_storage(plots, state)
        call figure_add_contour_operation(plots, state, x_grid, y_grid, z_grid, &
                                          levels, label)
        plot_count = state%plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_add_contour

    subroutine core_add_contour_filled(plots, state, x_grid, y_grid, z_grid, levels, &
                                       cmap, show_colorbar, label, colormap, plot_count)
        !! Add a filled contour plot to the figure
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar
        integer, intent(inout) :: plot_count

        call ensure_figure_storage(plots, state)
        call figure_add_contour_filled_operation(plots, state, x_grid, y_grid, z_grid, &
                                                 levels=levels, cmap=cmap, &
                                                 show_colorbar=show_colorbar, label=label, &
                                                 colormap=colormap)
        plot_count = state%plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_add_contour_filled

    subroutine core_add_surface(plots, state, x_grid, y_grid, z_grid, label, cmap, &
                                show_colorbar, alpha, edgecolor, linewidth, filled, &
                                plot_count, colormap)
        !! Add a 3D surface plot to the figure
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        character(len=*), intent(in), optional :: label, cmap, colormap
        logical, intent(in), optional :: show_colorbar, filled
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)
        integer, intent(inout) :: plot_count

        call ensure_figure_storage(plots, state)
        call figure_add_surface_operation(plots, state, x_grid, y_grid, z_grid, &
                                          label=label, cmap=cmap, &
                                          show_colorbar=show_colorbar, alpha=alpha, &
                                          edgecolor=edgecolor, linewidth=linewidth, &
                                          filled=filled, colormap=colormap)
        plot_count = state%plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_add_surface

    subroutine core_add_pcolormesh(plots, state, x, y, c, cmap, vmin, vmax, &
                                   edgecolors, linewidths, plot_count, colormap)
        !! Add a pcolormesh plot to the figure
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: cmap, colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        integer, intent(inout) :: plot_count

        call ensure_figure_storage(plots, state)
        call figure_add_pcolormesh_operation(plots, state, x, y, c, cmap=cmap, &
                                             vmin=vmin, vmax=vmax, &
                                             edgecolors=edgecolors, linewidths=linewidths, &
                                             colormap=colormap)
        plot_count = state%plot_count
        call update_data_ranges_pcolormesh_figure(plots, state, state%plot_count)
    end subroutine core_add_pcolormesh

    subroutine core_add_fill_between(plots, state, x, upper, lower, mask, &
                                     color_string, alpha, &
                                     plot_count)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: upper(:)
        real(wp), intent(in) :: lower(:)
        logical, intent(in), optional :: mask(:)
        character(len=*), intent(in), optional :: color_string
        real(wp), intent(in), optional :: alpha
        integer, intent(inout) :: plot_count

        call ensure_figure_storage(plots, state)
        call figure_add_fill_between_operation(plots, state, x, upper, lower, mask, &
                                               color_string, alpha)
        plot_count = state%plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_add_fill_between

    subroutine core_add_pie(plots, state, values, labels, autopct, startangle, colors, &
                            explode, plot_count)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:)
        character(len=*), intent(in), optional :: autopct
        real(wp), intent(in), optional :: startangle
        character(len=*), intent(in), optional :: colors(:)
        real(wp), intent(in), optional :: explode(:)
        integer, intent(inout) :: plot_count

        call ensure_figure_storage(plots, state)
        call figure_add_pie_operation(plots, state, values, labels, startangle, &
                                      colors, explode, autopct)
        plot_count = state%plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_add_pie

    subroutine core_streamplot(plots, state, plot_count, x, y, u, v, &
                               density, color, &
                               linewidth, rtol, atol, max_time)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth, rtol, atol, max_time

        call ensure_figure_storage(plots, state)
        call figure_streamplot_operation(plots, state, plot_count, x, y, u, v, &
                                         density, color, linewidth, rtol, &
                                         atol, max_time)
        ! Sync state%plot_count with plot_count (streamplot updates plot_count directly)
        state%plot_count = plot_count
    end subroutine core_streamplot

    subroutine core_quiver(plots, state, plot_count, x, y, u, v, scale, color, &
                           width, headwidth, headlength, units, pivot, scale_units)
        !! Add quiver plot (discrete vector arrows) to figure
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units, pivot, scale_units

        call ensure_figure_storage(plots, state)
        call quiver_figure(plots, state, plot_count, x, y, u, v, scale, color, &
                           width, headwidth, headlength, units, pivot, scale_units)
        call update_data_ranges_figure(plots, state, plot_count)
    end subroutine core_quiver

    subroutine core_savefig(state, plots, plot_count, filename, blocking, &
                            annotations, annotation_count, subplots_array, &
                            subplot_rows, subplot_cols)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        type(text_annotation_t), allocatable, intent(inout) :: annotations(:)
        integer, intent(in) :: annotation_count
        type(subplot_data_t), intent(in), optional :: subplots_array(:, :)
        integer, intent(in), optional :: subplot_rows, subplot_cols

        call ensure_figure_storage(plots, state)
        call figure_savefig(state, plots, plot_count, filename, blocking, &
                            annotations, annotation_count, subplots_array, &
                            subplot_rows, subplot_cols)
    end subroutine core_savefig

    subroutine core_savefig_with_status(state, plots, plot_count, filename, status, &
                                        blocking, &
                                        annotations, annotation_count, subplots_array, &
                                        subplot_rows, subplot_cols)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        logical, intent(in), optional :: blocking
        type(text_annotation_t), allocatable, intent(inout) :: annotations(:)
        integer, intent(in) :: annotation_count
        type(subplot_data_t), intent(in), optional :: subplots_array(:, :)
        integer, intent(in), optional :: subplot_rows, subplot_cols

        call ensure_figure_storage(plots, state)
        call figure_savefig_with_status(state, plots, plot_count, filename, status, &
                                        blocking, &
                                        annotations, annotation_count, subplots_array, &
                                        subplot_rows, subplot_cols)
    end subroutine core_savefig_with_status

    subroutine core_show(state, plots, plot_count, blocking, annotations, &
                         annotation_count, &
                         subplots_array, subplot_rows, subplot_cols)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in), optional :: blocking
        type(text_annotation_t), allocatable, intent(inout) :: annotations(:)
        integer, intent(in) :: annotation_count
        type(subplot_data_t), intent(in), optional :: subplots_array(:, :)
        integer, intent(in), optional :: subplot_rows, subplot_cols

        call ensure_figure_storage(plots, state)
        call figure_show(state, plots, plot_count, blocking, annotations, &
                         annotation_count, &
                         subplots_array, subplot_rows, subplot_cols)
    end subroutine core_show

end module fortplot_figure_core_operations
