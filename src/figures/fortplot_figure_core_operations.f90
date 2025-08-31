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
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_operations
    use fortplot_figure_management
    use fortplot_figure_core_ranges, only: update_data_ranges_figure, update_data_ranges_pcolormesh_figure
    implicit none

    private
    public :: core_initialize, core_add_plot, core_add_contour, core_add_contour_filled
    public :: core_add_pcolormesh, core_streamplot, core_savefig, core_savefig_with_status
    public :: core_show

contains

    subroutine core_initialize(state, plots, streamlines, subplots_array, subplot_rows, &
                               subplot_cols, current_subplot, title, xlabel, ylabel, &
                               plot_count, width, height, backend)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(plot_data_t), allocatable, intent(inout) :: streamlines(:)
        type(subplot_data_t), allocatable, intent(inout) :: subplots_array(:,:)
        integer, intent(inout) :: subplot_rows, subplot_cols, current_subplot, plot_count
        character(len=:), allocatable, intent(inout) :: title, xlabel, ylabel
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        
        call figure_initialize(state, plots, streamlines, subplots_array, subplot_rows, &
                              subplot_cols, current_subplot, title, xlabel, ylabel, &
                              plot_count, width, height, backend)
    end subroutine core_initialize

    subroutine core_add_plot(plots, state, x, y, label, linestyle, color, plot_count)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        integer, intent(inout) :: plot_count
        
        call figure_add_plot_operation(plots, state, x, y, label, linestyle, color)
        plot_count = state%plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_add_plot

    subroutine core_add_contour(plots, state, x_grid, y_grid, z_grid, levels, label, plot_count)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        integer, intent(inout) :: plot_count
        
        call figure_add_contour_operation(plots, state, x_grid, y_grid, z_grid, levels, label)
        plot_count = state%plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_add_contour

    subroutine core_add_contour_filled(plots, state, x_grid, y_grid, z_grid, levels, &
                                       colormap, show_colorbar, label, plot_count)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        integer, intent(inout) :: plot_count
        
        call figure_add_contour_filled_operation(plots, state, x_grid, y_grid, z_grid, &
                                                 levels, colormap, show_colorbar, label)
        plot_count = state%plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_add_contour_filled

    subroutine core_add_pcolormesh(plots, state, x, y, c, colormap, vmin, vmax, &
                                   edgecolors, linewidths, plot_count)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        integer, intent(inout) :: plot_count
        
        call figure_add_pcolormesh_operation(plots, state, x, y, c, colormap, &
                                            vmin, vmax, edgecolors, linewidths)
        plot_count = state%plot_count
        call update_data_ranges_pcolormesh_figure(plots, state, state%plot_count)
    end subroutine core_add_pcolormesh

    subroutine core_streamplot(plots, state, plot_count, x, y, u, v, density, color, &
                               linewidth, rtol, atol, max_time)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth
        real(wp), intent(in), optional :: rtol, atol, max_time
        
        call figure_streamplot_operation(plots, state, plot_count, x, y, u, v, &
                                         density, color, linewidth, rtol, atol, max_time)
    end subroutine core_streamplot

    subroutine core_savefig(state, plots, plot_count, filename, blocking, &
                            annotations, annotation_count)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        type(text_annotation_t), allocatable, intent(inout) :: annotations(:)
        integer, intent(in) :: annotation_count
        
        ! Render with annotations before saving (Issue #844: ASCII annotation functionality)
        if (.not. state%rendered) then
            call figure_render(state, plots, plot_count, annotations, annotation_count)
        end if
        
        call figure_savefig(state, plots, plot_count, filename, blocking, &
                            annotations, annotation_count)
    end subroutine core_savefig
    
    subroutine core_savefig_with_status(state, plots, plot_count, filename, status, blocking, &
                                        annotations, annotation_count)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        logical, intent(in), optional :: blocking
        type(text_annotation_t), allocatable, intent(inout) :: annotations(:)
        integer, intent(in) :: annotation_count
        
        ! Render with annotations before saving (Issue #844: ASCII annotation functionality)
        if (.not. state%rendered) then
            call figure_render(state, plots, plot_count, annotations, annotation_count)
        end if
        
        call figure_savefig_with_status(state, plots, plot_count, filename, status, blocking, &
                                        annotations, annotation_count)
    end subroutine core_savefig_with_status

    subroutine core_show(state, plots, plot_count, blocking, annotations, annotation_count)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        logical, intent(in), optional :: blocking
        type(text_annotation_t), allocatable, intent(inout) :: annotations(:)
        integer, intent(in) :: annotation_count
        
        ! Render with annotations before showing (Issue #844: ASCII annotation functionality)
        if (.not. state%rendered) then
            call figure_render(state, plots, plot_count, annotations, annotation_count)
        end if
        
        call figure_show(state, plots, plot_count, blocking, annotations, annotation_count)
    end subroutine core_show

end module fortplot_figure_core_operations