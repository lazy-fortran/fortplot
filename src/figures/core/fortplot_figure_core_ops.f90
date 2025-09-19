! Consolidated (Issue #934)

! ==== Begin: src/figures/core/fortplot_figure_core_ranges.f90 ====

module fortplot_figure_core_ranges
    !! Figure data range management module
    !! 
    !! This module contains data range calculation methods
    !! extracted from fortplot_figure_core for architectural compliance
    !!
    !! ARCHITECTURAL REFACTORING (Issue #678):
    !! - Focused module for data range management
    !! - Single Responsibility Principle compliance
    !! - Clean separation from main figure functionality

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges
    use fortplot_figure_boxplot, only: update_boxplot_ranges
    implicit none

    private
    public :: update_data_ranges_figure, update_data_ranges_pcolormesh_figure
    public :: update_data_ranges_boxplot_figure

contains

    subroutine update_data_ranges_figure(plots, state, plot_count)
        !! Update data ranges based on current plot
        type(plot_data_t), intent(in) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(in) :: plot_count
        
        call calculate_figure_data_ranges(plots, plot_count, &
                                        state%xlim_set, state%ylim_set, &
                                        state%x_min, state%x_max, &
                                        state%y_min, state%y_max, &
                                        state%x_min_transformed, &
                                        state%x_max_transformed, &
                                        state%y_min_transformed, &
                                        state%y_max_transformed, &
                                        state%xscale, state%yscale, &
                                        state%symlog_threshold)
    end subroutine update_data_ranges_figure

    subroutine update_data_ranges_pcolormesh_figure(plots, state, plot_count)
        !! Update data ranges after adding pcolormesh plot
        type(plot_data_t), intent(in) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(in) :: plot_count
        
        real(wp) :: x_min_new, x_max_new, y_min_new, y_max_new
        
        ! Safety check: ensure pcolormesh arrays are allocated before accessing
        if (.not. allocated(plots(plot_count)%pcolormesh_data%x_vertices) .or. &
            .not. allocated(plots(plot_count)%pcolormesh_data%y_vertices)) then
            ! Arrays not allocated - pcolormesh initialization failed
            ! Skip data range update to prevent segfault
            return
        end if
        
        ! Additional safety: check arrays have valid size
        if (size(plots(plot_count)%pcolormesh_data%x_vertices) == 0 .or. &
            size(plots(plot_count)%pcolormesh_data%y_vertices) == 0) then
            ! Zero-size arrays - skip data range update
            return
        end if
        
        x_min_new = minval(plots(plot_count)%pcolormesh_data%x_vertices)
        x_max_new = maxval(plots(plot_count)%pcolormesh_data%x_vertices)
        y_min_new = minval(plots(plot_count)%pcolormesh_data%y_vertices)
        y_max_new = maxval(plots(plot_count)%pcolormesh_data%y_vertices)
        
        if (.not. state%xlim_set) then
            if (plot_count == 1) then
                state%x_min = x_min_new
                state%x_max = x_max_new
            else
                state%x_min = min(state%x_min, x_min_new)
                state%x_max = max(state%x_max, x_max_new)
            end if
        end if
        
        if (.not. state%ylim_set) then
            if (plot_count == 1) then
                state%y_min = y_min_new
                state%y_max = y_max_new
            else
                state%y_min = min(state%y_min, y_min_new)
                state%y_max = max(state%y_max, y_max_new)
            end if
        end if
    end subroutine update_data_ranges_pcolormesh_figure

    subroutine update_data_ranges_boxplot_figure(data, position, state)
        !! Update data ranges after adding boxplot
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        type(figure_state_t), intent(inout) :: state
        
        ! Delegate to module implementation
        call update_boxplot_ranges(data, position, &
                                   state%x_min, state%x_max, &
                                   state%y_min, state%y_max, &
                                   state%xlim_set, state%ylim_set)
    end subroutine update_data_ranges_boxplot_figure

end module fortplot_figure_core_ranges
! ==== End: src/figures/core/fortplot_figure_core_ranges.f90 ====

! ==== Begin: src/figures/core/fortplot_figure_core_operations.f90 ====

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
    public :: core_add_pcolormesh, core_add_fill_between
    public :: core_streamplot, core_savefig, core_savefig_with_status
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

    subroutine core_add_fill_between(plots, state, x, upper, lower, mask, color_string, alpha, &
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

        call figure_add_fill_between_operation(plots, state, x, upper, lower, mask, color_string, alpha)
        plot_count = state%plot_count
        call update_data_ranges_figure(plots, state, state%plot_count)
    end subroutine core_add_fill_between

    subroutine core_streamplot(plots, state, plot_count, x, y, u, v, density, color)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        
        call figure_streamplot_operation(plots, state, plot_count, x, y, u, v, &
                                         density, color)
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
! ==== End: src/figures/core/fortplot_figure_core_operations.f90 ====

! ==== Begin: src/figures/core/fortplot_figure_core_accessors.f90 ====

module fortplot_figure_core_accessors
    !! Property accessor methods extracted from fortplot_figure_core
    !! 
    !! This module contains property accessor methods for the core figure
    !! to maintain architectural compliance with size limits.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_annotations, only: text_annotation_t
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_properties_new
    use fortplot_figure_management
    use fortplot_figure_operations, only: figure_render
    implicit none

    private
    public :: core_get_width, core_get_height, core_get_rendered, core_set_rendered
    public :: core_get_plot_count, core_get_plots, core_get_x_min, core_get_x_max
    public :: core_get_y_min, core_get_y_max, core_backend_color, core_backend_associated
    public :: core_backend_line, core_setup_png_backend_for_animation, core_backend_arrow
    public :: core_extract_rgb_data_for_animation, core_extract_png_data_for_animation

contains

    function core_get_width(state) result(width)
        type(figure_state_t), intent(in) :: state
        integer :: width
        width = figure_get_width(state)
    end function core_get_width

    function core_get_height(state) result(height)
        type(figure_state_t), intent(in) :: state
        integer :: height
        height = figure_get_height(state)
    end function core_get_height

    function core_get_rendered(state) result(rendered)
        type(figure_state_t), intent(in) :: state
        logical :: rendered
        rendered = figure_get_rendered(state)
    end function core_get_rendered

    subroutine core_set_rendered(state, rendered)
        type(figure_state_t), intent(inout) :: state
        logical, intent(in) :: rendered
        call figure_set_rendered(state, rendered)
    end subroutine core_set_rendered

    function core_get_plot_count(state) result(plot_count)
        type(figure_state_t), intent(in) :: state
        integer :: plot_count
        plot_count = figure_get_plot_count(state)
    end function core_get_plot_count

    function core_get_plots(plots) result(plots_ptr)
        type(plot_data_t), intent(in), target :: plots(:)
        type(plot_data_t), pointer :: plots_ptr(:)
        plots_ptr => figure_get_plots(plots)
    end function core_get_plots

    function core_get_x_min(state) result(x_min)
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_min
        x_min = figure_get_x_min(state)
    end function core_get_x_min

    function core_get_x_max(state) result(x_max)
        type(figure_state_t), intent(in) :: state
        real(wp) :: x_max
        x_max = figure_get_x_max(state)
    end function core_get_x_max

    function core_get_y_min(state) result(y_min)
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_min
        y_min = figure_get_y_min(state)
    end function core_get_y_min

    function core_get_y_max(state) result(y_max)
        type(figure_state_t), intent(in) :: state
        real(wp) :: y_max
        y_max = figure_get_y_max(state)
    end function core_get_y_max

    subroutine core_backend_color(state, r, g, b)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: r, g, b
        call figure_backend_color(state, r, g, b)
    end subroutine core_backend_color

    function core_backend_associated(state) result(is_associated)
        type(figure_state_t), intent(in) :: state
        logical :: is_associated
        is_associated = figure_backend_associated(state)
    end function core_backend_associated

    subroutine core_backend_line(state, x1, y1, x2, y2)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x1, y1, x2, y2
        call figure_backend_line(state, x1, y1, x2, y2)
    end subroutine core_backend_line

    subroutine core_backend_arrow(state, x, y, dx, dy, size, style)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        call figure_backend_arrow(state, x, y, dx, dy, size, style)
    end subroutine core_backend_arrow

    ! Animation support - delegate to animation module
    subroutine core_setup_png_backend_for_animation(state)
        type(figure_state_t), intent(inout) :: state
        call figure_setup_png_backend_for_animation(state)
    end subroutine core_setup_png_backend_for_animation

    subroutine core_extract_rgb_data_for_animation(state, rgb_data, plots, plot_count, &
                                                   annotations, annotation_count, rendered)
        type(figure_state_t), intent(inout) :: state
        real(wp), intent(out) :: rgb_data(:,:,:)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count, annotation_count
        type(text_annotation_t), allocatable, intent(inout) :: annotations(:)
        logical, intent(in) :: rendered
        
        if (.not. rendered) call figure_render(state, plots, plot_count, &
            annotations, annotation_count)
        call figure_extract_rgb_data_for_animation(state, rgb_data, rendered)
    end subroutine core_extract_rgb_data_for_animation

    subroutine core_extract_png_data_for_animation(state, png_data, status, plots, plot_count, &
                                                   annotations, annotation_count, rendered)
        type(figure_state_t), intent(inout) :: state
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count, annotation_count
        type(text_annotation_t), allocatable, intent(inout) :: annotations(:)
        logical, intent(in) :: rendered
        
        if (.not. rendered) call figure_render(state, plots, plot_count, &
            annotations, annotation_count)
        call figure_extract_png_data_for_animation(state, png_data, status, rendered)
    end subroutine core_extract_png_data_for_animation

end module fortplot_figure_core_accessors
! ==== End: src/figures/core/fortplot_figure_core_accessors.f90 ====

! ==== Begin: src/figures/core/fortplot_figure_core_advanced.f90 ====

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
                           color, colormap, vmin, vmax, label, show_colorbar, default_color)
        !! Add an efficient scatter plot using a single plot object
        !! Properly handles thousands of points without O(n) overhead
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), c(:)
        character(len=*), intent(in), optional :: marker, colormap, label
        real(wp), intent(in), optional :: markersize, vmin, vmax
        real(wp), intent(in), optional :: color(3)
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in) :: default_color(3)
        
        ! Delegate to efficient scatter implementation
        call figure_scatter_operation(plots, state%plot_count, &
                                     x, y, s, c, marker, markersize, color, &
                                     colormap, vmin, vmax, label, show_colorbar, &
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
! ==== End: src/figures/core/fortplot_figure_core_advanced.f90 ====

! ==== Begin: src/figures/core/fortplot_figure_core_utils.f90 ====

module fortplot_figure_core_utils
    !! Utility operations extracted from fortplot_figure_core
    !! 
    !! This module contains utility operations like set_ydata and legend
    !! to maintain architectural compliance with size limits.
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_operations
    implicit none

    private
    public :: core_set_ydata, core_figure_legend

contains

    subroutine core_set_ydata(plots, plot_count, plot_index, y_new)
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count, plot_index
        real(wp), intent(in) :: y_new(:)
        call figure_set_ydata_operation(plots, plot_count, plot_index, y_new)
    end subroutine core_set_ydata

    subroutine core_figure_legend(state, plots, plot_count, location)
        type(figure_state_t), intent(inout) :: state
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        integer, intent(in) :: plot_count
        character(len=*), intent(in), optional :: location
        call figure_legend_operation(state%legend_data, state%show_legend, &
                                    plots, plot_count, location)
    end subroutine core_figure_legend

end module fortplot_figure_core_utils
! ==== End: src/figures/core/fortplot_figure_core_utils.f90 ====
