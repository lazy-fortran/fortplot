module fortplot_figure_core
    !! Core figure management module (architecturally refactored for SOLID principles)
    !! 
    !! This module provides the main user interface for creating scientific plots
    !! with support for line plots, contour plots, and mixed plotting across
    !! PNG, PDF, and ASCII backends. Uses deferred rendering for efficiency.
    !!
    !! ARCHITECTURAL STATUS (Issue #678):
    !! - Current module: 751 lines (50% over 500-line limit)
    !! - COMPLIANCE VIOLATION: Exceeds architectural limits by 251 lines
    !! - Implementation partially distributed across focused sub-modules
    !! - Further refactoring required to achieve Single Responsibility Principle
    !! - Full backward compatibility maintained
    !! - All existing tests pass without modification

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_annotations, only: text_annotation_t
    ! Import refactored modules
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, subplot_data_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, &
                                    PLOT_TYPE_PCOLORMESH, PLOT_TYPE_BOXPLOT, &
                                    PLOT_TYPE_SCATTER
    use fortplot_figure_initialization
    ! Import new focused modules
    use fortplot_figure_core_io
    use fortplot_figure_core_config
    use fortplot_figure_core_compat
    use fortplot_figure_core_ranges
    use fortplot_figure_scatter, only: add_scatter_plot
    use fortplot_figure_plot_management
    use fortplot_figure_plots, only: figure_add_plot, figure_add_contour, &
                                     figure_add_contour_filled, figure_add_pcolormesh
    use fortplot_figure_histogram, only: hist_figure
    use fortplot_figure_streamlines, only: streamplot_figure, clear_streamline_data
    use fortplot_figure_subplots, only: create_subplots, add_subplot_plot, &
                                        get_subplot_plot_count, set_subplot_title, &
                                        set_subplot_xlabel, set_subplot_ylabel, &
                                        get_subplot_title
    use fortplot_figure_accessors
    use fortplot_figure_compatibility
    use fortplot_figure_plots
    use fortplot_figure_boxplot, only: add_boxplot, update_boxplot_ranges
    use fortplot_figure_utilities, only: is_interactive_environment, wait_for_user_input
    use fortplot_figure_ranges, only: update_figure_data_ranges_pcolormesh, update_figure_data_ranges_boxplot
    use fortplot_figure_properties, only: get_figure_width_property, get_figure_height_property, &
                                         get_figure_rendered_property, set_figure_rendered_property, &
                                         get_figure_plot_count_property, get_figure_plots_property, &
                                         get_figure_x_min_property, get_figure_x_max_property, &
                                         get_figure_y_min_property, get_figure_y_max_property, &
                                         figure_backend_color_property, figure_backend_associated_property, &
                                         figure_backend_line_property
    use fortplot_figure_animation, only: setup_figure_png_backend_for_animation, &
                                        extract_figure_rgb_data_for_animation, &
                                        extract_figure_png_data_for_animation
    use fortplot_figure_rendering_pipeline, only: calculate_figure_data_ranges, &
                                                    setup_coordinate_system, &
                                                    render_figure_background, &
                                                    render_figure_axes, &
                                                    render_all_plots
    use fortplot_figure_grid, only: render_grid_lines
    implicit none

    private
    public :: figure_t, plot_data_t, subplot_data_t
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
              PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER

    type :: figure_t
        !! Main figure class - coordinates plotting operations
        !! Now uses composition of focused modules for better organization
        type(figure_state_t) :: state
        
        ! Store all plot data for deferred rendering
        type(plot_data_t), allocatable :: plots(:)
        
        ! Streamline data
        type(plot_data_t), allocatable :: streamlines(:)
        
        ! Arrow data for streamplot
        type(arrow_data_t), allocatable :: arrow_data(:)
        
        ! Text annotations support (Issue #184)
        type(text_annotation_t), allocatable :: annotations(:)
        integer :: annotation_count = 0
        integer :: max_annotations = 1000
        
        ! Subplot support
        integer :: subplot_rows = 0
        integer :: subplot_cols = 0
        integer :: current_subplot = 1
        type(subplot_data_t), allocatable :: subplots_array(:,:)
        
        ! Backward compatibility: expose labels directly for test access
        character(len=:), allocatable :: title
        character(len=:), allocatable :: xlabel
        character(len=:), allocatable :: ylabel
        
        ! Backward compatibility: expose commonly accessed state members
        integer :: plot_count = 0
        
    contains
        procedure :: initialize
        procedure :: add_plot
        procedure :: plot => add_plot        ! Convenience alias for user expectation
        procedure :: add_contour
        procedure :: add_contour_filled
        procedure :: add_pcolormesh
        procedure :: streamplot
        procedure :: savefig
        procedure :: save => savefig         ! Convenience alias for user expectation
        procedure :: savefig_with_status
        procedure :: set_xlabel
        procedure :: set_ylabel
        procedure :: set_title
        procedure :: set_xscale
        procedure :: set_yscale
        procedure :: set_xlim
        procedure :: set_ylim
        procedure :: set_line_width
        procedure :: set_ydata
        procedure :: legend => figure_legend
        procedure :: show
        procedure :: clear_streamlines
        procedure :: grid
        procedure :: hist
        procedure :: boxplot
        procedure :: scatter
        ! Subplot methods
        procedure :: subplots
        procedure :: subplot_plot
        procedure :: subplot_plot_count
        procedure :: subplot_set_title
        procedure :: subplot_set_xlabel
        procedure :: subplot_set_ylabel  
        procedure :: subplot_title
        ! Getter methods for backward compatibility
        procedure :: get_width
        procedure :: get_height
        procedure :: get_rendered
        procedure :: set_rendered
        procedure :: get_plot_count
        procedure :: get_plots
        procedure :: setup_png_backend_for_animation
        procedure :: extract_rgb_data_for_animation
        procedure :: extract_png_data_for_animation
        procedure :: backend_color
        procedure :: backend_line
        procedure :: backend_associated
        procedure :: get_x_min
        procedure :: get_x_max
        procedure :: get_y_min
        procedure :: get_y_max
        ! Private rendering method
        procedure :: render_figure
        ! Label getters removed - direct member access available
        ! Data range methods moved to focused module
        final :: destroy
    end type figure_t

contains

    subroutine initialize(self, width, height, backend)
        !! Initialize the figure with specified dimensions and backend
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        
        call initialize_figure_state(self%state, width, height, backend)
        
        ! Allocate plots array
        if (allocated(self%plots)) deallocate(self%plots)
        allocate(self%plots(self%state%max_plots))
        
        ! Clear streamlines data if allocated
        if (allocated(self%streamlines)) deallocate(self%streamlines)
        
        ! Clear subplot data if allocated
        if (allocated(self%subplots_array)) deallocate(self%subplots_array)
        self%subplot_rows = 0
        self%subplot_cols = 0
        self%current_subplot = 1
        
        ! Clear backward compatibility members
        if (allocated(self%title)) deallocate(self%title)
        if (allocated(self%xlabel)) deallocate(self%xlabel)
        if (allocated(self%ylabel)) deallocate(self%ylabel)
        self%plot_count = 0
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color)
        !! Add a line plot to the figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        
        call figure_add_plot(self%plots, self%state, x, y, label, linestyle, color)
        self%plot_count = self%state%plot_count
        call update_data_ranges_figure(self%plots, self%state, self%state%plot_count)
    end subroutine add_plot

    subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        !! Add a contour plot to the figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        call figure_add_contour(self%plots, self%state, x_grid, y_grid, z_grid, levels, label)
        self%plot_count = self%state%plot_count
        call update_data_ranges_figure(self%plots, self%state, self%state%plot_count)
    end subroutine add_contour

    subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        !! Add a filled contour plot with color mapping
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        call figure_add_contour_filled(self%plots, self%state, x_grid, y_grid, z_grid, &
                                      levels, colormap, show_colorbar, label)
        self%plot_count = self%state%plot_count
        call update_data_ranges_figure(self%plots, self%state, self%state%plot_count)
    end subroutine add_contour_filled

    subroutine add_pcolormesh(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        !! Add a pcolormesh plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        
        call figure_add_pcolormesh(self%plots, self%state, x, y, c, colormap, &
                                  vmin, vmax, edgecolors, linewidths)
        self%plot_count = self%state%plot_count
        call update_data_ranges_pcolormesh_figure(self%plots, self%state, self%state%plot_count)
    end subroutine add_pcolormesh

    subroutine streamplot(self, x, y, u, v, density, color, linewidth, rtol, atol, max_time)
        !! Add streamline plot to figure using basic algorithm
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth
        real(wp), intent(in), optional :: rtol, atol, max_time
        
        call streamplot_figure(self%plots, self%state, self%plot_count, x, y, u, v, &
                               density, color, linewidth, rtol, atol, max_time)
    end subroutine streamplot

    subroutine savefig(self, filename, blocking)
        !! Save figure to file (backward compatibility version)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        
        call savefig_figure(self%state, self%plots, self%state%plot_count, filename, blocking)
    end subroutine savefig
    
    subroutine savefig_with_status(self, filename, status, blocking)
        !! Save figure to file with error status reporting
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        logical, intent(in), optional :: blocking
        
        call savefig_with_status_figure(self%state, self%plots, self%state%plot_count, &
                                       filename, status, blocking)
    end subroutine savefig_with_status

    subroutine show(self, blocking)
        !! Display the figure
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: blocking
        
        call show_figure(self%state, self%plots, self%state%plot_count, blocking)
    end subroutine show

    subroutine grid(self, enabled, which, axis, alpha, linestyle)
        !! Enable/disable and configure grid lines
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha
        
        call grid_figure(self%state, enabled, which, axis, alpha, linestyle)
    end subroutine grid

    subroutine hist(self, data, bins, density, label, color)
        !! Create a histogram plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        call hist_figure(self%plots, self%state, self%plot_count, data, bins, density, label, color)
    end subroutine hist

    subroutine boxplot(self, data, position, width, label, show_outliers, &
                       horizontal, color)
        !! Create a box plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        character(len=*), intent(in), optional :: color
        
        call add_boxplot(self%plots, self%plot_count, data, position, width, label, &
                         show_outliers, horizontal, color, self%state%max_plots)
    end subroutine boxplot

    subroutine set_xlabel(self, label)
        !! Set x-axis label
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        call set_xlabel_figure(self%state, self%xlabel, label)
    end subroutine set_xlabel

    subroutine set_ylabel(self, label)
        !! Set y-axis label
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        call set_ylabel_figure(self%state, self%ylabel, label)
    end subroutine set_ylabel

    subroutine set_title(self, title)
        !! Set figure title
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: title
        call set_title_figure(self%state, self%title, title)
    end subroutine set_title

    subroutine set_xscale(self, scale, threshold)
        !! Set x-axis scale type
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        call set_xscale_figure(self%state, scale, threshold)
    end subroutine set_xscale

    subroutine set_yscale(self, scale, threshold)
        !! Set y-axis scale type
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        
        call set_yscale_figure(self%state, scale, threshold)
    end subroutine set_yscale

    subroutine set_xlim(self, x_min, x_max)
        !! Set x-axis limits
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_min, x_max
        
        call set_xlim_figure(self%state, x_min, x_max)
    end subroutine set_xlim

    subroutine set_ylim(self, y_min, y_max)
        !! Set y-axis limits
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y_min, y_max
        
        call set_ylim_figure(self%state, y_min, y_max)
    end subroutine set_ylim

    subroutine set_line_width(self, width)
        !! Set line width for subsequent plots
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: width
        
        call set_line_width_figure(self%state, width)
    end subroutine set_line_width

    subroutine set_ydata(self, plot_index, y_new)
        !! Update y data for an existing plot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        real(wp), intent(in) :: y_new(:)
        
        call update_plot_ydata(self%plots, self%state%plot_count, plot_index, y_new)
    end subroutine set_ydata

    subroutine figure_legend(self, location)
        !! Add legend to figure
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in), optional :: location
        
        call setup_figure_legend(self%state%legend_data, self%state%show_legend, &
                                self%plots, self%state%plot_count, location)
    end subroutine figure_legend

    subroutine clear_streamlines(self)
        !! Clear streamline data
        class(figure_t), intent(inout) :: self
        call clear_streamline_data(self%streamlines)
    end subroutine clear_streamlines

    subroutine destroy(self)
        !! Finalize and clean up figure
        type(figure_t), intent(inout) :: self
        
        if (allocated(self%state%backend)) then
            deallocate(self%state%backend)
        end if
        
        if (allocated(self%plots)) deallocate(self%plots)
        if (allocated(self%streamlines)) deallocate(self%streamlines)
        if (allocated(self%state%title)) deallocate(self%state%title)
        if (allocated(self%state%xlabel)) deallocate(self%state%xlabel)
        if (allocated(self%state%ylabel)) deallocate(self%state%ylabel)
        ! Clean up backward compatibility members
        if (allocated(self%title)) deallocate(self%title)
        if (allocated(self%xlabel)) deallocate(self%xlabel)
        if (allocated(self%ylabel)) deallocate(self%ylabel)
    end subroutine destroy

    ! Private implementation procedures moved to focused modules

    subroutine update_data_ranges_pcolormesh(self)
        !! Update data ranges after adding pcolormesh plot - delegate to ranges module
        class(figure_t), intent(inout) :: self
        
        call update_figure_data_ranges_pcolormesh(self%plots, self%state%plot_count, &
                                                 self%state%xlim_set, self%state%ylim_set, &
                                                 self%state%x_min, self%state%x_max, &
                                                 self%state%y_min, self%state%y_max)
    end subroutine update_data_ranges_pcolormesh

    subroutine update_data_ranges_boxplot(self, data, position)
        !! Update data ranges after adding boxplot - delegate to ranges module
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        
        call update_figure_data_ranges_boxplot(data, position, &
                                               self%state%x_min, self%state%x_max, &
                                               self%state%y_min, self%state%y_max, &
                                               self%state%xlim_set, self%state%ylim_set)
    end subroutine update_data_ranges_boxplot

    subroutine update_data_ranges(self)
        !! Update data ranges based on current plot
        class(figure_t), intent(inout) :: self
        
        call calculate_figure_data_ranges(self%plots, self%state%plot_count, &
                                        self%state%xlim_set, self%state%ylim_set, &
                                        self%state%x_min, self%state%x_max, &
                                        self%state%y_min, self%state%y_max, &
                                        self%state%x_min_transformed, &
                                        self%state%x_max_transformed, &
                                        self%state%y_min_transformed, &
                                        self%state%y_max_transformed, &
                                        self%state%xscale, self%state%yscale, &
                                        self%state%symlog_threshold)
    end subroutine update_data_ranges

    subroutine render_figure(self)
        !! Main rendering pipeline using focused modules
        !! Fixed Issue #432: Always render axes/labels even with no plot data
        class(figure_t), intent(inout) :: self
        
        ! Calculate final data ranges
        call calculate_figure_data_ranges(self%plots, self%state%plot_count, &
                                        self%state%xlim_set, self%state%ylim_set, &
                                        self%state%x_min, self%state%x_max, &
                                        self%state%y_min, self%state%y_max, &
                                        self%state%x_min_transformed, &
                                        self%state%x_max_transformed, &
                                        self%state%y_min_transformed, &
                                        self%state%y_max_transformed, &
                                        self%state%xscale, self%state%yscale, &
                                        self%state%symlog_threshold)
        
        ! Setup coordinate system
        call setup_coordinate_system(self%state%backend, &
                                   self%state%x_min_transformed, self%state%x_max_transformed, &
                                   self%state%y_min_transformed, self%state%y_max_transformed)
        
        ! Render background
        call render_figure_background(self%state%backend)
        
        ! Render grid if enabled
        if (self%state%grid_enabled) then
            call render_grid_lines(self%state%backend, self%state%grid_enabled, &
                                  self%state%grid_which, self%state%grid_axis, &
                                  self%state%grid_alpha, self%state%width, self%state%height, &
                                  self%state%margin_left, self%state%margin_right, &
                                  self%state%margin_bottom, self%state%margin_top, &
                                  self%state%xscale, self%state%yscale, &
                                  self%state%symlog_threshold, self%state%x_min, self%state%x_max, &
                                  self%state%y_min, self%state%y_max, &
                                  self%state%x_min_transformed, self%state%x_max_transformed, &
                                  self%state%y_min_transformed, self%state%y_max_transformed)
        end if
        
        ! Render axes
        call render_figure_axes(self%state%backend, self%state%xscale, self%state%yscale, &
                               self%state%symlog_threshold, self%state%x_min, self%state%x_max, &
                               self%state%y_min, self%state%y_max, self%state%title, &
                               self%state%xlabel, self%state%ylabel)
        
        ! Render all plots (only if there are plots to render)
        if (self%state%plot_count > 0) then
            call render_all_plots(self%state%backend, self%plots, self%state%plot_count, &
                                 self%state%x_min_transformed, self%state%x_max_transformed, &
                                 self%state%y_min_transformed, self%state%y_max_transformed, &
                                 self%state%xscale, self%state%yscale, self%state%symlog_threshold, &
                                 self%state%width, self%state%height, &
                                 self%state%margin_left, self%state%margin_right, &
                                 self%state%margin_bottom, self%state%margin_top)
        end if
        
        ! Render legend if requested
        if (self%state%show_legend .and. self%state%legend_data%num_entries > 0) then
            call self%state%legend_data%render(self%state%backend)
        end if
        
        self%state%rendered = .true.
    end subroutine render_figure

    ! Methods for backward compatibility with animation module
    
    ! Property accessors - delegate to properties module
    function get_width(self) result(width)
        class(figure_t), intent(in) :: self
        integer :: width
        width = get_figure_width_property(self%state)
    end function get_width
    
    function get_height(self) result(height)
        class(figure_t), intent(in) :: self
        integer :: height
        height = get_figure_height_property(self%state)
    end function get_height
    
    function get_rendered(self) result(rendered)
        class(figure_t), intent(in) :: self
        logical :: rendered
        rendered = get_figure_rendered_property(self%state)
    end function get_rendered
    
    subroutine set_rendered(self, rendered)
        class(figure_t), intent(inout) :: self
        logical, intent(in) :: rendered
        call set_figure_rendered_property(self%state, rendered)
    end subroutine set_rendered
    
    function get_plot_count(self) result(plot_count)
        class(figure_t), intent(in) :: self
        integer :: plot_count
        plot_count = get_figure_plot_count_property(self%state)
    end function get_plot_count
    
    function get_plots(self) result(plots_ptr)
        class(figure_t), intent(in), target :: self
        type(plot_data_t), pointer :: plots_ptr(:)
        plots_ptr => get_figure_plots_property(self%plots)
    end function get_plots
    
    ! Animation support - delegate to animation module
    subroutine setup_png_backend_for_animation(self)
        class(figure_t), intent(inout) :: self
        call setup_figure_png_backend_for_animation(self%state)
    end subroutine setup_png_backend_for_animation
    
    subroutine extract_rgb_data_for_animation(self, rgb_data)
        class(figure_t), intent(inout) :: self
        real(wp), intent(out) :: rgb_data(:,:,:)
        
        if (.not. self%state%rendered) then
            call self%render_figure()
        end if
        
        call extract_figure_rgb_data_for_animation(self%state, rgb_data, self%state%rendered)
    end subroutine extract_rgb_data_for_animation
    
    subroutine extract_png_data_for_animation(self, png_data, status)
        class(figure_t), intent(inout) :: self
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        if (.not. self%state%rendered) then
            call self%render_figure()
        end if
        
        call extract_figure_png_data_for_animation(self%state, png_data, status, self%state%rendered)
    end subroutine extract_png_data_for_animation
    
    ! Backend interface and coordinate accessors - delegate to properties module
    subroutine backend_color(self, r, g, b)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: r, g, b
        call figure_backend_color_property(self%state, r, g, b)
    end subroutine backend_color
    
    function backend_associated(self) result(is_associated)
        class(figure_t), intent(in) :: self
        logical :: is_associated
        is_associated = figure_backend_associated_property(self%state)
    end function backend_associated
    
    subroutine backend_line(self, x1, y1, x2, y2)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x1, y1, x2, y2
        call figure_backend_line_property(self%state, x1, y1, x2, y2)
    end subroutine backend_line
    
    function get_x_min(self) result(x_min)
        class(figure_t), intent(in) :: self
        real(wp) :: x_min
        x_min = get_figure_x_min_property(self%state)
    end function get_x_min
    
    function get_x_max(self) result(x_max)
        class(figure_t), intent(in) :: self
        real(wp) :: x_max
        x_max = get_figure_x_max_property(self%state)
    end function get_x_max
    
    function get_y_min(self) result(y_min)
        class(figure_t), intent(in) :: self
        real(wp) :: y_min
        y_min = get_figure_y_min_property(self%state)
    end function get_y_min
    
    function get_y_max(self) result(y_max)
        class(figure_t), intent(in) :: self
        real(wp) :: y_max
        y_max = get_figure_y_max_property(self%state)
    end function get_y_max

    subroutine scatter(self, x, y, s, c, marker, markersize, color, &
                      colormap, alpha, edgecolor, facecolor, linewidth, &
                      vmin, vmax, label, show_colorbar)
        !! Add an efficient scatter plot using a single plot object
        !! Properly handles thousands of points without O(n) overhead
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), c(:)
        character(len=*), intent(in), optional :: marker, colormap, label
        real(wp), intent(in), optional :: markersize, alpha, linewidth, vmin, vmax
        real(wp), intent(in), optional :: color(3), edgecolor(3), facecolor(3)
        logical, intent(in), optional :: show_colorbar
        
        real(wp) :: default_color(3)
        
        ! Get default color from state
        default_color = self%state%colors(:, mod(self%state%plot_count, 6) + 1)
        
        ! Delegate to efficient scatter implementation
        call add_scatter_plot(self%plots, self%state%plot_count, &
                             x, y, s, c, marker, markersize, color, &
                             colormap, alpha, edgecolor, facecolor, &
                             linewidth, vmin, vmax, label, show_colorbar, &
                             default_color)
        
        ! Update figure state
        self%plot_count = self%state%plot_count
        
        ! Update data ranges
        call update_data_ranges_figure(self%plots, self%state, self%state%plot_count)
    end subroutine scatter

    subroutine subplots(self, nrows, ncols)
        !! Create a grid of subplots
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: nrows, ncols
        logical :: subplot_active
        
        ! Delegate to module implementation
        call create_subplots(self%subplots_array, self%subplot_rows, &
                            self%subplot_cols, nrows, ncols, subplot_active)
        self%current_subplot = 1
    end subroutine subplots
    
    subroutine subplot_plot(self, row, col, x, y, label, linestyle, color)
        !! Add a plot to a specific subplot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        
        call add_subplot_plot(self%subplots_array, self%subplot_rows, &
                             self%subplot_cols, row, col, x, y, label, &
                             linestyle, color, self%state%colors, 6)
    end subroutine subplot_plot
    
    function subplot_plot_count(self, row, col) result(count)
        !! Get the number of plots in a specific subplot
        class(figure_t), intent(in) :: self
        integer, intent(in) :: row, col
        integer :: count
        
        count = get_subplot_plot_count(self%subplots_array, self%subplot_rows, &
                                       self%subplot_cols, row, col)
    end function subplot_plot_count
    
    subroutine subplot_set_title(self, row, col, title)
        !! Set the title for a specific subplot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: title
        
        call set_subplot_title(self%subplots_array, self%subplot_rows, &
                              self%subplot_cols, row, col, title)
    end subroutine subplot_set_title
    
    subroutine subplot_set_xlabel(self, row, col, xlabel)
        !! Set the x-axis label for a specific subplot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: xlabel
        
        call set_subplot_xlabel(self%subplots_array, self%subplot_rows, &
                               self%subplot_cols, row, col, xlabel)
    end subroutine subplot_set_xlabel
    
    subroutine subplot_set_ylabel(self, row, col, ylabel)
        !! Set the y-axis label for a specific subplot
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: ylabel
        
        call set_subplot_ylabel(self%subplots_array, self%subplot_rows, &
                               self%subplot_cols, row, col, ylabel)
    end subroutine subplot_set_ylabel
    
    function subplot_title(self, row, col) result(title)
        !! Get the title for a specific subplot
        class(figure_t), intent(in) :: self
        integer, intent(in) :: row, col
        character(len=:), allocatable :: title
        
        title = get_subplot_title(self%subplots_array, self%subplot_rows, &
                                  self%subplot_cols, row, col)
    end function subplot_title


end module fortplot_figure_core