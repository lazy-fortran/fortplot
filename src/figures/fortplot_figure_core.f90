module fortplot_figure_core
    !! Core figure management module (architecturally refactored for SOLID principles)
    !! 
    !! This module provides the main user interface for creating scientific plots
    !! with support for line plots, contour plots, and mixed plotting across
    !! PNG, PDF, and ASCII backends. Uses deferred rendering for efficiency.
    !!
    !! ARCHITECTURAL STATUS (Issue #809):
    !! - Refactored from 751 lines into focused modules <500 lines each
    !! - COMPLIANCE ACHIEVED: Now uses composition pattern with focused modules
    !! - Implementation fully distributed across Single Responsibility modules
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
    ! ARCHITECTURE REFACTOR: Facade Pattern (Issue #821)
    ! Reduced coupling from 19 dependencies to 1 comprehensive interface
    ! Facade pattern encapsulates all subsystem interactions
    
    ! Core state management (always needed)
    use fortplot_figure_initialization, only: figure_state_t
    
    ! COMPREHENSIVE OPERATIONS FACADE (1 dependency):
    ! Single interface that aggregates ALL figure operations
    use fortplot_figure_comprehensive_operations
    ! Explicit import to resolve implicit interface warnings
    use fortplot_figure_comprehensive_operations, only: figure_backend_color, figure_backend_associated, figure_backend_line
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
        procedure :: clear
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
        
        call figure_initialize(self%state, self%plots, self%streamlines, &
                              self%subplots_array, self%subplot_rows, &
                              self%subplot_cols, self%current_subplot, &
                              self%title, self%xlabel, self%ylabel, &
                              self%plot_count, width, height, backend)
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color)
        !! Add a line plot to the figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        
        call figure_add_plot_operation(self%plots, self%state, x, y, label, linestyle, color)
        self%plot_count = self%state%plot_count
        call update_data_ranges_figure(self%plots, self%state, self%state%plot_count)
    end subroutine add_plot

    subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        !! Add a contour plot to the figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        call figure_add_contour_operation(self%plots, self%state, x_grid, y_grid, z_grid, levels, label)
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
        
        call figure_add_contour_filled_operation(self%plots, self%state, x_grid, y_grid, z_grid, &
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
        
        call figure_add_pcolormesh_operation(self%plots, self%state, x, y, c, colormap, &
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
        
        call figure_streamplot_operation(self%plots, self%state, self%plot_count, x, y, u, v, &
                                         density, color, linewidth, rtol, atol, max_time)
    end subroutine streamplot

    subroutine savefig(self, filename, blocking)
        !! Save figure to file (backward compatibility version)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        
        call figure_savefig(self%state, self%plots, self%state%plot_count, filename, blocking)
    end subroutine savefig
    
    subroutine savefig_with_status(self, filename, status, blocking)
        !! Save figure to file with error status reporting
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        logical, intent(in), optional :: blocking
        
        call figure_savefig_with_status(self%state, self%plots, self%state%plot_count, &
                                        filename, status, blocking)
    end subroutine savefig_with_status

    subroutine show(self, blocking)
        !! Display the figure
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: blocking
        
        call figure_show(self%state, self%plots, self%state%plot_count, blocking)
    end subroutine show

    subroutine grid(self, enabled, which, axis, alpha, linestyle)
        !! Enable/disable and configure grid lines
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha
        
        call figure_grid_operation(self%state, enabled, which, axis, alpha, linestyle)
    end subroutine grid

    subroutine hist(self, data, bins, density, label, color)
        !! Create a histogram plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)
        
        call figure_hist_operation(self%plots, self%state, self%plot_count, data, bins, density, label, color)
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
        
        call figure_boxplot_operation(self%plots, self%plot_count, data, position, width, label, &
                                     show_outliers, horizontal, color, self%state%max_plots)
    end subroutine boxplot

    subroutine set_xlabel(self, label)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: label
        call figure_set_xlabel_operation(self%state, self%xlabel, label)
    end subroutine set_xlabel
    subroutine set_ylabel(self, label)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: label
        call figure_set_ylabel_operation(self%state, self%ylabel, label)
    end subroutine set_ylabel
    subroutine set_title(self, title)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: title
        call figure_set_title_operation(self%state, self%title, title)
    end subroutine set_title
    subroutine set_xscale(self, scale, threshold)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call figure_set_xscale_operation(self%state, scale, threshold)
    end subroutine set_xscale
    subroutine set_yscale(self, scale, threshold)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call figure_set_yscale_operation(self%state, scale, threshold)
    end subroutine set_yscale
    subroutine set_xlim(self, x_min, x_max)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x_min, x_max
        call figure_set_xlim_operation(self%state, x_min, x_max)
    end subroutine set_xlim
    subroutine set_ylim(self, y_min, y_max)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: y_min, y_max
        call figure_set_ylim_operation(self%state, y_min, y_max)
    end subroutine set_ylim
    subroutine set_line_width(self, width)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: width
        call figure_set_line_width_operation(self%state, width)
    end subroutine set_line_width
    subroutine set_ydata(self, plot_index, y_new)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: plot_index
        real(wp), intent(in) :: y_new(:)
        call figure_set_ydata_operation(self%plots, self%state%plot_count, plot_index, y_new)
    end subroutine set_ydata
    subroutine figure_legend(self, location)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in), optional :: location
        call figure_legend_operation(self%state%legend_data, self%state%show_legend, &
                                    self%plots, self%state%plot_count, location)
    end subroutine figure_legend
    subroutine clear(self)
        !! Clear the figure for reuse, preserving backend settings
        class(figure_t), intent(inout) :: self
        call figure_clear(self%state, self%plots, self%streamlines, &
                         self%subplots_array, self%subplot_rows, self%subplot_cols, &
                         self%current_subplot, self%title, self%xlabel, self%ylabel, &
                         self%plot_count, self%annotation_count)
    end subroutine clear
    subroutine clear_streamlines(self)
        class(figure_t), intent(inout) :: self
        call figure_clear_streamlines(self%streamlines)
    end subroutine clear_streamlines
    subroutine destroy(self)
        type(figure_t), intent(inout) :: self
        call figure_destroy(self%state, self%plots, self%streamlines, &
                           self%title, self%xlabel, self%ylabel)
    end subroutine destroy

    ! Property accessors - delegate to properties module
    function get_width(self) result(width)
        class(figure_t), intent(in) :: self; integer :: width
        width = figure_get_width(self%state)
    end function get_width
    function get_height(self) result(height)
        class(figure_t), intent(in) :: self; integer :: height
        height = figure_get_height(self%state)
    end function get_height
    function get_rendered(self) result(rendered)
        class(figure_t), intent(in) :: self; logical :: rendered
        rendered = figure_get_rendered(self%state)
    end function get_rendered
    subroutine set_rendered(self, rendered)
        class(figure_t), intent(inout) :: self; logical, intent(in) :: rendered
        call figure_set_rendered(self%state, rendered)
    end subroutine set_rendered
    function get_plot_count(self) result(plot_count)
        class(figure_t), intent(in) :: self; integer :: plot_count
        plot_count = figure_get_plot_count(self%state)
    end function get_plot_count
    function get_plots(self) result(plots_ptr)
        class(figure_t), intent(in), target :: self; type(plot_data_t), pointer :: plots_ptr(:)
        plots_ptr => figure_get_plots(self%plots)
    end function get_plots
    
    ! Animation support - delegate to animation module
    subroutine setup_png_backend_for_animation(self)
        class(figure_t), intent(inout) :: self
        call figure_setup_png_backend_for_animation(self%state)
    end subroutine setup_png_backend_for_animation
    subroutine extract_rgb_data_for_animation(self, rgb_data)
        class(figure_t), intent(inout) :: self; real(wp), intent(out) :: rgb_data(:,:,:)
        if (.not. self%state%rendered) call figure_render(self%state, self%plots, self%state%plot_count)
        call figure_extract_rgb_data_for_animation(self%state, rgb_data, self%state%rendered)
    end subroutine extract_rgb_data_for_animation
    subroutine extract_png_data_for_animation(self, png_data, status)
        class(figure_t), intent(inout) :: self; integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        if (.not. self%state%rendered) call figure_render(self%state, self%plots, self%state%plot_count)
        call figure_extract_png_data_for_animation(self%state, png_data, status, self%state%rendered)
    end subroutine extract_png_data_for_animation
    ! Backend interface and coordinate accessors - delegate to properties module
    subroutine backend_color(self, r, g, b)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: r, g, b
        call figure_backend_color(self%state, r, g, b)
    end subroutine backend_color
    function backend_associated(self) result(is_associated)
        class(figure_t), intent(in) :: self; logical :: is_associated
        is_associated = figure_backend_associated(self%state)
    end function backend_associated
    subroutine backend_line(self, x1, y1, x2, y2)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x1, y1, x2, y2
        call figure_backend_line(self%state, x1, y1, x2, y2)
    end subroutine backend_line
    function get_x_min(self) result(x_min)
        class(figure_t), intent(in) :: self; real(wp) :: x_min
        x_min = figure_get_x_min(self%state)
    end function get_x_min
    function get_x_max(self) result(x_max)
        class(figure_t), intent(in) :: self; real(wp) :: x_max
        x_max = figure_get_x_max(self%state)
    end function get_x_max
    function get_y_min(self) result(y_min)
        class(figure_t), intent(in) :: self; real(wp) :: y_min
        y_min = figure_get_y_min(self%state)
    end function get_y_min
    function get_y_max(self) result(y_max)
        class(figure_t), intent(in) :: self; real(wp) :: y_max
        y_max = figure_get_y_max(self%state)
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
        call figure_scatter_operation(self%plots, self%state%plot_count, &
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
        class(figure_t), intent(inout) :: self; integer, intent(in) :: nrows, ncols
        call figure_subplots(self%subplots_array, self%subplot_rows, &
                            self%subplot_cols, self%current_subplot, nrows, ncols)
    end subroutine subplots
    
    subroutine subplot_plot(self, row, col, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: row, col
        real(wp), intent(in) :: x(:), y(:); character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        call figure_subplot_plot(self%subplots_array, self%subplot_rows, &
                                self%subplot_cols, row, col, x, y, label, &
                                linestyle, color, self%state%colors, 6)
    end subroutine subplot_plot
    
    function subplot_plot_count(self, row, col) result(count)
        class(figure_t), intent(in) :: self; integer, intent(in) :: row, col; integer :: count
        count = figure_subplot_plot_count(self%subplots_array, self%subplot_rows, &
                                         self%subplot_cols, row, col)
    end function subplot_plot_count
    
    subroutine subplot_set_title(self, row, col, title)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: row, col
        character(len=*), intent(in) :: title
        call figure_subplot_set_title(self%subplots_array, self%subplot_rows, &
                                     self%subplot_cols, row, col, title)
    end subroutine subplot_set_title
    
    subroutine subplot_set_xlabel(self, row, col, xlabel)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: row, col
        character(len=*), intent(in) :: xlabel
        call figure_subplot_set_xlabel(self%subplots_array, self%subplot_rows, &
                                      self%subplot_cols, row, col, xlabel)
    end subroutine subplot_set_xlabel
    
    subroutine subplot_set_ylabel(self, row, col, ylabel)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: row, col
        character(len=*), intent(in) :: ylabel
        call figure_subplot_set_ylabel(self%subplots_array, self%subplot_rows, &
                                      self%subplot_cols, row, col, ylabel)
    end subroutine subplot_set_ylabel
    
    function subplot_title(self, row, col) result(title)
        class(figure_t), intent(in) :: self; integer, intent(in) :: row, col
        character(len=:), allocatable :: title
        title = figure_subplot_title(self%subplots_array, self%subplot_rows, &
                                     self%subplot_cols, row, col)
    end function subplot_title

end module fortplot_figure_core