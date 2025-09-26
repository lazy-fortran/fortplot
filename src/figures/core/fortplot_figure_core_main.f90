module fortplot_figure_core

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_annotations, only: text_annotation_t
    use fortplot_logging, only: log_error, log_warning
    ! Import refactored modules
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, subplot_data_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, &
                                    PLOT_TYPE_PCOLORMESH, PLOT_TYPE_BOXPLOT, &
                                    PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
                                    PLOT_TYPE_SURFACE
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_plot_management, only: next_plot_color
    use fortplot_figure_comprehensive_operations
    use fortplot_figure_comprehensive_operations, only: figure_backend_color, &
        figure_backend_associated, figure_backend_line
    implicit none

    private
    public :: figure_t, plot_data_t, subplot_data_t
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
              PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
              PLOT_TYPE_SURFACE

    !! CORE TYPE DEFINITION
    type :: figure_t
        !! Main figure class implementing Facade Pattern for plotting operations
        !!
        !! This type provides a unified interface for all figure operations while
        !! delegating implementation details to specialized modules. The design 
        !! follows object-oriented principles with clear separation of concerns.
        type(figure_state_t) :: state
        
        type(plot_data_t), allocatable :: plots(:)
        type(plot_data_t), allocatable :: streamlines(:)
        type(arrow_data_t), allocatable :: arrow_data(:)
        type(text_annotation_t), allocatable :: annotations(:)
        integer :: annotation_count = 0
        integer :: max_annotations = 1000
        integer :: subplot_rows = 0
        integer :: subplot_cols = 0
        integer :: current_subplot = 1
        type(subplot_data_t), allocatable :: subplots_array(:,:)
        character(len=:), allocatable :: title
        character(len=:), allocatable :: xlabel
        character(len=:), allocatable :: ylabel
        integer :: plot_count = 0
        
    contains
        procedure :: initialize
        procedure :: add_plot
        procedure :: plot => add_plot
        procedure :: add_contour
        procedure :: add_contour_filled
        procedure :: add_surface
        procedure :: add_pcolormesh
        procedure :: streamplot
        procedure :: savefig
        procedure :: save => savefig
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
        procedure :: add_hist
        procedure :: hist => add_hist
        procedure :: boxplot
        procedure :: scatter
        procedure :: add_imshow
        procedure :: add_pie
        procedure :: add_polar
        procedure :: add_step
        procedure :: add_stem
        procedure :: add_fill
        procedure :: add_fill_between
        procedure :: twinx
        procedure :: twiny
        ! Subplot methods
        procedure :: subplots
        procedure :: subplot_plot
        procedure :: subplot_plot_count
        procedure :: subplot_set_title
        procedure :: subplot_set_xlabel
        procedure :: subplot_set_ylabel
        procedure :: subplot_title
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
        procedure :: backend_arrow
        procedure :: backend_associated
        procedure :: get_x_min
        procedure :: get_x_max
        procedure :: get_y_min
        procedure :: get_y_max
        final :: destroy
    end type figure_t

    interface
        module subroutine add_imshow(self, z, xlim, ylim, cmap, alpha, vmin, vmax, &
                                     origin, extent, interpolation, aspect)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: z(:,:)
            real(wp), intent(in), optional :: xlim(2), ylim(2)
            character(len=*), intent(in), optional :: cmap, origin
            character(len=*), intent(in), optional :: interpolation, aspect
            real(wp), intent(in), optional :: alpha, vmin, vmax
            real(wp), intent(in), optional :: extent(4)
        end subroutine add_imshow

        module subroutine add_polar(self, theta, r, label, fmt, linestyle, marker, &
                                    color)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: theta(:), r(:)
            character(len=*), intent(in), optional :: label, fmt
            character(len=*), intent(in), optional :: linestyle, marker, color
        end subroutine add_polar

        module subroutine add_step(self, x, y, label, where, linestyle, color, &
                                   linewidth)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x(:), y(:)
            character(len=*), intent(in), optional :: label, where
            character(len=*), intent(in), optional :: linestyle, color
            real(wp), intent(in), optional :: linewidth
        end subroutine add_step

        module subroutine add_stem(self, x, y, label, linefmt, markerfmt, basefmt, &
                                   bottom)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x(:), y(:)
            character(len=*), intent(in), optional :: label, linefmt
            character(len=*), intent(in), optional :: markerfmt, basefmt
            real(wp), intent(in), optional :: bottom
        end subroutine add_stem

        module subroutine add_fill(self, x, y, color, alpha)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x(:), y(:)
            character(len=*), intent(in), optional :: color
            real(wp), intent(in), optional :: alpha
        end subroutine add_fill

        module subroutine add_fill_between(self, x, y1, y2, where, color, alpha, &
                                           interpolate)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x(:)
            real(wp), intent(in), optional :: y1(:), y2(:)
            logical, intent(in), optional :: where(:)
            character(len=*), intent(in), optional :: color
            real(wp), intent(in), optional :: alpha
            logical, intent(in), optional :: interpolate
        end subroutine add_fill_between

        module subroutine add_pie(self, values, labels, autopct, startangle, colors, &
                                  explode)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: values(:)
            character(len=*), intent(in), optional :: labels(:)
            character(len=*), intent(in), optional :: autopct
            real(wp), intent(in), optional :: startangle
            character(len=*), intent(in), optional :: colors(:)
            real(wp), intent(in), optional :: explode(:)
        end subroutine add_pie
    end interface

contains

    !! CORE OPERATIONS - Delegated to specialized modules

    subroutine initialize(self, width, height, backend)
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        
        call core_initialize(self%state, self%plots, self%streamlines, &
                            self%subplots_array, self%subplot_rows, &
                            self%subplot_cols, self%current_subplot, &
                            self%title, self%xlabel, self%ylabel, &
                            self%plot_count, width, height, backend)
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        
        call core_add_plot(self%plots, self%state, x, y, label, linestyle, color, &
                           self%plot_count)
    end subroutine add_plot

    subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        call core_add_contour(self%plots, self%state, x_grid, y_grid, z_grid, &
                              levels, label, self%plot_count)
    end subroutine add_contour

    subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, colormap, &
                                  show_colorbar, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar

        call core_add_contour_filled(self%plots, self%state, x_grid, y_grid, &
                                     z_grid, levels, colormap, show_colorbar, &
                                     label, self%plot_count)
    end subroutine add_contour_filled

    subroutine add_surface(self, x_grid, y_grid, z_grid, label, colormap, &
                           show_colorbar, alpha, edgecolor, linewidth)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        character(len=*), intent(in), optional :: label, colormap
        logical, intent(in), optional :: show_colorbar
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        call core_add_surface(self%plots, self%state, x_grid, y_grid, z_grid, &
                              label, colormap, show_colorbar, alpha, edgecolor, &
                              linewidth, self%plot_count)
    end subroutine add_surface

    subroutine add_pcolormesh(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        
        call core_add_pcolormesh(self%plots, self%state, x, y, c, colormap, &
                                 vmin, vmax, edgecolors, linewidths, &
                                 self%plot_count)
    end subroutine add_pcolormesh

    subroutine streamplot(self, x, y, u, v, density, color, linewidth, rtol, &
                          atol, max_time)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth
        real(wp), intent(in), optional :: rtol, atol, max_time
        real(wp) :: lw_dummy1, rt_dummy1, at_dummy1, mt_dummy1
        if (present(linewidth)) lw_dummy1 = linewidth
        if (present(rtol)) rt_dummy1 = rtol
        if (present(atol)) at_dummy1 = atol
        if (present(max_time)) mt_dummy1 = max_time
        
        call core_streamplot(self%plots, self%state, self%plot_count, x, y, u, v, &
                            density, color)
    end subroutine streamplot

    !! I/O OPERATIONS - Delegated to core I/O module
    
    subroutine savefig(self, filename, blocking)
        !! Save figure to file (backward compatibility version)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        
        call core_savefig(self%state, self%plots, self%plot_count, filename, &
                          blocking, self%annotations, self%annotation_count, &
                          self%subplots_array, self%subplot_rows, &
                          self%subplot_cols)
    end subroutine savefig
    
    subroutine savefig_with_status(self, filename, status, blocking)
        !! Save figure to file with error status reporting
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        logical, intent(in), optional :: blocking
        
        call core_savefig_with_status(self%state, self%plots, self%plot_count, &
                                      filename, status, blocking, &
                                      self%annotations, self%annotation_count, &
                                      self%subplots_array, self%subplot_rows, &
                                      self%subplot_cols)
    end subroutine savefig_with_status

    subroutine show(self, blocking)
        !! Display the figure
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: blocking
        
        call core_show(self%state, self%plots, self%plot_count, blocking, &
                       self%annotations, self%annotation_count, &
                       self%subplots_array, self%subplot_rows, &
                       self%subplot_cols)
    end subroutine show

    !! CONFIGURATION METHODS - Delegated to core config module
    
    subroutine grid(self, enabled, which, axis, alpha, linestyle)
        !! Enable/disable and configure grid lines
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha
        
        call core_grid(self%state, enabled, which, axis, alpha, linestyle)
    end subroutine grid

    subroutine add_hist(self, data, bins, density, label, color)
        !! Create a histogram plot
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)

        call core_hist(self%plots, self%state, self%plot_count, data, bins, &
                       density, label, color)
    end subroutine add_hist

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
        
        call core_boxplot(self%plots, self%plot_count, data, position, width, &
                          label, show_outliers, horizontal, color, &
                          self%state%max_plots)
    end subroutine boxplot

    subroutine set_xlabel(self, label)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        call core_set_xlabel(self%state, self%xlabel, label)
    end subroutine set_xlabel
    subroutine set_ylabel(self, label)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        call core_set_ylabel(self%state, self%ylabel, label)
    end subroutine set_ylabel
    subroutine set_title(self, title)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: title
        call core_set_title(self%state, self%title, title)
    end subroutine set_title
    subroutine set_xscale(self, scale, threshold)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call core_set_xscale(self%state, scale, threshold)
    end subroutine set_xscale
    subroutine set_yscale(self, scale, threshold)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call core_set_yscale(self%state, scale, threshold)
    end subroutine set_yscale
    subroutine set_xlim(self, x_min, x_max)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_min, x_max
        call core_set_xlim(self%state, x_min, x_max)
    end subroutine set_xlim
    subroutine set_ylim(self, y_min, y_max)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y_min, y_max
        call core_set_ylim(self%state, y_min, y_max)
    end subroutine set_ylim
    subroutine set_line_width(self, width)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: width
        call core_set_line_width(self%state, width)
    end subroutine set_line_width
    subroutine set_ydata(self, plot_index, y_new)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        real(wp), intent(in) :: y_new(:)
        call core_set_ydata(self%plots, self%state%plot_count, plot_index, y_new)
    end subroutine set_ydata
    subroutine figure_legend(self, location)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in), optional :: location
        call core_figure_legend(self%state, self%plots, self%state%plot_count, &
                                location)
    end subroutine figure_legend
    subroutine clear(self)
        !! Clear the figure for reuse, preserving backend settings
        class(figure_t), intent(inout) :: self
        call core_clear(self%state, self%streamlines, &
                       self%subplots_array, self%subplot_rows, self%subplot_cols, &
                       self%current_subplot, self%title, self%xlabel, self%ylabel, &
                       self%plot_count, self%annotation_count)
    end subroutine clear
    subroutine clear_streamlines(self)
        class(figure_t), intent(inout) :: self
        call core_clear_streamlines(self%streamlines)
    end subroutine clear_streamlines
    subroutine destroy(self)
        type(figure_t), intent(inout) :: self
        call core_destroy(self%state, self%plots, self%streamlines, &
                         self%title, self%xlabel, self%ylabel)
    end subroutine destroy

    !! PROPERTY ACCESSORS - Delegated to core accessors module
    
    function get_width(self) result(width)
        class(figure_t), intent(in) :: self
        integer :: width
        width = core_get_width(self%state)
    end function get_width
    function get_height(self) result(height)
        class(figure_t), intent(in) :: self
        integer :: height
        height = core_get_height(self%state)
    end function get_height
    function get_rendered(self) result(rendered)
        class(figure_t), intent(in) :: self
        logical :: rendered
        rendered = core_get_rendered(self%state)
    end function get_rendered
    subroutine set_rendered(self, rendered)
        class(figure_t), intent(inout) :: self
        logical, intent(in) :: rendered
        call core_set_rendered(self%state, rendered)
    end subroutine set_rendered
    function get_plot_count(self) result(plot_count)
        class(figure_t), intent(in) :: self
        integer :: plot_count
        plot_count = core_get_plot_count(self%state)
    end function get_plot_count
    function get_plots(self) result(plots_ptr)
        class(figure_t), intent(in), target :: self
        type(plot_data_t), pointer :: plots_ptr(:)
        plots_ptr => core_get_plots(self%plots)
    end function get_plots
    
    ! Animation support - delegate to animation module
    subroutine setup_png_backend_for_animation(self)
        class(figure_t), intent(inout) :: self
        call core_setup_png_backend_for_animation(self%state)
    end subroutine setup_png_backend_for_animation
    subroutine extract_rgb_data_for_animation(self, rgb_data)
        class(figure_t), intent(inout) :: self
        real(wp), intent(out) :: rgb_data(:,:,:)
        call core_extract_rgb_data_for_animation(self%state, rgb_data, &
            self%plots, self%state%plot_count, self%annotations, &
            self%annotation_count, self%state%rendered)
    end subroutine extract_rgb_data_for_animation
    subroutine extract_png_data_for_animation(self, png_data, status)
        class(figure_t), intent(inout) :: self
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        call core_extract_png_data_for_animation(self%state, png_data, status, &
            self%plots, self%state%plot_count, self%annotations, &
            self%annotation_count, self%state%rendered)
    end subroutine extract_png_data_for_animation
    ! Backend interface and coordinate accessors - delegate to properties module
    subroutine backend_color(self, r, g, b)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: r, g, b
        call core_backend_color(self%state, r, g, b)
    end subroutine backend_color
    function backend_associated(self) result(is_associated)
        class(figure_t), intent(in) :: self
        logical :: is_associated
        is_associated = core_backend_associated(self%state)
    end function backend_associated
    subroutine backend_line(self, x1, y1, x2, y2)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x1, y1, x2, y2
        call core_backend_line(self%state, x1, y1, x2, y2)
    end subroutine backend_line
    subroutine backend_arrow(self, x, y, dx, dy, size, style)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        call core_backend_arrow(self%state, x, y, dx, dy, size, style)
    end subroutine backend_arrow
    function get_x_min(self) result(x_min)
        class(figure_t), intent(in) :: self
        real(wp) :: x_min
        x_min = core_get_x_min(self%state)
    end function get_x_min
    function get_x_max(self) result(x_max)
        class(figure_t), intent(in) :: self
        real(wp) :: x_max
        x_max = core_get_x_max(self%state)
    end function get_x_max
    function get_y_min(self) result(y_min)
        class(figure_t), intent(in) :: self
        real(wp) :: y_min
        y_min = core_get_y_min(self%state)
    end function get_y_min
    function get_y_max(self) result(y_max)
        class(figure_t), intent(in) :: self
        real(wp) :: y_max
        y_max = core_get_y_max(self%state)
    end function get_y_max

    !! ADVANCED PLOTTING - Delegated to core advanced module
    
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
        real(wp) :: al_dummy1, ec_dummy1, fc_dummy1, lw_dummy2
        
        ! Get default color from state using shared cycling logic
        default_color = next_plot_color(self%state)
        if (present(alpha)) al_dummy1 = alpha
        if (present(edgecolor)) ec_dummy1 = edgecolor(1)
        if (present(facecolor)) fc_dummy1 = facecolor(1)
        if (present(linewidth)) lw_dummy2 = linewidth
        
        call core_scatter(self%plots, self%state, self%plot_count, x, y, s, c, &
                         marker, markersize, color, colormap, vmin, vmax, label, &
                         show_colorbar, default_color)
    end subroutine scatter

    !! Placeholder twin-axis helpers (currently unimplemented)

    subroutine twinx(self)
        class(figure_t), intent(inout) :: self
        call log_warning('twinx: dual axis plots not yet implemented')
    end subroutine twinx

    subroutine twiny(self)
        class(figure_t), intent(inout) :: self
        call log_warning('twiny: dual axis plots not yet implemented')
    end subroutine twiny

    !! SUBPLOT OPERATIONS - Delegated to management module
    
    subroutine subplots(self, nrows, ncols)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: nrows, ncols
        call figure_subplots(self%subplots_array, self%subplot_rows, &
                             self%subplot_cols, self%current_subplot, nrows, &
                             ncols)
    end subroutine subplots
    
    subroutine subplot_plot(self, row, col, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        call figure_subplot_plot(self%subplots_array, self%subplot_rows, &
                                 self%subplot_cols, row, col, x, y, label, &
                                 linestyle, color, self%state%colors, 6)
    end subroutine subplot_plot
    
    function subplot_plot_count(self, row, col) result(count)
        class(figure_t), intent(in) :: self
        integer, intent(in) :: row, col
        integer :: count
        count = figure_subplot_plot_count(self%subplots_array, self%subplot_rows, &
                                          self%subplot_cols, row, col)
    end function subplot_plot_count
    
    subroutine subplot_set_title(self, row, col, title)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: title
        call figure_subplot_set_title(self%subplots_array, self%subplot_rows, &
                                     self%subplot_cols, row, col, title)
    end subroutine subplot_set_title
    
    subroutine subplot_set_xlabel(self, row, col, xlabel)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: xlabel
        call figure_subplot_set_xlabel(self%subplots_array, self%subplot_rows, &
                                      self%subplot_cols, row, col, xlabel)
    end subroutine subplot_set_xlabel
    
    subroutine subplot_set_ylabel(self, row, col, ylabel)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: ylabel
        call figure_subplot_set_ylabel(self%subplots_array, self%subplot_rows, &
                                      self%subplot_cols, row, col, ylabel)
    end subroutine subplot_set_ylabel
    
    function subplot_title(self, row, col) result(title)
        class(figure_t), intent(in) :: self
        integer, intent(in) :: row, col
        character(len=:), allocatable :: title
        title = figure_subplot_title(self%subplots_array, self%subplot_rows, &
                                     self%subplot_cols, row, col)
    end function subplot_title

end module fortplot_figure_core
