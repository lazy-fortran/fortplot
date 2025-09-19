! Consolidated (Issue #934)

! ==== Begin: src/figures/core/fortplot_figure_core.f90 ====

module fortplot_figure_core
    !! Core figure management module for scientific plotting
    !!
    !! ARCHITECTURAL DESIGN:
    !! ====================
    !! This module implements the Facade Pattern to provide a clean, unified
    !! interface for figure operations while delegating complex functionality
    !! to specialized modules.
    !!
    !! DESIGN PATTERNS USED:
    !! - Facade Pattern: Single entry point hiding subsystem complexity
    !! - Delegation Pattern: Methods delegate to specialized operations modules
    !! - Strategy Pattern: Backend selection for rendering
    !! - State Pattern: Figure state management through figure_state_t
    !!
    !! ARCHITECTURAL BENEFITS:
    !! - Clean separation between interface and implementation
    !! - Reduced coupling through dependency injection
    !! - Maintainable code through clear responsibility boundaries
    !! - Extensible design supporting new plot types and backends
    !!
    !! DELEGATION ARCHITECTURE: All operations delegate to specialized modules
    !! maintaining clean separation of concerns and architectural compliance.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_annotations, only: text_annotation_t
    use fortplot_logging, only: log_error, log_warning
    ! Import refactored modules
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, subplot_data_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, &
                                    PLOT_TYPE_PCOLORMESH, PLOT_TYPE_BOXPLOT, &
                                    PLOT_TYPE_SCATTER, PLOT_TYPE_FILL
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_plot_management, only: next_plot_color
    use fortplot_figure_comprehensive_operations
    use fortplot_figure_comprehensive_operations, only: figure_backend_color, figure_backend_associated, figure_backend_line
    implicit none

    private
    public :: figure_t, plot_data_t, subplot_data_t
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
              PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER, PLOT_TYPE_FILL

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
        
        call core_add_plot(self%plots, self%state, x, y, label, linestyle, color, self%plot_count)
    end subroutine add_plot

    subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label
        
        call core_add_contour(self%plots, self%state, x_grid, y_grid, z_grid, levels, label, self%plot_count)
    end subroutine add_contour

    subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        
        call core_add_contour_filled(self%plots, self%state, x_grid, y_grid, z_grid, &
                                     levels, colormap, show_colorbar, label, self%plot_count)
    end subroutine add_contour_filled

    subroutine add_pcolormesh(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths
        
        call core_add_pcolormesh(self%plots, self%state, x, y, c, colormap, &
                                vmin, vmax, edgecolors, linewidths, self%plot_count)
    end subroutine add_pcolormesh

    subroutine streamplot(self, x, y, u, v, density, color, linewidth, rtol, atol, max_time)
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
        
        call core_savefig(self%state, self%plots, self%plot_count, filename, blocking, &
                         self%annotations, self%annotation_count, &
                         self%subplots_array, self%subplot_rows, self%subplot_cols)
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
                                     self%subplots_array, self%subplot_rows, self%subplot_cols)
    end subroutine savefig_with_status

    subroutine show(self, blocking)
        !! Display the figure
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: blocking
        
        call core_show(self%state, self%plots, self%plot_count, blocking, &
                      self%annotations, self%annotation_count, &
                      self%subplots_array, self%subplot_rows, self%subplot_cols)
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

        call core_hist(self%plots, self%state, self%plot_count, data, bins, density, label, color)
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
        
        call core_boxplot(self%plots, self%plot_count, data, position, width, label, &
                         show_outliers, horizontal, color, self%state%max_plots)
    end subroutine boxplot

    subroutine set_xlabel(self, label)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: label
        call core_set_xlabel(self%state, self%xlabel, label)
    end subroutine set_xlabel
    subroutine set_ylabel(self, label)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: label
        call core_set_ylabel(self%state, self%ylabel, label)
    end subroutine set_ylabel
    subroutine set_title(self, title)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: title
        call core_set_title(self%state, self%title, title)
    end subroutine set_title
    subroutine set_xscale(self, scale, threshold)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call core_set_xscale(self%state, scale, threshold)
    end subroutine set_xscale
    subroutine set_yscale(self, scale, threshold)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold
        call core_set_yscale(self%state, scale, threshold)
    end subroutine set_yscale
    subroutine set_xlim(self, x_min, x_max)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x_min, x_max
        call core_set_xlim(self%state, x_min, x_max)
    end subroutine set_xlim
    subroutine set_ylim(self, y_min, y_max)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: y_min, y_max
        call core_set_ylim(self%state, y_min, y_max)
    end subroutine set_ylim
    subroutine set_line_width(self, width)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: width
        call core_set_line_width(self%state, width)
    end subroutine set_line_width
    subroutine set_ydata(self, plot_index, y_new)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: plot_index
        real(wp), intent(in) :: y_new(:)
        call core_set_ydata(self%plots, self%state%plot_count, plot_index, y_new)
    end subroutine set_ydata
    subroutine figure_legend(self, location)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in), optional :: location
        call core_figure_legend(self%state, self%plots, self%state%plot_count, location)
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
        class(figure_t), intent(in) :: self; integer :: width
        width = core_get_width(self%state)
    end function get_width
    function get_height(self) result(height)
        class(figure_t), intent(in) :: self; integer :: height
        height = core_get_height(self%state)
    end function get_height
    function get_rendered(self) result(rendered)
        class(figure_t), intent(in) :: self; logical :: rendered
        rendered = core_get_rendered(self%state)
    end function get_rendered
    subroutine set_rendered(self, rendered)
        class(figure_t), intent(inout) :: self; logical, intent(in) :: rendered
        call core_set_rendered(self%state, rendered)
    end subroutine set_rendered
    function get_plot_count(self) result(plot_count)
        class(figure_t), intent(in) :: self; integer :: plot_count
        plot_count = core_get_plot_count(self%state)
    end function get_plot_count
    function get_plots(self) result(plots_ptr)
        class(figure_t), intent(in), target :: self; type(plot_data_t), pointer :: plots_ptr(:)
        plots_ptr => core_get_plots(self%plots)
    end function get_plots
    
    ! Animation support - delegate to animation module
    subroutine setup_png_backend_for_animation(self)
        class(figure_t), intent(inout) :: self
        call core_setup_png_backend_for_animation(self%state)
    end subroutine setup_png_backend_for_animation
    subroutine extract_rgb_data_for_animation(self, rgb_data)
        class(figure_t), intent(inout) :: self; real(wp), intent(out) :: rgb_data(:,:,:)
        call core_extract_rgb_data_for_animation(self%state, rgb_data, self%plots, &
            self%state%plot_count, self%annotations, self%annotation_count, self%state%rendered)
    end subroutine extract_rgb_data_for_animation
    subroutine extract_png_data_for_animation(self, png_data, status)
        class(figure_t), intent(inout) :: self; integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        call core_extract_png_data_for_animation(self%state, png_data, status, self%plots, &
            self%state%plot_count, self%annotations, self%annotation_count, self%state%rendered)
    end subroutine extract_png_data_for_animation
    ! Backend interface and coordinate accessors - delegate to properties module
    subroutine backend_color(self, r, g, b)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: r, g, b
        call core_backend_color(self%state, r, g, b)
    end subroutine backend_color
    function backend_associated(self) result(is_associated)
        class(figure_t), intent(in) :: self; logical :: is_associated
        is_associated = core_backend_associated(self%state)
    end function backend_associated
    subroutine backend_line(self, x1, y1, x2, y2)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x1, y1, x2, y2
        call core_backend_line(self%state, x1, y1, x2, y2)
    end subroutine backend_line
    subroutine backend_arrow(self, x, y, dx, dy, size, style)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        call core_backend_arrow(self%state, x, y, dx, dy, size, style)
    end subroutine backend_arrow
    function get_x_min(self) result(x_min)
        class(figure_t), intent(in) :: self; real(wp) :: x_min
        x_min = core_get_x_min(self%state)
    end function get_x_min
    function get_x_max(self) result(x_max)
        class(figure_t), intent(in) :: self; real(wp) :: x_max
        x_max = core_get_x_max(self%state)
    end function get_x_max
    function get_y_min(self) result(y_min)
        class(figure_t), intent(in) :: self; real(wp) :: y_min
        y_min = core_get_y_min(self%state)
    end function get_y_min
    function get_y_max(self) result(y_max)
        class(figure_t), intent(in) :: self; real(wp) :: y_max
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

    subroutine add_imshow(self, z, xlim, ylim, cmap, alpha, vmin, vmax, origin, &
                          extent, interpolation, aspect)
        !! Display 2D array as an image using the pcolormesh backend
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: z(:,:)
        real(wp), intent(in), optional :: xlim(2), ylim(2)
        character(len=*), intent(in), optional :: cmap, origin, interpolation, aspect
        real(wp), intent(in), optional :: alpha, vmin, vmax
        real(wp), intent(in), optional :: extent(4)

        integer :: nx, ny, i
        real(wp) :: x0, x1, y0, y1, tmp_edge
        real(wp), allocatable :: x_edges(:), y_edges(:), z_flip(:,:)
        character(len=8) :: origin_mode

        nx = size(z, 2)
        ny = size(z, 1)
        if (nx == 0 .or. ny == 0) then
            call log_error("imshow: input array must be non-empty")
            return
        end if

        x0 = 0.0_wp; x1 = real(nx, wp)
        y0 = 0.0_wp; y1 = real(ny, wp)
        if (present(extent)) then
            if (size(extent) /= 4) then
                call log_error("imshow: extent must contain exactly 4 values")
                return
            end if
            x0 = extent(1); x1 = extent(2)
            y0 = extent(3); y1 = extent(4)
            if (present(xlim) .or. present(ylim)) then
                call log_warning('imshow: ignoring xlim/ylim because extent is set')
            end if
        else
            if (present(xlim)) then
                x0 = xlim(1)
                x1 = xlim(2)
            end if
            if (present(ylim)) then
                y0 = ylim(1)
                y1 = ylim(2)
            end if
        end if

        allocate(x_edges(nx+1), y_edges(ny+1))
        do i = 1, nx + 1
            x_edges(i) = x0 + (x1 - x0) * real(i - 1, wp) / real(nx, wp)
        end do
        do i = 1, ny + 1
            y_edges(i) = y0 + (y1 - y0) * real(i - 1, wp) / real(ny, wp)
        end do

        origin_mode = 'lower'
        if (present(origin)) then
            select case (trim(origin))
            case ('upper', 'Upper', 'UPPER')
                origin_mode = 'upper'
            case ('lower', 'Lower', 'LOWER')
                origin_mode = 'lower'
            case default
                call log_warning('imshow: unsupported origin "' // trim(origin) // &
                                 '"; using "lower"')
            end select
        end if

        if (origin_mode == 'upper') then
            do i = 1, ny/2
                tmp_edge = y_edges(i)
                y_edges(i) = y_edges(ny - i + 2)
                y_edges(ny - i + 2) = tmp_edge
            end do
            allocate(z_flip(ny, nx))
            do i = 1, ny
                z_flip(i, :) = z(ny - i + 1, :)
            end do
        end if

        if (present(alpha)) then
            call log_warning('imshow: alpha not yet supported')
        end if
        if (present(interpolation)) then
            call log_warning('imshow: interpolation ignored by current backend')
        end if
        if (present(aspect)) then
            call log_warning('imshow: aspect not configurable on current backend')
        end if

        if (origin_mode == 'upper') then
            call self%add_pcolormesh(x_edges, y_edges, z_flip, colormap=cmap, &
                                     vmin=vmin, vmax=vmax)
            deallocate(z_flip)
        else
            call self%add_pcolormesh(x_edges, y_edges, z, colormap=cmap, &
                                     vmin=vmin, vmax=vmax)
        end if

        deallocate(x_edges, y_edges)
    end subroutine add_imshow

    subroutine add_polar(self, theta, r, label, fmt, linestyle, marker, color)
        !! Plot data provided in polar coordinates by converting to Cartesian
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: label, fmt
        character(len=*), intent(in), optional :: linestyle, marker, color

        integer :: n, i
        real(wp), allocatable :: x(:), y(:)

        n = min(size(theta), size(r))
        if (n == 0) then
            call log_error('polar: theta and r must contain values')
            return
        end if

        allocate(x(n), y(n))
        do i = 1, n
            x(i) = r(i) * cos(theta(i))
            y(i) = r(i) * sin(theta(i))
        end do

        if (present(fmt)) then
            call log_warning('polar: fmt ignored; use linestyle/marker arguments')
        end if
        if (present(marker)) then
            call log_warning('polar: marker styling not yet supported')
        end if
        if (present(color)) then
            call log_warning('polar: color strings not mapped to RGB yet')
        end if

        call self%add_plot(x, y, label=label, linestyle=linestyle)
        deallocate(x, y)
    end subroutine add_polar

    subroutine add_step(self, x, y, label, where, linestyle, color, linewidth)
        !! Create a stepped line plot using repeated x positions
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, where, linestyle, color
        real(wp), intent(in), optional :: linewidth

        integer :: n, i, n_points
        character(len=8) :: step_type
        real(wp), allocatable :: x_step(:), y_step(:)

        n = min(size(x), size(y))
        if (n < 2) then
            call log_error('step: need at least two samples')
            return
        end if

        step_type = 'pre'
        if (present(where)) then
            select case (trim(where))
            case ('post', 'Post', 'POST')
                step_type = 'post'
            case ('mid', 'Mid', 'MID')
                step_type = 'mid'
            case ('pre', 'Pre', 'PRE')
                step_type = 'pre'
            case default
                call log_warning('step: unsupported where value; using "pre"')
            end select
        end if

        select case (step_type)
        case ('pre', 'PRE')
            n_points = 2 * n - 1
            allocate(x_step(n_points), y_step(n_points))
            do i = 1, n - 1
                x_step(2 * i - 1) = x(i)
                y_step(2 * i - 1) = y(i)
                x_step(2 * i) = x(i + 1)
                y_step(2 * i) = y(i)
            end do
            x_step(n_points) = x(n)
            y_step(n_points) = y(n)

        case ('post', 'POST')
            n_points = 2 * n - 1
            allocate(x_step(n_points), y_step(n_points))
            x_step(1) = x(1)
            y_step(1) = y(1)
            do i = 2, n
                x_step(2 * i - 2) = x(i)
                y_step(2 * i - 2) = y(i - 1)
                x_step(2 * i - 1) = x(i)
                y_step(2 * i - 1) = y(i)
            end do

        case ('mid', 'MID')
            n_points = 2 * n
            allocate(x_step(n_points), y_step(n_points))
            do i = 1, n - 1
                x_step(2 * i - 1) = x(i)
                y_step(2 * i - 1) = y(i)
                x_step(2 * i) = 0.5_wp * (x(i) + x(i + 1))
                y_step(2 * i) = y(i)
            end do
            x_step(n_points - 1) = x(n)
            y_step(n_points - 1) = y(n - 1)
            x_step(n_points) = x(n)
            y_step(n_points) = y(n)
        end select

        if (present(color)) then
            call log_warning('step: color strings not yet mapped to RGB values')
        end if
        if (present(linewidth)) then
            call log_warning('step: linewidth not configurable in current backend')
        end if

        call self%add_plot(x_step, y_step, label=label, linestyle=linestyle)
        deallocate(x_step, y_step)
    end subroutine add_step

    subroutine add_stem(self, x, y, label, linefmt, markerfmt, basefmt, bottom)
        !! Draw vertical stems from a baseline to each data point
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linefmt, markerfmt, basefmt
        real(wp), intent(in), optional :: bottom

        integer :: n, i
        real(wp) :: baseline, xmin, xmax
        real(wp), allocatable :: xs(:), ys(:)
        logical :: label_used

        n = min(size(x), size(y))
        if (n == 0) then
            call log_error('stem: x and y must contain values')
            return
        end if

        baseline = 0.0_wp
        if (present(bottom)) baseline = bottom

        xmin = minval(x(1:n))
        xmax = maxval(x(1:n))
        allocate(xs(2), ys(2))
        label_used = .false.

        if (present(linefmt)) then
            call log_warning('stem: linefmt ignored; use subplot styling instead')
        end if
        if (present(markerfmt)) then
            call log_warning('stem: markerfmt ignored by current backend')
        end if
        if (present(basefmt)) then
            call log_warning('stem: basefmt ignored by current backend')
        end if

        do i = 1, n
            xs(1) = x(i); xs(2) = x(i)
            ys(1) = baseline; ys(2) = y(i)
            if (present(label) .and. .not. label_used) then
                call self%add_plot(xs, ys, label=label)
                label_used = .true.
            else
                call self%add_plot(xs, ys)
            end if
        end do

        xs(1) = xmin; xs(2) = xmax
        ys(1) = baseline; ys(2) = baseline
        call self%add_plot(xs, ys)
        deallocate(xs, ys)

        call self%add_plot(x(1:n), y(1:n))
    end subroutine add_stem

    subroutine add_fill(self, x, y, color, alpha)
        !! Fill area between curve and baseline using fill_between helper
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha

        if (present(color)) then
            call log_warning('fill: color strings not yet supported; using default')
        end if
        if (present(alpha)) then
            call log_warning('fill: transparency not implemented for current backend')
        end if

        call self%add_fill_between(x, y1=y)
    end subroutine add_fill

    subroutine add_fill_between(self, x, y1, y2, where, color, alpha, interpolate)
        !! Fill area between two curves, honouring optional masks
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:)
        real(wp), intent(in), optional :: y1(:), y2(:)
        logical, intent(in), optional :: where(:)
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: alpha
        logical, intent(in), optional :: interpolate

        integer :: n
        real(wp), allocatable :: upper_vals(:), lower_vals(:)
        logical, allocatable :: mask_vals(:)
        logical :: has_mask, has_color, has_alpha
        character(len=:), allocatable :: color_value
        real(wp) :: alpha_value

        n = size(x)
        if (n < 2) then
            call log_error('fill_between: need at least two points to form area')
            return
        end if

        allocate(upper_vals(n), lower_vals(n))
        if (present(y1)) then
            if (size(y1) /= n) then
                call log_error('fill_between: y1 size mismatch')
                deallocate(upper_vals, lower_vals)
                return
            end if
            upper_vals = y1
        else
            upper_vals = 0.0_wp
        end if

        if (present(y2)) then
            if (size(y2) /= n) then
                call log_error('fill_between: y2 size mismatch')
                deallocate(upper_vals, lower_vals)
                return
            end if
            lower_vals = y2
        else
            lower_vals = 0.0_wp
        end if

        has_mask = .false.
        if (present(where)) then
            if (size(where) /= n) then
                call log_error('fill_between: where mask size mismatch')
                deallocate(upper_vals, lower_vals)
                return
            end if
            allocate(mask_vals(n))
            mask_vals = where
            if (.not. any(mask_vals)) then
                call log_warning('fill_between: mask excludes all data points')
                deallocate(upper_vals, lower_vals, mask_vals)
                return
            end if
            has_mask = .true.
        end if

        if (present(interpolate)) call log_warning(&
            'fill_between: interpolate option ignored')

        has_color = present(color)
        if (has_color) color_value = color
        has_alpha = present(alpha)
        if (has_alpha) alpha_value = alpha

        select case (merge(1, 0, has_mask) + merge(2, 0, has_color) + merge(4, 0, has_alpha))
        case (0)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, lower_vals, &
                                       plot_count=self%plot_count)
        case (1)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, lower_vals, &
                                       mask=mask_vals, plot_count=self%plot_count)
        case (2)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, lower_vals, &
                                       color_string=color_value, plot_count=self%plot_count)
        case (3)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, lower_vals, &
                                       mask=mask_vals, color_string=color_value, &
                                       plot_count=self%plot_count)
        case (4)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, lower_vals, &
                                       alpha=alpha_value, plot_count=self%plot_count)
        case (5)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, lower_vals, &
                                       mask=mask_vals, alpha=alpha_value, &
                                       plot_count=self%plot_count)
        case (6)
            call core_add_fill_between(self%plots, self%state, x, upper_vals, lower_vals, &
                                       color_string=color_value, alpha=alpha_value, &
                                       plot_count=self%plot_count)
        case default
            call core_add_fill_between(self%plots, self%state, x, upper_vals, lower_vals, &
                                       mask=mask_vals, color_string=color_value, &
                                       alpha=alpha_value, plot_count=self%plot_count)
        end select

        self%plot_count = self%state%plot_count

        if (has_mask) deallocate(mask_vals)
        deallocate(upper_vals, lower_vals)
    end subroutine add_fill_between

    subroutine twinx(self)
        !! Placeholder for twin x-axis support (not yet implemented)
        class(figure_t), intent(inout) :: self
        call log_warning('twinx: dual axis plots not yet implemented')
    end subroutine twinx

    subroutine twiny(self)
        !! Placeholder for twin y-axis support (not yet implemented)
        class(figure_t), intent(inout) :: self
        call log_warning('twiny: dual axis plots not yet implemented')
    end subroutine twiny

    subroutine add_pie(self, values, labels, autopct, startangle, colors, explode)
        !! Draw a simple pie chart using line segments for wedges
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:)
        character(len=*), intent(in), optional :: autopct
        real(wp), intent(in), optional :: startangle
        character(len=*), intent(in), optional :: colors(:)
        real(wp), intent(in), optional :: explode(:)

        integer :: n, i, seg_count, j
        real(wp) :: total, angle_start, angle_span, radius
        real(wp) :: offset, cx, cy, base_angle
        real(wp), allocatable :: x_pts(:), y_pts(:)
        real(wp), parameter :: PI = acos(-1.0_wp)

        n = size(values)
        if (n == 0) then
            call log_error('pie: values array must contain data')
            return
        end if

        total = sum(values)
        if (total <= 0.0_wp) then
            call log_error('pie: sum of values must be positive')
            return
        end if

        angle_start = 90.0_wp
        if (present(startangle)) angle_start = startangle
        angle_start = angle_start * PI / 180.0_wp

        radius = 1.0_wp
        cx = 0.0_wp
        cy = 0.0_wp

        if (present(colors)) then
            call log_warning('pie: custom colors not yet supported; using defaults')
        end if
        if (present(autopct)) then
            call log_warning('pie: autopct formatting is not implemented')
        end if

        do i = 1, n
            angle_span = 2.0_wp * PI * values(i) / total
            seg_count = max(12, int(abs(angle_span) * 180.0_wp / PI) + 1)
            allocate(x_pts(seg_count + 2), y_pts(seg_count + 2))

            offset = 0.0_wp
            if (present(explode)) then
                if (i <= size(explode)) offset = explode(i)
            end if
            offset = offset * radius * 0.1_wp

            base_angle = angle_start + 0.5_wp * angle_span
            x_pts(1) = cx + offset * cos(base_angle)
            y_pts(1) = cy + offset * sin(base_angle)

            do j = 1, seg_count + 1
                x_pts(j + 1) = x_pts(1) + radius * cos(angle_start + &
                                 angle_span * real(j - 1, wp) / real(seg_count, wp))
                y_pts(j + 1) = y_pts(1) + radius * sin(angle_start + &
                                 angle_span * real(j - 1, wp) / real(seg_count, wp))
            end do

            if (present(labels) .and. i <= size(labels)) then
                call self%add_plot(x_pts, y_pts, label=labels(i))
            else
                call self%add_plot(x_pts, y_pts)
            end if

            deallocate(x_pts, y_pts)
            angle_start = angle_start + angle_span
        end do
    end subroutine add_pie

    !! SUBPLOT OPERATIONS - Delegated to management module
    
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
! ==== End: src/figures/core/fortplot_figure_core.f90 ====
