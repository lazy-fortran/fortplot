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
                                  PLOT_TYPE_SURFACE, AXIS_PRIMARY, &
                                  AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_plot_management, only: next_plot_color
    use fortplot_figure_comprehensive_operations
    use fortplot_figure_comprehensive_operations, only: figure_backend_color, &
                                                        figure_backend_associated, &
                                                        figure_backend_line
    use fortplot_figure_reflines, only: core_axhline, core_axvline, &
                                        core_hlines, core_vlines
    use fortplot_string_utils, only: to_lowercase
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
        type(text_annotation_t), allocatable :: annotations(:)
        integer :: annotation_count = 0
        integer :: max_annotations = 1000
        integer :: subplot_rows = 0
        integer :: subplot_cols = 0
        integer :: current_subplot = 1
        type(subplot_data_t), allocatable :: subplots_array(:, :)
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
        procedure :: quiver
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
        procedure :: colorbar
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
        procedure :: use_axis
        procedure :: get_active_axis
        ! Subplot methods
        procedure :: subplots
        procedure :: suptitle
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
        procedure :: clear_backend_arrows
        procedure :: backend_associated
        procedure :: get_x_min
        procedure :: get_x_max
        procedure :: get_y_min
        procedure :: get_y_max
        procedure :: get_dpi
        procedure :: set_dpi
        procedure :: axhline
        procedure :: axvline
        procedure :: hlines
        procedure :: vlines
        procedure :: set_minor_ticks
        procedure :: set_minor_tick_count
        procedure :: minorticks_on
        procedure :: set_xticks
        procedure :: set_yticks
        procedure :: set_xtick_labels
        procedure :: set_ytick_labels
        procedure, private :: set_aspect_str
        procedure, private :: set_aspect_num
        generic :: set_aspect => set_aspect_str, set_aspect_num
        procedure :: tight_layout
        final :: destroy
    end type figure_t

    interface
        module subroutine set_xticks(self, positions, labels)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: positions(:)
            character(len=*), intent(in), optional :: labels(:)
        end subroutine set_xticks

        module subroutine set_yticks(self, positions, labels)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: positions(:)
            character(len=*), intent(in), optional :: labels(:)
        end subroutine set_yticks

        module subroutine set_xtick_labels(self, labels)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: labels(:)
        end subroutine set_xtick_labels

        module subroutine set_ytick_labels(self, labels)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: labels(:)
        end subroutine set_ytick_labels
    end interface

    interface
        module subroutine add_imshow(self, z, xlim, ylim, cmap, alpha, vmin, vmax, &
                                     origin, extent, interpolation, aspect)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: z(:, :)
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
            logical, intent(in), optional :: where (:)
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

    subroutine initialize(self, width, height, backend, dpi)
        !! Initialize figure with optional DPI support
        !! Added DPI parameter for consistency with matplotlib interface
        use, intrinsic :: iso_fortran_env, only: wp => real64
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        real(wp), intent(in), optional :: dpi

        call core_initialize(self%state, self%plots, self%streamlines, &
                             self%subplots_array, self%subplot_rows, &
                             self%subplot_cols, self%current_subplot, &
                             self%title, self%xlabel, self%ylabel, &
                             self%plot_count, width, height, backend, dpi)
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)

        call core_add_plot(self%plots, self%state, x, y, label, linestyle, color, &
                           self%plot_count)
    end subroutine add_plot

    subroutine colorbar(self, plot_index, label, location, fraction, pad, shrink, &
                        ticks, ticklabels, label_fontsize)
        class(figure_t), intent(inout) :: self
        integer, intent(in), optional :: plot_index
        character(len=*), intent(in), optional :: label, location
        real(wp), intent(in), optional :: fraction, pad, shrink
        real(wp), intent(in), optional :: ticks(:)
        character(len=*), intent(in), optional :: ticklabels(:)
        real(wp), intent(in), optional :: label_fontsize

        if (self%subplot_rows > 0 .and. self%subplot_cols > 0) then
            call log_error("colorbar: Subplot grids are not supported yet")
            return
        end if

        call core_colorbar(self%state, self%plots, self%plot_count, &
                           plot_index=plot_index, &
                           label=label, location=location, fraction=fraction, pad=pad, &
                           shrink=shrink, ticks=ticks, ticklabels=ticklabels, &
                           label_fontsize=label_fontsize)
    end subroutine colorbar

    subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label

        call core_add_contour(self%plots, self%state, x_grid, y_grid, z_grid, &
                              levels, label, self%plot_count)
    end subroutine add_contour

    subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, colormap, &
                                  show_colorbar, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar

        call core_add_contour_filled(self%plots, self%state, x_grid, y_grid, &
                                     z_grid, levels, colormap, show_colorbar, &
                                     label, self%plot_count)
    end subroutine add_contour_filled

    subroutine add_surface(self, x_grid, y_grid, z_grid, label, colormap, &
                           show_colorbar, alpha, edgecolor, linewidth, filled)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        character(len=*), intent(in), optional :: label, colormap
        logical, intent(in), optional :: show_colorbar, filled
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        call core_add_surface(self%plots, self%state, x_grid, y_grid, z_grid, &
                              label, colormap, show_colorbar, alpha, edgecolor, &
                              linewidth, filled, self%plot_count)
    end subroutine add_surface

    subroutine add_pcolormesh(self, x, y, c, colormap, vmin, vmax, edgecolors, &
                              linewidths)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:, :)
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
        real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
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

    subroutine quiver(self, x, y, u, v, scale, color, width, headwidth, &
                      headlength, units)
        !! Add quiver plot (discrete vector arrows) to figure
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units

        call core_quiver(self%plots, self%state, self%plot_count, x, y, u, v, &
                         scale, color, width, headwidth, headlength, units)
    end subroutine quiver

    !! I/O OPERATIONS - Delegated to core I/O module

    subroutine savefig(self, filename, blocking)
        !! Save figure to file (backward compatibility version)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking

        if (allocated(self%subplots_array) .and. self%subplot_rows > 0 .and. &
            self%subplot_cols > 0) then
            call core_savefig(self%state, self%plots, self%plot_count, filename, &
                              blocking, self%annotations, self%annotation_count, &
                              subplots_array=self%subplots_array, &
                              subplot_rows=self%subplot_rows, &
                              subplot_cols=self%subplot_cols)
        else
            call core_savefig(self%state, self%plots, self%plot_count, filename, &
                              blocking, self%annotations, self%annotation_count)
        end if
    end subroutine savefig

    subroutine savefig_with_status(self, filename, status, blocking)
        !! Save figure to file with error status reporting
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        logical, intent(in), optional :: blocking

        if (allocated(self%subplots_array) .and. self%subplot_rows > 0 .and. &
            self%subplot_cols > 0) then
            call core_savefig_with_status(self%state, self%plots, self%plot_count, &
                                          filename, status, blocking, &
                                          self%annotations, self%annotation_count, &
                                          subplots_array=self%subplots_array, &
                                          subplot_rows=self%subplot_rows, &
                                          subplot_cols=self%subplot_cols)
        else
            call core_savefig_with_status(self%state, self%plots, self%plot_count, &
                                          filename, status, blocking, &
                                          self%annotations, self%annotation_count)
        end if
    end subroutine savefig_with_status

    subroutine show(self, blocking)
        !! Display the figure
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: blocking

        if (allocated(self%subplots_array) .and. self%subplot_rows > 0 .and. &
            self%subplot_cols > 0) then
            call core_show(self%state, self%plots, self%plot_count, blocking, &
                           self%annotations, self%annotation_count, &
                           subplots_array=self%subplots_array, &
                           subplot_rows=self%subplot_rows, &
                           subplot_cols=self%subplot_cols)
        else
            call core_show(self%state, self%plots, self%plot_count, blocking, &
                           self%annotations, self%annotation_count)
        end if
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

        call core_boxplot(self%plots, self%state, self%plot_count, data, &
                          position, width, &
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
        real(wp), intent(out) :: rgb_data(:, :, :)
        call core_extract_rgb_data_for_animation(self%state, rgb_data, &
                                                 self%plots, self%state%plot_count, &
                                                 self%annotations, &
                                                 self%annotation_count, &
                                                 self%state%rendered)
    end subroutine extract_rgb_data_for_animation
    subroutine extract_png_data_for_animation(self, png_data, status)
        class(figure_t), intent(inout) :: self
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        call core_extract_png_data_for_animation(self%state, png_data, status, &
                                                 self%plots, self%state%plot_count, &
                                                 self%annotations, &
                                                 self%annotation_count, &
                                                 self%state%rendered)
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

    subroutine clear_backend_arrows(self)
        class(figure_t), intent(inout) :: self
        logical :: had_arrows

        had_arrows = .false.
        if (allocated(self%state%stream_arrows)) then
            had_arrows = size(self%state%stream_arrows) > 0
            deallocate (self%state%stream_arrows)
        end if

        if (had_arrows) self%state%rendered = .false.
    end subroutine clear_backend_arrows
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

    !! DPI PROPERTY ACCESSORS
    function get_dpi(self) result(dpi)
        !! Get the current DPI setting
        class(figure_t), intent(in) :: self
        real(wp) :: dpi
        dpi = self%state%dpi
    end function get_dpi

    subroutine set_dpi(self, dpi)
        !! Set the DPI and update backend if needed
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: dpi

        ! Validate DPI value
        if (dpi <= 0.0_wp) then
            call log_error('set_dpi: DPI must be positive')
            return
        end if

        ! Update DPI in state
        self%state%dpi = dpi

        ! Mark as needing re-rendering for consistent output
        self%state%rendered = .false.
    end subroutine set_dpi

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

        if (.not. self%state%has_twinx) then
            self%state%has_twinx = .true.
            if (.not. self%state%twinx_ylim_set) then
                self%state%twinx_y_min = self%state%y_min
                self%state%twinx_y_max = self%state%y_max
            end if
            self%state%twinx_yscale = self%state%yscale
            self%state%twinx_y_min_transformed = self%state%y_min_transformed
            self%state%twinx_y_max_transformed = self%state%y_max_transformed
            if (.not. allocated(self%state%twinx_ylabel)) then
                self%state%twinx_ylabel = ''
            end if
        end if

        self%state%active_axis = AXIS_TWINX
    end subroutine twinx

    subroutine twiny(self)
        class(figure_t), intent(inout) :: self

        if (.not. self%state%has_twiny) then
            self%state%has_twiny = .true.
            if (.not. self%state%twiny_xlim_set) then
                self%state%twiny_x_min = self%state%x_min
                self%state%twiny_x_max = self%state%x_max
            end if
            self%state%twiny_xscale = self%state%xscale
            self%state%twiny_x_min_transformed = self%state%x_min_transformed
            self%state%twiny_x_max_transformed = self%state%x_max_transformed
            if (.not. allocated(self%state%twiny_xlabel)) then
                self%state%twiny_xlabel = ''
            end if
        end if

        self%state%active_axis = AXIS_TWINY
    end subroutine twiny

    subroutine use_axis(self, axis_name)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: axis_name
        character(len=:), allocatable :: normalized

        normalized = to_lowercase(adjustl(axis_name))

        select case (trim(normalized))
        case ('left', 'primary', 'default', 'y')
            self%state%active_axis = AXIS_PRIMARY
        case ('right', 'twinx', 'secondary', 'y2')
            self%state%has_twinx = .true.
            if (.not. allocated(self%state%twinx_ylabel)) self%state%twinx_ylabel = ''
            self%state%active_axis = AXIS_TWINX
        case ('top', 'twiny', 'x2')
            self%state%has_twiny = .true.
            if (.not. allocated(self%state%twiny_xlabel)) self%state%twiny_xlabel = ''
            self%state%active_axis = AXIS_TWINY
        case ('bottom', 'x')
            self%state%active_axis = AXIS_PRIMARY
        case default
            call log_warning( &
                'use_axis: unknown axis "'//trim(axis_name)//'"; using primary axis')
            self%state%active_axis = AXIS_PRIMARY
        end select
    end subroutine use_axis

    function get_active_axis(self) result(axis_name)
        class(figure_t), intent(in) :: self
        character(len=10) :: axis_name

        select case (self%state%active_axis)
        case (AXIS_TWINX)
            axis_name = 'twinx'
        case (AXIS_TWINY)
            axis_name = 'twiny'
        case default
            axis_name = 'primary'
        end select
    end function get_active_axis

    !! SUBPLOT OPERATIONS - Delegated to management module

    subroutine subplots(self, nrows, ncols)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: nrows, ncols
        call figure_subplots(self%subplots_array, self%subplot_rows, &
                             self%subplot_cols, self%current_subplot, nrows, &
                             ncols)
    end subroutine subplots

    subroutine suptitle(self, title_text, fontsize)
        !! Set a centered figure-level title above all subplots
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: title_text
        real(wp), intent(in), optional :: fontsize

        self%state%suptitle = trim(title_text)
        if (present(fontsize)) then
            self%state%suptitle_fontsize = fontsize
        end if
        self%state%rendered = .false.
    end subroutine suptitle

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

    !! REFERENCE LINES - Horizontal and vertical reference lines

    subroutine axhline(self, y, xmin, xmax, color, linestyle, linewidth, label)
        !! Draw a horizontal line spanning the axes at y position
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y
        real(wp), intent(in), optional :: xmin, xmax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth

        call core_axhline(self%plots, self%state, self%plot_count, y, &
                          xmin, xmax, color, linestyle, linewidth, label)
    end subroutine axhline

    subroutine axvline(self, x, ymin, ymax, color, linestyle, linewidth, label)
        !! Draw a vertical line spanning the axes at x position
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x
        real(wp), intent(in), optional :: ymin, ymax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth
        call core_axvline(self%plots, self%state, self%plot_count, x, &
                          ymin, ymax, color, linestyle, linewidth, label)
    end subroutine axvline

    subroutine hlines(self, y, xmin, xmax, colors, linestyles, linewidth, label)
        !! Draw horizontal lines at each y position from xmin to xmax
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y(:)
        real(wp), intent(in) :: xmin, xmax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth
        call core_hlines(self%plots, self%state, self%plot_count, y, &
                         xmin, xmax, colors, linestyles, linewidth, label)
    end subroutine hlines

    subroutine vlines(self, x, ymin, ymax, colors, linestyles, linewidth, label)
        !! Draw vertical lines at each x position from ymin to ymax
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: ymin, ymax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth

        call core_vlines(self%plots, self%state, self%plot_count, x, &
                         ymin, ymax, colors, linestyles, linewidth, label)
    end subroutine vlines

    subroutine set_minor_ticks(self, x, y)
        !! Enable or disable minor ticks on x and/or y axes
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: x, y
        if (present(x)) self%state%minor_ticks_x = x
        if (present(y)) self%state%minor_ticks_y = y
        self%state%rendered = .false.
    end subroutine set_minor_ticks

    subroutine set_minor_tick_count(self, count)
        !! Set the number of minor ticks between each pair of major ticks
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: count
        if (count >= 1 .and. count <= 20) then
            self%state%minor_tick_count = count
            self%state%rendered = .false.
        else
            call log_warning('set_minor_tick_count: count must be between 1 and 20')
        end if
    end subroutine set_minor_tick_count

    subroutine minorticks_on(self)
        !! Enable minor ticks on both axes (matplotlib-compatible convenience method)
        class(figure_t), intent(inout) :: self
        self%state%minor_ticks_x = .true.
        self%state%minor_ticks_y = .true.
        self%state%rendered = .false.
    end subroutine minorticks_on

    subroutine set_aspect_str(self, aspect)
        !! Set aspect ratio using a string mode: equal or auto
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: aspect
        character(len=:), allocatable :: aspect_lower
        aspect_lower = to_lowercase(trim(aspect))
        select case (aspect_lower)
        case ('equal')
            self%state%aspect_mode = 'equal'
            self%state%aspect_ratio = 1.0_wp
        case ('auto')
            self%state%aspect_mode = 'auto'
        case default
            call log_warning('set_aspect: unknown mode "'//trim(aspect)// &
                             '"; use "equal", "auto", or numeric value')
            return
        end select
        self%state%rendered = .false.
    end subroutine set_aspect_str

    subroutine set_aspect_num(self, ratio)
        !! Set aspect ratio using a numeric value (y-scale = ratio * x-scale)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: ratio
        if (ratio <= 0.0_wp) then
            call log_warning('set_aspect: ratio must be positive')
            return
        end if
        self%state%aspect_mode = 'numeric'
        self%state%aspect_ratio = ratio
        self%state%rendered = .false.
    end subroutine set_aspect_num

    subroutine tight_layout(self, pad, w_pad, h_pad)
        !! Enable tight layout to minimize subplot overlap
        class(figure_t), intent(inout) :: self
        real(wp), intent(in), optional :: pad, w_pad, h_pad
        self%state%tight_layout_enabled = .true.
        if (present(pad)) then
            if (pad > 0.0_wp) self%state%tight_pad = pad
        end if
        if (present(w_pad)) then
            if (w_pad >= 0.0_wp) self%state%tight_w_pad = w_pad
        end if
        if (present(h_pad)) then
            if (h_pad >= 0.0_wp) self%state%tight_h_pad = h_pad
        end if
        self%state%rendered = .false.
    end subroutine tight_layout

end module fortplot_figure_core
