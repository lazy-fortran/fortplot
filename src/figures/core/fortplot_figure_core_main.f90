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
                                  PLOT_TYPE_SURFACE, PLOT_TYPE_POLAR, &
                                  AXIS_PRIMARY, AXIS_TWINX, AXIS_TWINY
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_plot_management, only: next_plot_color
    use fortplot_figure_comprehensive_operations
    use fortplot_figure_comprehensive_operations, only: figure_backend_color, &
                                                        figure_backend_associated, &
                                                        figure_backend_line
    use fortplot_figure_reflines, only: core_axhline, core_axvline, &
                                        core_hlines, core_vlines
    use fortplot_string_utils, only: to_lowercase
    use fortplot_datetime, only: datetime_t, datetime_to_unix_seconds
    implicit none

    private
    public :: figure_t, plot_data_t, subplot_data_t
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
              PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
              PLOT_TYPE_SURFACE, PLOT_TYPE_POLAR

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
        procedure, private :: add_plot_real
        procedure, private :: add_plot_datetime
        generic :: add_plot => add_plot_real, add_plot_datetime
        generic :: plot => add_plot_real, add_plot_datetime
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
        procedure :: set_xaxis_date_format
        procedure :: set_yaxis_date_format
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

    interface
        module subroutine add_plot_datetime(self, x, y, label, linestyle, color)
            class(figure_t), intent(inout) :: self
            type(datetime_t), intent(in) :: x(:)
            real(wp), intent(in) :: y(:)
            character(len=*), intent(in), optional :: label, linestyle
            real(wp), intent(in), optional :: color(3)
        end subroutine add_plot_datetime

        module subroutine savefig(self, filename, blocking)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: filename
            logical, intent(in), optional :: blocking
        end subroutine savefig

        module subroutine savefig_with_status(self, filename, status, blocking)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: filename
            integer, intent(out) :: status
            logical, intent(in), optional :: blocking
        end subroutine savefig_with_status

        module subroutine show(self, blocking)
            class(figure_t), intent(inout) :: self
            logical, intent(in), optional :: blocking
        end subroutine show

        module subroutine set_xaxis_date_format(self, format)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: format
        end subroutine set_xaxis_date_format

        module subroutine set_yaxis_date_format(self, format)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: format
        end subroutine set_yaxis_date_format
    end interface


    interface
        module subroutine initialize(self, width, height, backend, dpi)
            class(figure_t), intent(inout) :: self
            integer, intent(in), optional :: width, height
            character(len=*), intent(in), optional :: backend
            real(wp), intent(in), optional :: dpi
        end subroutine initialize

        module subroutine add_plot_real(self, x, y, label, linestyle, color)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x(:), y(:)
            character(len=*), intent(in), optional :: label, linestyle
            real(wp), intent(in), optional :: color(3)
        end subroutine add_plot_real

        module subroutine colorbar(self, plot_index, label, location, fraction, pad, &
                                  shrink, ticks, ticklabels, label_fontsize)
            class(figure_t), intent(inout) :: self
            integer, intent(in), optional :: plot_index
            character(len=*), intent(in), optional :: label, location
            real(wp), intent(in), optional :: fraction, pad, shrink
            real(wp), intent(in), optional :: ticks(:)
            character(len=*), intent(in), optional :: ticklabels(:)
            real(wp), intent(in), optional :: label_fontsize
        end subroutine colorbar

        module subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
            real(wp), intent(in), optional :: levels(:)
            character(len=*), intent(in), optional :: label
        end subroutine add_contour

        module subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, &
                                            colormap, show_colorbar, label)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
            real(wp), intent(in), optional :: levels(:)
            character(len=*), intent(in), optional :: colormap, label
            logical, intent(in), optional :: show_colorbar
        end subroutine add_contour_filled

        module subroutine add_surface(self, x_grid, y_grid, z_grid, label, colormap, &
                                     show_colorbar, alpha, edgecolor, linewidth, &
                                     filled)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
            character(len=*), intent(in), optional :: label, colormap
            logical, intent(in), optional :: show_colorbar, filled
            real(wp), intent(in), optional :: alpha, linewidth
            real(wp), intent(in), optional :: edgecolor(3)
        end subroutine add_surface

        module subroutine add_pcolormesh(self, x, y, c, colormap, vmin, vmax, &
                                        edgecolors, linewidths)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x(:), y(:), c(:, :)
            character(len=*), intent(in), optional :: colormap
            real(wp), intent(in), optional :: vmin, vmax
            real(wp), intent(in), optional :: edgecolors(3)
            real(wp), intent(in), optional :: linewidths
        end subroutine add_pcolormesh

        module subroutine streamplot(self, x, y, u, v, density, color, linewidth, &
                                    rtol, atol, max_time)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
            real(wp), intent(in), optional :: density
            real(wp), intent(in), optional :: color(3)
            real(wp), intent(in), optional :: linewidth
            real(wp), intent(in), optional :: rtol, atol, max_time
        end subroutine streamplot

        module subroutine quiver(self, x, y, u, v, scale, color, width, headwidth, &
                                headlength, units)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x(:), y(:), u(:), v(:)
            real(wp), intent(in), optional :: scale
            real(wp), intent(in), optional :: color(3)
            real(wp), intent(in), optional :: width, headwidth, headlength
            character(len=*), intent(in), optional :: units
        end subroutine quiver

        module subroutine grid(self, enabled, which, axis, alpha, linestyle)
            class(figure_t), intent(inout) :: self
            logical, intent(in), optional :: enabled
            character(len=*), intent(in), optional :: which, axis, linestyle
            real(wp), intent(in), optional :: alpha
        end subroutine grid

        module subroutine add_hist(self, data, bins, density, label, color)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: data(:)
            integer, intent(in), optional :: bins
            logical, intent(in), optional :: density
            character(len=*), intent(in), optional :: label
            real(wp), intent(in), optional :: color(3)
        end subroutine add_hist

        module subroutine boxplot(self, data, position, width, label, show_outliers, &
                                 horizontal, color)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: data(:)
            real(wp), intent(in), optional :: position
            real(wp), intent(in), optional :: width
            character(len=*), intent(in), optional :: label
            logical, intent(in), optional :: show_outliers
            logical, intent(in), optional :: horizontal
            character(len=*), intent(in), optional :: color
        end subroutine boxplot

        module subroutine set_xlabel(self, label)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: label
        end subroutine set_xlabel

        module subroutine set_ylabel(self, label)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: label
        end subroutine set_ylabel

        module subroutine set_title(self, title)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: title
        end subroutine set_title

        module subroutine set_xscale(self, scale, threshold)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: scale
            real(wp), intent(in), optional :: threshold
        end subroutine set_xscale

        module subroutine set_yscale(self, scale, threshold)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: scale
            real(wp), intent(in), optional :: threshold
        end subroutine set_yscale

        module subroutine set_xlim(self, x_min, x_max)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x_min, x_max
        end subroutine set_xlim

        module subroutine set_ylim(self, y_min, y_max)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: y_min, y_max
        end subroutine set_ylim

        module subroutine set_line_width(self, width)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: width
        end subroutine set_line_width

        module subroutine set_ydata(self, plot_index, y_new)
            class(figure_t), intent(inout) :: self
            integer, intent(in) :: plot_index
            real(wp), intent(in) :: y_new(:)
        end subroutine set_ydata

        module subroutine figure_legend(self, location)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in), optional :: location
        end subroutine figure_legend

        module subroutine clear(self)
            class(figure_t), intent(inout) :: self
        end subroutine clear

        module subroutine clear_streamlines(self)
            class(figure_t), intent(inout) :: self
        end subroutine clear_streamlines

        module subroutine destroy(self)
            type(figure_t), intent(inout) :: self
        end subroutine destroy

        module function get_width(self) result(width)
            class(figure_t), intent(in) :: self
            integer :: width
        end function get_width

        module function get_height(self) result(height)
            class(figure_t), intent(in) :: self
            integer :: height
        end function get_height

        module function get_rendered(self) result(rendered)
            class(figure_t), intent(in) :: self
            logical :: rendered
        end function get_rendered

        module subroutine set_rendered(self, rendered)
            class(figure_t), intent(inout) :: self
            logical, intent(in) :: rendered
        end subroutine set_rendered

        module function get_plot_count(self) result(plot_count)
            class(figure_t), intent(in) :: self
            integer :: plot_count
        end function get_plot_count

        module function get_plots(self) result(plots_ptr)
            class(figure_t), intent(in), target :: self
            type(plot_data_t), pointer :: plots_ptr(:)
        end function get_plots

        module subroutine setup_png_backend_for_animation(self)
            class(figure_t), intent(inout) :: self
        end subroutine setup_png_backend_for_animation

        module subroutine extract_rgb_data_for_animation(self, rgb_data)
            class(figure_t), intent(inout) :: self
            real(wp), intent(out) :: rgb_data(:, :, :)
        end subroutine extract_rgb_data_for_animation

        module subroutine extract_png_data_for_animation(self, png_data, status)
            class(figure_t), intent(inout) :: self
            integer(1), allocatable, intent(out) :: png_data(:)
            integer, intent(out) :: status
        end subroutine extract_png_data_for_animation

        module subroutine backend_color(self, r, g, b)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: r, g, b
        end subroutine backend_color

        module function backend_associated(self) result(is_associated)
            class(figure_t), intent(in) :: self
            logical :: is_associated
        end function backend_associated

        module subroutine backend_line(self, x1, y1, x2, y2)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x1, y1, x2, y2
        end subroutine backend_line

        module subroutine backend_arrow(self, x, y, dx, dy, size, style)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x, y, dx, dy, size
            character(len=*), intent(in) :: style
        end subroutine backend_arrow

        module subroutine clear_backend_arrows(self)
            class(figure_t), intent(inout) :: self
        end subroutine clear_backend_arrows

        module function get_x_min(self) result(x_min)
            class(figure_t), intent(in) :: self
            real(wp) :: x_min
        end function get_x_min

        module function get_x_max(self) result(x_max)
            class(figure_t), intent(in) :: self
            real(wp) :: x_max
        end function get_x_max

        module function get_y_min(self) result(y_min)
            class(figure_t), intent(in) :: self
            real(wp) :: y_min
        end function get_y_min

        module function get_y_max(self) result(y_max)
            class(figure_t), intent(in) :: self
            real(wp) :: y_max
        end function get_y_max

        module function get_dpi(self) result(dpi)
            class(figure_t), intent(in) :: self
            real(wp) :: dpi
        end function get_dpi

        module subroutine set_dpi(self, dpi)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: dpi
        end subroutine set_dpi

        module subroutine scatter(self, x, y, s, c, marker, markersize, color, &
                                 colormap, alpha, edgecolor, facecolor, linewidth, &
                                 vmin, vmax, label, show_colorbar)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x(:), y(:)
            real(wp), intent(in), optional :: s(:), c(:)
            character(len=*), intent(in), optional :: marker, colormap, label
            real(wp), intent(in), optional :: markersize, alpha, linewidth, vmin, &
                                             vmax
            real(wp), intent(in), optional :: color(3), edgecolor(3), facecolor(3)
            logical, intent(in), optional :: show_colorbar
        end subroutine scatter

        module subroutine twinx(self)
            class(figure_t), intent(inout) :: self
        end subroutine twinx

        module subroutine twiny(self)
            class(figure_t), intent(inout) :: self
        end subroutine twiny

        module subroutine use_axis(self, axis_name)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: axis_name
        end subroutine use_axis

        module function get_active_axis(self) result(axis_name)
            class(figure_t), intent(in) :: self
            character(len=10) :: axis_name
        end function get_active_axis

        module subroutine subplots(self, nrows, ncols)
            class(figure_t), intent(inout) :: self
            integer, intent(in) :: nrows, ncols
        end subroutine subplots

        module subroutine suptitle(self, title_text, fontsize)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: title_text
            real(wp), intent(in), optional :: fontsize
        end subroutine suptitle

        module subroutine subplot_plot(self, row, col, x, y, label, linestyle, &
                                       color)
            class(figure_t), intent(inout) :: self
            integer, intent(in) :: row, col
            real(wp), intent(in) :: x(:), y(:)
            character(len=*), intent(in), optional :: label, linestyle
            real(wp), intent(in), optional :: color(3)
        end subroutine subplot_plot

        module function subplot_plot_count(self, row, col) result(count)
            class(figure_t), intent(in) :: self
            integer, intent(in) :: row, col
            integer :: count
        end function subplot_plot_count

        module subroutine subplot_set_title(self, row, col, title)
            class(figure_t), intent(inout) :: self
            integer, intent(in) :: row, col
            character(len=*), intent(in) :: title
        end subroutine subplot_set_title

        module subroutine subplot_set_xlabel(self, row, col, xlabel)
            class(figure_t), intent(inout) :: self
            integer, intent(in) :: row, col
            character(len=*), intent(in) :: xlabel
        end subroutine subplot_set_xlabel

        module subroutine subplot_set_ylabel(self, row, col, ylabel)
            class(figure_t), intent(inout) :: self
            integer, intent(in) :: row, col
            character(len=*), intent(in) :: ylabel
        end subroutine subplot_set_ylabel

        module function subplot_title(self, row, col) result(title)
            class(figure_t), intent(in) :: self
            integer, intent(in) :: row, col
            character(len=:), allocatable :: title
        end function subplot_title

        module subroutine axhline(self, y, xmin, xmax, color, linestyle, linewidth, &
                                 label)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: y
            real(wp), intent(in), optional :: xmin, xmax
            character(len=*), intent(in), optional :: color, linestyle, label
            real(wp), intent(in), optional :: linewidth
        end subroutine axhline

        module subroutine axvline(self, x, ymin, ymax, color, linestyle, linewidth, &
                                 label)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x
            real(wp), intent(in), optional :: ymin, ymax
            character(len=*), intent(in), optional :: color, linestyle, label
            real(wp), intent(in), optional :: linewidth
        end subroutine axvline

        module subroutine hlines(self, y, xmin, xmax, colors, linestyles, linewidth, &
                                label)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: y(:)
            real(wp), intent(in) :: xmin, xmax
            character(len=*), intent(in), optional :: colors, linestyles, label
            real(wp), intent(in), optional :: linewidth
        end subroutine hlines

        module subroutine vlines(self, x, ymin, ymax, colors, linestyles, linewidth, &
                                label)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: x(:)
            real(wp), intent(in) :: ymin, ymax
            character(len=*), intent(in), optional :: colors, linestyles, label
            real(wp), intent(in), optional :: linewidth
        end subroutine vlines

        module subroutine set_minor_ticks(self, x, y)
            class(figure_t), intent(inout) :: self
            logical, intent(in), optional :: x, y
        end subroutine set_minor_ticks

        module subroutine set_minor_tick_count(self, count)
            class(figure_t), intent(inout) :: self
            integer, intent(in) :: count
        end subroutine set_minor_tick_count

        module subroutine minorticks_on(self)
            class(figure_t), intent(inout) :: self
        end subroutine minorticks_on

        module subroutine set_aspect_str(self, aspect)
            class(figure_t), intent(inout) :: self
            character(len=*), intent(in) :: aspect
        end subroutine set_aspect_str

        module subroutine set_aspect_num(self, ratio)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in) :: ratio
        end subroutine set_aspect_num

        module subroutine tight_layout(self, pad, w_pad, h_pad)
            class(figure_t), intent(inout) :: self
            real(wp), intent(in), optional :: pad, w_pad, h_pad
        end subroutine tight_layout
    end interface
end module fortplot_figure_core
