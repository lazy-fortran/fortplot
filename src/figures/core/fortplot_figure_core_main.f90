module fortplot_figure_core

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context
    use fortplot_annotations, only: text_annotation_t
    use fortplot_logging, only: log_error, log_warning
    use fortplot_plot_data, only: plot_data_t, arrow_data_t, subplot_data_t, &
                                    PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, &
                                    PLOT_TYPE_PCOLORMESH, PLOT_TYPE_BOXPLOT, &
                                    PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
                                    PLOT_TYPE_SURFACE
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_plot_management, only: next_plot_color
    use fortplot_figure_comprehensive_operations
    use fortplot_figure_comprehensive_operations, only: figure_backend_color, figure_backend_associated, figure_backend_line
    use fortplot_figure_core_specialized_plots
    ! Note: Properties functions come from comprehensive_operations to avoid conflicts
    ! Note: Animation functions converted to core_* calls
    ! Note: Subplot functions use original figure_* naming
    implicit none

    private
    public :: figure_t, plot_data_t, subplot_data_t
    public :: PLOT_TYPE_LINE, PLOT_TYPE_CONTOUR, PLOT_TYPE_PCOLORMESH, &
              PLOT_TYPE_BOXPLOT, PLOT_TYPE_SCATTER, PLOT_TYPE_FILL, &
              PLOT_TYPE_SURFACE

    type :: figure_t
        !! Main figure class implementing Facade Pattern for plotting operations
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
        !! Core operations
        procedure :: initialize; procedure :: add_plot; procedure :: plot => add_plot
        procedure :: add_contour; procedure :: add_contour_filled; procedure :: add_surface
        procedure :: add_pcolormesh; procedure :: streamplot; procedure :: savefig
        procedure :: save => savefig; procedure :: savefig_with_status; procedure :: show
        procedure :: clear; procedure :: clear_streamlines; procedure :: grid
        procedure :: add_hist; procedure :: hist => add_hist; procedure :: boxplot; procedure :: scatter
        !! Property management
        procedure :: set_xlabel; procedure :: set_ylabel; procedure :: set_title
        procedure :: set_xscale; procedure :: set_yscale; procedure :: set_xlim; procedure :: set_ylim
        procedure :: set_line_width; procedure :: set_ydata; procedure :: get_width; procedure :: get_height
        procedure :: get_rendered; procedure :: set_rendered; procedure :: get_plot_count; procedure :: get_plots
        procedure :: get_x_min; procedure :: get_x_max; procedure :: get_y_min; procedure :: get_y_max
        !! Specialized plots
        procedure :: add_imshow; procedure :: add_pie; procedure :: add_polar; procedure :: add_step
        procedure :: add_stem; procedure :: add_fill; procedure :: add_fill_between
        !! Animation and backend
        procedure :: setup_png_backend_for_animation; procedure :: extract_rgb_data_for_animation
        procedure :: extract_png_data_for_animation; procedure :: backend_color; procedure :: backend_line
        procedure :: backend_arrow; procedure :: backend_associated; procedure :: twinx; procedure :: twiny
        !! Subplot methods
        procedure :: subplots; procedure :: subplot_plot; procedure :: subplot_plot_count
        procedure :: subplot_set_title; procedure :: subplot_set_xlabel; procedure :: subplot_set_ylabel
        procedure :: subplot_title; procedure :: legend => figure_legend
        final :: destroy
    end type figure_t

contains

    !! CORE OPERATIONS - Delegated to specialized modules
    subroutine initialize(self, width, height, backend)
        class(figure_t), intent(inout) :: self; integer, intent(in), optional :: width, height
        character(len=*), intent(in), optional :: backend
        call core_initialize(self%state, self%plots, self%streamlines, self%subplots_array, &
                            self%subplot_rows, self%subplot_cols, self%current_subplot, &
                            self%title, self%xlabel, self%ylabel, self%plot_count, width, height, backend)
    end subroutine initialize

    subroutine add_plot(self, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle; real(wp), intent(in), optional :: color(3)
        call core_add_plot(self%plots, self%state, x, y, label, linestyle, color, self%plot_count)
    end subroutine add_plot

    subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:); character(len=*), intent(in), optional :: label
        call core_add_contour(self%plots, self%state, x_grid, y_grid, z_grid, levels, label, self%plot_count)
    end subroutine add_contour

    subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, colormap, show_colorbar, label)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in), optional :: levels(:); character(len=*), intent(in), optional :: colormap, label
        logical, intent(in), optional :: show_colorbar
        call core_add_contour_filled(self%plots, self%state, x_grid, y_grid, z_grid, &
                                    levels, colormap, show_colorbar, label, self%plot_count)
    end subroutine add_contour_filled

    subroutine add_surface(self, x_grid, y_grid, z_grid, label, colormap, show_colorbar, alpha, edgecolor, linewidth)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        character(len=*), intent(in), optional :: label, colormap, edgecolor
        logical, intent(in), optional :: show_colorbar; real(wp), intent(in), optional :: alpha, linewidth
        call core_add_surface(self%plots, self%state, x_grid, y_grid, z_grid, label, colormap, &
                             show_colorbar, alpha, edgecolor, linewidth, self%plot_count)
    end subroutine add_surface

    subroutine add_pcolormesh(self, x, y, c, colormap, vmin, vmax, edgecolors, linewidths)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x(:), y(:), c(:,:)
        character(len=*), intent(in), optional :: colormap, edgecolors
        real(wp), intent(in), optional :: vmin, vmax, linewidths
        call core_add_pcolormesh(self%plots, self%state, x, y, c, colormap, &
                                vmin, vmax, edgecolors, linewidths, self%plot_count)
    end subroutine add_pcolormesh

    subroutine streamplot(self, x, y, u, v, density, color, linewidth, rtol, atol, max_time)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x(:), y(:), u(:,:), v(:,:)
        real(wp), intent(in), optional :: density, linewidth, rtol, atol, max_time
        character(len=*), intent(in), optional :: color
        call core_streamplot(self%plots, self%state, self%plot_count, x, y, u, v, &
                            density, color, linewidth, rtol, atol, max_time)
    end subroutine streamplot

    subroutine savefig(self, filename, blocking)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: filename
        logical, intent(in), optional :: blocking
        call core_savefig(self%state, self%plots, self%plot_count, filename, blocking, &
                         self%streamlines, self%title, self%xlabel, self%ylabel, &
                         self%subplots_array, self%subplot_rows, self%subplot_cols)
    end subroutine savefig

    subroutine savefig_with_status(self, filename, status, blocking)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: filename
        integer, intent(out) :: status; logical, intent(in), optional :: blocking
        call core_savefig_with_status(self%state, self%plots, self%plot_count, filename, status, blocking, &
                                     self%streamlines, self%title, self%xlabel, self%ylabel, &
                                     self%subplots_array, self%subplot_rows, self%subplot_cols)
    end subroutine savefig_with_status

    subroutine show(self, blocking)
        class(figure_t), intent(inout) :: self; logical, intent(in), optional :: blocking
        call core_show(self%state, self%plots, self%plot_count, blocking, self%streamlines, &
                      self%title, self%xlabel, self%ylabel, self%subplots_array, self%subplot_rows, self%subplot_cols)
    end subroutine show

    subroutine grid(self, enabled, which, axis, alpha, linestyle)
        class(figure_t), intent(inout) :: self; logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle; real(wp), intent(in), optional :: alpha
        call core_grid(self%state, enabled, which, axis, alpha, linestyle)
    end subroutine grid

    subroutine add_hist(self, data, bins, density, label, color)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins; logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label; real(wp), intent(in), optional :: color(3)
        call core_add_hist(self%plots, self%state, data, bins, density, label, color, self%plot_count)
    end subroutine add_hist

    subroutine boxplot(self, data, position, width, label, show_outliers, notch, horizontal, color)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position, width; character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers, notch, horizontal; real(wp), intent(in), optional :: color(3)
        call core_boxplot(self%plots, self%state, data, position, width, label, &
                         show_outliers, notch, horizontal, color, self%plot_count)
    end subroutine boxplot

    subroutine scatter(self, x, y, s, c, marker, alpha, label, edgecolors, linewidths)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), c(:), alpha, linewidths
        character(len=*), intent(in), optional :: marker, label, edgecolors
        call core_scatter(self%plots, self%state, x, y, s, c, marker, alpha, label, &
                         edgecolors, linewidths, self%plot_count)
    end subroutine scatter

    subroutine clear(self)
        class(figure_t), intent(inout) :: self
        call core_clear(self%state, self%streamlines, &
                       self%subplots_array, self%subplot_rows, self%subplot_cols, &
                       self%current_subplot, self%title, self%xlabel, self%ylabel, &
                       self%plot_count, self%annotation_count)
    end subroutine clear

    subroutine clear_streamlines(self)
        class(figure_t), intent(inout) :: self; call core_clear_streamlines(self%streamlines)
    end subroutine clear_streamlines

    subroutine figure_legend(self, location)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in), optional :: location
        call core_figure_legend(self%state, self%plots, self%plot_count, location)
    end subroutine figure_legend

    !! PROPERTY MANAGEMENT - Delegated to properties module
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
        real(wp), intent(in), optional :: threshold; call core_set_xscale(self%state, scale, threshold)
    end subroutine set_xscale

    subroutine set_yscale(self, scale, threshold)
        class(figure_t), intent(inout) :: self; character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold; call core_set_yscale(self%state, scale, threshold)
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

    subroutine set_ydata(self, plot_index, y_data)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: plot_index; real(wp), intent(in) :: y_data(:)
        call core_set_ydata(self%plots, self%state%plot_count, plot_index, y_data)
    end subroutine set_ydata

    function get_width(self) result(width)
        class(figure_t), intent(in) :: self; integer :: width; width = core_get_width(self%state)
    end function get_width

    function get_height(self) result(height)
        class(figure_t), intent(in) :: self; integer :: height; height = core_get_height(self%state)
    end function get_height

    function get_rendered(self) result(rendered)
        class(figure_t), intent(in) :: self; logical :: rendered; rendered = core_get_rendered(self%state)
    end function get_rendered

    subroutine set_rendered(self, rendered)
        class(figure_t), intent(inout) :: self; logical, intent(in) :: rendered
        call core_set_rendered(self%state, rendered)
    end subroutine set_rendered

    function get_plot_count(self) result(count)
        class(figure_t), intent(in) :: self; integer :: count; count = core_get_plot_count(self%state)
    end function get_plot_count

    function get_plots(self) result(plot_array)
        class(figure_t), intent(in) :: self; type(plot_data_t), allocatable :: plot_array(:)
        plot_array = core_get_plots(self%plots)
    end function get_plots

    function get_x_min(self) result(x_min)
        class(figure_t), intent(in) :: self; real(wp) :: x_min; x_min = core_get_x_min(self%state)
    end function get_x_min

    function get_x_max(self) result(x_max)
        class(figure_t), intent(in) :: self; real(wp) :: x_max; x_max = core_get_x_max(self%state)
    end function get_x_max

    function get_y_min(self) result(y_min)
        class(figure_t), intent(in) :: self; real(wp) :: y_min; y_min = core_get_y_min(self%state)
    end function get_y_min

    function get_y_max(self) result(y_max)
        class(figure_t), intent(in) :: self; real(wp) :: y_max; y_max = core_get_y_max(self%state)
    end function get_y_max

    !! SPECIALIZED PLOTS - Delegated to specialized plots module
    subroutine add_imshow(self, image, extent, colormap, vmin, vmax, aspect, interpolation)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: image(:,:)
        real(wp), intent(in), optional :: extent(4), vmin, vmax
        character(len=*), intent(in), optional :: colormap, aspect, interpolation
        call figure_add_imshow(self%state, self%plots, self%plot_count, image, extent, colormap, vmin, vmax, aspect, interpolation)
    end subroutine add_imshow

    subroutine add_pie(self, values, labels, autopct, startangle, colors, explode)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: labels(:), autopct, colors(:); real(wp), intent(in), optional :: startangle, explode(:)
        call figure_add_pie(self%state, self%plots, self%plot_count, values, labels, colors, explode, start_angle=startangle)
    end subroutine add_pie

    subroutine add_polar(self, theta, r, label, linestyle, color)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: theta(:), r(:)
        character(len=*), intent(in), optional :: label, linestyle; real(wp), intent(in), optional :: color(3)
        call figure_add_polar(self%state, self%plots, self%plot_count, theta, r, label, linestyle, color)
    end subroutine add_polar

    subroutine add_step(self, x, y, where, label, linestyle, color)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: where, label, linestyle; real(wp), intent(in), optional :: color(3)
        call figure_add_step(self%state, self%plots, self%plot_count, x, y, where, label, linestyle, color)
    end subroutine add_step

    subroutine add_stem(self, x, y, linestyle, marker, color, bottom_level, label)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: linestyle, marker, label; real(wp), intent(in), optional :: color(3), bottom_level
        call figure_add_stem(self%state, self%plots, self%plot_count, x, y, linestyle, marker, color, bottom_level, label)
    end subroutine add_stem

    subroutine add_fill(self, x, y, color, alpha, label)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: color(3), alpha; character(len=*), intent(in), optional :: label
        call figure_add_fill(self%state, self%plots, self%plot_count, x, y, color, alpha, label)
    end subroutine add_fill

    subroutine add_fill_between(self, x, y1, y2, color, alpha, label)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x(:), y1(:), y2(:)
        real(wp), intent(in), optional :: color(3), alpha; character(len=*), intent(in), optional :: label
        call figure_add_fill_between(self%state, self%plots, self%plot_count, x, y1, y2, color, alpha, label)
    end subroutine add_fill_between

    !! ANIMATION AND BACKEND - Delegated to animation module
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

    subroutine backend_color(self, r, g, b)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: r, g, b
        call core_backend_color(self%state, r, g, b)
    end subroutine backend_color

    subroutine backend_line(self, x1, y1, x2, y2)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x1, y1, x2, y2
        call core_backend_line(self%state, x1, y1, x2, y2)
    end subroutine backend_line

    subroutine backend_arrow(self, x, y, dx, dy, size, style)
        class(figure_t), intent(inout) :: self; real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        call core_backend_arrow(self%state, x, y, dx, dy, size, style)
    end subroutine backend_arrow

    function backend_associated(self) result(associated)
        class(figure_t), intent(in) :: self; logical :: associated; associated = core_backend_associated(self%state)
    end function backend_associated

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

    !! SUBPLOT OPERATIONS - Delegated to subplots module
    subroutine subplots(self, nrows, ncols)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: nrows, ncols
        call figure_subplots(self%subplots_array, self%subplot_rows, &
                            self%subplot_cols, self%current_subplot, nrows, ncols)
    end subroutine subplots

    subroutine subplot_plot(self, row, col, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: row, col; real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle; real(wp), intent(in), optional :: color(3)
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
        class(figure_t), intent(inout) :: self; integer, intent(in) :: row, col; character(len=*), intent(in) :: title
        call figure_subplot_set_title(self%subplots_array, self%subplot_rows, &
                                     self%subplot_cols, row, col, title)
    end subroutine subplot_set_title

    subroutine subplot_set_xlabel(self, row, col, xlabel)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: row, col; character(len=*), intent(in) :: xlabel
        call figure_subplot_set_xlabel(self%subplots_array, self%subplot_rows, &
                                      self%subplot_cols, row, col, xlabel)
    end subroutine subplot_set_xlabel

    subroutine subplot_set_ylabel(self, row, col, ylabel)
        class(figure_t), intent(inout) :: self; integer, intent(in) :: row, col; character(len=*), intent(in) :: ylabel
        call figure_subplot_set_ylabel(self%subplots_array, self%subplot_rows, &
                                      self%subplot_cols, row, col, ylabel)
    end subroutine subplot_set_ylabel

    function subplot_title(self, row, col) result(title)
        class(figure_t), intent(in) :: self; integer, intent(in) :: row, col; character(len=:), allocatable :: title
        title = figure_subplot_title(self%subplots_array, self%subplot_rows, &
                                     self%subplot_cols, row, col)
    end function subplot_title

    !! CLEANUP
    subroutine destroy(self)
        type(figure_t), intent(inout) :: self
        if (allocated(self%title)) deallocate(self%title)
        if (allocated(self%xlabel)) deallocate(self%xlabel)
        if (allocated(self%ylabel)) deallocate(self%ylabel)
    end subroutine destroy

end module fortplot_figure_core