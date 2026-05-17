submodule(fortplot_figure_core) fortplot_figure_core_impl

    implicit none

contains

    !! ── State management ──────────────────────────────────────────────

    module subroutine grid(self, enabled, which, axis, alpha, linestyle)
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: enabled
        character(len=*), intent(in), optional :: which, axis, linestyle
        real(wp), intent(in), optional :: alpha

        call core_grid(self%state, enabled, which, axis, alpha, linestyle)
    end subroutine grid

    module subroutine set_xlabel(self, label)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        call core_set_xlabel(self%state, self%xlabel, label)
    end subroutine set_xlabel

    module subroutine set_ylabel(self, label)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: label
        call core_set_ylabel(self%state, self%ylabel, label)
    end subroutine set_ylabel

    module subroutine set_title(self, title)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: title
        call core_set_title(self%state, self%title, title)
    end subroutine set_title

    module subroutine set_xscale(self, scale, threshold, base, linscale)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold, base, linscale
        call core_set_xscale(self%state, scale, threshold, base, linscale)
    end subroutine set_xscale

    module subroutine set_yscale(self, scale, threshold, base, linscale)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: scale
        real(wp), intent(in), optional :: threshold, base, linscale
        call core_set_yscale(self%state, scale, threshold, base, linscale)
    end subroutine set_yscale

    module subroutine set_xlim(self, x_min, x_max)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_min, x_max
        call core_set_xlim(self%state, x_min, x_max)
    end subroutine set_xlim

    module subroutine set_ylim(self, y_min, y_max)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y_min, y_max
        call core_set_ylim(self%state, y_min, y_max)
    end subroutine set_ylim

    module subroutine set_line_width(self, width)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: width
        call core_set_line_width(self%state, width)
    end subroutine set_line_width

    module subroutine set_ydata(self, plot_index, y_new)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        real(wp), intent(in) :: y_new(:)
        call core_set_ydata(self%plots, self%state%plot_count, plot_index, y_new)
    end subroutine set_ydata

    module subroutine figure_legend(self, location)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in), optional :: location
        call core_figure_legend(self%state, self%plots, self%state%plot_count, &
                                location)
    end subroutine figure_legend

    module subroutine clear(self)
        class(figure_t), intent(inout) :: self
        call core_clear(self%state, self%plots, self%streamlines, self%annotations, &
                        self%subplots_array, self%subplot_rows, self%subplot_cols, &
                        self%current_subplot, self%title, self%xlabel, self%ylabel, &
                        self%plot_count, self%annotation_count)
    end subroutine clear

    module subroutine clear_streamlines(self)
        class(figure_t), intent(inout) :: self
        call core_clear_streamlines(self%streamlines)
    end subroutine clear_streamlines

    module subroutine destroy(self)
        type(figure_t), intent(inout) :: self
        call core_destroy(self%state, self%plots, self%streamlines, &
                          self%title, self%xlabel, self%ylabel)
    end subroutine destroy

    module function get_width(self) result(width)
        class(figure_t), intent(in) :: self
        integer :: width
        width = core_get_width(self%state)
    end function get_width

    module function get_height(self) result(height)
        class(figure_t), intent(in) :: self
        integer :: height
        height = core_get_height(self%state)
    end function get_height

    module function get_rendered(self) result(rendered)
        class(figure_t), intent(in) :: self
        logical :: rendered
        rendered = core_get_rendered(self%state)
    end function get_rendered

    module subroutine set_rendered(self, rendered)
        class(figure_t), intent(inout) :: self
        logical, intent(in) :: rendered
        call core_set_rendered(self%state, rendered)
    end subroutine set_rendered

    module function get_plot_count(self) result(plot_count)
        class(figure_t), intent(in) :: self
        integer :: plot_count
        plot_count = core_get_plot_count(self%state)
    end function get_plot_count

    module function get_plots(self) result(plots_ptr)
        class(figure_t), intent(in), target :: self
        type(plot_data_t), pointer :: plots_ptr(:)
        plots_ptr => core_get_plots(self%plots)
    end function get_plots

    module function get_dpi(self) result(dpi)
        class(figure_t), intent(in) :: self
        real(wp) :: dpi
        dpi = self%state%dpi
    end function get_dpi

    module subroutine set_dpi(self, dpi)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: dpi

        if (dpi <= 0.0_wp) then
            call log_error('set_dpi: DPI must be positive')
            return
        end if

        self%state%dpi = dpi
        self%state%rendered = .false.
    end subroutine set_dpi

    module subroutine setup_png_backend_for_animation(self)
        class(figure_t), intent(inout) :: self
        call core_setup_png_backend_for_animation(self%state)
    end subroutine setup_png_backend_for_animation

    module subroutine extract_rgb_data_for_animation(self, rgb_data)
        class(figure_t), intent(inout) :: self
        real(wp), intent(out) :: rgb_data(:, :, :)
        call core_extract_rgb_data_for_animation(self%state, rgb_data, &
                                                   self%plots, self%state%plot_count, &
                                                   self%annotations, &
                                                   self%annotation_count, &
                                                   self%state%rendered)
    end subroutine extract_rgb_data_for_animation

    module subroutine extract_png_data_for_animation(self, png_data, status)
        class(figure_t), intent(inout) :: self
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        call core_extract_png_data_for_animation(self%state, png_data, status, &
                                                   self%plots, self%state%plot_count, &
                                                   self%annotations, &
                                                   self%annotation_count, &
                                                   self%state%rendered)
    end subroutine extract_png_data_for_animation

    module subroutine backend_color(self, r, g, b)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: r, g, b
        call core_backend_color(self%state, r, g, b)
    end subroutine backend_color

    module function backend_associated(self) result(is_associated)
        class(figure_t), intent(in) :: self
        logical :: is_associated
        is_associated = core_backend_associated(self%state)
    end function backend_associated

    module subroutine backend_line(self, x1, y1, x2, y2)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x1, y1, x2, y2
        call core_backend_line(self%state, x1, y1, x2, y2)
    end subroutine backend_line

    module subroutine backend_arrow(self, x, y, dx, dy, size, style)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        call core_backend_arrow(self%state, x, y, dx, dy, size, style)
    end subroutine backend_arrow

    module subroutine clear_backend_arrows(self)
        class(figure_t), intent(inout) :: self
        logical :: had_arrows
        type(arrow_data_t), allocatable :: arrows_tmp(:)

        had_arrows = .false.
        if (allocated(self%state%stream_arrows)) then
            had_arrows = size(self%state%stream_arrows) > 0
            call move_alloc(self%state%stream_arrows, arrows_tmp)
        end if

        if (had_arrows) self%state%rendered = .false.
    end subroutine clear_backend_arrows

    module function get_x_min(self) result(x_min)
        class(figure_t), intent(in) :: self
        real(wp) :: x_min
        x_min = core_get_x_min(self%state)
    end function get_x_min

    module function get_x_max(self) result(x_max)
        class(figure_t), intent(in) :: self
        real(wp) :: x_max
        x_max = core_get_x_max(self%state)
    end function get_x_max

    module function get_y_min(self) result(y_min)
        class(figure_t), intent(in) :: self
        real(wp) :: y_min
        y_min = core_get_y_min(self%state)
    end function get_y_min

    module function get_y_max(self) result(y_max)
        class(figure_t), intent(in) :: self
        real(wp) :: y_max
        y_max = core_get_y_max(self%state)
    end function get_y_max

    !! ── Plot operations ───────────────────────────────────────────────

    module subroutine initialize(self, width, height, backend, dpi)
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

    module subroutine add_plot_real(self, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)

        call core_add_plot(self%plots, self%state, x, y, label, linestyle, color, &
                            self%plot_count)
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

    module subroutine add_contour(self, x_grid, y_grid, z_grid, levels, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: label

        call core_add_contour(self%plots, self%state, x_grid, y_grid, z_grid, &
                               levels, label, self%plot_count)
    end subroutine add_contour

module subroutine add_contour_filled(self, x_grid, y_grid, z_grid, levels, &
                                           cmap, show_colorbar, label, colormap)
        !! Add a filled contour plot to the figure
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar

        call core_add_contour_filled(self%plots, self%state, x_grid, y_grid, &
                                       z_grid, levels=levels, cmap=cmap, &
                                       show_colorbar=show_colorbar, label=label, &
                                       colormap=colormap, plot_count=self%plot_count)
    end subroutine add_contour_filled

    module subroutine add_contourf(self, x_grid, y_grid, z_grid, levels, &
                                   cmap, show_colorbar, label, colormap)
        !! Add a filled contour plot to the figure
        !!
        !! Matplotlib-canonical alias for add_contour_filled.
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in), optional :: levels(:)
        character(len=*), intent(in), optional :: cmap, label, colormap
        logical, intent(in), optional :: show_colorbar

        call self%add_contour_filled(x_grid, y_grid, z_grid, levels=levels, &
                                     cmap=cmap, show_colorbar=show_colorbar, &
                                     label=label, colormap=colormap)
    end subroutine add_contourf

module subroutine add_surface(self, x_grid, y_grid, z_grid, label, cmap, &
                                   show_colorbar, alpha, edgecolor, linewidth, filled, &
                                   colormap)
        !! Add a 3D surface plot to the figure
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        character(len=*), intent(in), optional :: label, cmap, colormap
        logical, intent(in), optional :: show_colorbar, filled
        real(wp), intent(in), optional :: alpha, linewidth
        real(wp), intent(in), optional :: edgecolor(3)

        call core_add_surface(self%plots, self%state, x_grid, y_grid, z_grid, &
                              label=label, cmap=cmap, show_colorbar=show_colorbar, &
                              alpha=alpha, edgecolor=edgecolor, linewidth=linewidth, &
                              filled=filled, plot_count=self%plot_count, &
                              colormap=colormap)
    end subroutine add_surface

module subroutine add_pcolormesh(self, x, y, c, cmap, vmin, vmax, edgecolors, &
                                       linewidths, colormap)
        !! Add a pcolormesh plot to the figure
        !!
        !! `cmap` is the matplotlib-canonical keyword; `colormap` is a
        !! backward-compatible alias.
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), c(:, :)
        character(len=*), intent(in), optional :: cmap, colormap
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: linewidths

        call core_add_pcolormesh(self%plots, self%state, x, y, c, cmap=cmap, &
                                  vmin=vmin, vmax=vmax, edgecolors=edgecolors, &
                                  linewidths=linewidths, plot_count=self%plot_count, &
                                  colormap=colormap)
    end subroutine add_pcolormesh

    module subroutine streamplot(self, x, y, u, v, density, color, &
                                  linewidth, rtol, &
                                  atol, max_time, arrowsize, arrowstyle)
        use fortplot_plotting_advanced, only: streamplot_impl
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:, :), v(:, :)
        real(wp), intent(in), optional :: density
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth
        real(wp), intent(in), optional :: rtol, atol, max_time
        real(wp), intent(in), optional :: arrowsize
        character(len=*), intent(in), optional :: arrowstyle

        call streamplot_impl(self, x, y, u, v, density=density, color=color, &
                              linewidth=linewidth, rtol=rtol, atol=atol, &
                              max_time=max_time, arrowsize=arrowsize, &
                              arrowstyle=arrowstyle)
    end subroutine streamplot

    module subroutine quiver(self, x, y, u, v, scale, color, width, headwidth, &
                              headlength, units, pivot, scale_units)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:), u(:), v(:)
        real(wp), intent(in), optional :: scale
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: width, headwidth, headlength
        character(len=*), intent(in), optional :: units, pivot, scale_units

        call core_quiver(self%plots, self%state, self%plot_count, x, y, u, v, &
                          scale, color, width, headwidth, headlength, units, &
                          pivot, scale_units)
    end subroutine quiver

    module subroutine add_hist(self, data, bins, density, label, color)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)

        call core_hist(self%plots, self%state, self%plot_count, data, bins, &
                        density, label, color)
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
        real(wp), intent(in), optional :: color(3)

        call core_boxplot(self%plots, self%state, self%plot_count, data, &
                           position, width, &
                           label, show_outliers, horizontal, color, &
                           self%state%max_plots)
    end subroutine boxplot

    module subroutine scatter(self, x, y, s, c, marker, markersize, color, &
                               colormap, alpha, edgecolor, facecolor, linewidth, &
                               vmin, vmax, label, show_colorbar)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), c(:)
        character(len=*), intent(in), optional :: marker, colormap, label
        real(wp), intent(in), optional :: markersize, alpha, linewidth, vmin, vmax
        real(wp), intent(in), optional :: color(3), edgecolor(3), facecolor(3)
        logical, intent(in), optional :: show_colorbar

        real(wp) :: default_color(3)

        default_color = next_plot_color(self%state)

        call core_scatter(self%plots, self%state, self%plot_count, x, y, s, c, &
                           marker, markersize, color, colormap, alpha, edgecolor, &
                           facecolor, linewidth, vmin, vmax, label, show_colorbar, &
                           default_color)
    end subroutine scatter

    !! ── Layout ────────────────────────────────────────────────────────

    module subroutine twinx(self)
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

    module subroutine twiny(self)
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

    module subroutine use_axis(self, axis_name)
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

    module function get_active_axis(self) result(axis_name)
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

    module subroutine subplots(self, nrows, ncols)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: nrows, ncols
        call figure_subplots(self%subplots_array, self%subplot_rows, &
                              self%subplot_cols, self%current_subplot, nrows, &
                              ncols)
    end subroutine subplots

    module subroutine suptitle(self, title_text, fontsize)
        class(figure_t), intent(inout) :: self
        character(len=*), intent(in) :: title_text
        real(wp), intent(in), optional :: fontsize

        self%state%suptitle = trim(title_text)
        if (present(fontsize)) then
            self%state%suptitle_fontsize = fontsize
        end if
        self%state%rendered = .false.
    end subroutine suptitle

    module subroutine subplot_plot(self, row, col, x, y, label, linestyle, color)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        call figure_subplot_plot(self%subplots_array, self%subplot_rows, &
                                  self%subplot_cols, row, col, x, y, label, &
                                  linestyle, color, self%state%colors, 6)
    end subroutine subplot_plot

    module function subplot_plot_count(self, row, col) result(count)
        class(figure_t), intent(in) :: self
        integer, intent(in) :: row, col
        integer :: count
        count = figure_subplot_plot_count(self%subplots_array, self%subplot_rows, &
                                           self%subplot_cols, row, col)
    end function subplot_plot_count

    module subroutine subplot_set_title(self, row, col, title)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: title
        call figure_subplot_set_title(self%subplots_array, self%subplot_rows, &
                                       self%subplot_cols, row, col, title)
    end subroutine subplot_set_title

    module subroutine subplot_set_xlabel(self, row, col, xlabel)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: xlabel
        call figure_subplot_set_xlabel(self%subplots_array, self%subplot_rows, &
                                        self%subplot_cols, row, col, xlabel)
    end subroutine subplot_set_xlabel

    module subroutine subplot_set_ylabel(self, row, col, ylabel)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: ylabel
        call figure_subplot_set_ylabel(self%subplots_array, self%subplot_rows, &
                                        self%subplot_cols, row, col, ylabel)
    end subroutine subplot_set_ylabel

    module function subplot_title(self, row, col) result(title)
        class(figure_t), intent(in) :: self
        integer, intent(in) :: row, col
        character(len=:), allocatable :: title
        title = figure_subplot_title(self%subplots_array, self%subplot_rows, &
                                      self%subplot_cols, row, col)
    end function subplot_title

    module subroutine axhline(self, y, xmin, xmax, color, linestyle, linewidth, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y
        real(wp), intent(in), optional :: xmin, xmax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth

        call core_axhline(self%plots, self%state, self%plot_count, y, &
                           xmin, xmax, color, linestyle, linewidth, label)
    end subroutine axhline

    module subroutine axvline(self, x, ymin, ymax, color, linestyle, linewidth, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x
        real(wp), intent(in), optional :: ymin, ymax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth
        call core_axvline(self%plots, self%state, self%plot_count, x, &
                           ymin, ymax, color, linestyle, linewidth, label)
    end subroutine axvline

    module subroutine hlines(self, y, xmin, xmax, colors, linestyles, linewidth, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y(:)
        real(wp), intent(in) :: xmin, xmax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth
        call core_hlines(self%plots, self%state, self%plot_count, y, &
                          xmin, xmax, colors, linestyles, linewidth, label)
    end subroutine hlines

    module subroutine vlines(self, x, ymin, ymax, colors, linestyles, linewidth, label)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: ymin, ymax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth

        call core_vlines(self%plots, self%state, self%plot_count, x, &
                          ymin, ymax, colors, linestyles, linewidth, label)
    end subroutine vlines

    module subroutine set_minor_ticks(self, x, y)
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: x, y
        if (present(x)) self%state%minor_ticks_x = x
        if (present(y)) self%state%minor_ticks_y = y
        self%state%rendered = .false.
    end subroutine set_minor_ticks

    module subroutine set_minor_tick_count(self, count)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: count
        if (count >= 1 .and. count <= 20) then
            self%state%minor_tick_count = count
            self%state%rendered = .false.
        else
            call log_warning('set_minor_tick_count: count must be between 1 and 20')
        end if
    end subroutine set_minor_tick_count

    module subroutine minorticks_on(self)
        class(figure_t), intent(inout) :: self
        self%state%minor_ticks_x = .true.
        self%state%minor_ticks_y = .true.
        self%state%rendered = .false.
    end subroutine minorticks_on

    module subroutine set_aspect_str(self, aspect)
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

    module subroutine set_aspect_num(self, ratio)
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

    module subroutine tight_layout(self, pad, w_pad, h_pad)
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

end submodule fortplot_figure_core_impl
