submodule(fortplot_figure_core) fortplot_figure_core_impl_layout

    !! Layout and axis operations implementations
    !!
    !! Single Responsibility: Handle twin axes, subplots, reference lines,
    !! tick configuration, aspect ratio, and tight layout operations.
    !! Extracted from fortplot_figure_core_impl to maintain file size compliance.

    use fortplot_logging, only: log_error
    use fortplot_plot_bars, only: bar_impl, barh_impl
    use fortplot_string_utils, only: to_lowercase
    implicit none

contains

    !! ── Twin axis operations ──────────────────────────────────────────

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

    !! ── Subplot operations ────────────────────────────────────────────

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

    module subroutine subplot_plot(self, row, col, x, y, label, linestyle, color, alpha)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        real(wp), contiguous, intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: alpha
        call figure_subplot_plot(self%subplots_array, self%subplot_rows, &
                                  self%subplot_cols, row, col, x, y, label, &
                                  linestyle, color, alpha, self%state%colors, 6)
    end subroutine subplot_plot

    module subroutine subplot_bar(self, row, col, x, heights, width, bottom, label, &
                                  color, edgecolor, align, alpha)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        real(wp), contiguous, intent(in) :: x(:), heights(:)
        real(wp), intent(in), optional :: width
        real(wp), intent(in), optional :: bottom(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: color(3), edgecolor(3), alpha
        integer :: previous_subplot
        logical :: ok

        call begin_subplot_call(self, row, col, 'subplot_bar', previous_subplot, ok)
        if (.not. ok) return
        call bar_impl(self, x, heights, width=width, bottom=bottom, label=label, &
                      color=color, edgecolor=edgecolor, alpha=alpha)
        self%current_subplot = previous_subplot
        if (present(align)) continue
    end subroutine subplot_bar

    module subroutine subplot_barh(self, row, col, y, widths, height, left, label, &
                                   color, edgecolor, align, alpha)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        real(wp), contiguous, intent(in) :: y(:), widths(:)
        real(wp), intent(in), optional :: height
        real(wp), intent(in), optional :: left(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: color(3), edgecolor(3), alpha
        integer :: previous_subplot
        logical :: ok

        call begin_subplot_call(self, row, col, 'subplot_barh', previous_subplot, ok)
        if (.not. ok) return
        call barh_impl(self, y, widths, height=height, left=left, label=label, &
                       color=color, edgecolor=edgecolor, alpha=alpha)
        self%current_subplot = previous_subplot
        if (present(align)) continue
    end subroutine subplot_barh

    module subroutine subplot_hist(self, row, col, data, bins, density, label, color, &
                                  range, weights, cumulative, orientation, alpha)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        real(wp), contiguous, intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        character(len=*), intent(in), optional :: color
        real(wp), intent(in), optional :: range(2)
        real(wp), intent(in), optional :: weights(:)
        logical, intent(in), optional :: cumulative
        character(len=*), intent(in), optional :: orientation
        real(wp), intent(in), optional :: alpha
        integer :: previous_subplot
        logical :: ok

        call begin_subplot_call(self, row, col, 'subplot_hist', previous_subplot, ok)
        if (.not. ok) return
        call self%add_hist(data, bins=bins, density=density, label=label, &
                           color=color, range=range, weights=weights, &
                           cumulative=cumulative, orientation=orientation, &
                           alpha=alpha)
        self%current_subplot = previous_subplot
    end subroutine subplot_hist

    module subroutine subplot_boxplot(self, row, col, data, position, width, label, &
                                      show_outliers, horizontal, color)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        real(wp), contiguous, intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers
        logical, intent(in), optional :: horizontal
        real(wp), intent(in), optional :: color(3)
        integer :: previous_subplot
        logical :: ok

        call begin_subplot_call(self, row, col, 'subplot_boxplot', previous_subplot, ok)
        if (.not. ok) return
        call self%boxplot(data, position=position, width=width, label=label, &
                          show_outliers=show_outliers, horizontal=horizontal, &
                          color=color)
        self%current_subplot = previous_subplot
    end subroutine subplot_boxplot

    module subroutine subplot_scatter(self, row, col, x, y, s, c, marker, markersize, &
                                      color, colormap, alpha, edgecolor, facecolor, &
                                      linewidth, vmin, vmax, label, show_colorbar)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(..), c(:)
        character(len=*), intent(in), optional :: marker, colormap, label
        real(wp), intent(in), optional :: markersize, alpha, linewidth, vmin, vmax
        real(wp), intent(in), optional :: color(3), edgecolor(3), facecolor(3)
        logical, intent(in), optional :: show_colorbar
        integer :: previous_subplot
        logical :: ok

        call begin_subplot_call(self, row, col, 'subplot_scatter', previous_subplot, ok)
        if (.not. ok) return
        call self%scatter(x, y, s=s, c=c, marker=marker, markersize=markersize, &
                          color=color, colormap=colormap, alpha=alpha, &
                          edgecolor=edgecolor, facecolor=facecolor, &
                          linewidth=linewidth, vmin=vmin, vmax=vmax, label=label, &
                          show_colorbar=show_colorbar)
        self%current_subplot = previous_subplot
    end subroutine subplot_scatter

    module subroutine relocate_last_plot_to_subplot(self)
        !! Copy the most recently added figure-level plot into the currently
        !! selected subplot. Plot commands other than plot() (hist, bar,
        !! scatter, ...) build their plot_data into the figure-level plots
        !! array; when a subplot grid is active that array is never rendered,
        !! so the data silently vanishes (issue #2021). Mirroring the entry
        !! into the active subplot makes those commands honour subplot
        !! selection exactly like plot() does.
        !!
        !! The figure-level slot is left in place rather than removed: the add
        !! routines assume monotonically growing slots and reuse would
        !! double-allocate plot storage. The figure-level array is not rendered
        !! while a subplot grid is active, so the retained copy is inert.
        class(figure_t), intent(inout) :: self
        integer :: idx, rows, cols, row, col, sidx
        type(plot_data_t), allocatable :: grown(:)

        rows = self%subplot_rows
        cols = self%subplot_cols
        idx = self%current_subplot
        if (rows <= 0 .or. cols <= 0) return
        if (idx < 1 .or. idx > rows*cols) return
        if (.not. allocated(self%subplots_array)) return
        if (.not. allocated(self%plots)) return
        if (self%plot_count < 1 .or. self%plot_count > size(self%plots)) return

        row = (idx - 1)/cols + 1
        col = mod(idx - 1, cols) + 1

        associate (sp => self%subplots_array(row, col))
            if (.not. allocated(sp%plots)) then
                allocate (sp%plots(sp%max_plots))
                sp%plot_count = 0
            end if
            sidx = sp%plot_count + 1
            if (sidx > sp%max_plots) then
                allocate (grown(sp%max_plots*2))
                grown(1:sp%plot_count) = sp%plots(1:sp%plot_count)
                call move_alloc(grown, sp%plots)
                sp%max_plots = sp%max_plots*2
            end if
            sp%plots(sidx) = self%plots(self%plot_count)
            sp%plot_count = sidx
        end associate
    end subroutine relocate_last_plot_to_subplot

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

    subroutine begin_subplot_call(self, row, col, context, previous_subplot, ok)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: row, col
        character(len=*), intent(in) :: context
        integer, intent(out) :: previous_subplot
        logical, intent(out) :: ok

        previous_subplot = self%current_subplot
        ok = .false.

        if (self%subplot_rows <= 0 .or. self%subplot_cols <= 0) then
            call log_error(trim(context)//": subplot grid is not active")
            return
        end if

        if (row <= 0 .or. row > self%subplot_rows .or. &
            col <= 0 .or. col > self%subplot_cols) then
            call log_error(trim(context)//": Invalid subplot indices")
            return
        end if

        self%current_subplot = (row - 1)*self%subplot_cols + col
        ok = .true.
    end subroutine begin_subplot_call

    !! ── Reference line operations ─────────────────────────────────────

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
        real(wp), contiguous, intent(in) :: y(:)
        real(wp), intent(in) :: xmin, xmax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth
        call core_hlines(self%plots, self%state, self%plot_count, y, &
                          xmin, xmax, colors, linestyles, linewidth, label)
    end subroutine hlines

    module subroutine vlines(self, x, ymin, ymax, colors, linestyles, linewidth, label)
        class(figure_t), intent(inout) :: self
        real(wp), contiguous, intent(in) :: x(:)
        real(wp), intent(in) :: ymin, ymax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth

        call core_vlines(self%plots, self%state, self%plot_count, x, &
                          ymin, ymax, colors, linestyles, linewidth, label)
    end subroutine vlines

    !! ── Tick operations ───────────────────────────────────────────────

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

    !! ── Aspect ratio ──────────────────────────────────────────────────

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

    !! ── Layout ────────────────────────────────────────────────────────

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

end submodule fortplot_figure_core_impl_layout
