submodule(fortplot_figure_core) fortplot_figure_core_wrappers_layout

    implicit none

contains

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
        !! Draw a horizontal line spanning the axes at y position
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y
        real(wp), intent(in), optional :: xmin, xmax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth

        call core_axhline(self%plots, self%state, self%plot_count, y, &
                          xmin, xmax, color, linestyle, linewidth, label)
    end subroutine axhline

    module subroutine axvline(self, x, ymin, ymax, color, linestyle, linewidth, label)
        !! Draw a vertical line spanning the axes at x position
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x
        real(wp), intent(in), optional :: ymin, ymax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth
        call core_axvline(self%plots, self%state, self%plot_count, x, &
                          ymin, ymax, color, linestyle, linewidth, label)
    end subroutine axvline

    module subroutine hlines(self, y, xmin, xmax, colors, linestyles, linewidth, label)
        !! Draw horizontal lines at each y position from xmin to xmax
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: y(:)
        real(wp), intent(in) :: xmin, xmax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth
        call core_hlines(self%plots, self%state, self%plot_count, y, &
                         xmin, xmax, colors, linestyles, linewidth, label)
    end subroutine hlines

    module subroutine vlines(self, x, ymin, ymax, colors, linestyles, linewidth, label)
        !! Draw vertical lines at each x position from ymin to ymax
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: ymin, ymax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth

        call core_vlines(self%plots, self%state, self%plot_count, x, &
                         ymin, ymax, colors, linestyles, linewidth, label)
    end subroutine vlines

    module subroutine set_minor_ticks(self, x, y)
        !! Enable or disable minor ticks on x and/or y axes
        class(figure_t), intent(inout) :: self
        logical, intent(in), optional :: x, y
        if (present(x)) self%state%minor_ticks_x = x
        if (present(y)) self%state%minor_ticks_y = y
        self%state%rendered = .false.
    end subroutine set_minor_ticks

    module subroutine set_minor_tick_count(self, count)
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

    module subroutine minorticks_on(self)
        !! Enable minor ticks on both axes (matplotlib-compatible convenience method)
        class(figure_t), intent(inout) :: self
        self%state%minor_ticks_x = .true.
        self%state%minor_ticks_y = .true.
        self%state%rendered = .false.
    end subroutine minorticks_on

    module subroutine set_aspect_str(self, aspect)
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

    module subroutine set_aspect_num(self, ratio)
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

    module subroutine tight_layout(self, pad, w_pad, h_pad)
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

end submodule fortplot_figure_core_wrappers_layout
