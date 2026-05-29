submodule(fortplot_figure_core) fortplot_figure_core_impl_state

    !! State management and configuration implementations
    !!
    !! Single Responsibility: Handle figure state, configuration,
    !! property accessors, animation support, and backend operations.
    !! Extracted from fortplot_figure_core_impl to maintain file size compliance.

    use fortplot_logging, only: log_error, log_warning
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

    module subroutine set_view(self, elev, azim, dist)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in), optional :: elev, azim, dist
        call core_set_view(self%state, elev, azim, dist)
    end subroutine set_view

    module subroutine set_line_width(self, width)
        class(figure_t), intent(inout) :: self
        real(wp), intent(in) :: width
        call core_set_line_width(self%state, width)
    end subroutine set_line_width

    module subroutine set_ydata(self, plot_index, y_new)
        class(figure_t), intent(inout) :: self
        integer, intent(in) :: plot_index
        real(wp), contiguous, intent(in) :: y_new(:)
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

    !! ── Property accessors ────────────────────────────────────────────

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

    !! ── Animation support ─────────────────────────────────────────────

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

    !! ── Backend operations ────────────────────────────────────────────

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

    !! ── Range accessors ───────────────────────────────────────────────

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

end submodule fortplot_figure_core_impl_state
