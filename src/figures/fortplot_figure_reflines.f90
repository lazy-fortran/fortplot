module fortplot_figure_reflines
    !! Reference line operations for figures
    !!
    !! This module provides horizontal and vertical reference line functionality
    !! similar to matplotlib's axhline, axvline, hlines, and vlines.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_plot_data, only: plot_data_t, PLOT_TYPE_REFLINE
    use fortplot_figure_initialization, only: figure_state_t
    use fortplot_figure_plot_management, only: next_plot_color
    use fortplot_colors, only: parse_color
    use fortplot_logging, only: log_error, log_warning
    implicit none

    private
    public :: core_axhline, core_axvline, core_hlines, core_vlines

contains

    subroutine core_axhline(plots, state, plot_count, y, xmin, xmax, color, &
                            linestyle, linewidth, label)
        !! Draw a horizontal line spanning the axes at y position
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: y
        real(wp), intent(in), optional :: xmin, xmax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth

        real(wp) :: x0, x1, plot_color(3)
        logical :: color_ok

        if (state%plot_count >= state%max_plots) then
            call log_warning('axhline: maximum number of plots reached')
            return
        end if

        x0 = 0.0_wp
        x1 = 1.0_wp
        if (present(xmin)) x0 = max(0.0_wp, min(1.0_wp, xmin))
        if (present(xmax)) x1 = max(0.0_wp, min(1.0_wp, xmax))

        plot_color = next_plot_color(state)
        if (present(color)) then
            call parse_color(color, plot_color, color_ok)
            if (.not. color_ok) then
                call log_warning('axhline: unsupported color; using default')
                plot_color = next_plot_color(state)
            end if
        end if

        state%plot_count = state%plot_count + 1
        plot_count = state%plot_count

        plots(plot_count)%plot_type = PLOT_TYPE_REFLINE
        allocate(plots(plot_count)%x(2))
        allocate(plots(plot_count)%y(2))
        plots(plot_count)%x = [x0, x1]
        plots(plot_count)%y = [y, y]
        plots(plot_count)%color = plot_color

        if (present(linestyle)) then
            plots(plot_count)%linestyle = trim(linestyle)
        else
            plots(plot_count)%linestyle = '-'
        end if

        if (present(label)) then
            plots(plot_count)%label = trim(label)
        end if

        if (present(linewidth)) then
            associate(unused => linewidth); end associate
        end if

        state%rendered = .false.
    end subroutine core_axhline

    subroutine core_axvline(plots, state, plot_count, x, ymin, ymax, color, &
                            linestyle, linewidth, label)
        !! Draw a vertical line spanning the axes at x position
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x
        real(wp), intent(in), optional :: ymin, ymax
        character(len=*), intent(in), optional :: color, linestyle, label
        real(wp), intent(in), optional :: linewidth

        real(wp) :: y0, y1, plot_color(3)
        logical :: color_ok

        if (state%plot_count >= state%max_plots) then
            call log_warning('axvline: maximum number of plots reached')
            return
        end if

        y0 = 0.0_wp
        y1 = 1.0_wp
        if (present(ymin)) y0 = max(0.0_wp, min(1.0_wp, ymin))
        if (present(ymax)) y1 = max(0.0_wp, min(1.0_wp, ymax))

        plot_color = next_plot_color(state)
        if (present(color)) then
            call parse_color(color, plot_color, color_ok)
            if (.not. color_ok) then
                call log_warning('axvline: unsupported color; using default')
                plot_color = next_plot_color(state)
            end if
        end if

        state%plot_count = state%plot_count + 1
        plot_count = state%plot_count

        plots(plot_count)%plot_type = PLOT_TYPE_REFLINE
        allocate(plots(plot_count)%x(2))
        allocate(plots(plot_count)%y(2))
        plots(plot_count)%x = [x, x]
        plots(plot_count)%y = [y0, y1]
        plots(plot_count)%color = plot_color

        if (present(linestyle)) then
            plots(plot_count)%linestyle = trim(linestyle)
        else
            plots(plot_count)%linestyle = '-'
        end if

        if (present(label)) then
            plots(plot_count)%label = trim(label)
        end if

        if (present(linewidth)) then
            associate(unused => linewidth); end associate
        end if

        state%rendered = .false.
    end subroutine core_axvline

    subroutine core_hlines(plots, state, plot_count, y, xmin, xmax, colors, &
                           linestyles, linewidth, label)
        !! Draw horizontal lines at each y position from xmin to xmax
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: y(:)
        real(wp), intent(in) :: xmin, xmax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth

        integer :: n, i, base_count
        real(wp) :: plot_color(3)
        logical :: color_ok

        n = size(y)
        if (n == 0) then
            call log_error('hlines: y array must contain at least one value')
            return
        end if

        if (state%plot_count + n > state%max_plots) then
            call log_warning('hlines: would exceed maximum plots; drawing partial')
            n = state%max_plots - state%plot_count
            if (n <= 0) return
        end if

        plot_color = next_plot_color(state)
        if (present(colors)) then
            call parse_color(colors, plot_color, color_ok)
            if (.not. color_ok) then
                call log_warning('hlines: unsupported color; using default')
                plot_color = next_plot_color(state)
            end if
        end if

        base_count = state%plot_count

        do i = 1, n
            state%plot_count = state%plot_count + 1
            plots(state%plot_count)%plot_type = PLOT_TYPE_REFLINE
            allocate(plots(state%plot_count)%x(2))
            allocate(plots(state%plot_count)%y(2))
            plots(state%plot_count)%x = [xmin, xmax]
            plots(state%plot_count)%y = [y(i), y(i)]
            plots(state%plot_count)%color = plot_color

            if (present(linestyles)) then
                plots(state%plot_count)%linestyle = trim(linestyles)
            else
                plots(state%plot_count)%linestyle = '-'
            end if

            if (i == 1 .and. present(label)) then
                plots(state%plot_count)%label = trim(label)
            end if
        end do

        plot_count = state%plot_count

        if (present(linewidth)) then
            associate(unused => linewidth); end associate
        end if

        state%rendered = .false.
    end subroutine core_hlines

    subroutine core_vlines(plots, state, plot_count, x, ymin, ymax, colors, &
                           linestyles, linewidth, label)
        !! Draw vertical lines at each x position from ymin to ymax
        type(plot_data_t), allocatable, intent(inout) :: plots(:)
        type(figure_state_t), intent(inout) :: state
        integer, intent(inout) :: plot_count
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: ymin, ymax
        character(len=*), intent(in), optional :: colors, linestyles, label
        real(wp), intent(in), optional :: linewidth

        integer :: n, i, base_count
        real(wp) :: plot_color(3)
        logical :: color_ok

        n = size(x)
        if (n == 0) then
            call log_error('vlines: x array must contain at least one value')
            return
        end if

        if (state%plot_count + n > state%max_plots) then
            call log_warning('vlines: would exceed maximum plots; drawing partial')
            n = state%max_plots - state%plot_count
            if (n <= 0) return
        end if

        plot_color = next_plot_color(state)
        if (present(colors)) then
            call parse_color(colors, plot_color, color_ok)
            if (.not. color_ok) then
                call log_warning('vlines: unsupported color; using default')
                plot_color = next_plot_color(state)
            end if
        end if

        base_count = state%plot_count

        do i = 1, n
            state%plot_count = state%plot_count + 1
            plots(state%plot_count)%plot_type = PLOT_TYPE_REFLINE
            allocate(plots(state%plot_count)%x(2))
            allocate(plots(state%plot_count)%y(2))
            plots(state%plot_count)%x = [x(i), x(i)]
            plots(state%plot_count)%y = [ymin, ymax]
            plots(state%plot_count)%color = plot_color

            if (present(linestyles)) then
                plots(state%plot_count)%linestyle = trim(linestyles)
            else
                plots(state%plot_count)%linestyle = '-'
            end if

            if (i == 1 .and. present(label)) then
                plots(state%plot_count)%label = trim(label)
            end if
        end do

        plot_count = state%plot_count

        if (present(linewidth)) then
            associate(unused => linewidth); end associate
        end if

        state%rendered = .false.
    end subroutine core_vlines

end module fortplot_figure_reflines
