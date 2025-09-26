module fortplot_matplotlib_plot_wrappers
    !! Matplotlib-style plot creation wrappers built on top of fortplot figures

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_errorbar_plots, only: errorbar_impl => errorbar
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_session, only: ensure_fig_init
    use fortplot_plotting_advanced, only: bar_impl, barh_impl
    use fortplot_scatter_plots, only: add_scatter_2d, add_scatter_3d
    use fortplot_3d_plots, only: add_3d_plot_impl => add_3d_plot

    implicit none
    private

    public :: plot
    public :: scatter
    public :: errorbar
    public :: boxplot
    public :: bar
    public :: barh
    public :: hist
    public :: histogram
    public :: add_plot
    public :: add_errorbar
    public :: add_scatter
    public :: add_3d_plot

    interface add_scatter
        module procedure add_scatter_2d_wrapper
        module procedure add_scatter_3d_wrapper
    end interface add_scatter

contains

    subroutine plot(x, y, label, linestyle)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle
        integer :: idx, nrows, ncols, row, col

        call ensure_fig_init()

        ! Route to subplot when a grid is active and a selection exists
        nrows = fig%subplot_rows
        ncols = fig%subplot_cols
        idx = fig%current_subplot

        if (nrows > 0 .and. ncols > 0 .and. idx >= 1 .and. idx <= nrows*ncols) then
            row = (idx - 1) / ncols + 1
            col = mod(idx - 1, ncols) + 1
            call fig%subplot_plot(row, col, x, y, label=label, linestyle=linestyle)
        else
            call fig%add_plot(x, y, label=label, linestyle=linestyle)
        end if
    end subroutine plot

    subroutine errorbar(x, y, xerr, yerr, fmt, label, capsize, linestyle, marker, &
                        color, elinewidth, capthick)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(wp), intent(in), optional :: capsize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: elinewidth, capthick

        call ensure_fig_init()
        ! Route to the actual errorbar implementation so error bars are visible
        call errorbar_impl(fig, x, y, xerr=xerr, yerr=yerr, label=label, &
                           capsize=capsize, marker=marker, color=color, &
                           elinewidth=elinewidth, capthick=capthick)
    end subroutine errorbar

    subroutine bar(x, height, width, bottom, label, color, edgecolor, align)
        real(wp), intent(in) :: x(:), height(:)
        real(wp), intent(in), optional :: width
        real(wp), intent(in), optional :: bottom(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: color(3), edgecolor(3)

        real(wp) :: bar_width
        real(wp), allocatable :: bar_bottom(:)
        character(len=32) :: bar_align

        call ensure_fig_init()

        bar_width = 0.8_wp
        if (present(width)) bar_width = width

        allocate(bar_bottom(size(x)))
        bar_bottom = 0.0_wp
        if (present(bottom)) then
            if (size(bottom) == size(x)) then
                bar_bottom = bottom
            else if (size(bottom) == 1) then
                bar_bottom = bottom(1)
            else
                call log_error("bar: bottom array size must match x array or be scalar")
                deallocate(bar_bottom)
                return
            end if
        end if

        bar_align = 'center'
        if (present(align)) bar_align = align

        call bar_impl(fig, x, height, width=bar_width, label=label, color=color)
        deallocate(bar_bottom)
    end subroutine bar

    subroutine barh(y, width, height, left, label, color, edgecolor, align)
        real(wp), intent(in) :: y(:), width(:)
        real(wp), intent(in), optional :: height
        real(wp), intent(in), optional :: left(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: color(3), edgecolor(3)

        real(wp) :: bar_height
        real(wp), allocatable :: bar_left(:)
        character(len=32) :: bar_align

        call ensure_fig_init()

        bar_height = 0.8_wp
        if (present(height)) bar_height = height

        allocate(bar_left(size(y)))
        bar_left = 0.0_wp
        if (present(left)) then
            if (size(left) == size(y)) then
                bar_left = left
            else if (size(left) == 1) then
                bar_left = left(1)
            else
                call log_error("barh: left array size must match y array or be scalar")
                deallocate(bar_left)
                return
            end if
        end if

        bar_align = 'center'
        if (present(align)) bar_align = align

        call barh_impl(fig, y, width, height=bar_height, label=label, color=color)
        deallocate(bar_left)
    end subroutine barh

    subroutine hist(data, bins, density, label, color)
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)

        call ensure_fig_init()
        call fig%add_hist(data, bins=bins, density=density, label=label, &
                          color=color)
    end subroutine hist

    subroutine histogram(data, bins, density, label, color)
        real(wp), intent(in) :: data(:)
        integer, intent(in), optional :: bins
        logical, intent(in), optional :: density
        character(len=*), intent(in), optional :: label
        real(wp), intent(in), optional :: color(3)

        call hist(data, bins=bins, density=density, label=label, color=color)
    end subroutine histogram

    subroutine boxplot(data, position, width, label, show_outliers, horizontal, color)
        real(wp), intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers, horizontal
        character(len=*), intent(in), optional :: color

        call ensure_fig_init()
        call fig%boxplot(data, position=position, width=width, label=label, &
                         show_outliers=show_outliers, horizontal=horizontal, &
                         color=color)
    end subroutine boxplot

    subroutine scatter(x, y, s, c, label, marker, markersize, color, &
                          linewidths, edgecolors, alpha)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: alpha

        call scatter_2d_entry(x, y, label=label, marker=marker)
    end subroutine scatter

    subroutine add_scatter_2d_wrapper(x, y, s, c, label, marker, markersize, &
                                          color, linewidths, edgecolors, alpha)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: alpha

        call scatter_2d_entry(x, y, label=label, marker=marker)
    end subroutine add_scatter_2d_wrapper

    subroutine add_scatter_3d_wrapper(x, y, z, s, c, label, marker, markersize, &
                                          color, linewidths, edgecolors, alpha)
        real(wp), intent(in) :: x(:), y(:), z(:)
        real(wp), intent(in), optional :: s
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: alpha

        real(wp), allocatable :: wx(:), wy(:), wz(:)

        call ensure_fig_init()
        allocate(wx(size(x)), wy(size(y)), wz(size(z)))
        wx = x
        wy = y
        wz = z
        call add_scatter_3d(fig, wx, wy, wz, label=label, marker=marker)
        deallocate(wx, wy, wz)
    end subroutine add_scatter_3d_wrapper

    subroutine add_plot(x, y, label, linestyle)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle

        call ensure_fig_init()
        call fig%add_plot(x, y, label=label, linestyle=linestyle)
    end subroutine add_plot

    subroutine add_errorbar(x, y, xerr, yerr, fmt, label, capsize, linestyle, &
                               marker, color, elinewidth, capthick)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(wp), intent(in), optional :: capsize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: elinewidth, capthick

        call ensure_fig_init()
        call errorbar_impl(fig, x, y, xerr=xerr, yerr=yerr, label=label, &
                           capsize=capsize, marker=marker, color=color, &
                           elinewidth=elinewidth, capthick=capthick)
    end subroutine add_errorbar

    subroutine add_3d_plot(x, y, z, label, linestyle, color, linewidth, marker, &
                              markersize)
        real(wp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth, markersize

        call ensure_fig_init()
        call add_3d_plot_impl(fig, x, y, z, label=label, linestyle=linestyle, &
                              marker=marker, markersize=markersize, linewidth=linewidth)
    end subroutine add_3d_plot

    subroutine scatter_2d_entry(x, y, label, marker)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, marker

        real(wp), allocatable :: wx(:), wy(:)

        call ensure_fig_init()
        allocate(wx(size(x)), wy(size(y)))
        wx = x
        wy = y
        call add_scatter_2d(fig, wx, wy, label=label, marker=marker)
        deallocate(wx, wy)
    end subroutine scatter_2d_entry

end module fortplot_matplotlib_plot_wrappers
