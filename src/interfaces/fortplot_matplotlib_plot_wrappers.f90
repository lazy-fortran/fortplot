module fortplot_matplotlib_plot_wrappers
    !! Matplotlib-style plot creation wrappers built on top of fortplot figures.
    !!
    !! Each wrapper exposes a matplotlib-compatible signature. Color kwargs
    !! accept either a character string (name, hex, single-letter) or an RGB
    !! triple through generic interfaces defined below. Parameters that have
    !! no visual effect in the current backend are accepted silently so that
    !! matplotlib-style code does not generate spurious runtime warnings.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_color_utils, only: resolve_color_string_or_rgb
    use fortplot_matplotlib_session, only: ensure_fig_init
    use fortplot_plotting_advanced, only: bar_impl, barh_impl
    use fortplot_3d_plots, only: add_3d_plot_impl => add_3d_plot
    use fortplot_matplotlib_scatter, only: &
        scatter, add_scatter
    use fortplot_matplotlib_errorbar, only: &
        errorbar, add_errorbar

    implicit none
    private

    public :: plot
    public :: scatter
    public :: errorbar
    public :: boxplot
    public :: bar
    public :: barh
    public :: bar_rgb_array
    public :: barh_rgb_array
    public :: add_plot
    public :: add_errorbar
    public :: add_scatter
    public :: add_3d_plot

    interface boxplot
        module procedure boxplot_string
        module procedure boxplot_rgb
    end interface boxplot

    interface add_3d_plot
        module procedure add_3d_plot_rgb
        module procedure add_3d_plot_string
    end interface add_3d_plot

    interface bar
        module procedure bar_rgb
        module procedure bar_string
        module procedure bar_rgb_edgecolor
    end interface bar

    interface barh
        module procedure barh_rgb
        module procedure barh_string
        module procedure barh_rgb_edgecolor
    end interface barh

    interface add_plot
        module procedure add_plot_rgb
        module procedure add_plot_string
    end interface add_plot

contains

    subroutine plot(x, y, label, linestyle, color, linewidth, marker, markersize, alpha)
        !! Plot x and y data with the active figure or subplot.
        !!
        !! Parameters
        !! x : real(wp), contiguous, intent(in)
        !!     X coordinates.
        !! y : real(wp), contiguous, intent(in)
        !!     Y coordinates.
        !! label : character(len=*), optional
        !!     Legend label.
        !! linestyle : character(len=*), optional
        !!     Line style string.
        !! color : real(wp)(3), optional
        !!     RGB line color.
        !! linewidth : real(wp), optional
        !!     Line width override.
        !! marker : character(len=*), optional
        !!     Marker style.
        !! markersize : real(wp), optional
        !!     Marker size override.
        !! alpha : real(wp), optional
        !!     Line transparency.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth, markersize, alpha
        integer :: idx, nrows, ncols, row, col

        call ensure_fig_init()

        nrows = fig%subplot_rows
        ncols = fig%subplot_cols
        idx = fig%current_subplot

        if (nrows > 0 .and. ncols > 0 .and. idx >= 1 .and. idx <= nrows*ncols) then
            row = (idx - 1)/ncols + 1
            col = mod(idx - 1, ncols) + 1
            call fig%subplot_plot(row, col, x, y, label=label, linestyle=linestyle, &
                                  color=color, alpha=alpha)
        else
            call fig%add_plot(x, y, label=label, linestyle=linestyle, color=color, &
                              alpha=alpha)
        end if

        call apply_line_style_overrides(linewidth, marker, markersize)
    end subroutine plot

    subroutine bar_rgb(x, height, width, bottom, label, color, edgecolor, align, alpha)
        real(wp), contiguous, intent(in) :: x(:), height(:)
        real(wp), intent(in), optional :: width
        real(wp), intent(in), optional :: bottom(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: color(3), edgecolor(3), alpha

        real(wp) :: bar_width
        real(wp), allocatable :: bar_bottom(:)

        call ensure_fig_init()

        bar_width = 0.8_wp
        if (present(width)) bar_width = width

        call resolve_bar_bottom(size(x), bottom, bar_bottom, 'bar')
        if (.not. allocated(bar_bottom)) return

        call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                      label=label, color=color, edgecolor=edgecolor, alpha=alpha)
    end subroutine bar_rgb

    subroutine bar_string(x, height, color, width, bottom, label, edgecolor, align, alpha)
        real(wp), contiguous, intent(in) :: x(:), height(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: width
        real(wp), intent(in), optional :: bottom(:)
        character(len=*), intent(in), optional :: label, align, edgecolor
        real(wp), intent(in), optional :: alpha

        real(wp) :: bar_width
        real(wp), allocatable :: bar_bottom(:)
        real(wp) :: color_rgb(3), edge_rgb(3)
        logical :: has_color, has_edge

        call ensure_fig_init()

        bar_width = 0.8_wp
        if (present(width)) bar_width = width

        call resolve_bar_bottom(size(x), bottom, bar_bottom, 'bar')
        if (.not. allocated(bar_bottom)) return

        call resolve_color_string_or_rgb(color_str=color, context='bar', &
                                         rgb_out=color_rgb, has_color=has_color)
        call resolve_color_string_or_rgb(color_str=edgecolor, context='bar', &
                                         rgb_out=edge_rgb, has_color=has_edge)

        if (has_color .and. has_edge) then
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label, color=color_rgb, edgecolor=edge_rgb, &
                          alpha=alpha)
        else if (has_color) then
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label, color=color_rgb, alpha=alpha)
        else if (has_edge) then
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label, edgecolor=edge_rgb, alpha=alpha)
        else
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label, alpha=alpha)
        end if
    end subroutine bar_string

    subroutine barh_rgb(y, width, height, left, label, color, edgecolor, align, alpha)
        real(wp), contiguous, intent(in) :: y(:), width(:)
        real(wp), intent(in), optional :: height
        real(wp), intent(in), optional :: left(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: color(3), edgecolor(3), alpha

        real(wp) :: bar_height
        real(wp), allocatable :: bar_left(:)

        call ensure_fig_init()

        bar_height = 0.8_wp
        if (present(height)) bar_height = height

        call resolve_bar_bottom(size(y), left, bar_left, 'barh')
        if (.not. allocated(bar_left)) return

        call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                       label=label, color=color, edgecolor=edgecolor, alpha=alpha)
    end subroutine barh_rgb

    subroutine barh_string(y, width, color, height, left, label, edgecolor, align, alpha)
        real(wp), contiguous, intent(in) :: y(:), width(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: height
        real(wp), intent(in), optional :: left(:)
        character(len=*), intent(in), optional :: label, align, edgecolor
        real(wp), intent(in), optional :: alpha

        real(wp) :: bar_height
        real(wp), allocatable :: bar_left(:)
        real(wp) :: color_rgb(3), edge_rgb(3)
        logical :: has_color, has_edge

        call ensure_fig_init()

        bar_height = 0.8_wp
        if (present(height)) bar_height = height

        call resolve_bar_bottom(size(y), left, bar_left, 'barh')
        if (.not. allocated(bar_left)) return

        call resolve_color_string_or_rgb(color_str=color, context='barh', &
                                         rgb_out=color_rgb, has_color=has_color)
        call resolve_color_string_or_rgb(color_str=edgecolor, context='barh', &
                                         rgb_out=edge_rgb, has_color=has_edge)

        if (has_color .and. has_edge) then
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label, color=color_rgb, edgecolor=edge_rgb, &
                           alpha=alpha)
        else if (has_color) then
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label, color=color_rgb, alpha=alpha)
        else if (has_edge) then
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label, edgecolor=edge_rgb, alpha=alpha)
        else
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label, alpha=alpha)
        end if
    end subroutine barh_string

    subroutine bar_rgb_edgecolor(x, height, color, edgecolor, width, bottom, label, align, alpha)
        !! Bar with RGB-triple color and named-color edgecolor
        real(wp), contiguous, intent(in) :: x(:), height(:)
        real(wp), intent(in) :: color(3)
        character(len=*), intent(in) :: edgecolor
        real(wp), intent(in), optional :: width
        real(wp), intent(in), optional :: bottom(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: alpha

        real(wp) :: bar_width
        real(wp), allocatable :: bar_bottom(:)
        real(wp) :: edge_rgb(3)
        logical :: has_edge

        call ensure_fig_init()

        bar_width = 0.8_wp
        if (present(width)) bar_width = width

        call resolve_bar_bottom(size(x), bottom, bar_bottom, 'bar')
        if (.not. allocated(bar_bottom)) return

        call resolve_color_string_or_rgb(color_str=edgecolor, context='bar', &
                                         rgb_out=edge_rgb, has_color=has_edge)

        if (has_edge) then
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label, color=color, edgecolor=edge_rgb, alpha=alpha)
        else
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label, color=color, alpha=alpha)
        end if
    end subroutine bar_rgb_edgecolor

    subroutine barh_rgb_edgecolor(y, width, color, edgecolor, height, left, label, align, alpha)
        !! Barh with RGB-triple color and named-color edgecolor
        real(wp), contiguous, intent(in) :: y(:), width(:)
        real(wp), intent(in) :: color(3)
        character(len=*), intent(in) :: edgecolor
        real(wp), intent(in), optional :: height
        real(wp), intent(in), optional :: left(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: alpha

        real(wp) :: bar_height
        real(wp), allocatable :: bar_left(:)
        real(wp) :: edge_rgb(3)
        logical :: has_edge

        call ensure_fig_init()

        bar_height = 0.8_wp
        if (present(height)) bar_height = height

        call resolve_bar_bottom(size(y), left, bar_left, 'barh')
        if (.not. allocated(bar_left)) return

        call resolve_color_string_or_rgb(color_str=edgecolor, context='barh', &
                                         rgb_out=edge_rgb, has_color=has_edge)

        if (has_edge) then
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label, color=color, edgecolor=edge_rgb, alpha=alpha)
        else
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label, color=color, alpha=alpha)
        end if
    end subroutine barh_rgb_edgecolor

    subroutine bar_rgb_array(x, height, color_per_bar, edgecolor_per_bar, width, bottom, label, align, alpha)
        !! Bar with per-bar RGB color arrays
        real(wp), contiguous, intent(in) :: x(:), height(:)
        real(wp), intent(in), optional :: color_per_bar(3, *)
        real(wp), intent(in), optional :: edgecolor_per_bar(3, *)
        real(wp), intent(in), optional :: width
        real(wp), intent(in), optional :: bottom(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: alpha

        real(wp) :: bar_width
        real(wp), allocatable :: bar_bottom(:)

        call ensure_fig_init()

        bar_width = 0.8_wp
        if (present(width)) bar_width = width

        call resolve_bar_bottom(size(x), bottom, bar_bottom, 'bar')
        if (.not. allocated(bar_bottom)) return

        if (present(edgecolor_per_bar)) then
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label, color_per_bar=color_per_bar, &
                          edgecolor_per_bar=edgecolor_per_bar, alpha=alpha)
        else
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label, color_per_bar=color_per_bar, alpha=alpha)
        end if
    end subroutine bar_rgb_array

    subroutine barh_rgb_array(y, width, color_per_bar, edgecolor_per_bar, height, left, label, align, alpha)
        !! Barh with per-bar RGB color arrays
        real(wp), contiguous, intent(in) :: y(:), width(:)
        real(wp), intent(in), optional :: color_per_bar(3, *)
        real(wp), intent(in), optional :: edgecolor_per_bar(3, *)
        real(wp), intent(in), optional :: height
        real(wp), intent(in), optional :: left(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: alpha

        real(wp) :: bar_height
        real(wp), allocatable :: bar_left(:)

        call ensure_fig_init()

        bar_height = 0.8_wp
        if (present(height)) bar_height = height

        call resolve_bar_bottom(size(y), left, bar_left, 'barh')
        if (.not. allocated(bar_left)) return

        if (present(edgecolor_per_bar)) then
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label, color_per_bar=color_per_bar, &
                           edgecolor_per_bar=edgecolor_per_bar, alpha=alpha)
        else
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label, color_per_bar=color_per_bar, alpha=alpha)
        end if
    end subroutine barh_rgb_array

    subroutine resolve_bar_bottom(n, bottom, bar_bottom, context)
        integer, intent(in) :: n
        real(wp), intent(in), optional :: bottom(:)
        real(wp), allocatable, intent(out) :: bar_bottom(:)
        character(len=*), intent(in) :: context

        allocate (bar_bottom(n))
        bar_bottom = 0.0_wp
        if (present(bottom)) then
            if (size(bottom) == n) then
                bar_bottom = bottom
            else if (size(bottom) == 1) then
                bar_bottom = bottom(1)
            else
                call log_error(context // ": bottom/left size must match or be scalar")
            end if
        end if
    end subroutine resolve_bar_bottom

    subroutine boxplot_string(data, position, width, label, show_outliers, horizontal, color)
        !! Boxplot with named-color string (matplotlib-compatible).
        !! Converts string color to RGB before delegating to the figure.
        real(wp), contiguous, intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers, horizontal
        character(len=*), intent(in) :: color

        real(wp) :: color_rgb(3)
        logical :: has_color

        call ensure_fig_init()
        call resolve_color_string_or_rgb(color_str=color, context='boxplot', &
                                         rgb_out=color_rgb, has_color=has_color)
        if (has_color) then
            call fig%boxplot(data, position=position, width=width, label=label, &
                             show_outliers=show_outliers, horizontal=horizontal, &
                             color=color_rgb)
        else
            call fig%boxplot(data, position=position, width=width, label=label, &
                             show_outliers=show_outliers, horizontal=horizontal)
        end if
    end subroutine boxplot_string

    subroutine boxplot_rgb(data, position, width, label, show_outliers, horizontal, color)
        !! Boxplot with RGB-triple color (matplotlib-compatible).
        real(wp), contiguous, intent(in) :: data(:)
        real(wp), intent(in), optional :: position
        real(wp), intent(in), optional :: width
        character(len=*), intent(in), optional :: label
        logical, intent(in), optional :: show_outliers, horizontal
        real(wp), intent(in), optional :: color(3)

        call ensure_fig_init()
        call fig%boxplot(data, position=position, width=width, label=label, &
                         show_outliers=show_outliers, horizontal=horizontal, &
                         color=color)
    end subroutine boxplot_rgb

    subroutine add_plot_rgb(x, y, color, label, linestyle, alpha)
        !! Add a line plot with an RGB color.
        !!
        !! Parameters
        !! x : real(wp), contiguous, intent(in)
        !!     X coordinates.
        !! y : real(wp), contiguous, intent(in)
        !!     Y coordinates.
        !! color : real(wp)(3), optional
        !!     RGB line color.
        !! label : character(len=*), optional
        !!     Legend label.
        !! linestyle : character(len=*), optional
        !!     Line style string.
        !! alpha : real(wp), optional
        !!     Line transparency.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: alpha

        call ensure_fig_init()
        call fig%add_plot(x, y, label=label, linestyle=linestyle, color=color, &
                          alpha=alpha)
    end subroutine add_plot_rgb

    subroutine add_plot_string(x, y, color, label, linestyle, alpha)
        !! Add a line plot with a named color.
        !!
        !! Parameters
        !! x : real(wp), contiguous, intent(in)
        !!     X coordinates.
        !! y : real(wp), contiguous, intent(in)
        !!     Y coordinates.
        !! color : character(len=*), intent(in)
        !!     Named or hex color.
        !! label : character(len=*), optional
        !!     Legend label.
        !! linestyle : character(len=*), optional
        !!     Line style string.
        !! alpha : real(wp), optional
        !!     Line transparency.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color
        character(len=*), intent(in), optional :: label, linestyle
        real(wp), intent(in), optional :: alpha

        real(wp) :: color_rgb(3)
        logical :: has_color

        call ensure_fig_init()
        call resolve_color_string_or_rgb(color_str=color, context='add_plot', &
                                         rgb_out=color_rgb, has_color=has_color)
        if (has_color) then
            call fig%add_plot(x, y, label=label, linestyle=linestyle, color=color_rgb, &
                              alpha=alpha)
        else
            call fig%add_plot(x, y, label=label, linestyle=linestyle, alpha=alpha)
        end if
    end subroutine add_plot_string

    subroutine add_3d_plot_rgb(x, y, z, label, linestyle, color, linewidth, marker, &
                                 markersize)
        !! 3D plot wrapper with RGB-triple color (matplotlib-compatible).
        real(wp), contiguous, intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label, linestyle, marker
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidth, markersize

        call ensure_fig_init()
        call add_3d_plot_impl(fig, x, y, z, label=label, linestyle=linestyle, &
                              marker=marker, markersize=markersize, linewidth=linewidth, &
                              color=color)
    end subroutine add_3d_plot_rgb

    subroutine add_3d_plot_string(x, y, z, label, linestyle, color, linewidth, marker, &
                                   markersize)
        !! 3D plot wrapper with named-color string (matplotlib-compatible).
        !! Converts string color to RGB before delegating to the figure.
        real(wp), contiguous, intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in), optional :: label, linestyle, marker
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: linewidth, markersize

        real(wp) :: color_rgb(3)
        logical :: has_color

        call ensure_fig_init()
        call resolve_color_string_or_rgb(color_str=color, context='add_3d_plot', &
                                         rgb_out=color_rgb, has_color=has_color)
        if (has_color) then
            call add_3d_plot_impl(fig, x, y, z, label=label, linestyle=linestyle, &
                                  marker=marker, markersize=markersize, linewidth=linewidth, &
                                  color=color_rgb)
        else
            call add_3d_plot_impl(fig, x, y, z, label=label, linestyle=linestyle, &
                                  marker=marker, markersize=markersize, linewidth=linewidth)
        end if
    end subroutine add_3d_plot_string

    subroutine apply_line_style_overrides(linewidth, marker, markersize)
        real(wp), intent(in), optional :: linewidth, markersize
        character(len=*), intent(in), optional :: marker
        integer :: idx

        if (.not. allocated(fig%plots)) return
        idx = fig%plot_count
        if (idx < 1 .or. idx > size(fig%plots)) return

        if (present(linewidth)) fig%plots(idx)%line_width = linewidth
        if (present(marker)) fig%plots(idx)%marker = marker
        if (present(markersize)) fig%plots(idx)%scatter_size_default = markersize
    end subroutine apply_line_style_overrides

end module fortplot_matplotlib_plot_wrappers
