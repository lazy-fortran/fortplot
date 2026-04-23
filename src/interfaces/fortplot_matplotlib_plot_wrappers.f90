module fortplot_matplotlib_plot_wrappers
    !! Matplotlib-style plot creation wrappers built on top of fortplot figures.
    !!
    !! Each wrapper exposes a matplotlib-compatible signature. Color kwargs
    !! accept either a character string (name, hex, single-letter) or an RGB
    !! triple through generic interfaces defined below. Parameters that have
    !! no visual effect in the current backend are accepted silently so that
    !! matplotlib-style code does not generate spurious runtime warnings.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_errorbar_plots, only: errorbar_impl => errorbar
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_color_utils, only: resolve_color_string_or_rgb
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
    public :: add_plot
    public :: add_errorbar
    public :: add_scatter
    public :: add_3d_plot

    interface scatter
        !! Dispatch by color keyword (RGB vs string) and size rank (scalar
        !! vs per-point array). The scalar-`s` overload keeps the classic
        !! matplotlib call `scatter(x, y, s=20.0)` working while the rank-1
        !! overload accepts per-point sizes as required by matplotlib.
        module procedure scatter_rgb
        module procedure scatter_rgb_scalar_s
        module procedure scatter_string
        module procedure scatter_string_scalar_s
    end interface scatter

    interface add_scatter
        !! Generic dispatch between 2D and 3D scatter entry points.
        !!
        !! Dimensionality is selected positionally: the 3D procedures require
        !! a `z(:)` third argument, while the 2D procedures have `s(:)` as an
        !! optional keyword. Color typing (RGB vs string) is resolved by the
        !! `color` kwarg keyword type.
        module procedure add_scatter_2d_rgb
        module procedure add_scatter_2d_string
        module procedure add_scatter_3d_rgb
        module procedure add_scatter_3d_string
    end interface add_scatter

    interface bar
        module procedure bar_rgb
        module procedure bar_string
    end interface bar

    interface barh
        module procedure barh_rgb
        module procedure barh_string
    end interface barh



    interface errorbar
        module procedure errorbar_rgb
        module procedure errorbar_string
    end interface errorbar

    interface add_errorbar
        module procedure add_errorbar_rgb
        module procedure add_errorbar_string
    end interface add_errorbar

    interface add_plot
        module procedure add_plot_rgb
        module procedure add_plot_string
    end interface add_plot

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
            row = (idx - 1)/ncols + 1
            col = mod(idx - 1, ncols) + 1
            call fig%subplot_plot(row, col, x, y, label=label, linestyle=linestyle)
        else
            call fig%add_plot(x, y, label=label, linestyle=linestyle)
        end if
    end subroutine plot

    subroutine scatter_rgb(x, y, s, c, label, marker, markersize, color, &
                           linewidths, edgecolors, alpha, cmap, vmin, vmax, &
                           linewidths_scalar)
        !! Matplotlib-style scatter accepting RGB triples for color/edgecolors.
        !!
        !! `s` accepts a per-point size array (rank 1). Scalar sizes route
        !! through `scatter_rgb_scalar_s`. `c` carries the scalar-mapped
        !! color array used together with `cmap`, `vmin`, `vmax`.
        !! When `c` is given the literal `color` kwarg is ignored to mirror
        !! matplotlib precedence (color-map wins over solid color).
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:)
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(:)
        real(wp), intent(in), optional :: linewidths_scalar
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax

        call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                 marker=marker, markersize=markersize, color=color, &
                                 linewidths=linewidths, &
                                 linewidths_scalar=linewidths_scalar, &
                                 edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                 vmin=vmin, vmax=vmax)
    end subroutine scatter_rgb

    subroutine scatter_rgb_scalar_s(x, y, s, c, label, marker, color, &
                                    linewidths, edgecolors, alpha, cmap, &
                                    vmin, vmax, linewidths_scalar)
        !! Scalar-`s` overload. Matplotlib treats scalar `s` as broadcast
        !! marker area; we route it through `markersize` so the existing
        !! scatter implementation reuses its single-size code path.
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: s
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(:)
        real(wp), intent(in), optional :: linewidths_scalar
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax

        call scatter_2d_dispatch(x, y, s_scalar=s, c=c, label=label, &
                                 marker=marker, color=color, &
                                 linewidths=linewidths, &
                                 linewidths_scalar=linewidths_scalar, &
                                 edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                 vmin=vmin, vmax=vmax)
    end subroutine scatter_rgb_scalar_s

    subroutine scatter_string(x, y, c, label, marker, markersize, color, &
                              linewidths, edgecolors, alpha, s, &
                              linewidths_scalar, cmap, vmin, vmax)
        !! String-color variant of scatter. `color` is parsed via
        !! fortplot_colors::parse_color and accepts names, hex, and single
        !! letters. `edgecolors` is parsed the same way. Precedence between
        !! `c` and `color` matches matplotlib: `c` wins when both are given.
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: linewidths(:)
        character(len=*), intent(in), optional :: edgecolors
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: s(:)
        real(wp), intent(in), optional :: linewidths_scalar
        real(wp), intent(in), optional :: vmin, vmax

        real(wp) :: color_rgb(3), edge_rgb(3)
        logical :: has_color, has_edge

        call resolve_color_string_or_rgb(color_str=color, context='scatter', &
                                         rgb_out=color_rgb, has_color=has_color)
        call resolve_color_string_or_rgb(color_str=edgecolors, context='scatter', &
                                         rgb_out=edge_rgb, has_color=has_edge)

        if (has_color .and. has_edge) then
            call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                     marker=marker, markersize=markersize, &
                                     color=color_rgb, linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edge_rgb, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        else if (has_color) then
            call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                     marker=marker, markersize=markersize, &
                                     color=color_rgb, linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     alpha=alpha, cmap=cmap, vmin=vmin, vmax=vmax)
        else
            call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                     marker=marker, markersize=markersize, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     alpha=alpha, cmap=cmap, vmin=vmin, vmax=vmax)
        end if
    end subroutine scatter_string

    subroutine scatter_string_scalar_s(x, y, s, c, label, marker, color, &
                                       linewidths, edgecolors, alpha, cmap, &
                                       vmin, vmax, linewidths_scalar)
        !! Scalar-`s` variant with string color. Bridges the classic
        !! `scatter(x, y, s=20.0)` pyplot signature while honouring the
        !! string color kwarg expected by matplotlib.
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in) :: s
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: linewidths(:)
        real(wp), intent(in), optional :: linewidths_scalar
        character(len=*), intent(in), optional :: edgecolors
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax

        real(wp) :: color_rgb(3), edge_rgb(3)
        logical :: has_color, has_edge

        call resolve_color_string_or_rgb(color_str=color, context='scatter', &
                                         rgb_out=color_rgb, has_color=has_color)
        call resolve_color_string_or_rgb(color_str=edgecolors, context='scatter', &
                                         rgb_out=edge_rgb, has_color=has_edge)

        if (has_color .and. has_edge) then
            call scatter_2d_dispatch(x, y, s_scalar=s, c=c, label=label, &
                                     marker=marker, color=color_rgb, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edge_rgb, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        else if (has_color) then
            call scatter_2d_dispatch(x, y, s_scalar=s, c=c, label=label, &
                                     marker=marker, color=color_rgb, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     alpha=alpha, cmap=cmap, vmin=vmin, vmax=vmax)
        else
            call scatter_2d_dispatch(x, y, s_scalar=s, c=c, label=label, &
                                     marker=marker, linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     alpha=alpha, cmap=cmap, vmin=vmin, vmax=vmax)
        end if
    end subroutine scatter_string_scalar_s

    subroutine add_scatter_2d_rgb(x, y, markersize, s, c, label, marker, color, &
                                  linewidths, edgecolors, alpha, cmap, vmin, &
                                  vmax, linewidths_scalar)
        !! 2D variant with RGB color. `markersize` occupies positional slot
        !! three so it is distinguishable (rank 0) from the 3D variant's
        !! `z` (rank 1) and from the `z` dummy of the 3D signatures.
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: s(:)
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(:)
        real(wp), intent(in), optional :: linewidths_scalar
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax

        call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                 marker=marker, markersize=markersize, color=color, &
                                 linewidths=linewidths, &
                                 linewidths_scalar=linewidths_scalar, &
                                 edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                 vmin=vmin, vmax=vmax)
    end subroutine add_scatter_2d_rgb

    subroutine add_scatter_2d_string(x, y, color, c, label, marker, markersize, &
                                     linewidths, edgecolors, alpha, s, &
                                     linewidths_scalar, cmap, vmin, vmax)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: linewidths(:)
        real(wp), intent(in), optional :: linewidths_scalar
        character(len=*), intent(in), optional :: edgecolors
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: s(:)
        real(wp), intent(in), optional :: vmin, vmax

        call scatter_string(x, y, c=c, label=label, marker=marker, &
                            markersize=markersize, color=color, &
                            linewidths=linewidths, edgecolors=edgecolors, &
                            alpha=alpha, s=s, &
                            linewidths_scalar=linewidths_scalar, cmap=cmap, &
                            vmin=vmin, vmax=vmax)
    end subroutine add_scatter_2d_string

    subroutine add_scatter_3d_rgb(x, y, z, s, c, label, marker, markersize, &
                                  color, linewidths, edgecolors, alpha, cmap, &
                                  vmin, vmax, linewidths_scalar)
        real(wp), intent(in) :: x(:), y(:), z(:)
        real(wp), intent(in), optional :: s(:)
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(:)
        real(wp), intent(in), optional :: linewidths_scalar
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax

        call scatter_3d_dispatch(x, y, z, s=s, c=c, &
                                 label=label, marker=marker, markersize=markersize, &
                                 color=color, linewidths=linewidths, &
                                 linewidths_scalar=linewidths_scalar, &
                                 edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                 vmin=vmin, vmax=vmax)
    end subroutine add_scatter_3d_rgb

    subroutine add_scatter_3d_string(x, y, z, color, s, c, label, marker, &
                                     markersize, linewidths, edgecolors, alpha, &
                                     linewidths_scalar, cmap, vmin, vmax)
        real(wp), intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: s(:)
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: linewidths(:)
        real(wp), intent(in), optional :: linewidths_scalar
        character(len=*), intent(in), optional :: edgecolors
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax

        real(wp) :: color_rgb(3), edge_rgb(3)
        logical :: has_color, has_edge

        call resolve_color_string_or_rgb(color_str=color, context='scatter', &
                                         rgb_out=color_rgb, has_color=has_color)
        call resolve_color_string_or_rgb(color_str=edgecolors, context='scatter', &
                                         rgb_out=edge_rgb, has_color=has_edge)

        if (has_color .and. has_edge) then
            call scatter_3d_dispatch(x, y, z, s=s, c=c, &
                                     label=label, marker=marker, markersize=markersize, &
                                     color=color_rgb, linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edge_rgb, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        else if (has_color) then
            call scatter_3d_dispatch(x, y, z, s=s, c=c, &
                                     label=label, marker=marker, markersize=markersize, &
                                     color=color_rgb, linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     alpha=alpha, cmap=cmap, vmin=vmin, vmax=vmax)
        else
            call scatter_3d_dispatch(x, y, z, s=s, c=c, &
                                     label=label, marker=marker, markersize=markersize, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     alpha=alpha, cmap=cmap, vmin=vmin, vmax=vmax)
        end if
    end subroutine add_scatter_3d_string

    subroutine scatter_2d_dispatch(x, y, s, s_scalar, c, label, marker, &
                                   markersize, color, linewidths, &
                                   linewidths_scalar, edgecolors, alpha, cmap, &
                                   vmin, vmax)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), s_scalar, c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(:), linewidths_scalar
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: alpha, vmin, vmax

        real(wp), allocatable :: wx(:), wy(:), s_arr(:)
        real(wp) :: lw_effective

        call ensure_fig_init()
        allocate (wx(size(x)), wy(size(y)))
        wx = x
        wy = y

        call build_scatter_size_array(size(x), s, s_scalar, markersize, s_arr)
        lw_effective = effective_linewidth(linewidths, linewidths_scalar)

        if (allocated(s_arr)) then
            call add_scatter_2d(fig, wx, wy, s=s_arr, c=c, label=label, &
                                marker=marker, color=color, colormap=cmap, &
                                vmin=vmin, vmax=vmax, alpha=alpha, &
                                edgecolor=edgecolors, linewidth=lw_effective)
        else if (present(markersize)) then
            call add_scatter_2d(fig, wx, wy, c=c, label=label, marker=marker, &
                                markersize=markersize, color=color, &
                                colormap=cmap, vmin=vmin, vmax=vmax, alpha=alpha, &
                                edgecolor=edgecolors, linewidth=lw_effective)
        else
            call add_scatter_2d(fig, wx, wy, c=c, label=label, marker=marker, &
                                color=color, colormap=cmap, vmin=vmin, vmax=vmax, &
                                alpha=alpha, edgecolor=edgecolors, &
                                linewidth=lw_effective)
        end if
    end subroutine scatter_2d_dispatch

    subroutine scatter_3d_dispatch(x, y, z, s, s_scalar, c, label, marker, &
                                   markersize, color, linewidths, &
                                   linewidths_scalar, edgecolors, alpha, cmap, &
                                   vmin, vmax)
        real(wp), intent(in) :: x(:), y(:), z(:)
        real(wp), intent(in), optional :: s(:), s_scalar, c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(:), linewidths_scalar
        real(wp), intent(in), optional :: edgecolors(3)
        real(wp), intent(in), optional :: alpha, vmin, vmax

        real(wp), allocatable :: wx(:), wy(:), wz(:), s_arr(:)
        real(wp) :: lw_effective

        call ensure_fig_init()
        allocate (wx(size(x)), wy(size(y)), wz(size(z)))
        wx = x
        wy = y
        wz = z

        call build_scatter_size_array(size(x), s, s_scalar, markersize, s_arr)
        lw_effective = effective_linewidth(linewidths, linewidths_scalar)

        if (allocated(s_arr)) then
            call add_scatter_3d(fig, wx, wy, wz, s=s_arr, c=c, label=label, &
                                marker=marker, color=color, colormap=cmap, &
                                vmin=vmin, vmax=vmax, alpha=alpha, &
                                edgecolor=edgecolors, linewidth=lw_effective)
        else if (present(markersize)) then
            call add_scatter_3d(fig, wx, wy, wz, c=c, label=label, &
                                marker=marker, markersize=markersize, &
                                color=color, colormap=cmap, vmin=vmin, vmax=vmax, &
                                alpha=alpha, edgecolor=edgecolors, &
                                linewidth=lw_effective)
        else
            call add_scatter_3d(fig, wx, wy, wz, c=c, label=label, &
                                marker=marker, color=color, colormap=cmap, &
                                vmin=vmin, vmax=vmax, alpha=alpha, &
                                edgecolor=edgecolors, linewidth=lw_effective)
        end if
    end subroutine scatter_3d_dispatch

    subroutine build_scatter_size_array(n, s, s_scalar, markersize, s_out)
        !! Resolve scatter size inputs into a per-point array.
        !!
        !! Matplotlib allows `s` to be either a scalar or an array of length
        !! n. Here we keep a single routine that broadcasts the scalar and
        !! validates the array length so the call sites stay declarative.
        integer, intent(in) :: n
        real(wp), intent(in), optional :: s(:), s_scalar, markersize
        real(wp), allocatable, intent(out) :: s_out(:)

        if (present(s)) then
            if (size(s) == n) then
                allocate (s_out(n))
                s_out = s
            else if (size(s) == 1) then
                allocate (s_out(n))
                s_out = s(1)
            else
                call log_error('scatter: s array length must match data or be 1')
            end if
            return
        end if

        if (present(s_scalar)) then
            allocate (s_out(n))
            s_out = s_scalar
            return
        end if

        if (present(markersize)) then
            allocate (s_out(n))
            s_out = markersize
        end if
    end subroutine build_scatter_size_array

    function effective_linewidth(linewidths, linewidths_scalar) result(lw)
        !! Collapse scalar/array linewidth inputs to a single scalar.
        !!
        !! Backend honours a scalar stroke width only; when an array is given
        !! the mean is used so the stroke still scales with user intent.
        real(wp), intent(in), optional :: linewidths(:), linewidths_scalar
        real(wp) :: lw

        lw = 1.0_wp
        if (present(linewidths_scalar)) then
            lw = linewidths_scalar
            return
        end if
        if (present(linewidths)) then
            if (size(linewidths) > 0) lw = sum(linewidths)/real(size(linewidths), wp)
        end if
    end function effective_linewidth

    subroutine errorbar_rgb(x, y, xerr, yerr, fmt, label, capsize, linestyle, &
                            marker, color, ecolor, elinewidth, capthick, &
                            barsabove, errorevery, lolims, uplims, xlolims, &
                            xuplims)
        !! Matplotlib-style errorbar with RGB color kwargs.
        !!
        !! `color` controls the main marker/line colour; `ecolor` controls
        !! the error-bar colour separately (matching matplotlib). Limit
        !! flags `lolims`/`uplims` flip the corresponding error side off and
        !! keep the other side visible; `errorevery` subsamples which points
        !! receive bars.
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(wp), intent(in), optional :: capsize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: ecolor(3)
        real(wp), intent(in), optional :: elinewidth, capthick
        logical, intent(in), optional :: barsabove
        integer, intent(in), optional :: errorevery
        logical, intent(in), optional :: lolims, uplims, xlolims, xuplims

        real(wp), allocatable :: xerr_use(:), yerr_use(:)

        call ensure_fig_init()
        call build_errorbar_arrays(x, y, xerr, yerr, errorevery, xlolims, &
                                   xuplims, lolims, uplims, xerr_use, yerr_use)

        call errorbar_impl(fig, x, y, xerr=xerr_use, yerr=yerr_use, label=label, &
                           capsize=capsize, marker=marker, color=color, &
                           ecolor=ecolor, elinewidth=elinewidth, capthick=capthick)
        call note_unsupported_barsabove(barsabove)
    end subroutine errorbar_rgb

    subroutine errorbar_string(x, y, color, xerr, yerr, fmt, label, capsize, &
                               linestyle, marker, ecolor, elinewidth, capthick, &
                               barsabove, errorevery, lolims, uplims, xlolims, &
                               xuplims)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(wp), intent(in), optional :: capsize
        character(len=*), intent(in), optional :: ecolor
        real(wp), intent(in), optional :: elinewidth, capthick
        logical, intent(in), optional :: barsabove
        integer, intent(in), optional :: errorevery
        logical, intent(in), optional :: lolims, uplims, xlolims, xuplims

        real(wp) :: color_rgb(3), ecolor_rgb(3)
        logical :: has_color, has_ecolor
        real(wp), allocatable :: xerr_use(:), yerr_use(:)

        call ensure_fig_init()
        call resolve_color_string_or_rgb(color_str=color, context='errorbar', &
                                         rgb_out=color_rgb, has_color=has_color)
        call resolve_color_string_or_rgb(color_str=ecolor, context='errorbar', &
                                         rgb_out=ecolor_rgb, has_color=has_ecolor)
        call build_errorbar_arrays(x, y, xerr, yerr, errorevery, xlolims, &
                                   xuplims, lolims, uplims, xerr_use, yerr_use)

        if (has_color .and. has_ecolor) then
            call errorbar_impl(fig, x, y, xerr=xerr_use, yerr=yerr_use, &
                               label=label, capsize=capsize, marker=marker, &
                               color=color_rgb, ecolor=ecolor_rgb, &
                               elinewidth=elinewidth, capthick=capthick)
        else if (has_color) then
            call errorbar_impl(fig, x, y, xerr=xerr_use, yerr=yerr_use, &
                               label=label, capsize=capsize, marker=marker, &
                               color=color_rgb, elinewidth=elinewidth, &
                               capthick=capthick)
        else if (has_ecolor) then
            call errorbar_impl(fig, x, y, xerr=xerr_use, yerr=yerr_use, &
                               label=label, capsize=capsize, marker=marker, &
                               ecolor=ecolor_rgb, elinewidth=elinewidth, &
                               capthick=capthick)
        else
            call errorbar_impl(fig, x, y, xerr=xerr_use, yerr=yerr_use, &
                               label=label, capsize=capsize, marker=marker, &
                               elinewidth=elinewidth, capthick=capthick)
        end if
        call note_unsupported_barsabove(barsabove)
    end subroutine errorbar_string

    subroutine add_errorbar_rgb(x, y, xerr, yerr, fmt, label, capsize, &
                                linestyle, marker, color, ecolor, elinewidth, &
                                capthick, barsabove, errorevery, lolims, uplims, &
                                xlolims, xuplims)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(wp), intent(in), optional :: capsize
        real(wp), intent(in), optional :: color(3), ecolor(3)
        real(wp), intent(in), optional :: elinewidth, capthick
        logical, intent(in), optional :: barsabove
        integer, intent(in), optional :: errorevery
        logical, intent(in), optional :: lolims, uplims, xlolims, xuplims

        call errorbar_rgb(x, y, xerr=xerr, yerr=yerr, fmt=fmt, label=label, &
                          capsize=capsize, linestyle=linestyle, marker=marker, &
                          color=color, ecolor=ecolor, elinewidth=elinewidth, &
                          capthick=capthick, barsabove=barsabove, &
                          errorevery=errorevery, lolims=lolims, uplims=uplims, &
                          xlolims=xlolims, xuplims=xuplims)
    end subroutine add_errorbar_rgb

    subroutine add_errorbar_string(x, y, color, xerr, yerr, fmt, label, capsize, &
                                   linestyle, marker, ecolor, elinewidth, capthick, &
                                   barsabove, errorevery, lolims, uplims, xlolims, &
                                   xuplims)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        character(len=*), intent(in), optional :: fmt, label, linestyle, marker
        real(wp), intent(in), optional :: capsize
        character(len=*), intent(in), optional :: ecolor
        real(wp), intent(in), optional :: elinewidth, capthick
        logical, intent(in), optional :: barsabove
        integer, intent(in), optional :: errorevery
        logical, intent(in), optional :: lolims, uplims, xlolims, xuplims

        call errorbar_string(x, y, color=color, xerr=xerr, yerr=yerr, fmt=fmt, &
                             label=label, capsize=capsize, linestyle=linestyle, &
                             marker=marker, ecolor=ecolor, elinewidth=elinewidth, &
                             capthick=capthick, barsabove=barsabove, &
                             errorevery=errorevery, lolims=lolims, uplims=uplims, &
                             xlolims=xlolims, xuplims=xuplims)
    end subroutine add_errorbar_string

    subroutine build_errorbar_arrays(x, y, xerr, yerr, errorevery, xlolims, &
                                     xuplims, lolims, uplims, xerr_out, yerr_out)
        !! Apply errorevery subsampling and limit-flag masking.
        !!
        !! `errorevery=n` draws a bar on every nth point; intermediate
        !! points receive a zero-magnitude bar so the main-line visuals are
        !! untouched. `lolims=.true.` hides the lower y error, `uplims=.true.`
        !! hides the upper y error (matplotlib renders a caret arrow there
        !! which we approximate by omitting the corresponding side).
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: xerr(:), yerr(:)
        integer, intent(in), optional :: errorevery
        logical, intent(in), optional :: xlolims, xuplims, lolims, uplims
        real(wp), allocatable, intent(out) :: xerr_out(:), yerr_out(:)

        integer :: i, n, stride
        logical :: hide_x, hide_y

        n = size(x)
        stride = 1
        if (present(errorevery)) stride = max(errorevery, 1)

        hide_x = limit_flag(xlolims) .or. limit_flag(xuplims)
        hide_y = limit_flag(lolims) .or. limit_flag(uplims)

        if (present(xerr) .and. .not. hide_x) then
            allocate (xerr_out(size(xerr)))
            xerr_out = xerr
            if (stride > 1 .and. size(xerr_out) == n) then
                do i = 1, n
                    if (mod(i - 1, stride) /= 0) xerr_out(i) = 0.0_wp
                end do
            end if
        end if

        if (present(yerr) .and. .not. hide_y) then
            allocate (yerr_out(size(yerr)))
            yerr_out = yerr
            if (stride > 1 .and. size(yerr_out) == n) then
                do i = 1, n
                    if (mod(i - 1, stride) /= 0) yerr_out(i) = 0.0_wp
                end do
            end if
        end if
    end subroutine build_errorbar_arrays

    pure function limit_flag(flag) result(is_set)
        logical, intent(in), optional :: flag
        logical :: is_set
        is_set = .false.
        if (present(flag)) is_set = flag
    end function limit_flag

    subroutine note_unsupported_barsabove(barsabove)
        !! Accept barsabove silently. Matplotlib paints error bars above the
        !! marker layer when enabled; fortplot currently draws both in one
        !! pass, so this is documented as a future-work flag.
        logical, intent(in), optional :: barsabove
        if (present(barsabove)) then
            ! no-op: matches matplotlib-style code without warning storms
            continue
        end if
    end subroutine note_unsupported_barsabove

    subroutine bar_rgb(x, height, width, bottom, label, color, edgecolor, align)
        real(wp), intent(in) :: x(:), height(:)
        real(wp), intent(in), optional :: width
        real(wp), intent(in), optional :: bottom(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: color(3), edgecolor(3)

        real(wp) :: bar_width
        real(wp), allocatable :: bar_bottom(:)

        call ensure_fig_init()

        bar_width = 0.8_wp
        if (present(width)) bar_width = width

        call resolve_bar_bottom(size(x), bottom, bar_bottom, 'bar')
        if (.not. allocated(bar_bottom)) return

        call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                      label=label, color=color, edgecolor=edgecolor)
    end subroutine bar_rgb

    subroutine bar_string(x, height, color, width, bottom, label, edgecolor, align)
        real(wp), intent(in) :: x(:), height(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: width
        real(wp), intent(in), optional :: bottom(:)
        character(len=*), intent(in), optional :: label, align, edgecolor

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
                          label=label, color=color_rgb, edgecolor=edge_rgb)
        else if (has_color) then
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label, color=color_rgb)
        else if (has_edge) then
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label, edgecolor=edge_rgb)
        else
            call bar_impl(fig, x, height, width=bar_width, bottom=bar_bottom, &
                          label=label)
        end if
    end subroutine bar_string

    subroutine barh_rgb(y, width, height, left, label, color, edgecolor, align)
        real(wp), intent(in) :: y(:), width(:)
        real(wp), intent(in), optional :: height
        real(wp), intent(in), optional :: left(:)
        character(len=*), intent(in), optional :: label, align
        real(wp), intent(in), optional :: color(3), edgecolor(3)

        real(wp) :: bar_height
        real(wp), allocatable :: bar_left(:)

        call ensure_fig_init()

        bar_height = 0.8_wp
        if (present(height)) bar_height = height

        call resolve_bar_bottom(size(y), left, bar_left, 'barh')
        if (.not. allocated(bar_left)) return

        call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                       label=label, color=color, edgecolor=edgecolor)
    end subroutine barh_rgb

    subroutine barh_string(y, width, color, height, left, label, edgecolor, align)
        real(wp), intent(in) :: y(:), width(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: height
        real(wp), intent(in), optional :: left(:)
        character(len=*), intent(in), optional :: label, align, edgecolor

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
                           label=label, color=color_rgb, edgecolor=edge_rgb)
        else if (has_color) then
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label, color=color_rgb)
        else if (has_edge) then
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label, edgecolor=edge_rgb)
        else
            call barh_impl(fig, y, width, height=bar_height, left=bar_left, &
                           label=label)
        end if
    end subroutine barh_string

    subroutine resolve_bar_bottom(n, bottom, bar_bottom, context)
        !! Materialise an explicit per-bar offset array from the optional
        !! `bottom` kwarg. Accepts scalar (broadcast), array of matching
        !! length, or absent (defaults to zero). Returns unallocated on
        !! validation failure so callers can abort.
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
                deallocate (bar_bottom)
            end if
        end if
    end subroutine resolve_bar_bottom


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

    subroutine add_plot_rgb(x, y, color, label, linestyle)
        !! RGB-color variant of add_plot. `color` occupies slot three so the
        !! TKR disambiguates from the string overload (character scalar).
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: color(3)
        character(len=*), intent(in), optional :: label, linestyle

        call ensure_fig_init()
        call fig%add_plot(x, y, label=label, linestyle=linestyle, color=color)
    end subroutine add_plot_rgb

    subroutine add_plot_string(x, y, color, label, linestyle)
        real(wp), intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color
        character(len=*), intent(in), optional :: label, linestyle

        real(wp) :: color_rgb(3)
        logical :: has_color

        call ensure_fig_init()
        call resolve_color_string_or_rgb(color_str=color, context='add_plot', &
                                         rgb_out=color_rgb, has_color=has_color)
        if (has_color) then
            call fig%add_plot(x, y, label=label, linestyle=linestyle, color=color_rgb)
        else
            call fig%add_plot(x, y, label=label, linestyle=linestyle)
        end if
    end subroutine add_plot_string

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

end module fortplot_matplotlib_plot_wrappers
