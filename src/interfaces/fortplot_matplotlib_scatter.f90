module fortplot_matplotlib_scatter
    !! Scatter plot wrappers (matplotlib-compatible).
    !!
    !! Extracted from fortplot_matplotlib_plot_wrappers to respect module
    !! size limits. Re-exported by that module for backward compatibility.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_color_utils, only: resolve_color_string_or_rgb
    use fortplot_matplotlib_session, only: ensure_fig_init
    use fortplot_scatter_plots, only: add_scatter_2d, add_scatter_3d

    implicit none
    private

    public :: scatter
    public :: add_scatter

    interface scatter
        module procedure scatter_rgb
        module procedure scatter_rgb_scalar_s
        module procedure scatter_string
        module procedure scatter_string_scalar_s
    end interface scatter

    interface add_scatter
        module procedure add_scatter_2d_rgb
        module procedure add_scatter_2d_string
        module procedure add_scatter_3d_rgb
        module procedure add_scatter_3d_string
    end interface add_scatter

contains

    subroutine scatter_rgb(x, y, s, c, label, marker, markersize, color, &
                           linewidths, edgecolors, alpha, cmap, vmin, vmax, &
                           linewidths_scalar)
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

end module fortplot_matplotlib_scatter
