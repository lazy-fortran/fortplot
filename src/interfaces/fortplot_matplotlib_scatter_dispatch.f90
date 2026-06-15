module fortplot_matplotlib_scatter_dispatch
    !! Internal dispatch layer for matplotlib-compatible scatter plots.
    !!
    !! Provides 2D and 3D dispatch procedures that prepare data arrays,
    !! resolve sizes/linewidths/edgecolors, and call the core scatter
    !! library (add_scatter_2d / add_scatter_3d).
    !!
    !! This module is internal to the scatter subsystem; consumers should
    !! use the public `scatter` and `add_scatter` interfaces instead.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_color_utils, only: resolve_color_string_or_rgb
    use fortplot_matplotlib_scatter_utils, only: build_scatter_size_array, &
                                                 effective_linewidth, &
                                                 optional_logical, &
                                                 uniform_edgecolor, &
                                                 store_scatter_style_arrays, &
                                                 edgecolors_are_none
    use fortplot_matplotlib_session, only: ensure_fig_init
    use fortplot_scatter_plots, only: add_scatter_2d, add_scatter_3d

    implicit none
    private

    public :: scatter_2d_dispatch
    public :: scatter_3d_dispatch
    public :: scatter_3d_string_dispatch

contains

    subroutine scatter_3d_string_dispatch(x, y, z, color_rgb, has_color, s, &
                                          s_scalar, c, label, marker, &
                                          linewidths, linewidths_scalar, &
                                          edgecolors, alpha, cmap, vmin, vmax)
        real(wp), contiguous, intent(in) :: x(:), y(:), z(:)
        real(wp), intent(in) :: color_rgb(3)
        logical, intent(in) :: has_color
        real(wp), intent(in), optional :: s(:), s_scalar, c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax

        if (has_color) then
            call scatter_3d_dispatch(x, y, z, s=s, s_scalar=s_scalar, c=c, &
                                     label=label, marker=marker, &
                                     color=color_rgb, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, &
                                     cmap=cmap, vmin=vmin, vmax=vmax)
        else
            call scatter_3d_dispatch(x, y, z, s=s, s_scalar=s_scalar, c=c, &
                                     label=label, marker=marker, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, &
                                     cmap=cmap, vmin=vmin, vmax=vmax)
        end if
    end subroutine scatter_3d_string_dispatch

    subroutine scatter_2d_dispatch(x, y, s, s_scalar, c, label, marker, &
                                   color, linewidths, &
                                   linewidths_scalar, edgecolors, alpha, cmap, &
                                   vmin, vmax, edgecolors_none)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), s_scalar, c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha, vmin, vmax
        logical, intent(in), optional :: edgecolors_none

        real(wp), allocatable :: wx(:), wy(:), s_arr(:)
        real(wp) :: edge_rgb(3)
        real(wp) :: lw_effective
        logical :: has_uniform_edge, no_edges

        call ensure_fig_init()
        allocate (wx(size(x)), wy(size(y)))
        wx = x
        wy = y

        call build_scatter_size_array(size(x), s, s_scalar, s_arr)
        lw_effective = effective_linewidth(linewidths, linewidths_scalar)
        call uniform_edgecolor(size(x), edgecolors, edge_rgb, has_uniform_edge)
        no_edges = optional_logical(edgecolors_none) .or. &
                   edgecolors_are_none(edgecolors)

        if (has_uniform_edge) then
            call add_scatter_2d(fig, wx, wy, s=s_arr, c=c, label=label, &
                                marker=marker, color=color, colormap=cmap, &
                                vmin=vmin, vmax=vmax, alpha=alpha, &
                                edgecolor=edge_rgb, linewidth=lw_effective)
        else
            call add_scatter_2d(fig, wx, wy, s=s_arr, c=c, label=label, &
                                marker=marker, color=color, colormap=cmap, &
                                vmin=vmin, vmax=vmax, alpha=alpha, &
                                linewidth=lw_effective)
        end if
        call store_scatter_style_arrays(size(x), edgecolors, linewidths, no_edges)
        call fig%relocate_last_plot_to_subplot()
    end subroutine scatter_2d_dispatch

    subroutine scatter_3d_dispatch(x, y, z, s, s_scalar, c, label, marker, &
                                   color, linewidths, &
                                   linewidths_scalar, edgecolors, alpha, cmap, &
                                   vmin, vmax, edgecolors_none)
        real(wp), contiguous, intent(in) :: x(:), y(:), z(:)
        real(wp), intent(in), optional :: s(:), s_scalar, c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha, vmin, vmax
        logical, intent(in), optional :: edgecolors_none

        real(wp), allocatable :: wx(:), wy(:), wz(:), s_arr(:)
        real(wp) :: edge_rgb(3)
        real(wp) :: lw_effective
        logical :: has_uniform_edge, no_edges

        call ensure_fig_init()
        allocate (wx(size(x)), wy(size(y)), wz(size(z)))
        wx = x
        wy = y
        wz = z

        call build_scatter_size_array(size(x), s, s_scalar, s_arr)
        lw_effective = effective_linewidth(linewidths, linewidths_scalar)
        call uniform_edgecolor(size(x), edgecolors, edge_rgb, has_uniform_edge)
        no_edges = optional_logical(edgecolors_none) .or. &
                   edgecolors_are_none(edgecolors)

        if (has_uniform_edge) then
            call add_scatter_3d(fig, wx, wy, wz, s=s_arr, c=c, label=label, &
                                marker=marker, color=color, colormap=cmap, &
                                vmin=vmin, vmax=vmax, alpha=alpha, &
                                edgecolor=edge_rgb, linewidth=lw_effective)
        else
            call add_scatter_3d(fig, wx, wy, wz, s=s_arr, c=c, label=label, &
                                marker=marker, color=color, colormap=cmap, &
                                vmin=vmin, vmax=vmax, alpha=alpha, &
                                linewidth=lw_effective)
        end if
        call store_scatter_style_arrays(size(x), edgecolors, linewidths, no_edges)
    end subroutine scatter_3d_dispatch

end module fortplot_matplotlib_scatter_dispatch
