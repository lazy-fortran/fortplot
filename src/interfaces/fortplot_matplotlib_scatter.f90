module fortplot_matplotlib_scatter
    !! Scatter plot wrappers (matplotlib-compatible).
    !!
    !! Public API: generic interfaces `scatter` and `add_scatter` that accept
    !! either RGB-triple or named-color string arguments. Dispatches to the
    !! core scatter library through internal dispatch and utility modules.
    !!
    !! Size arguments:
    !!   s          - primary parameter; scalar or per-point sizes in points^2.
    !!   markersize - documented alias for s; used only when s is absent.
    !!                When both are provided, s takes priority.
    !!
    !! Color arguments:
    !!   c      - real array of scalar values mapped through cmap (colormap).
    !!            Matches matplotlib's c parameter for data-driven coloring.
    !!   color  - literal RGB triple (or color name string) applied uniformly
    !!            to all points. This is a fortplot extension; matplotlib uses
    !!            c for both scalar mapping and literal color via kwargs.
    !!            If both c and color are provided, c takes precedence for
    !!            per-point coloring and color is ignored.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_global, only: fig => global_figure
    use fortplot_logging, only: log_error
    use fortplot_matplotlib_color_utils, only: resolve_color_string_or_rgb, &
                                               resolve_sequence_to_rgb
    use fortplot_matplotlib_scatter_dispatch, only: scatter_2d_dispatch, &
                                                    scatter_3d_dispatch, &
                                                    scatter_3d_string_dispatch
    use fortplot_matplotlib_scatter_utils, only: build_scatter_size_array, &
                                                 effective_linewidth, &
                                                 optional_logical, &
                                                 uniform_edgecolor, &
                                                 store_scatter_style_arrays
    use fortplot_scatter_plots, only: add_scatter_2d, add_scatter_3d

    implicit none
    private

    public :: scatter
    public :: add_scatter

    interface scatter
        module procedure scatter_rgb
        module procedure scatter_string
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
        !! Unified scatter with RGB color; accepts s as scalar or array
        !! via deferred-shape `s(..)` so interface resolution works
        !! correctly regardless of whether s is scalar or rank-1.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(..)
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha, vmin, vmax
        logical :: has_s_scalar

        ! markersize is a documented alias for s; s takes priority.
        ! Convert markersize -> s_scalar early so the rest of the body
        ! uses a single code path (no dual-interpretation branching).
        if (.not. present(s) .and. present(markersize)) then
            call scatter_2d_dispatch(x, y, s_scalar=markersize, c=c, &
                                     label=label, marker=marker, color=color, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
            return
        end if
        if (.not. present(s)) then
            call scatter_2d_dispatch(x, y, c=c, label=label, marker=marker, &
                                     color=color, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
            return
        end if
        has_s_scalar = .true.
        select rank (s)
        rank (0)
            call scatter_2d_dispatch(x, y, s_scalar=s, c=c, label=label, &
                                     marker=marker, color=color, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        rank (1)
            call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                     marker=marker, color=color, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        rank default
            call log_error('scatter: s must be scalar or rank-1')
        end select
    end subroutine scatter_rgb

    subroutine scatter_string(x, y, c, label, marker, markersize, color, &
                               linewidths, edgecolors, alpha, s, &
                               linewidths_scalar, cmap, vmin, vmax)
        !! Unified scatter with string color; accepts s as scalar or array
        !! via deferred-shape `s(..)` so interface resolution works
        !! correctly regardless of whether s is scalar or rank-1.
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: linewidths(..)
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: s(..)
        real(wp), intent(in), optional :: linewidths_scalar
        real(wp), intent(in), optional :: vmin, vmax

        real(wp) :: color_rgb(3)
        logical :: has_color

        call resolve_color_string_or_rgb(color_str=color, context='scatter', &
                                         rgb_out=color_rgb, has_color=has_color)

        ! markersize is a documented alias for s; s takes priority.
        if (.not. present(s) .and. present(markersize)) then
            if (has_color) then
                call scatter_2d_dispatch(x, y, s_scalar=markersize, c=c, &
                                         label=label, marker=marker, &
                                         color=color_rgb, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                         vmin=vmin, vmax=vmax)
            else
                call scatter_2d_dispatch(x, y, s_scalar=markersize, c=c, &
                                         label=label, marker=marker, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                         vmin=vmin, vmax=vmax)
            end if
            return
        end if
        if (.not. present(s)) then
            if (has_color) then
                call scatter_2d_dispatch(x, y, c=c, label=label, marker=marker, &
                                         color=color_rgb, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                         vmin=vmin, vmax=vmax)
            else
                call scatter_2d_dispatch(x, y, c=c, label=label, marker=marker, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                         vmin=vmin, vmax=vmax)
            end if
            return
        end if
        select rank (s)
        rank (0)
            if (has_color) then
                call scatter_2d_dispatch(x, y, s_scalar=s, c=c, label=label, &
                                         marker=marker, color=color_rgb, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                         vmin=vmin, vmax=vmax)
            else
                call scatter_2d_dispatch(x, y, s_scalar=s, c=c, label=label, &
                                         marker=marker, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                         vmin=vmin, vmax=vmax)
            end if
        rank (1)
            if (has_color) then
                call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                         marker=marker, color=color_rgb, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                         vmin=vmin, vmax=vmax)
            else
                call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                         marker=marker, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                         vmin=vmin, vmax=vmax)
            end if
        rank default
            call log_error('scatter: s must be scalar or rank-1')
        end select
    end subroutine scatter_string

    subroutine add_scatter_2d_rgb(x, y, markersize, s, c, label, marker, color, &
                                  linewidths, edgecolors, alpha, cmap, vmin, &
                                  vmax, linewidths_scalar)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: s(..), c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), allocatable :: s_arr(:)
        logical :: has_s

        has_s = present(s)
        ! markersize is a documented alias for s; s takes priority.
        if (.not. has_s .and. present(markersize)) then
            allocate (s_arr(1))
            s_arr(1) = markersize
            call scatter_2d_dispatch(x, y, s=s_arr, c=c, label=label, &
                                     marker=marker, color=color, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, &
                                     cmap=cmap, vmin=vmin, vmax=vmax)
            return
        end if

        if (has_s) then
            select rank (s)
            rank (0)
                call scatter_2d_dispatch(x, y, s_scalar=s, c=c, label=label, &
                                         marker=marker, color=color, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, &
                                         cmap=cmap, vmin=vmin, vmax=vmax)
            rank (1)
                call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                         marker=marker, color=color, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, &
                                         cmap=cmap, vmin=vmin, vmax=vmax)
            rank default
                call log_error('scatter: s must be scalar or rank-1')
            end select
        else
            call scatter_2d_dispatch(x, y, c=c, label=label, marker=marker, &
                                     color=color, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        end if
    end subroutine add_scatter_2d_rgb

    subroutine add_scatter_2d_string(x, y, color, c, label, marker, markersize, &
                                     linewidths, edgecolors, alpha, s, &
                                     linewidths_scalar, cmap, vmin, vmax)
        real(wp), contiguous, intent(in) :: x(:), y(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: s(..)
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), allocatable :: s_arr(:)
        logical :: has_s

        has_s = present(s)
        ! markersize is a documented alias for s; s takes priority.
        if (.not. has_s .and. present(markersize)) then
            allocate (s_arr(1))
            s_arr(1) = markersize
            call scatter_string(x, y, c=c, label=label, marker=marker, &
                                color=color, &
                                linewidths=linewidths, edgecolors=edgecolors, &
                                alpha=alpha, s=s_arr, &
                                linewidths_scalar=linewidths_scalar, &
                                cmap=cmap, vmin=vmin, vmax=vmax)
            return
        end if

        if (has_s) then
            select rank (s)
            rank (0)
                call scatter_string(x, y, c=c, label=label, marker=marker, &
                                    color=color, &
                                    linewidths=linewidths, edgecolors=edgecolors, &
                                    alpha=alpha, s=s, &
                                    linewidths_scalar=linewidths_scalar, &
                                    cmap=cmap, vmin=vmin, vmax=vmax)
            rank (1)
                call scatter_string(x, y, c=c, label=label, marker=marker, &
                                    color=color, &
                                    linewidths=linewidths, edgecolors=edgecolors, &
                                    alpha=alpha, s=s, &
                                    linewidths_scalar=linewidths_scalar, &
                                    cmap=cmap, vmin=vmin, vmax=vmax)
            rank default
                call log_error('scatter: s must be scalar or rank-1')
            end select
        else
            call scatter_string(x, y, c=c, label=label, marker=marker, &
                                color=color, &
                                linewidths=linewidths, edgecolors=edgecolors, &
                                alpha=alpha, linewidths_scalar=linewidths_scalar, &
                                cmap=cmap, vmin=vmin, vmax=vmax)
        end if
    end subroutine add_scatter_2d_string

    subroutine add_scatter_3d_rgb(x, y, z, s, c, label, marker, markersize, &
                                  color, linewidths, edgecolors, alpha, cmap, &
                                  vmin, vmax, linewidths_scalar)
        real(wp), contiguous, intent(in) :: x(:), y(:), z(:)
        real(wp), intent(in), optional :: s(..)
        real(wp), intent(in), optional :: c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: color(3)
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), allocatable :: s_arr(:)
        logical :: has_s

        has_s = present(s)
        ! markersize is a documented alias for s; s takes priority.
        if (.not. has_s .and. present(markersize)) then
            allocate (s_arr(1))
            s_arr(1) = markersize
            call scatter_3d_dispatch(x, y, z, s=s_arr, c=c, label=label, &
                                     marker=marker, color=color, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, &
                                     cmap=cmap, vmin=vmin, vmax=vmax)
            return
        end if

        if (has_s) then
            select rank (s)
            rank (0)
                call scatter_3d_dispatch(x, y, z, s_scalar=s, c=c, &
                                         label=label, marker=marker, color=color, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, &
                                         cmap=cmap, vmin=vmin, vmax=vmax)
            rank (1)
                call scatter_3d_dispatch(x, y, z, s=s, c=c, label=label, &
                                         marker=marker, color=color, &
                                         linewidths=linewidths, &
                                         linewidths_scalar=linewidths_scalar, &
                                         edgecolors=edgecolors, alpha=alpha, &
                                         cmap=cmap, vmin=vmin, vmax=vmax)
            rank default
                call log_error('scatter: s must be scalar or rank-1')
            end select
        else
            call scatter_3d_dispatch(x, y, z, c=c, label=label, marker=marker, &
                                     color=color, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        end if
    end subroutine add_scatter_3d_rgb

    subroutine add_scatter_3d_string(x, y, z, color, s, c, label, marker, &
                                     markersize, linewidths, edgecolors, alpha, &
                                     linewidths_scalar, cmap, vmin, vmax)
        real(wp), contiguous, intent(in) :: x(:), y(:), z(:)
        character(len=*), intent(in) :: color
        real(wp), intent(in), optional :: s(..), c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax
        real(wp), allocatable :: s_arr(:)
        logical :: has_s

        real(wp) :: color_rgb(3)
        logical :: has_color

        has_s = present(s)
        ! markersize is a documented alias for s; s takes priority.
        if (.not. has_s .and. present(markersize)) then
            allocate (s_arr(1))
            s_arr(1) = markersize
            call resolve_color_string_or_rgb(color_str=color, context='scatter', &
                                             rgb_out=color_rgb, has_color=has_color)
            call scatter_3d_string_dispatch(x, y, z, color_rgb, has_color, &
                                            s=s_arr, c=c, label=label, &
                                            marker=marker, &
                                            linewidths=linewidths, &
                                            linewidths_scalar=linewidths_scalar, &
                                            edgecolors=edgecolors, alpha=alpha, &
                                            cmap=cmap, vmin=vmin, vmax=vmax)
            return
        end if

        call resolve_color_string_or_rgb(color_str=color, context='scatter', &
                                         rgb_out=color_rgb, has_color=has_color)

        if (has_s) then
            select rank (s)
            rank (0)
                call scatter_3d_string_dispatch(x, y, z, color_rgb, has_color, &
                                                s_scalar=s, c=c, label=label, &
                                                marker=marker, linewidths=linewidths, &
                                                linewidths_scalar=linewidths_scalar, &
                                                edgecolors=edgecolors, alpha=alpha, &
                                                cmap=cmap, vmin=vmin, vmax=vmax)
            rank (1)
                call scatter_3d_string_dispatch(x, y, z, color_rgb, has_color, &
                                                s=s, c=c, label=label, &
                                                marker=marker, &
                                                linewidths=linewidths, &
                                                linewidths_scalar=linewidths_scalar, &
                                                edgecolors=edgecolors, alpha=alpha, &
                                                cmap=cmap, vmin=vmin, vmax=vmax)
            rank default
                call log_error('scatter: s must be scalar or rank-1')
            end select
        else
            call scatter_3d_string_dispatch(x, y, z, color_rgb, has_color, &
                                            c=c, label=label, marker=marker, &
                                            linewidths=linewidths, &
                                            linewidths_scalar=linewidths_scalar, &
                                            edgecolors=edgecolors, alpha=alpha, &
                                            cmap=cmap, vmin=vmin, vmax=vmax)
        end if
    end subroutine add_scatter_3d_string

end module fortplot_matplotlib_scatter
