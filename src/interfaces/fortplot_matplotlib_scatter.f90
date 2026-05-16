module fortplot_matplotlib_scatter
    !! Scatter plot wrappers (matplotlib-compatible).
    !!
    !! Extracted from fortplot_matplotlib_plot_wrappers to respect module
    !! size limits. Re-exported by that module for backward compatibility.
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
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
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
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
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
        real(wp), intent(in), optional :: linewidths(..)
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: s(:)
        real(wp), intent(in), optional :: linewidths_scalar
        real(wp), intent(in), optional :: vmin, vmax

        real(wp) :: color_rgb(3)
        logical :: has_color

        call resolve_color_string_or_rgb(color_str=color, context='scatter', &
                                         rgb_out=color_rgb, has_color=has_color)

        if (has_color) then
            call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                     marker=marker, markersize=markersize, &
                                     color=color_rgb, linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        else
            call scatter_2d_dispatch(x, y, s=s, c=c, label=label, &
                                     marker=marker, markersize=markersize, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
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
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax

        real(wp) :: color_rgb(3)
        logical :: has_color

        call resolve_color_string_or_rgb(color_str=color, context='scatter', &
                                         rgb_out=color_rgb, has_color=has_color)

        if (has_color) then
            call scatter_2d_dispatch(x, y, s_scalar=s, c=c, label=label, &
                                     marker=marker, color=color_rgb, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        else
            call scatter_2d_dispatch(x, y, s_scalar=s, c=c, label=label, &
                                     marker=marker, linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
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
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
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
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
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
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
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
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: alpha
        real(wp), intent(in), optional :: vmin, vmax

        real(wp) :: color_rgb(3)
        logical :: has_color

        call resolve_color_string_or_rgb(color_str=color, context='scatter', &
                                         rgb_out=color_rgb, has_color=has_color)

        if (has_color) then
            call scatter_3d_dispatch(x, y, z, s=s, c=c, label=label, &
                                     marker=marker, markersize=markersize, &
                                     color=color_rgb, linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        else
            call scatter_3d_dispatch(x, y, z, s=s, c=c, label=label, &
                                     marker=marker, markersize=markersize, &
                                     linewidths=linewidths, &
                                     linewidths_scalar=linewidths_scalar, &
                                     edgecolors=edgecolors, alpha=alpha, cmap=cmap, &
                                     vmin=vmin, vmax=vmax)
        end if
    end subroutine add_scatter_3d_string

    subroutine scatter_2d_dispatch(x, y, s, s_scalar, c, label, marker, &
                                    markersize, color, linewidths, &
                                    linewidths_scalar, edgecolors, alpha, cmap, &
                                    vmin, vmax, edgecolors_none)
        real(wp), intent(in) :: x(:), y(:)
        real(wp), intent(in), optional :: s(:), s_scalar, c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
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

        call build_scatter_size_array(size(x), s, s_scalar, markersize, s_arr)
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
    end subroutine scatter_2d_dispatch

    subroutine scatter_3d_dispatch(x, y, z, s, s_scalar, c, label, marker, &
                                    markersize, color, linewidths, &
                                    linewidths_scalar, edgecolors, alpha, cmap, &
                                    vmin, vmax, edgecolors_none)
        real(wp), intent(in) :: x(:), y(:), z(:)
        real(wp), intent(in), optional :: s(:), s_scalar, c(:)
        character(len=*), intent(in), optional :: label, marker, cmap
        real(wp), intent(in), optional :: markersize
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

        call build_scatter_size_array(size(x), s, s_scalar, markersize, s_arr)
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

    subroutine build_scatter_size_array(n, s, s_scalar, markersize, s_out)
        !! Build a uniform or per-point size array for scatter markers.
        !!
        !! Priority: `s(:)` array > `s_scalar` scalar > `markersize` scalar.
        !! If none is supplied, the core scatter default remains in effect.
        !! `markersize` is accepted as a backward-compatible alias for `s`.
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
            return
        end if

    end subroutine build_scatter_size_array

    function effective_linewidth(linewidths, linewidths_scalar) result(lw)
        real(wp), intent(in), optional :: linewidths(..)
        real(wp), intent(in), optional :: linewidths_scalar
        real(wp) :: lw

        lw = 1.0_wp
        if (present(linewidths_scalar)) then
            lw = linewidths_scalar
            return
        end if
        if (present(linewidths)) then
            select rank (linewidths)
            rank (0)
                lw = linewidths
            rank (1)
                if (size(linewidths) > 0) lw = linewidths(1)
            rank default
                call log_error('scatter: linewidths must be scalar or rank-1')
            end select
        end if
    end function effective_linewidth

    logical function optional_logical(value)
        logical, intent(in), optional :: value

        optional_logical = .false.
        if (present(value)) optional_logical = value
    end function optional_logical

    logical function is_none_color(value)
        character(len=*), intent(in), optional :: value

        is_none_color = .false.
        if (.not. present(value)) return
        select case (trim(adjustl(value)))
        case ('none', 'None', 'NONE')
            is_none_color = .true.
        end select
    end function is_none_color

    subroutine uniform_edgecolor(n, edgecolors, edge_rgb, has_uniform_edge)
        integer, intent(in) :: n
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(out) :: edge_rgb(3)
        logical, intent(out) :: has_uniform_edge

        logical :: parsed

        edge_rgb = [0.0_wp, 0.0_wp, 0.0_wp]
        has_uniform_edge = .false.
        if (.not. present(edgecolors)) return

        select rank (edgecolors)
        rank (0)
            select type (edgecolors)
            type is (character(len=*))
                if (is_none_color(edgecolors)) return
                call resolve_color_string_or_rgb(color_str=edgecolors, &
                                                 context='scatter', &
                                                 rgb_out=edge_rgb, &
                                                 has_color=parsed)
                has_uniform_edge = parsed
            class default
                call log_error('scatter: edgecolors scalar must be a color string')
            end select
        rank (1)
            select type (edgecolors)
            type is (real(wp))
                if (size(edgecolors) == 3) then
                    edge_rgb = edgecolors
                    has_uniform_edge = .true.
                else if (size(edgecolors) /= 3*n) then
                    call log_error('scatter: edgecolors must be an RGB triple ' // &
                                   'or 3*n sequence')
                end if
            class default
                call log_error('scatter: edgecolors sequence must contain ' // &
                               'real RGB values')
            end select
        rank default
            call log_error('scatter: edgecolors must be a string, RGB triple, ' // &
                           'or 3*n sequence')
        end select
    end subroutine uniform_edgecolor

    subroutine store_scatter_style_arrays(n, edgecolors, linewidths, no_edges)
        integer, intent(in) :: n
        class(*), intent(in), optional :: edgecolors(..)
        real(wp), intent(in), optional :: linewidths(..)
        logical, intent(in) :: no_edges

        integer :: plot_idx

        if (.not. allocated(fig%plots)) return
        plot_idx = fig%plot_count
        if (plot_idx < 1 .or. plot_idx > size(fig%plots)) return

        if (no_edges) then
            fig%plots(plot_idx)%marker_edge_alpha = 0.0_wp
        end if

        call store_edgecolor_sequence(n, edgecolors, plot_idx)

        call store_linewidths(n, linewidths, plot_idx)
    end subroutine store_scatter_style_arrays

    subroutine store_linewidths(n, linewidths, plot_idx)
        integer, intent(in) :: n, plot_idx
        real(wp), intent(in), optional :: linewidths(..)

        if (.not. present(linewidths)) return
        select rank (linewidths)
        rank (0)
            fig%plots(plot_idx)%marker_linewidth = max(0.0_wp, linewidths)
        rank (1)
            if (size(linewidths) == n) then
                allocate (fig%plots(plot_idx)%scatter_linewidths(n))
                fig%plots(plot_idx)%scatter_linewidths = linewidths
            else if (size(linewidths) == 1) then
                fig%plots(plot_idx)%marker_linewidth = max(0.0_wp, linewidths(1))
            else
                call log_error('scatter: linewidths length must match data or be 1')
            end if
        rank default
            call log_error('scatter: linewidths must be scalar or rank-1')
        end select
    end subroutine store_linewidths

    logical function edgecolors_are_none(edgecolors)
        class(*), intent(in), optional :: edgecolors(..)

        edgecolors_are_none = .false.
        if (.not. present(edgecolors)) return
        select rank (edgecolors)
        rank (0)
            select type (edgecolors)
            type is (character(len=*))
                edgecolors_are_none = is_none_color(edgecolors)
            end select
        end select
    end function edgecolors_are_none

    subroutine store_edgecolor_sequence(n, edgecolors, plot_idx)
        integer, intent(in) :: n, plot_idx
        class(*), intent(in), optional :: edgecolors(..)

        integer :: i

        if (.not. present(edgecolors)) return
        select rank (edgecolors)
        rank (1)
            select type (edgecolors)
            type is (real(wp))
                if (size(edgecolors) == 3*n .and. n > 1) then
                    allocate (fig%plots(plot_idx)%scatter_edgecolors(3, n))
                    do i = 1, n
                        fig%plots(plot_idx)%scatter_edgecolors(:, i) = &
                            edgecolors(3*i - 2:3*i)
                    end do
                end if
            end select
        end select
    end subroutine store_edgecolor_sequence

end module fortplot_matplotlib_scatter
