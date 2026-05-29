module fortplot_contour_rendering
    !! Contour plot rendering module
    !!
    !! This module handles high-level contour plot rendering operations including
    !! filled contour regions, default contour level rendering, and polygon clipping.
    !! Tracing and chaining are delegated to fortplot_contour_tracing.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_colormap, only: colormap_value_to_color
    use fortplot_contour_algorithms, only: calculate_marching_squares_config, &
                                           get_contour_lines, smooth_contour_chain
    use fortplot_plot_data, only: plot_data_t
    use fortplot_contour_level_calculation, only: compute_default_contour_levels
    use fortplot_contour_tracing, only: trace_contour_level
    implicit none

    private
    public :: render_contour_plot

    !! Hairline width (points) used while stroking filled-band quads. Wide enough
    !! to bridge sub-pixel seams between neighbouring cells, narrow enough that a
    !! thin boundary sliver is not fattened into a stray arc.
    real(wp), parameter :: FILL_SEAM_LINE_WIDTH = 0.35_wp

contains

    subroutine render_contour_plot(backend, plot_data, x_min_t, x_max_t, &
                                   y_min_t, y_max_t, &
                                   xscale, yscale, symlog_threshold, width, height, &
                                   margin_left, margin_right, margin_bottom, margin_top)
        !! Render a contour plot
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        integer, intent(in) :: width, height
        real(wp), intent(in) :: margin_left, margin_right, margin_bottom, margin_top

        real(wp) :: z_min, z_max
        real(wp), dimension(3) :: level_color
        integer :: i, nlev
        real(wp) :: level

        ! Reference otherwise-unused viewport/margin parameters to keep interface stable
        associate (dxmin => x_min_t, dxmax => x_max_t, dymin => y_min_t, &
                   dymax => y_max_t)
        end associate
        associate (dxs => len_trim(xscale), dys => len_trim(yscale))
        end associate
        associate (dst => symlog_threshold, dw => width, dh => height)
        end associate
        associate (dml => margin_left, dmr => margin_right, dmb => margin_bottom, &
                   dmt => margin_top)
        end associate
        ! Get data ranges
        z_min = minval(plot_data%z_grid)
        z_max = maxval(plot_data%z_grid)

        ! grid sizes available via plot_data if needed

        ! If colored contours requested and fill enabled, render filled regions.
        if (plot_data%use_color_levels .and. plot_data%fill_contours) then
            call render_filled_contour_regions(backend, plot_data, z_min, z_max, &
                                               xscale, yscale, symlog_threshold)
        end if

        ! Render contour levels (lines).
        !
        ! For filled contours (contourf), match matplotlib: do not draw contour lines
        ! unless the user overlays them via a separate contour() call.
        if (.not. plot_data%fill_contours) then
            if (allocated(plot_data%contour_levels)) then
                nlev = size(plot_data%contour_levels)
                do i = 1, nlev
                    level = plot_data%contour_levels(i)

                    if (plot_data%use_color_levels) then
                        call colormap_value_to_color(level, z_min, z_max, &
                                                     plot_data%colormap, level_color)
                        call backend%color(level_color(1), level_color(2), &
                                           level_color(3))
                    else
                        call backend%color(plot_data%color(1), plot_data%color(2), &
                                           plot_data%color(3))
                    end if

                    call trace_contour_level(backend, plot_data, level, &
                                             xscale, yscale, &
                                             symlog_threshold, x_min_t, x_max_t, &
                                             y_min_t, y_max_t)
                end do
            else
                call render_default_contour_levels(backend, plot_data, z_min, z_max, &
                                                   xscale, yscale, symlog_threshold, &
                                                   x_min_t, x_max_t, y_min_t, y_max_t)
            end if
        end if

    end subroutine render_contour_plot

    subroutine render_filled_contour_regions(backend, plot_data, z_min, z_max, &
                                             xscale, yscale, symlog_threshold)
        !! Render filled contours as nested super-level regions.
        !!
        !! Painting each band as the thin annulus [levels(k), levels(k+1)] left
        !! broken arcs on coarse grids (issue #1961): the per-cell slivers of a
        !! thin band do not tile cleanly and gaps show the band beneath. Instead
        !! we paint, from the lowest level up, the full region clamped to
        !! [levels(k), levels(nlev)] in that band's colour. Each region is a
        !! large super-level set whose boundary cells always clip to >= 3
        !! vertices, so no thin slivers and no gaps arise; a higher band simply
        !! overpaints the interior of the previous one. The visible colour in
        !! [levels(k), levels(k+1)] is the last region painted there, i.e. band
        !! k at its midpoint, matching the previous per-band colouring.
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: z_min, z_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: nx, ny, nx_z, ny_z, nx_cells, ny_cells
        integer :: k
        integer :: nlev
        real(wp), allocatable :: levels(:)
        real(wp) :: lo, hi, mid, top
        real(wp) :: color(3)
        real(wp) :: eps_z
        logical :: linear_x, linear_y

        nx = size(plot_data%x_grid)
        ny = size(plot_data%y_grid)
        ny_z = size(plot_data%z_grid, 1)
        nx_z = size(plot_data%z_grid, 2)

        nx_cells = min(nx, nx_z) - 1
        ny_cells = min(ny, ny_z) - 1
        if (nx_cells <= 0 .or. ny_cells <= 0) return

        eps_z = 1.0e-12_wp*max(1.0_wp, abs(z_max - z_min))

        ! Hoist scale check: detect linear scales once for fast-path rendering
        linear_x = (trim(xscale) == 'linear')
        linear_y = (trim(yscale) == 'linear')

        call build_fill_levels(plot_data, z_min, z_max, levels)
        nlev = size(levels)
        if (nlev < 2) return
        top = levels(nlev)

        ! Filled bands tile cell-by-cell; vector backends stroke each quad to
        ! bridge sub-pixel seams between neighbours. A data-weight stroke would
        ! fatten the thin boundary slivers where a level grazes the data into
        ! prominent arcs (issue #1961). A hairline still closes the seams while
        ! keeping the slivers at their true size.
        call backend%set_line_width(FILL_SEAM_LINE_WIDTH)

        do k = 1, nlev - 1
            lo = levels(k)
            hi = levels(k + 1)

            mid = 0.5_wp*(lo + hi)
            mid = max(z_min, min(z_max, mid))
            call colormap_value_to_color(mid, z_min, z_max, plot_data%colormap, &
                                         color)
            call backend%color(color(1), color(2), color(3))

            call fill_band_region(backend, plot_data, nx_cells, ny_cells, lo, top, &
                                  eps_z, linear_x, linear_y, xscale, yscale, &
                                  symlog_threshold)
        end do
    end subroutine render_filled_contour_regions

    subroutine fill_band_region(backend, plot_data, nx_cells, ny_cells, lo, hi, &
                                eps_z, linear_x, linear_y, xscale, yscale, &
                                symlog_threshold)
        !! Fill every grid cell's portion of the super-level region [lo, hi] with
        !! the backend's current colour. The caller paints regions from the
        !! lowest level up so higher bands overpaint the shared interior.
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        integer, intent(in) :: nx_cells, ny_cells
        real(wp), intent(in) :: lo, hi, eps_z
        logical, intent(in) :: linear_x, linear_y
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer, parameter :: MAXV = 8
        integer :: ix, iy, t, n0, n1, n2
        real(wp) :: z_min_cell, z_max_cell
        real(wp) :: xin(MAXV), yin(MAXV), zin(MAXV)
        real(wp) :: xw(MAXV), yw(MAXV), zw(MAXV)
        real(wp) :: xout(MAXV), yout(MAXV), zout(MAXV)
        real(wp) :: xq(4), yq(4)

        do iy = 1, ny_cells
            do ix = 1, nx_cells
                xin(1) = plot_data%x_grid(ix)
                yin(1) = plot_data%y_grid(iy)
                zin(1) = plot_data%z_grid(iy, ix)
                xin(2) = plot_data%x_grid(ix + 1)
                yin(2) = plot_data%y_grid(iy)
                zin(2) = plot_data%z_grid(iy, ix + 1)
                xin(3) = plot_data%x_grid(ix + 1)
                yin(3) = plot_data%y_grid(iy + 1)
                zin(3) = plot_data%z_grid(iy + 1, ix + 1)
                xin(4) = plot_data%x_grid(ix)
                yin(4) = plot_data%y_grid(iy + 1)
                zin(4) = plot_data%z_grid(iy + 1, ix)

                ! Quick rejection: skip cells outside the [lo, hi] super-level set.
                z_min_cell = min(zin(1), zin(2), zin(3), zin(4))
                z_max_cell = max(zin(1), zin(2), zin(3), zin(4))
                if (z_max_cell < lo .or. z_min_cell > hi) cycle

                n0 = 4
                call clip_poly_z_plane(n0, xin, yin, zin, lo, .true., eps_z, &
                                       n1, xw, yw, zw)
                if (n1 < 3) cycle
                call clip_poly_z_plane(n1, xw, yw, zw, hi, .false., eps_z, &
                                       n2, xout, yout, zout)
                if (n2 < 3) cycle

                do t = 2, n2 - 1
                    if (linear_x .and. linear_y) then
                        xq(1) = xout(1); yq(1) = yout(1)
                        xq(2) = xout(t); yq(2) = yout(t)
                        xq(3) = xout(t + 1); yq(3) = yout(t + 1)
                    else
                        xq(1) = apply_scale_transform(xout(1), xscale, symlog_threshold)
                        yq(1) = apply_scale_transform(yout(1), yscale, symlog_threshold)
                        xq(2) = apply_scale_transform(xout(t), xscale, symlog_threshold)
                        yq(2) = apply_scale_transform(yout(t), yscale, symlog_threshold)
                        xq(3) = apply_scale_transform(xout(t + 1), xscale, &
                                                      symlog_threshold)
                        yq(3) = apply_scale_transform(yout(t + 1), yscale, &
                                                      symlog_threshold)
                    end if
                    xq(4) = xq(3)
                    yq(4) = yq(3)
                    call backend%fill_quad(xq, yq)
                end do
            end do
        end do
    end subroutine fill_band_region

    subroutine build_fill_levels(plot_data, z_min, z_max, levels)
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: z_min, z_max
        real(wp), allocatable, intent(out) :: levels(:)

        if (allocated(plot_data%contour_levels)) then
            if (size(plot_data%contour_levels) >= 2) then
                allocate (levels(size(plot_data%contour_levels)))
                levels = plot_data%contour_levels
                call sort_levels_inplace(levels)
                return
            end if
        end if

        call compute_default_contour_levels(z_min, z_max, levels)
    end subroutine build_fill_levels

    subroutine sort_levels_inplace(levels)
        !! Sort levels using a simple O(n log^2 n) shell sort.
        !! For the small arrays typical of contour levels, this is fast enough.
        real(wp), contiguous, intent(inout) :: levels(:)
        integer :: n, gap, k, m
        real(wp) :: tmp

        n = size(levels)
        if (n <= 1) return

        ! Shell sort
        gap = n/2
        do while (gap > 0)
            do k = gap + 1, n
                tmp = levels(k)
                m = k
                do
                    if (m <= gap) exit
                    if (levels(m - gap) <= tmp) exit
                    levels(m) = levels(m - gap)
                    m = m - gap
                end do
                levels(m) = tmp
            end do
            gap = gap/2
        end do
    end subroutine sort_levels_inplace

    subroutine clip_poly_z_plane(n_in, xin, yin, zin, z_cut, keep_above, eps_z, &
                                 n_out, xout, yout, zout)
        integer, intent(in) :: n_in
        real(wp), contiguous, intent(in) :: xin(:), yin(:), zin(:)
        real(wp), intent(in) :: z_cut
        logical, intent(in) :: keep_above
        real(wp), intent(in) :: eps_z
        integer, intent(out) :: n_out
        real(wp), intent(out) :: xout(:), yout(:), zout(:)

        integer :: i, j
        logical :: in_s, in_e
        real(wp) :: xs, ys, zs
        real(wp) :: xe, ye, ze
        real(wp) :: t, denom
        real(wp) :: xi, yi

        if (n_in < 3) then
            n_out = 0
            return
        end if

        n_out = 0
        j = n_in
        xs = xin(j)
        ys = yin(j)
        zs = zin(j)
        in_s = is_inside_z(zs, z_cut, keep_above, eps_z)

        do i = 1, n_in
            xe = xin(i)
            ye = yin(i)
            ze = zin(i)
            in_e = is_inside_z(ze, z_cut, keep_above, eps_z)

            if (in_e) then
                if (.not. in_s) then
                    denom = ze - zs
                    if (abs(denom) > 0.0_wp) then
                        t = (z_cut - zs)/denom
                    else
                        t = 0.0_wp
                    end if
                    t = max(0.0_wp, min(1.0_wp, t))
                    xi = xs + t*(xe - xs)
                    yi = ys + t*(ye - ys)
                    call emit_vertex(xi, yi, z_cut, n_out, xout, yout, zout)
                end if
                call emit_vertex(xe, ye, ze, n_out, xout, yout, zout)
            else
                if (in_s) then
                    denom = ze - zs
                    if (abs(denom) > 0.0_wp) then
                        t = (z_cut - zs)/denom
                    else
                        t = 0.0_wp
                    end if
                    t = max(0.0_wp, min(1.0_wp, t))
                    xi = xs + t*(xe - xs)
                    yi = ys + t*(ye - ys)
                    call emit_vertex(xi, yi, z_cut, n_out, xout, yout, zout)
                end if
            end if

            xs = xe
            ys = ye
            zs = ze
            in_s = in_e
        end do
    contains
        pure logical function is_inside_z(z, z_cut, keep_above, eps_z)
            real(wp), intent(in) :: z, z_cut, eps_z
            logical, intent(in) :: keep_above
            if (keep_above) then
                is_inside_z = (z >= z_cut - eps_z)
            else
                is_inside_z = (z <= z_cut + eps_z)
            end if
        end function is_inside_z

        subroutine emit_vertex(x, y, z, n, xo, yo, zo)
            real(wp), intent(in) :: x, y, z
            integer, intent(inout) :: n
            real(wp), contiguous, intent(inout) :: xo(:), yo(:), zo(:)

            integer :: cap

            cap = size(xo)
            if (n >= cap) return
            n = n + 1
            xo(n) = x
            yo(n) = y
            zo(n) = z
        end subroutine emit_vertex
    end subroutine clip_poly_z_plane

    subroutine render_default_contour_levels(backend, plot_data, z_min, z_max, &
                                             xscale, yscale, symlog_threshold, &
                                             x_min_t, x_max_t, y_min_t, y_max_t)
        !! Render default contour levels
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: z_min, z_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t

        real(wp), dimension(3) :: level_color
        real(wp), allocatable :: level_values(:)
        integer :: i

        call build_fill_levels(plot_data, z_min, z_max, level_values)
        do i = 1, size(level_values)
            if (plot_data%use_color_levels) then
                call colormap_value_to_color(level_values(i), z_min, z_max, &
                                             plot_data%colormap, level_color)
                call backend%color(level_color(1), level_color(2), level_color(3))
            end if

            call trace_contour_level(backend, plot_data, level_values(i), &
                                     xscale, yscale, symlog_threshold, &
                                     x_min_t, x_max_t, y_min_t, y_max_t)
        end do

    end subroutine render_default_contour_levels

end module fortplot_contour_rendering
