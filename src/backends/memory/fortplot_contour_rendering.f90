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
        !! Render filled contours by clipping each grid cell into level bands.
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: z_min, z_max
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: nx, ny, nx_z, ny_z, nx_cells, ny_cells
        integer :: ix, iy, k, t
        integer :: nlev
        real(wp), allocatable :: levels(:)
        real(wp) :: lo, hi, mid
        real(wp) :: color(3)
        real(wp) :: eps_z

        real(wp) :: x1, x2, x3, x4
        real(wp) :: y1, y2, y3, y4
        real(wp) :: z1, z2, z3, z4
        real(wp) :: z_min_cell, z_max_cell

        integer, parameter :: MAXV = 8
        integer :: n0, n1, n2
        real(wp) :: xin(MAXV), yin(MAXV), zin(MAXV)
        real(wp) :: xw(MAXV), yw(MAXV), zw(MAXV)
        real(wp) :: xout(MAXV), yout(MAXV), zout(MAXV)
        real(wp) :: xq(4), yq(4)

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

        do k = 1, nlev - 1
            lo = levels(k)
            hi = levels(k + 1)

            mid = 0.5_wp*(lo + hi)
            mid = max(z_min, min(z_max, mid))
            call colormap_value_to_color(mid, z_min, z_max, plot_data%colormap, &
                                         color)
            call backend%color(color(1), color(2), color(3))

            do iy = 1, ny_cells
                do ix = 1, nx_cells
                    x1 = plot_data%x_grid(ix)
                    y1 = plot_data%y_grid(iy)
                    z1 = plot_data%z_grid(iy, ix)

                    x2 = plot_data%x_grid(ix + 1)
                    y2 = plot_data%y_grid(iy)
                    z2 = plot_data%z_grid(iy, ix + 1)

                    x3 = plot_data%x_grid(ix + 1)
                    y3 = plot_data%y_grid(iy + 1)
                    z3 = plot_data%z_grid(iy + 1, ix + 1)

                    x4 = plot_data%x_grid(ix)
                    y4 = plot_data%y_grid(iy + 1)
                    z4 = plot_data%z_grid(iy + 1, ix)

                    ! Quick rejection: skip cells whose z-range does not
                    ! intersect the [lo, hi] band.
                    z_min_cell = min(z1, z2, z3, z4)
                    z_max_cell = max(z1, z2, z3, z4)
                    if (z_max_cell < lo .or. z_min_cell > hi) cycle

                    xin(1) = x1;  xin(2) = x2;  xin(3) = x3;  xin(4) = x4
                    yin(1) = y1;  yin(2) = y2;  yin(3) = y3;  yin(4) = y4
                    zin(1) = z1;  zin(2) = z2;  zin(3) = z3;  zin(4) = z4
                    n0 = 4

                    call clip_poly_z_plane(n0, xin, yin, zin, lo, .true., eps_z, &
                                           n1, xw, yw, zw)
                    if (n1 < 3) cycle

                    call clip_poly_z_plane(n1, xw, yw, zw, hi, .false., eps_z, &
                                           n2, xout, yout, zout)
                    if (n2 < 3) cycle

                    if (linear_x .and. linear_y) then
                        ! Fast path: no scale transform needed
                        do t = 2, n2 - 1
                            xq(1) = xout(1)
                            yq(1) = yout(1)
                            xq(2) = xout(t)
                            yq(2) = yout(t)
                            xq(3) = xout(t + 1)
                            yq(3) = yout(t + 1)
                            xq(4) = xq(3)
                            yq(4) = yq(3)
                            call backend%fill_quad(xq, yq)
                        end do
                    else
                        ! General path: apply scale transforms
                        do t = 2, n2 - 1
                            xq(1) = apply_scale_transform(xout(1), xscale, &
                                                          symlog_threshold)
                            yq(1) = apply_scale_transform(yout(1), yscale, &
                                                          symlog_threshold)
                            xq(2) = apply_scale_transform(xout(t), xscale, &
                                                          symlog_threshold)
                            yq(2) = apply_scale_transform(yout(t), yscale, &
                                                          symlog_threshold)
                            xq(3) = apply_scale_transform(xout(t + 1), xscale, &
                                                          symlog_threshold)
                            yq(3) = apply_scale_transform(yout(t + 1), yscale, &
                                                          symlog_threshold)
                            xq(4) = xq(3)
                            yq(4) = yq(3)
                            call backend%fill_quad(xq, yq)
                        end do
                    end if
                end do
            end do
        end do
    end subroutine render_filled_contour_regions

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
