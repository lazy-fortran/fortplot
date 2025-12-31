module fortplot_contour_rendering
    !! Contour plot rendering module
    !!
    !! This module handles all contour plot rendering operations including
    !! contour level tracing, marching squares algorithm, and contour line drawing.
    !! Contour lines are smoothed using Catmull-Rom spline interpolation to reduce
    !! the polygonal appearance from marching squares.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_colormap, only: colormap_value_to_color
    use fortplot_contour_algorithms, only: calculate_marching_squares_config, &
                                           get_contour_lines, smooth_contour_chain
    use fortplot_plot_data, only: plot_data_t
    use fortplot_contour_level_calculation, only: compute_default_contour_levels
    implicit none

    private
    public :: render_contour_plot

    integer, parameter :: CONTOUR_SUBDIVISIONS = 4
    real(wp), parameter :: ENDPOINT_TOL = 1.0e-10_wp

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

        integer, parameter :: MAXV = 8
        integer :: n0, n1, n2
        real(wp) :: xin(MAXV), yin(MAXV), zin(MAXV)
        real(wp) :: xw(MAXV), yw(MAXV), zw(MAXV)
        real(wp) :: xout(MAXV), yout(MAXV), zout(MAXV)
        real(wp) :: xq(4), yq(4)

        nx = size(plot_data%x_grid)
        ny = size(plot_data%y_grid)
        ny_z = size(plot_data%z_grid, 1)
        nx_z = size(plot_data%z_grid, 2)

        nx_cells = min(nx, nx_z) - 1
        ny_cells = min(ny, ny_z) - 1
        if (nx_cells <= 0 .or. ny_cells <= 0) return

        eps_z = 1.0e-12_wp*max(1.0_wp, abs(z_max - z_min))

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

                    xin(1:4) = [x1, x2, x3, x4]
                    yin(1:4) = [y1, y2, y3, y4]
                    zin(1:4) = [z1, z2, z3, z4]
                    n0 = 4

                    call clip_poly_z_plane(n0, xin, yin, zin, lo, .true., eps_z, &
                                           n1, xw, yw, zw)
                    if (n1 < 3) cycle

                    call clip_poly_z_plane(n1, xw, yw, zw, hi, .false., eps_z, &
                                           n2, xout, yout, zout)
                    if (n2 < 3) cycle

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
        real(wp), intent(inout) :: levels(:)
        integer :: a, b
        real(wp) :: tmp
        do a = 1, size(levels) - 1
            do b = a + 1, size(levels)
                if (levels(b) < levels(a)) then
                    tmp = levels(a)
                    levels(a) = levels(b)
                    levels(b) = tmp
                end if
            end do
        end do
    end subroutine sort_levels_inplace

    subroutine clip_poly_z_plane(n_in, xin, yin, zin, z_cut, keep_above, eps_z, &
                                 n_out, xout, yout, zout)
        integer, intent(in) :: n_in
        real(wp), intent(in) :: xin(:), yin(:), zin(:)
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
            real(wp), intent(inout) :: xo(:), yo(:), zo(:)

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

    subroutine trace_contour_level(backend, plot_data, level, xscale, yscale, &
                                   symlog_threshold, x_min_t, x_max_t, &
                                   y_min_t, y_max_t)
        !! Trace a single contour level with smoothing
        class(plot_context), intent(inout) :: backend
        type(plot_data_t), intent(in) :: plot_data
        real(wp), intent(in) :: level
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min_t, x_max_t, y_min_t, y_max_t

        integer :: nx, ny, i, j, max_segs, n_segs
        real(wp), allocatable :: seg_x1(:), seg_y1(:), seg_x2(:), seg_y2(:)
        logical, allocatable :: seg_used(:)
        real(wp) :: x1c, y1c, x2c, y2c, x3c, y3c, x4c, y4c
        real(wp) :: z1, z2, z3, z4
        integer :: config, num_lines
        real(wp) :: line_points(8)

        associate (dxmin => x_min_t, dxmax => x_max_t, &
                   dymin => y_min_t, dymax => y_max_t)
        end associate

        nx = size(plot_data%x_grid)
        ny = size(plot_data%y_grid)
        max_segs = (nx - 1)*(ny - 1)*2
        allocate (seg_x1(max_segs), seg_y1(max_segs))
        allocate (seg_x2(max_segs), seg_y2(max_segs))
        allocate (seg_used(max_segs))
        seg_used = .false.
        n_segs = 0

        do i = 1, nx - 1
            do j = 1, ny - 1
                x1c = plot_data%x_grid(i)
                y1c = plot_data%y_grid(j)
                x2c = plot_data%x_grid(i + 1)
                y2c = plot_data%y_grid(j)
                x3c = plot_data%x_grid(i + 1)
                y3c = plot_data%y_grid(j + 1)
                x4c = plot_data%x_grid(i)
                y4c = plot_data%y_grid(j + 1)

                z1 = plot_data%z_grid(i, j)
                z2 = plot_data%z_grid(i + 1, j)
                z3 = plot_data%z_grid(i + 1, j + 1)
                z4 = plot_data%z_grid(i, j + 1)

                call calculate_marching_squares_config(z1, z2, z3, z4, &
                                                       level, config)
                call get_contour_lines(config, x1c, y1c, x2c, y2c, x3c, y3c, &
                                       x4c, y4c, z1, z2, z3, z4, level, &
                                       line_points, num_lines)

                if (num_lines >= 1) then
                    n_segs = n_segs + 1
                    seg_x1(n_segs) = line_points(1)
                    seg_y1(n_segs) = line_points(2)
                    seg_x2(n_segs) = line_points(3)
                    seg_y2(n_segs) = line_points(4)
                end if
                if (num_lines >= 2) then
                    n_segs = n_segs + 1
                    seg_x1(n_segs) = line_points(5)
                    seg_y1(n_segs) = line_points(6)
                    seg_x2(n_segs) = line_points(7)
                    seg_y2(n_segs) = line_points(8)
                end if
            end do
        end do

        call chain_and_draw_segments(backend, n_segs, seg_x1, seg_y1, seg_x2, &
                                     seg_y2, seg_used, xscale, yscale, &
                                     symlog_threshold)
    end subroutine trace_contour_level

    subroutine chain_and_draw_segments(backend, n_segs, seg_x1, seg_y1, &
                                       seg_x2, seg_y2, seg_used, &
                                       xscale, yscale, symlog_threshold)
        !! Chain segments and draw smoothed contours
        class(plot_context), intent(inout) :: backend
        integer, intent(in) :: n_segs
        real(wp), intent(in) :: seg_x1(:), seg_y1(:), seg_x2(:), seg_y2(:)
        logical, intent(inout) :: seg_used(:)
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: start_idx, chain_len, max_chain
        real(wp), allocatable :: chain_x(:), chain_y(:)
        real(wp) :: cur_x, cur_y

        if (n_segs == 0) return

        max_chain = n_segs + 1
        allocate (chain_x(max_chain), chain_y(max_chain))

        do start_idx = 1, n_segs
            if (seg_used(start_idx)) cycle

            seg_used(start_idx) = .true.
            chain_len = 2
            chain_x(1) = seg_x1(start_idx)
            chain_y(1) = seg_y1(start_idx)
            chain_x(2) = seg_x2(start_idx)
            chain_y(2) = seg_y2(start_idx)

            cur_x = chain_x(chain_len)
            cur_y = chain_y(chain_len)
            call extend_chain_forward(n_segs, seg_x1, seg_y1, seg_x2, seg_y2, &
                                      seg_used, cur_x, cur_y, chain_x, chain_y, &
                                      chain_len, max_chain)

            cur_x = chain_x(1)
            cur_y = chain_y(1)
            call extend_chain_backward(n_segs, seg_x1, seg_y1, seg_x2, seg_y2, &
                                       seg_used, cur_x, cur_y, chain_x, chain_y, &
                                       chain_len, max_chain)

            call draw_smoothed_chain(backend, chain_len, chain_x, chain_y, &
                                     xscale, yscale, symlog_threshold)
        end do
    end subroutine chain_and_draw_segments

    subroutine extend_chain_forward(n_segs, seg_x1, seg_y1, seg_x2, seg_y2, &
                                    seg_used, cur_x, cur_y, chain_x, chain_y, &
                                    chain_len, max_chain)
        !! Extend chain forward by finding connecting segments
        integer, intent(in) :: n_segs, max_chain
        real(wp), intent(in) :: seg_x1(:), seg_y1(:), seg_x2(:), seg_y2(:)
        logical, intent(inout) :: seg_used(:)
        real(wp), intent(inout) :: cur_x, cur_y
        real(wp), intent(inout) :: chain_x(:), chain_y(:)
        integer, intent(inout) :: chain_len

        integer :: k
        logical :: found
        real(wp) :: next_x, next_y

        found = .true.
        do while (found .and. chain_len < max_chain)
            found = .false.
            do k = 1, n_segs
                if (seg_used(k)) cycle
                if (points_match(cur_x, cur_y, seg_x1(k), seg_y1(k))) then
                    next_x = seg_x2(k)
                    next_y = seg_y2(k)
                else if (points_match(cur_x, cur_y, seg_x2(k), seg_y2(k))) then
                    next_x = seg_x1(k)
                    next_y = seg_y1(k)
                else
                    cycle
                end if
                seg_used(k) = .true.
                chain_len = chain_len + 1
                chain_x(chain_len) = next_x
                chain_y(chain_len) = next_y
                cur_x = next_x
                cur_y = next_y
                found = .true.
                exit
            end do
        end do
    end subroutine extend_chain_forward

    subroutine extend_chain_backward(n_segs, seg_x1, seg_y1, seg_x2, seg_y2, &
                                     seg_used, cur_x, cur_y, chain_x, chain_y, &
                                     chain_len, max_chain)
        !! Extend chain backward by prepending connecting segments
        integer, intent(in) :: n_segs, max_chain
        real(wp), intent(in) :: seg_x1(:), seg_y1(:), seg_x2(:), seg_y2(:)
        logical, intent(inout) :: seg_used(:)
        real(wp), intent(inout) :: cur_x, cur_y
        real(wp), intent(inout) :: chain_x(:), chain_y(:)
        integer, intent(inout) :: chain_len

        integer :: k, m
        logical :: found
        real(wp) :: next_x, next_y

        found = .true.
        do while (found .and. chain_len < max_chain)
            found = .false.
            do k = 1, n_segs
                if (seg_used(k)) cycle
                if (points_match(cur_x, cur_y, seg_x1(k), seg_y1(k))) then
                    next_x = seg_x2(k)
                    next_y = seg_y2(k)
                else if (points_match(cur_x, cur_y, seg_x2(k), seg_y2(k))) then
                    next_x = seg_x1(k)
                    next_y = seg_y1(k)
                else
                    cycle
                end if
                seg_used(k) = .true.
                do m = chain_len, 1, -1
                    chain_x(m + 1) = chain_x(m)
                    chain_y(m + 1) = chain_y(m)
                end do
                chain_x(1) = next_x
                chain_y(1) = next_y
                chain_len = chain_len + 1
                cur_x = next_x
                cur_y = next_y
                found = .true.
                exit
            end do
        end do
    end subroutine extend_chain_backward

    pure function points_match(x1, y1, x2, y2) result(match)
        !! Check if two points are within tolerance
        real(wp), intent(in) :: x1, y1, x2, y2
        logical :: match
        match = (abs(x1 - x2) < ENDPOINT_TOL) .and. (abs(y1 - y2) < ENDPOINT_TOL)
    end function points_match

    subroutine draw_smoothed_chain(backend, n_pts, chain_x, chain_y, &
                                   xscale, yscale, symlog_threshold)
        !! Apply smoothing and draw chain
        class(plot_context), intent(inout) :: backend
        integer, intent(in) :: n_pts
        real(wp), intent(in) :: chain_x(:), chain_y(:)
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: n_smooth, k
        real(wp), allocatable :: smooth_x(:), smooth_y(:)
        real(wp) :: x1, y1, x2, y2

        if (n_pts < 2) return

        call smooth_contour_chain(n_pts, chain_x(1:n_pts), chain_y(1:n_pts), &
                                  CONTOUR_SUBDIVISIONS, n_smooth, &
                                  smooth_x, smooth_y)

        do k = 1, n_smooth - 1
            x1 = apply_scale_transform(smooth_x(k), xscale, symlog_threshold)
            y1 = apply_scale_transform(smooth_y(k), yscale, symlog_threshold)
            x2 = apply_scale_transform(smooth_x(k + 1), xscale, symlog_threshold)
            y2 = apply_scale_transform(smooth_y(k + 1), yscale, symlog_threshold)
            call backend%line(x1, y1, x2, y2)
        end do
    end subroutine draw_smoothed_chain

end module fortplot_contour_rendering
