module fortplot_tt_rasterizer
    !! TrueType scanline rasterizer (v2 analytical antialiasing).
    !! Converts tessellated glyph outlines into 8-bit grayscale bitmaps.
    !! Fortran port of the STB v2 rasterizer from stb_truetype.h.
    use fortplot_tt_curves, only: tt_point_t
    use, intrinsic :: iso_fortran_env, only: dp => real64, int8
    implicit none

    private
    public :: tt_rasterize

    type :: tt_edge_t
        real(dp) :: x0, y0
        real(dp) :: x1, y1
        logical :: invert
    end type tt_edge_t

    type :: tt_active_edge_t
        real(dp) :: fx
        real(dp) :: fdx
        real(dp) :: fdy
        real(dp) :: direction
        real(dp) :: sy
        real(dp) :: ey
    end type tt_active_edge_t

contains

    subroutine tt_rasterize(result_pixels, result_w, result_h, result_stride, &
            pts, wcount, windings, &
            scale_x, scale_y, shift_x, shift_y, &
            off_x, off_y, invert)
        !! Rasterize tessellated glyph outlines into an 8-bit bitmap.
        integer(int8), intent(inout) :: result_pixels(:)
        integer, intent(in) :: result_w, result_h, result_stride
        type(tt_point_t), intent(in) :: pts(:)
        integer, intent(in) :: wcount(:)
        integer, intent(in) :: windings
        real(dp), intent(in) :: scale_x, scale_y, shift_x, shift_y
        integer, intent(in) :: off_x, off_y
        logical, intent(in) :: invert

        type(tt_edge_t), allocatable :: edges(:)
        integer :: n_total, n_edges, i, j, k, m_idx, a, b
        real(dp) :: y_scale_inv

        if (invert) then
            y_scale_inv = -scale_y
        else
            y_scale_inv = scale_y
        end if

        ! Count total points
        n_total = 0
        do i = 1, windings
            n_total = n_total + wcount(i)
        end do

        allocate(edges(n_total + 1))
        n_edges = 0
        m_idx = 0

        do i = 1, windings
            j = wcount(i)
            do k = 1, wcount(i)
                if (pts(m_idx + j)%y == pts(m_idx + k)%y) then
                    j = k
                    cycle
                end if

                n_edges = n_edges + 1
                edges(n_edges)%invert = .false.
                a = k; b = j

                if (invert) then
                    if (pts(m_idx + j)%y > pts(m_idx + k)%y) then
                        edges(n_edges)%invert = .true.
                        a = j; b = k
                    end if
                else
                    if (pts(m_idx + j)%y < pts(m_idx + k)%y) then
                        edges(n_edges)%invert = .true.
                        a = j; b = k
                    end if
                end if

                edges(n_edges)%x0 = pts(m_idx + a)%x * scale_x + shift_x
                edges(n_edges)%y0 = pts(m_idx + a)%y * y_scale_inv + shift_y
                edges(n_edges)%x1 = pts(m_idx + b)%x * scale_x + shift_x
                edges(n_edges)%y1 = pts(m_idx + b)%y * y_scale_inv + shift_y

                j = k
            end do
            m_idx = m_idx + wcount(i)
        end do

        call sort_edges(edges, n_edges)

        edges(n_edges + 1)%y0 = real(off_y + result_h, dp) + 1.0_dp

        call rasterize_sorted_edges(result_pixels, result_w, result_h, &
            result_stride, edges, n_edges, off_x, off_y)

        deallocate(edges)
    end subroutine tt_rasterize

    subroutine sort_edges(edges, n)
        !! Sort edges by y0 ascending. Quicksort + insertion sort.
        type(tt_edge_t), intent(inout) :: edges(:)
        integer, intent(in) :: n

        call sort_edges_quicksort(edges, n)
        call sort_edges_ins_sort(edges, n)
    end subroutine sort_edges

    recursive subroutine sort_edges_quicksort(p, n)
        type(tt_edge_t), intent(inout) :: p(:)
        integer, intent(in) :: n
        type(tt_edge_t) :: t
        integer :: c01, c12, c, m, i, j, z

        if (n <= 12) return

        m = ishft(n, -1) + 1
        c01 = merge(1, 0, p(1)%y0 < p(m)%y0)
        c12 = merge(1, 0, p(m)%y0 < p(n)%y0)

        if (c01 /= c12) then
            c = merge(1, 0, p(1)%y0 < p(n)%y0)
            if (c == c12) then
                z = 1
            else
                z = n
            end if
            t = p(z); p(z) = p(m); p(m) = t
        end if

        t = p(1); p(1) = p(m); p(m) = t

        i = 2
        j = n
        do
            do while (i <= n .and. p(i)%y0 < p(1)%y0)
                i = i + 1
            end do
            do while (j >= 1 .and. p(1)%y0 < p(j)%y0)
                j = j - 1
            end do
            if (i >= j) exit
            t = p(i); p(i) = p(j); p(j) = t
            i = i + 1
            j = j - 1
        end do

        if (j < n - i + 1) then
            call sort_edges_quicksort(p(1:j), j)
            call sort_edges_quicksort(p(i:n), n - i + 1)
        else
            call sort_edges_quicksort(p(i:n), n - i + 1)
            call sort_edges_quicksort(p(1:j), j)
        end if
    end subroutine sort_edges_quicksort

    subroutine sort_edges_ins_sort(p, n)
        type(tt_edge_t), intent(inout) :: p(:)
        integer, intent(in) :: n
        type(tt_edge_t) :: t
        integer :: i, j

        do i = 2, n
            t = p(i)
            j = i
            do while (j > 1)
                if (.not. (t%y0 < p(j - 1)%y0)) exit
                p(j) = p(j - 1)
                j = j - 1
            end do
            if (i /= j) p(j) = t
        end do
    end subroutine sort_edges_ins_sort

    function new_active_edge(e, off_x, start_point) result(ae)
        type(tt_edge_t), intent(in) :: e
        integer, intent(in) :: off_x
        real(dp), intent(in) :: start_point
        type(tt_active_edge_t) :: ae
        real(dp) :: dxdy

        dxdy = (e%x1 - e%x0) / (e%y1 - e%y0)
        ae%fdx = dxdy
        if (dxdy /= 0.0_dp) then
            ae%fdy = 1.0_dp / dxdy
        else
            ae%fdy = 0.0_dp
        end if
        ae%fx = e%x0 + dxdy * (start_point - e%y0) - real(off_x, dp)
        if (e%invert) then
            ae%direction = 1.0_dp
        else
            ae%direction = -1.0_dp
        end if
        ae%sy = e%y0
        ae%ey = e%y1
    end function new_active_edge

    subroutine handle_clipped_edge(scanline, x, len, direction, sy, ey, &
            x0_in, y0_in, x1_in, y1_in)
        real(dp), intent(inout) :: scanline(0:)
        integer, intent(in) :: x, len
        real(dp), intent(in) :: direction, sy, ey
        real(dp), intent(in) :: x0_in, y0_in, x1_in, y1_in
        real(dp) :: lx0, ly0, lx1, ly1

        if (y0_in == y1_in) return

        ly0 = y0_in; ly1 = y1_in; lx0 = x0_in; lx1 = x1_in

        if (ly0 > ey) return
        if (ly1 < sy) return

        if (ly0 < sy) then
            lx0 = lx0 + (lx1 - lx0) * (sy - ly0) / (ly1 - ly0)
            ly0 = sy
        end if
        if (ly1 > ey) then
            lx1 = lx1 + (lx1 - lx0) * (ey - ly1) / (ly1 - ly0)
            ly1 = ey
        end if

        if (x < 0 .or. x >= len) return

        if (lx0 <= real(x, dp) .and. lx1 <= real(x, dp)) then
            scanline(x) = scanline(x) + direction * (ly1 - ly0)
        else if (lx0 >= real(x + 1, dp) .and. lx1 >= real(x + 1, dp)) then
            return
        else
            scanline(x) = scanline(x) + direction * (ly1 - ly0) * &
                (1.0_dp - ((lx0 - real(x, dp)) + (lx1 - real(x, dp))) / 2.0_dp)
        end if
    end subroutine handle_clipped_edge

    subroutine fill_active_edges(scanline, scanline2, w, &
            active, num_active, y_top)
        !! Fill scanline coverage from active edges.
        !! Port of stbtt__fill_active_edges_new.
        !! scanline is 0-based [0..w-1], scanline2 is 0-based [0..w].
        !! In STB, fill function receives scanline_fill = scanline2+1.
        !! Here scanline2(k+1) = STB's scanline_fill[k].
        real(dp), intent(inout) :: scanline(0:)
        real(dp), intent(inout) :: scanline2(0:)
        integer, intent(in) :: w
        type(tt_active_edge_t), intent(in) :: active(:)
        integer, intent(in) :: num_active
        real(dp), intent(in) :: y_top

        real(dp) :: y_bottom, x0, dx, xb, x_top, x_bottom
        real(dp) :: sy0, sy1, dy, y_crossing, y_final
        real(dp) :: step_val, sign_val, area, height_val
        real(dp) :: t
        integer :: ei, x, x1_int, x2_int
        integer :: len

        len = w
        y_bottom = y_top + 1.0_dp

        do ei = 1, num_active
            associate(e => active(ei))

            if (e%ey <= y_top) cycle

            if (e%fdx == 0.0_dp) then
                x0 = e%fx
                if (x0 < real(len, dp)) then
                    if (x0 >= 0.0_dp) then
                        call handle_clipped_edge(scanline, int(x0), len, &
                            e%direction, e%sy, e%ey, x0, y_top, x0, y_bottom)
                        call handle_clipped_edge(scanline2, int(x0) + 1, len + 1, &
                            e%direction, e%sy, e%ey, x0, y_top, x0, y_bottom)
                    else
                        call handle_clipped_edge(scanline2, 0, len + 1, &
                            e%direction, e%sy, e%ey, x0, y_top, x0, y_bottom)
                    end if
                end if
            else
                x0 = e%fx
                dx = e%fdx
                xb = x0 + dx
                dy = e%fdy

                if (e%sy > y_top) then
                    x_top = x0 + dx * (e%sy - y_top)
                    sy0 = e%sy
                else
                    x_top = x0
                    sy0 = y_top
                end if
                if (e%ey < y_bottom) then
                    x_bottom = x0 + dx * (e%ey - y_top)
                    sy1 = e%ey
                else
                    x_bottom = xb
                    sy1 = y_bottom
                end if

                if (x_top >= 0.0_dp .and. x_bottom >= 0.0_dp .and. &
                    x_top < real(len, dp) .and. x_bottom < real(len, dp)) then

                    if (int(x_top) == int(x_bottom)) then
                        x = int(x_top)
                        height_val = (sy1 - sy0) * e%direction
                        scanline(x) = scanline(x) + &
                            position_trapezoid_area(height_val, x_top, &
                            real(x + 1, dp), x_bottom, real(x + 1, dp))
                        scanline2(x + 1) = scanline2(x + 1) + height_val
                    else
                        if (x_top > x_bottom) then
                            sy0 = y_bottom - (sy0 - y_top)
                            sy1 = y_bottom - (sy1 - y_top)
                            t = sy0; sy0 = sy1; sy1 = t
                            t = x_bottom; x_bottom = x_top; x_top = t
                            dx = -dx
                            dy = -dy
                            t = x0; x0 = xb; xb = t
                        end if

                        x1_int = int(x_top)
                        x2_int = int(x_bottom)

                        y_crossing = y_top + dy * (real(x1_int + 1, dp) - x0)
                        y_final = y_top + dy * (real(x2_int, dp) - x0)

                        if (y_crossing > y_bottom) y_crossing = y_bottom

                        sign_val = e%direction
                        area = sign_val * (y_crossing - sy0)

                        scanline(x1_int) = scanline(x1_int) + &
                            sized_triangle_area(area, real(x1_int + 1, dp) - x_top)

                        if (y_final > y_bottom) then
                            y_final = y_bottom
                            if (x2_int /= x1_int + 1) then
                                dy = (y_final - y_crossing) / &
                                    real(x2_int - (x1_int + 1), dp)
                            end if
                        end if

                        step_val = sign_val * dy

                        do x = x1_int + 1, x2_int - 1
                            scanline(x) = scanline(x) + area + step_val / 2.0_dp
                            area = area + step_val
                        end do

                        scanline(x2_int) = scanline(x2_int) + area + &
                            sign_val * position_trapezoid_area( &
                            sy1 - y_final, real(x2_int, dp), &
                            real(x2_int + 1, dp), x_bottom, &
                            real(x2_int + 1, dp))

                        scanline2(x2_int + 1) = scanline2(x2_int + 1) + &
                            sign_val * (sy1 - sy0)
                    end if
                else
                    do x = 0, len - 1
                        call fill_edge_brute(scanline, scanline2, x, len, &
                            e%direction, e%sy, e%ey, &
                            x0, y_top, dx, xb, y_bottom)
                    end do
                end if
            end if

            end associate
        end do
    end subroutine fill_active_edges

    subroutine fill_edge_brute(scanline, scanline2, x, len, &
            direction, sy, ey, x0, y_top, dx, xb, y_bottom)
        !! Brute-force per-pixel clipping for edges outside the fast path.
        real(dp), intent(inout) :: scanline(0:), scanline2(0:)
        integer, intent(in) :: x, len
        real(dp), intent(in) :: direction, sy, ey
        real(dp), intent(in) :: x0, y_top, dx, xb, y_bottom

        real(dp) :: y0, x1, x2, x3, y3, y1, y2

        y0 = y_top
        x1 = real(x, dp)
        x2 = real(x + 1, dp)
        x3 = xb
        y3 = y_bottom

        y1 = (x1 - x0) / dx + y_top
        y2 = (x2 - x0) / dx + y_top

        if (x0 < x1 .and. x3 > x2) then
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x0, y0, x1, y1)
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x1, y1, x2, y2)
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x2, y2, x3, y3)
        else if (x3 < x1 .and. x0 > x2) then
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x0, y0, x2, y2)
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x2, y2, x1, y1)
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x1, y1, x3, y3)
        else if (x0 < x1 .and. x3 > x1) then
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x0, y0, x1, y1)
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x1, y1, x3, y3)
        else if (x3 < x1 .and. x0 > x1) then
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x0, y0, x1, y1)
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x1, y1, x3, y3)
        else if (x0 < x2 .and. x3 > x2) then
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x0, y0, x2, y2)
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x2, y2, x3, y3)
        else if (x3 < x2 .and. x0 > x2) then
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x0, y0, x2, y2)
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x2, y2, x3, y3)
        else
            call handle_clipped_edge(scanline, x, len, direction, sy, ey, &
                x0, y0, x3, y3)
        end if
    end subroutine fill_edge_brute

    pure function sized_triangle_area(height, width) result(area)
        real(dp), intent(in) :: height, width
        real(dp) :: area
        area = height * width / 2.0_dp
    end function sized_triangle_area

    pure function position_trapezoid_area(height, tx0, tx1, bx0, bx1) result(area)
        real(dp), intent(in) :: height, tx0, tx1, bx0, bx1
        real(dp) :: area
        area = (tx1 - tx0 + bx1 - bx0) / 2.0_dp * height
    end function position_trapezoid_area

    subroutine rasterize_sorted_edges(result_pixels, result_w, result_h, &
            result_stride, edges, n_edges, off_x, off_y)
        !! Main scanline rasterizer loop.
        integer(int8), intent(inout) :: result_pixels(:)
        integer, intent(in) :: result_w, result_h, result_stride
        type(tt_edge_t), intent(inout) :: edges(:)
        integer, intent(in) :: n_edges, off_x, off_y

        type(tt_active_edge_t), allocatable :: active(:)
        real(dp), allocatable :: scanline(:), scanline2(:)
        integer :: num_active, max_active
        integer :: y, j, row, ei, pixel_idx
        real(dp) :: scan_y_top, scan_y_bottom, sum_val, k_val
        integer :: m_val
        type(tt_active_edge_t), allocatable :: new_active(:)

        allocate(scanline(0:result_w - 1))
        allocate(scanline2(0:result_w))

        max_active = 64
        allocate(active(max_active))
        num_active = 0

        y = off_y
        j = 1

        do row = 1, result_h
            scan_y_top = real(y, dp)
            scan_y_bottom = real(y, dp) + 1.0_dp

            scanline(:) = 0.0_dp
            scanline2(:) = 0.0_dp

            ! Remove expired active edges
            ei = 1
            do while (ei <= num_active)
                if (active(ei)%ey <= scan_y_top) then
                    active(ei) = active(num_active)
                    num_active = num_active - 1
                else
                    ei = ei + 1
                end if
            end do

            ! Insert new edges
            do while (j <= n_edges .and. edges(j)%y0 <= scan_y_bottom)
                if (edges(j)%y0 /= edges(j)%y1) then
                    num_active = num_active + 1
                    if (num_active > max_active) then
                        max_active = max_active * 2
                        allocate(new_active(max_active))
                        new_active(1:num_active - 1) = active(1:num_active - 1)
                        call move_alloc(new_active, active)
                    end if
                    active(num_active) = new_active_edge(edges(j), off_x, scan_y_top)

                    if (row == 1 .and. off_y /= 0) then
                        if (active(num_active)%ey < scan_y_top) then
                            active(num_active)%ey = scan_y_top
                        end if
                    end if
                end if
                j = j + 1
            end do

            ! Process active edges
            if (num_active > 0) then
                call fill_active_edges(scanline, scanline2, result_w, &
                    active, num_active, scan_y_top)
            end if

            ! Convert coverage to pixels
            sum_val = 0.0_dp
            do ei = 0, result_w - 1
                sum_val = sum_val + scanline2(ei)
                k_val = scanline(ei) + sum_val
                k_val = abs(k_val) * 255.0_dp + 0.5_dp
                m_val = int(k_val)
                if (m_val > 255) m_val = 255
                pixel_idx = (row - 1) * result_stride + ei + 1
                if (pixel_idx >= 1 .and. pixel_idx <= size(result_pixels)) then
                    result_pixels(pixel_idx) = int(m_val, int8)
                end if
            end do

            ! Advance active edges
            do ei = 1, num_active
                active(ei)%fx = active(ei)%fx + active(ei)%fdx
            end do

            y = y + 1
        end do

        deallocate(scanline, scanline2, active)
    end subroutine rasterize_sorted_edges

end module fortplot_tt_rasterizer
