module fortplot_tt_scanline_fill
    !! Scanline coverage helpers for the TrueType analytical antialias rasterizer.
    !! Split from fortplot_tt_rasterizer to keep that module under the size limit.
    use fortplot_tt_edge_sort, only: tt_edge_t
    use, intrinsic :: iso_fortran_env, only: dp => real64
    implicit none

    private
    public :: tt_active_edge_t, new_active_edge, fill_active_edges

    type :: tt_active_edge_t
        real(dp) :: fx
        real(dp) :: fdx
        real(dp) :: fdy
        real(dp) :: direction
        real(dp) :: sy
        real(dp) :: ey
    end type tt_active_edge_t

    type :: tt_scanline_edge_t
        real(dp) :: x0
        real(dp) :: dx
        real(dp) :: xb
        real(dp) :: dy
        real(dp) :: x_top
        real(dp) :: x_bottom
        real(dp) :: sy0
        real(dp) :: sy1
    end type tt_scanline_edge_t

contains

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

        real(dp) :: y_bottom, x0, dx, xb
        integer :: ei, x
        integer :: len
        type(tt_scanline_edge_t) :: span

        len = w
        y_bottom = y_top + 1.0_dp

        do ei = 1, num_active
            associate(e => active(ei))

            if (e%ey <= y_top) cycle

            if (e%fdx == 0.0_dp) then
                call fill_vertical_active_edge(scanline, scanline2, len, e, &
                    y_top, y_bottom)
            else
                span = build_scanline_edge(e, y_top, y_bottom)

                if (scanline_edge_in_bounds(span, len)) then

                    if (int(span%x_top) == int(span%x_bottom)) then
                        call fill_single_pixel_span(scanline, scanline2, e, span)
                    else
                        call fill_multi_pixel_span(scanline, scanline2, e, span, &
                            y_top, y_bottom)
                    end if
                else
                    x0 = span%x0
                    dx = span%dx
                    xb = span%xb
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

    subroutine fill_vertical_active_edge(scanline, scanline2, len, edge, &
            y_top, y_bottom)
        real(dp), intent(inout) :: scanline(0:), scanline2(0:)
        integer, intent(in) :: len
        type(tt_active_edge_t), intent(in) :: edge
        real(dp), intent(in) :: y_top, y_bottom

        real(dp) :: x0

        x0 = edge%fx
        if (x0 >= real(len, dp)) return

        if (x0 >= 0.0_dp) then
            call handle_clipped_edge(scanline, int(x0), len, &
                edge%direction, edge%sy, edge%ey, &
                x0, y_top, x0, y_bottom)
            call handle_clipped_edge(scanline2, int(x0) + 1, len + 1, &
                edge%direction, edge%sy, edge%ey, &
                x0, y_top, x0, y_bottom)
        else
            call handle_clipped_edge(scanline2, 0, len + 1, edge%direction, &
                edge%sy, edge%ey, x0, y_top, x0, y_bottom)
        end if
    end subroutine fill_vertical_active_edge

    function build_scanline_edge(edge, y_top, y_bottom) result(span)
        type(tt_active_edge_t), intent(in) :: edge
        real(dp), intent(in) :: y_top, y_bottom
        type(tt_scanline_edge_t) :: span

        span%x0 = edge%fx
        span%dx = edge%fdx
        span%xb = span%x0 + span%dx
        span%dy = edge%fdy

        if (edge%sy > y_top) then
            span%x_top = span%x0 + span%dx * (edge%sy - y_top)
            span%sy0 = edge%sy
        else
            span%x_top = span%x0
            span%sy0 = y_top
        end if

        if (edge%ey < y_bottom) then
            span%x_bottom = span%x0 + span%dx * (edge%ey - y_top)
            span%sy1 = edge%ey
        else
            span%x_bottom = span%xb
            span%sy1 = y_bottom
        end if
    end function build_scanline_edge

    pure logical function scanline_edge_in_bounds(span, len)
        type(tt_scanline_edge_t), intent(in) :: span
        integer, intent(in) :: len

        scanline_edge_in_bounds = span%x_top >= 0.0_dp .and. &
            span%x_bottom >= 0.0_dp .and. &
            span%x_top < real(len, dp) .and. &
            span%x_bottom < real(len, dp)
    end function scanline_edge_in_bounds

    subroutine fill_single_pixel_span(scanline, scanline2, edge, span)
        real(dp), intent(inout) :: scanline(0:), scanline2(0:)
        type(tt_active_edge_t), intent(in) :: edge
        type(tt_scanline_edge_t), intent(in) :: span

        real(dp) :: height_val
        integer :: x

        x = int(span%x_top)
        height_val = (span%sy1 - span%sy0) * edge%direction
        scanline(x) = scanline(x) + position_trapezoid_area(height_val, &
            span%x_top, real(x + 1, dp), span%x_bottom, real(x + 1, dp))
        scanline2(x + 1) = scanline2(x + 1) + height_val
    end subroutine fill_single_pixel_span

    subroutine fill_multi_pixel_span(scanline, scanline2, edge, span, &
            y_top, y_bottom)
        real(dp), intent(inout) :: scanline(0:), scanline2(0:)
        type(tt_active_edge_t), intent(in) :: edge
        type(tt_scanline_edge_t), intent(in) :: span
        real(dp), intent(in) :: y_top, y_bottom

        type(tt_scanline_edge_t) :: work
        real(dp) :: y_crossing, y_final, step_val, sign_val, area
        integer :: x, x1_int, x2_int

        work = normalized_left_to_right_span(span, y_top, y_bottom)

        x1_int = int(work%x_top)
        x2_int = int(work%x_bottom)
        y_crossing = y_top + work%dy * (real(x1_int + 1, dp) - work%x0)
        y_final = y_top + work%dy * (real(x2_int, dp) - work%x0)
        if (y_crossing > y_bottom) y_crossing = y_bottom

        sign_val = edge%direction
        area = sign_val * (y_crossing - work%sy0)

        scanline(x1_int) = scanline(x1_int) + &
            sized_triangle_area(area, real(x1_int + 1, dp) - work%x_top)

        if (y_final > y_bottom) then
            y_final = y_bottom
            if (x2_int /= x1_int + 1) then
                work%dy = (y_final - y_crossing) / &
                    real(x2_int - (x1_int + 1), dp)
            end if
        end if

        step_val = sign_val * work%dy
        do x = x1_int + 1, x2_int - 1
            scanline(x) = scanline(x) + area + step_val / 2.0_dp
            area = area + step_val
        end do

        scanline(x2_int) = scanline(x2_int) + area + &
            sign_val * position_trapezoid_area(work%sy1 - y_final, &
            real(x2_int, dp), real(x2_int + 1, dp), work%x_bottom, &
            real(x2_int + 1, dp))
        scanline2(x2_int + 1) = scanline2(x2_int + 1) + &
            sign_val * (work%sy1 - work%sy0)
    end subroutine fill_multi_pixel_span

    function normalized_left_to_right_span(span, y_top, y_bottom) result(work)
        type(tt_scanline_edge_t), intent(in) :: span
        real(dp), intent(in) :: y_top, y_bottom
        type(tt_scanline_edge_t) :: work
        real(dp) :: t

        work = span
        if (work%x_top <= work%x_bottom) return

        work%sy0 = y_bottom - (span%sy0 - y_top)
        work%sy1 = y_bottom - (span%sy1 - y_top)
        t = work%sy0; work%sy0 = work%sy1; work%sy1 = t
        t = work%x_bottom; work%x_bottom = work%x_top; work%x_top = t
        work%dx = -work%dx
        work%dy = -work%dy
        t = work%x0; work%x0 = work%xb; work%xb = t
    end function normalized_left_to_right_span

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

end module fortplot_tt_scanline_fill
