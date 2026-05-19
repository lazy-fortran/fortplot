module fortplot_tt_rasterizer
    !! TrueType scanline rasterizer (v2 analytical antialiasing).
    !! Converts tessellated glyph outlines into 8-bit grayscale bitmaps.
    !! Fortran port of the STB v2 rasterizer from stb_truetype.h.
    use fortplot_tt_curves, only: tt_point_t
    use fortplot_tt_edge_sort, only: tt_edge_t, sort_edges
    use fortplot_tt_scanline_fill, only: tt_active_edge_t, &
        new_active_edge, fill_active_edges
    use, intrinsic :: iso_fortran_env, only: dp => real64, int8
    implicit none

    private
    public :: tt_rasterize

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
    end subroutine tt_rasterize

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
    end subroutine rasterize_sorted_edges

end module fortplot_tt_rasterizer
