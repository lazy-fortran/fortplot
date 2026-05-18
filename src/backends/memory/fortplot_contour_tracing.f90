module fortplot_contour_tracing
    !! Contour tracing and segment chaining module
    !!
    !! This module handles the low-level contour tracing algorithm (marching
    !! squares segment extraction), segment chaining via hash-table lookup,
    !! and drawing of smoothed contour chains.
    !!
    !! Procedures: trace_contour_level, chain_and_draw_segments,
    !!             extend_chain_forward_hash, extend_chain_backward_hash,
    !!             endpoint_hash, points_match, draw_smoothed_chain

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_scales, only: apply_scale_transform
    use fortplot_contour_algorithms, only: calculate_marching_squares_config, &
                                           get_contour_lines, smooth_contour_chain
    use fortplot_plot_data, only: plot_data_t
    implicit none

    private
    public :: trace_contour_level

    integer, parameter :: CONTOUR_SUBDIVISIONS = 4
    real(wp), parameter :: ENDPOINT_TOL = 1.0e-10_wp

contains

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
        !!
        !! Uses a hash table keyed by endpoint position to replace the
        !! O(n_segs^2) linear scan with O(n_segs) total work.
        class(plot_context), intent(inout) :: backend
        integer, intent(in) :: n_segs
        real(wp), contiguous, intent(in) :: seg_x1(:), seg_y1(:), seg_x2(:), seg_y2(:)
        logical, intent(inout) :: seg_used(:)
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold

        integer :: start_idx, chain_len, max_chain
        real(wp), allocatable :: chain_x(:), chain_y(:)
        real(wp) :: cur_x, cur_y
        integer, allocatable :: ep_hash(:), ep_seg(:)
        integer :: n_entries, i, h

        if (n_segs == 0) return

        max_chain = n_segs + 1
        allocate (chain_x(max_chain), chain_y(max_chain))

        ! Build hash table: each entry stores a rounded hash key and a
        ! segment index.  Two entries per segment (one for each endpoint).
        n_entries = 2*n_segs
        allocate (ep_hash(n_entries), ep_seg(n_entries))
        do i = 1, n_segs
            h = endpoint_hash(seg_x1(i), seg_y1(i))
            ep_hash(2*i - 1) = h
            ep_seg(2*i - 1) = i
            h = endpoint_hash(seg_x2(i), seg_y2(i))
            ep_hash(2*i) = h
            ep_seg(2*i) = i
        end do

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
            call extend_chain_forward_hash(n_segs, seg_x1, seg_y1, seg_x2, seg_y2, &
                                           seg_used, cur_x, cur_y, chain_x, chain_y, &
                                           chain_len, max_chain, ep_hash, ep_seg, &
                                           n_entries)

            cur_x = chain_x(1)
            cur_y = chain_y(1)
            call extend_chain_backward_hash(n_segs, seg_x1, seg_y1, seg_x2, seg_y2, &
                                            seg_used, cur_x, cur_y, chain_x, chain_y, &
                                            chain_len, max_chain, ep_hash, ep_seg, &
                                            n_entries)

            call draw_smoothed_chain(backend, chain_len, chain_x, chain_y, &
                                     xscale, yscale, symlog_threshold)
        end do

    end subroutine chain_and_draw_segments

    subroutine extend_chain_forward_hash(n_segs, seg_x1, seg_y1, seg_x2, seg_y2, &
                                          seg_used, cur_x, cur_y, chain_x, chain_y, &
                                          chain_len, max_chain, ep_hash, ep_seg, &
                                          n_entries)
        !! Hash-table accelerated version of extend_chain_forward.
        integer, intent(in) :: n_segs, max_chain, n_entries
        real(wp), contiguous, intent(in) :: seg_x1(:), seg_y1(:), seg_x2(:), seg_y2(:)
        logical, intent(inout) :: seg_used(:)
        real(wp), intent(inout) :: cur_x, cur_y
        real(wp), contiguous, intent(inout) :: chain_x(:), chain_y(:)
        integer, intent(inout) :: chain_len
        integer, intent(in) :: ep_hash(:), ep_seg(:)

        integer :: k, e, h
        logical :: found
        real(wp) :: next_x, next_y

        found = .true.
        do while (found .and. chain_len < max_chain)
            found = .false.
            h = endpoint_hash(cur_x, cur_y)
            do e = 1, n_entries
                if (ep_hash(e) /= h) cycle
                k = ep_seg(e)
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
    end subroutine extend_chain_forward_hash

    subroutine extend_chain_backward_hash(n_segs, seg_x1, seg_y1, seg_x2, seg_y2, &
                                           seg_used, cur_x, cur_y, chain_x, chain_y, &
                                           chain_len, max_chain, ep_hash, ep_seg, &
                                           n_entries)
        !! Hash-table accelerated version of extend_chain_backward.
        integer, intent(in) :: n_segs, max_chain, n_entries
        real(wp), contiguous, intent(in) :: seg_x1(:), seg_y1(:), seg_x2(:), seg_y2(:)
        logical, intent(inout) :: seg_used(:)
        real(wp), intent(inout) :: cur_x, cur_y
        real(wp), contiguous, intent(inout) :: chain_x(:), chain_y(:)
        integer, intent(inout) :: chain_len
        integer, intent(in) :: ep_hash(:), ep_seg(:)

        integer :: k, e, m, h
        logical :: found
        real(wp) :: next_x, next_y

        found = .true.
        do while (found .and. chain_len < max_chain)
            found = .false.
            h = endpoint_hash(cur_x, cur_y)
            do e = 1, n_entries
                if (ep_hash(e) /= h) cycle
                k = ep_seg(e)
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
    end subroutine extend_chain_backward_hash

    pure function endpoint_hash(x, y) result(h)
        !! Hash function for endpoint coordinates.
        !! Rounds to 6 decimal places and combines into an integer.
        real(wp), intent(in) :: x, y
        integer :: h
        integer :: ix, iy
        ix = nint(x*1.0e6_wp)
        iy = nint(y*1.0e6_wp)
        h = iand(ieor(ix, iy), 2147483647)
    end function endpoint_hash

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
        real(wp), contiguous, intent(in) :: chain_x(:), chain_y(:)
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

end module fortplot_contour_tracing
