module fortplot_raster_marker_paths
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_raster_core, only: raster_image_t
    use fortplot_marker_paths, only: marker_vertices, MAX_MARKER_VERTICES
    use fortplot_raster_primitives, only: blend_pixel, draw_line_distance_aa
    implicit none
    private
    public :: draw_raster_marker_path

contains

    subroutine draw_raster_marker_path(raster, width, height, cx, cy, size, style)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        real(wp), intent(in) :: cx, cy, size
        character(len=*), intent(in) :: style
        real(wp) :: x(MAX_MARKER_VERTICES), y(MAX_MARKER_VERTICES)
        integer :: n, i, j, stride
        logical :: closed

        call marker_vertices(style, x, y, n, closed)
        if (n == 0) return
        if (size <= 0.0_wp) return
        x = cx + size*x
        y = cy - size*y
        if (closed) call fill_marker(raster, width, height, x(:n), y(:n))
        if (raster%marker_edge_alpha <= 0.0_wp) return
        if (raster%current_line_width <= 0.0_wp) return
        stride = 1
        if (.not. closed) stride = 2
        do i = 1, n, stride
            j = mod(i, n) + 1
            if (.not. closed) then
                call stroke_open_marker(raster, width, height, &
                    x(i), y(i), x(j), y(j))
                cycle
            end if
            call draw_line_distance_aa(raster%image_data, width, height, &
                x(i), y(i), x(j), y(j), &
                raster%marker_edge_r, raster%marker_edge_g, &
                raster%marker_edge_b, &
                raster%current_line_width, &
                raster%marker_edge_alpha)
        end do
    end subroutine draw_raster_marker_path

    subroutine stroke_open_marker(raster, width, height, x1, y1, x2, y2)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: nx, ny, length, vx(4), vy(4)

        length = sqrt((x2 - x1)**2 + (y2 - y1)**2)
        if (length <= 0.0_wp) return
        nx = -0.5_wp*raster%current_line_width*(y2 - y1)/length
        ny = 0.5_wp*raster%current_line_width*(x2 - x1)/length
        vx(1) = x1 + nx
        vx(2) = x2 + nx
        vx(3) = x2 - nx
        vx(4) = x1 - nx
        vy(1) = y1 + ny
        vy(2) = y2 + ny
        vy(3) = y2 - ny
        vy(4) = y1 - ny
        call fill_marker(raster, width, height, vx, vy, edge=.true.)
    end subroutine stroke_open_marker

    subroutine fill_marker(raster, width, height, vx, vy, edge)
        type(raster_image_t), intent(inout) :: raster
        integer, intent(in) :: width, height
        real(wp), intent(in) :: vx(:), vy(:)
        logical, intent(in), optional :: edge
        integer :: x, y, sx, sy, covered
        real(wp) :: px, py, alpha, opacity, r, g, b
        integer, parameter :: samples = 4

        opacity = raster%marker_face_alpha
        r = raster%marker_face_r
        g = raster%marker_face_g
        b = raster%marker_face_b
        if (present(edge)) then
            if (edge) then
                opacity = raster%marker_edge_alpha
                r = raster%marker_edge_r
                g = raster%marker_edge_g
                b = raster%marker_edge_b
            end if
        end if
        if (opacity <= 0.0_wp) return
        do y = max(1, floor(minval(vy))), min(height, ceiling(maxval(vy)))
            do x = max(1, floor(minval(vx))), min(width, ceiling(maxval(vx)))
                covered = 0
                do sy = 1, samples
                    py = real(y, wp) - 0.5_wp + (real(sy, wp) - 0.5_wp)/samples
                    do sx = 1, samples
                        px = real(x, wp) - 0.5_wp + (real(sx, wp) - 0.5_wp)/samples
                        if (inside_marker(px, py, vx, vy)) covered = covered + 1
                    end do
                end do
                alpha = opacity*real(covered, wp)/(samples*samples)
                if (covered == 0) cycle
                call blend_pixel(raster%image_data, width, height, &
                    real(x, wp), real(y, wp), alpha, &
                    r, g, b)
            end do
        end do
    end subroutine fill_marker

    pure logical function inside_marker(x, y, vx, vy) result(inside)
        real(wp), intent(in) :: x, y, vx(:), vy(:)
        integer :: i, j
        real(wp) :: crossing

        inside = .false.
        j = size(vx)
        do i = 1, size(vx)
            if ((vy(i) > y) .neqv. (vy(j) > y)) then
                crossing = vx(i) + (y - vy(i))*(vx(j) - vx(i))/(vy(j) - vy(i))
                if (x < crossing) inside = .not. inside
            end if
            j = i
        end do
    end function inside_marker

end module fortplot_raster_marker_paths
