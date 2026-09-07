module fortplot_quiver_polygons
    !! Filled, clipped quiver polygons. Preserve a single path for alpha blending.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context
    use fortplot_raster, only: raster_context
    use fortplot_pdf, only: pdf_context
    use fortplot_svg, only: svg_context
    use fortplot_svg_draw, only: svg_add_to_stream
    use fortplot_raster_primitives, only: blend_pixel
    implicit none
    private
    public :: fill_quiver_polygon

contains

    subroutine fill_quiver_polygon(backend, x, y, rgba, bounds)
        class(plot_context), intent(inout) :: backend
        real(wp), intent(in) :: x(8), y(8), rgba(4), bounds(4)
        real(wp) :: px(32), py(32), qx(4), qy(4)
        integer :: n, i
        real(wp) :: normalized_x(32), normalized_y(32)

        if (rgba(4) <= 0.0_wp) return
        call clip_quiver_polygon(x, y, bounds, px, py, n)
        if (n < 3) return
        normalized_x(:n) = (px(:n) - bounds(1))/(bounds(2) - bounds(1))
        normalized_y(:n) = (py(:n) - bounds(3))/(bounds(4) - bounds(3))
        select type (backend)
        class is (raster_context)
            px(:n) = real(backend%plot_area%left, wp) + &
                normalized_x(:n)*backend%plot_area%width
            py(:n) = real(backend%plot_area%bottom + backend%plot_area%height, wp) - &
                normalized_y(:n)*backend%plot_area%height
            call fill_raster_polygon(backend, px(:n), py(:n), rgba)
        class is (pdf_context)
            px(:n) = real(backend%plot_area%left, wp) + &
                normalized_x(:n)*backend%plot_area%width
            py(:n) = real(backend%plot_area%bottom, wp) + &
                normalized_y(:n)*backend%plot_area%height
            call fill_pdf_polygon(backend, px(:n), py(:n), rgba)
        class is (svg_context)
            px(:n) = real(backend%plot_area%left, wp) + &
                normalized_x(:n)*backend%plot_area%width
            py(:n) = real(backend%plot_area%bottom + backend%plot_area%height, wp) - &
                normalized_y(:n)*backend%plot_area%height
            call fill_svg_polygon(backend, px(:n), py(:n), rgba)
        class default
            call backend%color(rgba(1), rgba(2), rgba(3))
            do i = 2, n - 1
                qx = [px(1), px(i), px(i + 1), px(i + 1)]
                qy = [py(1), py(i), py(i + 1), py(i + 1)]
                call backend%fill_quad(qx, qy)
            end do
        end select
    end subroutine fill_quiver_polygon

    subroutine clip_quiver_polygon(x, y, bounds, px, py, n)
        real(wp), intent(in) :: x(8), y(8), bounds(4)
        real(wp), intent(out) :: px(32), py(32)
        integer, intent(out) :: n
        real(wp) :: tx(32), ty(32), a, b, f, edge
        logical :: inside_a, inside_b
        integer :: side, i, prev, count

        n = 8
        px(:n) = x
        py(:n) = y
        do side = 1, 4
            if (n == 0) exit
            count = 0
            edge = bounds(side)
            prev = n
            do i = 1, n
                a = px(prev)
                b = px(i)
                if (side > 2) then
                    a = py(prev)
                    b = py(i)
                end if
                inside_a = a >= edge
                inside_b = b >= edge
                if (mod(side, 2) == 0) then
                    inside_a = a <= edge
                    inside_b = b <= edge
                end if
                if (inside_a .neqv. inside_b) then
                    f = (edge - a)/(b - a)
                    count = count + 1
                    tx(count) = px(prev) + f*(px(i) - px(prev))
                    ty(count) = py(prev) + f*(py(i) - py(prev))
                end if
                if (inside_b) then
                    count = count + 1
                    tx(count) = px(i)
                    ty(count) = py(i)
                end if
                prev = i
            end do
            n = count
            px(:n) = tx(:n)
            py(:n) = ty(:n)
        end do
    end subroutine clip_quiver_polygon

    subroutine fill_pdf_polygon(backend, x, y, rgba)
        class(pdf_context), intent(inout) :: backend
        real(wp), intent(in) :: x(:), y(:), rgba(4)
        character(len=96) :: command
        integer :: i

        call backend%stream_writer%add_to_stream('q')
        call backend%set_marker_colors_with_alpha(rgba(1), rgba(2), rgba(3), &
            rgba(4), rgba(1), rgba(2), &
            rgba(3), rgba(4))
        call backend%stream_writer%apply_marker_gstate()
        do i = 1, size(x)
            write (command, '(F0.6,1X,F0.6)') x(i), y(i)
            if (i == 1) then
                call backend%stream_writer%add_to_stream(trim(command)//' m')
            else
                call backend%stream_writer%add_to_stream(trim(command)//' l')
            end if
        end do
        call backend%stream_writer%add_to_stream('h f Q')
    end subroutine fill_pdf_polygon

    subroutine fill_svg_polygon(backend, x, y, rgba)
        class(svg_context), intent(inout) :: backend
        real(wp), intent(in) :: x(:), y(:), rgba(4)
        character(len=96) :: part
        character(len=4096) :: command
        integer :: i

        command = '<polygon class="quiver" points="'
        do i = 1, size(x)
            write (part, '(F0.6,A,F0.6)') x(i), ',', y(i)
            command = trim(command)//' '//trim(part)
        end do
        write (part, '(A,I0,A,I0,A,I0,A,F0.6,A)') '" fill="rgb(', &
            nint(255*rgba(1)), ',', nint(255*rgba(2)), ',', nint(255*rgba(3)), &
            ')" fill-opacity="', rgba(4), '" stroke="none"/>'
        call svg_add_to_stream(backend%content_stream, trim(command)//trim(part))
    end subroutine fill_svg_polygon

    subroutine fill_raster_polygon(backend, px, py, rgba)
        class(raster_context), intent(inout) :: backend
        real(wp), intent(in) :: px(:), py(:), rgba(4)
        real(wp) :: cover(0:backend%width - 1), intersections(32), yy, overlap
        integer :: x, y, s, k, count, xmin, xmax, ymin, ymax, first, last

        ! Filled paths cover device cells [x,x+1] by [y,y+1]. Unlike snapped
        ! axis strokes, their sample centers are half a pixel from the origin.
        xmin = max(0, floor(minval(px)))
        xmax = min(backend%width - 1, ceiling(maxval(px)) - 1)
        ymin = max(0, floor(minval(py)))
        ymax = min(backend%height - 1, ceiling(maxval(py)) - 1)
        do y = ymin, ymax
            cover(xmin:xmax) = 0.0_wp
            do s = 1, 4
                yy = real(y, wp) + (real(s, wp) - 0.5_wp)/4.0_wp
                call polygon_intersections(px, py, yy, intersections, count)
                do k = 1, count - 1, 2
                    first = max(xmin, floor(intersections(k)))
                    last = min(xmax, ceiling(intersections(k + 1)))
                    do x = first, last
                        overlap = min(real(x + 1, wp), intersections(k + 1))
                        overlap = overlap - max(real(x, wp), intersections(k))
                        cover(x) = cover(x) + max(0.0_wp, overlap)/4.0_wp
                    end do
                end do
            end do
            do x = xmin, xmax
                if (cover(x) <= 0.0_wp) cycle
                call blend_pixel(backend%raster%image_data, backend%width, &
                    backend%height, real(x, wp), real(y, wp), &
                    min(1.0_wp, cover(x))*rgba(4), &
                    rgba(1), rgba(2), rgba(3))
            end do
        end do
    end subroutine fill_raster_polygon

    subroutine polygon_intersections(x, y, row, intersections, count)
        real(wp), intent(in) :: x(:), y(:), row
        real(wp), intent(out) :: intersections(32)
        integer, intent(out) :: count
        real(wp) :: crossing
        integer :: i, j, prev

        count = 0
        prev = size(x)
        do i = 1, size(x)
            if ((y(i) > row) .neqv. (y(prev) > row)) then
                crossing = x(prev) + (row - y(prev))*(x(i) - x(prev))/(y(i) - y(prev))
                count = count + 1
                j = count
                do while (j > 1)
                    if (intersections(j - 1) <= crossing) exit
                    intersections(j) = intersections(j - 1)
                    j = j - 1
                end do
                intersections(j) = crossing
            end if
            prev = i
        end do
    end subroutine polygon_intersections

end module fortplot_quiver_polygons
