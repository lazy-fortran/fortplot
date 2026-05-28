submodule (fortplot_pdf) fortplot_pdf_draw

    !! PDF drawing, text rendering, fill operations, and markers
    !!
    !! Single Responsibility: Handle all drawing operations including lines,
    !! text, fills, markers, and arrows.

    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    implicit none

contains

    module subroutine draw_pdf_line(this, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: pdf_x1, pdf_y1, pdf_x2, pdf_y2
        ! Ensure coordinate context reflects latest figure ranges and plot area
        call this%update_coord_context()

        ! Skip drawing if any coordinate is NaN (disconnected line segments)
        if (ieee_is_nan(x1) .or. ieee_is_nan(y1) .or. &
            ieee_is_nan(x2) .or. ieee_is_nan(y2)) then
            return
        end if

        call normalize_to_pdf_coords(this%coord_ctx, x1, y1, pdf_x1, pdf_y1)
        call normalize_to_pdf_coords(this%coord_ctx, x2, y2, pdf_x2, pdf_y2)
        call this%stream_writer%draw_vector_line(pdf_x1, pdf_y1, pdf_x2, pdf_y2)
    end subroutine draw_pdf_line

    module subroutine set_pdf_color(this, r, g, b)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        call this%stream_writer%set_vector_color(r, g, b)
        call this%core_ctx%set_color(r, g, b)
    end subroutine set_pdf_color

    module subroutine set_pdf_line_width(this, width)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: width

        call this%stream_writer%set_vector_line_width(width)
        call this%core_ctx%set_line_width(width)
    end subroutine set_pdf_line_width

    module subroutine set_pdf_line_style(this, style)
        use fortplot_line_styles, only: get_line_pattern
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: style
        character(len=64) :: dash_pattern
        real(wp) :: pattern(20), lw
        integer :: pattern_size

        ! PDF dash arrays are in points, exactly matplotlib's unit, so emit the
        ! point patterns scaled by line width (no DPI conversion needed).
        select case (trim(style))
        case ('-', 'solid')
            dash_pattern = '[] 0 d'  ! Solid line (empty dash array)
        case ('--', 'dashed', ':', 'dotted', '-.', 'dashdot')
            lw = this%core_ctx%current_line_width
            call get_line_pattern(style, pattern, pattern_size)
            dash_pattern = format_pdf_dash_array(pattern, pattern_size, lw)
        case default
            dash_pattern = '[] 0 d'  ! Default to solid
        end select

        call this%stream_writer%add_to_stream(trim(dash_pattern))
    end subroutine set_pdf_line_style

    function format_pdf_dash_array(pattern, pattern_size, line_width) result(dash)
        !! Build a PDF dash-array operator from a point pattern scaled by the
        !! line width (matplotlib: each on/off length is multiplied by lw).
        real(wp), intent(in) :: pattern(20)
        integer, intent(in) :: pattern_size
        real(wp), intent(in) :: line_width
        character(len=64) :: dash

        character(len=16) :: token
        integer :: i

        dash = '['
        do i = 1, pattern_size
            write(token, '(F0.3)') pattern(i) * line_width
            if (i > 1) then
                dash = trim(dash)//' '//trim(adjustl(token))
            else
                dash = trim(dash)//trim(adjustl(token))
            end if
        end do
        dash = trim(dash)//'] 0 d'
    end function format_pdf_dash_array

    module subroutine draw_pdf_text_wrapper(this, x, y, text)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: pdf_x, pdf_y

        ! Keep context in sync for text coordinate normalization
        call this%update_coord_context()
        call normalize_to_pdf_coords(this%coord_ctx, x, y, pdf_x, pdf_y)

        ! Use render_mixed_text which handles LaTeX processing and mathtext
        ! (superscripts/subscripts) properly, just like titles do
        call render_mixed_text(this%core_ctx, pdf_x, pdf_y, text)
    end subroutine draw_pdf_text_wrapper

    module subroutine draw_pdf_text_styled(this, x_pt, y_pt, text, font_size, rotation, &
                                    ha, va, bbox, color)
        use fortplot_pdf_text_metrics, only: estimate_pdf_text_width
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_pt, y_pt
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: font_size
        real(wp), intent(in) :: rotation
        character(len=*), intent(in) :: ha, va
        logical, intent(in) :: bbox
        real(wp), intent(in) :: color(3)

        real(wp) :: x0, y0
        real(wp) :: w_pt, h_pt, pad
        real(wp) :: ascent_pt, descent_pt
        real(wp) :: baseline_pt, box_bottom_pt
        character(len=256) :: cmd

        w_pt = estimate_pdf_text_width(trim(text), font_size)
        h_pt = max(1.0_wp, 1.2_wp*font_size)
        ascent_pt = 0.8_wp*h_pt
        descent_pt = 0.2_wp*h_pt

        x0 = x_pt
        select case (trim(ha))
        case ('center')
            x0 = x0-0.5_wp*w_pt
        case ('right')
            x0 = x0-w_pt
        case default
        end select

        ! Matplotlib semantics: the (x_pt, y_pt) anchor is the aligned bounding-box
        ! position, not the baseline.
        baseline_pt = y_pt
        box_bottom_pt = y_pt
        select case (trim(va))
        case ('center')
            box_bottom_pt = y_pt-0.5_wp*h_pt
            baseline_pt = box_bottom_pt+descent_pt
        case ('top')
            box_bottom_pt = y_pt-h_pt
            baseline_pt = y_pt-ascent_pt
        case ('bottom')
            box_bottom_pt = y_pt
            baseline_pt = y_pt+descent_pt
        case default
            box_bottom_pt = y_pt
            baseline_pt = y_pt
        end select
        y0 = box_bottom_pt

        if (bbox) then
            pad = max(1.0_wp, 0.2_wp*font_size)
            call this%stream_writer%add_to_stream('q')
            call this%stream_writer%add_to_stream('1 1 1 rg')
            call this%stream_writer%add_to_stream('0 0 0 RG')
            call this%stream_writer%add_to_stream('0.5 w')
            write (cmd, '(F0.3,1X,F0.3,1X,F0.3,1X,F0.3," re B")') &
                x0-pad, y0-pad, w_pt+2.0_wp*pad, h_pt+2.0_wp*pad
            call this%stream_writer%add_to_stream(trim(cmd))
            call this%stream_writer%add_to_stream('Q')
        end if

        call this%core_ctx%set_color(color(1), color(2), color(3))
        if (abs(rotation) > 1.0e-6_wp) then
            call draw_rotated_mixed_font_text(this%core_ctx, x0, baseline_pt, &
                                              trim(text), &
                                              font_size, rotation)
        else
            call render_mixed_text(this%core_ctx, x0, baseline_pt, trim(text), &
                                   font_size)
        end if
    end subroutine draw_pdf_text_styled

    module subroutine fill_quad_wrapper(this, x_quad, y_quad)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp) :: px(4), py(4)
        character(len=512) :: cmd
        integer :: i
        real(wp) :: minx, maxx, miny, maxy, eps

        call this%update_coord_context()

        ! Convert to PDF coordinates
        do i = 1, 4
            call normalize_to_pdf_coords(this%coord_ctx, x_quad(i), y_quad(i), &
                                         px(i), py(i))
        end do

        ! Check if quad is axis-aligned for potential optimization
        minx = min(min(px(1), px(2)), min(px(3), px(4)))
        maxx = max(max(px(1), px(2)), max(px(3), px(4)))
        miny = min(min(py(1), py(2)), min(py(3), py(4)))
        maxy = max(max(py(1), py(2)), max(py(3), py(4)))
        eps = 0.05_wp

        if ((abs(py(1)-py(2)) < 1.0e-6_wp .and. abs(px(2)-px(3)) < &
             1.0e-6_wp .and. &
             abs(py(3)-py(4)) < 1.0e-6_wp .and. abs(px(4)-px(1)) < &
             1.0e-6_wp)) then
            write (cmd, '(F0.3,1X,F0.3)') minx-eps, miny-eps
            call this%stream_writer%add_to_stream(trim(cmd)//' m')
            write (cmd, '(F0.3,1X,F0.3)') maxx+eps, miny-eps
            call this%stream_writer%add_to_stream(trim(cmd)//' l')
            write (cmd, '(F0.3,1X,F0.3)') maxx+eps, maxy+eps
            call this%stream_writer%add_to_stream(trim(cmd)//' l')
            write (cmd, '(F0.3,1X,F0.3)') minx-eps, maxy+eps
            call this%stream_writer%add_to_stream(trim(cmd)//' l')
            call this%stream_writer%add_to_stream('h')
            ! Use B (fill and stroke) instead of f-star to eliminate anti-aliasing gaps
            call this%stream_writer%add_to_stream('B')
        else
            write (cmd, '(F0.3,1X,F0.3)') px(1), py(1)
            call this%stream_writer%add_to_stream(trim(cmd)//' m')
            write (cmd, '(F0.3,1X,F0.3)') px(2), py(2)
            call this%stream_writer%add_to_stream(trim(cmd)//' l')
            write (cmd, '(F0.3,1X,F0.3)') px(3), py(3)
            call this%stream_writer%add_to_stream(trim(cmd)//' l')
            write (cmd, '(F0.3,1X,F0.3)') px(4), py(4)
            call this%stream_writer%add_to_stream(trim(cmd)//' l')
            call this%stream_writer%add_to_stream('h')
            ! Use B (fill and stroke) instead of f-star to eliminate anti-aliasing gaps
            call this%stream_writer%add_to_stream('B')
        end if
    end subroutine fill_quad_wrapper

    module subroutine fill_heatmap_wrapper(this, x_grid, y_grid, z_grid, z_min, z_max, colormap_name)
        class(pdf_context), intent(inout) :: this
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: z_min, z_max
        character(len=*), intent(in), optional :: colormap_name

        integer :: i, j, nx, ny, W, H
        real(wp) :: value
        real(wp), dimension(3) :: color
        integer :: idx
        integer :: out_len
        integer, allocatable :: rgb_u8(:)
        character(len=:), allocatable :: img_data
        real(wp) :: pdf_x0, pdf_y0, pdf_x1, pdf_y1, width_pt, height_pt
        real(wp) :: px_w, px_h, bleed_x, bleed_y
        character(len=256) :: cmd
        real(wp) :: v1, v2, v3

        call this%update_coord_context()

        nx = size(x_grid)
        ny = size(y_grid)

        ! Expect z_grid(ny, nx)
        if (size(z_grid, 1) /= ny .or. size(z_grid, 2) /= nx) return

        W = nx-1; H = ny-1
        if (W <= 0 .or. H <= 0) return

        ! Build RGB image with 1-pixel replicated border padding to avoid
        ! sampling outside the image at arbitrary zoom levels.
        block
            integer :: WP, HP
            integer, allocatable :: img(:, :, :)
            integer :: ii, jj, src_i, src_j
            WP = W+2; HP = H+2
            allocate (img(3, WP, HP))
            do jj = 1, HP
                do ii = 1, WP
                    src_i = max(1, min(W, ii-1))
                    src_j = max(1, min(H, jj-1))
                    value = z_grid(src_j, src_i)
                    if (present(colormap_name)) then
                        call colormap_value_to_color(value, z_min, z_max, trim(colormap_name), color)
                    else
                        call colormap_value_to_color(value, z_min, z_max, 'viridis', color)
                    end if
                    v1 = max(0.0d0, min(1.0d0, color(1)))
                    v2 = max(0.0d0, min(1.0d0, color(2)))
                    v3 = max(0.0d0, min(1.0d0, color(3)))
                    img(1, ii, jj) = int(nint(v1*255.0d0), kind=4)
                    img(2, ii, jj) = int(nint(v2*255.0d0), kind=4)
                    img(3, ii, jj) = int(nint(v3*255.0d0), kind=4)
                end do
            end do
            allocate (rgb_u8(WP*HP*3))
            idx = 1
            do j = 1, HP
                do i = 1, WP
                    rgb_u8(idx) = img(1, i, j); idx = idx+1
                    rgb_u8(idx) = img(2, i, j); idx = idx+1
                    rgb_u8(idx) = img(3, i, j); idx = idx+1
                end do
            end do
            W = WP; H = HP
        end block

        block
            use, intrinsic :: iso_fortran_env, only: int8
            integer(int8), allocatable :: in_bytes(:), out_bytes(:)
            integer :: k, n
            n = size(rgb_u8)
            allocate (in_bytes(n))
            do k = 1, n
                in_bytes(k) = int(iand(rgb_u8(k), 255))
            end do
            call zlib_compress_into(in_bytes, n, out_bytes, out_len)
            img_data = repeat(' ', out_len)
            do k = 1, out_len
                img_data(k:k) = achar(iand(int(out_bytes(k), kind=4), 255))
            end do
        end block

        ! Align placement to the exact PDF plot area (consistent with PNG backend)
        pdf_x0 = real(this%coord_ctx%plot_area%left, wp)
        pdf_y0 = real(this%coord_ctx%plot_area%bottom, wp)
        width_pt = real(this%coord_ctx%plot_area%width, wp)
        height_pt = real(this%coord_ctx%plot_area%height, wp)

        ! Compute a half-pixel bleed in user-space and clip to the exact plot area
        px_w = width_pt/real(W, wp)
        px_h = height_pt/real(H, wp)
        bleed_x = 0.5_wp*px_w
        bleed_y = 0.5_wp*px_h

        call this%stream_writer%add_to_stream('q')
        ! Clip to the exact target rectangle to keep padded borders inside
        write (cmd, '(F0.12,1X,F0.12,1X,F0.12,1X,F0.12,1X,A)') pdf_x0, pdf_y0, &
            width_pt, height_pt, ' re W n'
        call this%stream_writer%add_to_stream(trim(cmd))
        ! Compute pixel scale and place padded image so that the extra 1px ring
        ! lies just outside the clip region
        px_w = width_pt/real(W-2, wp)
        px_h = height_pt/real(H-2, wp)
        write (cmd, '(F0.12,1X,F0.12,1X,F0.12,1X,F0.12,1X,F0.12,1X,F0.12,1X,A)') &
            px_w*real(W, wp), 0.0_wp, 0.0_wp, -(px_h*real(H, wp)), &
            pdf_x0-px_w, (pdf_y0+height_pt)+px_h, ' cm'
        call this%stream_writer%add_to_stream(trim(cmd))
        ! Place image XObject instead of inline image
        call this%core_ctx%set_image(W, H, img_data)
        call this%stream_writer%add_to_stream('/Im1 Do')
        call this%stream_writer%add_to_stream('Q')
    end subroutine fill_heatmap_wrapper

    module subroutine draw_pdf_marker_wrapper(this, x, y, style, size)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp), intent(in), optional :: size

        call this%update_coord_context()
        call draw_pdf_marker_at_coords(this%coord_ctx, this%stream_writer, x, y, &
                                       style, size)
    end subroutine draw_pdf_marker_wrapper

    module subroutine set_marker_colors_wrapper(this, edge_r, edge_g, edge_b, face_r, &
                                         face_g, face_b)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, face_r, face_g, face_b
        call this%stream_writer%set_marker_gstate('')
        call pdf_set_marker_colors(this%stream_writer, edge_r, edge_g, edge_b, &
                                   face_r, face_g, face_b)
    end subroutine set_marker_colors_wrapper

    module subroutine set_marker_colors_with_alpha_wrapper(this, edge_r, edge_g, edge_b, &
                                                    edge_alpha, &
                                                    face_r, face_g, face_b, &
                                                    face_alpha)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        character(len=32) :: gstate_name

        call this%core_ctx%register_extgstate(edge_alpha, face_alpha, gstate_name)
        call pdf_set_marker_colors_with_alpha(this%stream_writer, edge_r, edge_g, &
                                              edge_b, edge_alpha, face_r, face_g, &
                                              face_b, face_alpha, gstate_name)
    end subroutine set_marker_colors_with_alpha_wrapper

    module subroutine draw_pdf_arrow_wrapper(this, x, y, dx, dy, size, style)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style

        call this%update_coord_context()
        call draw_pdf_arrow_at_coords(this%coord_ctx, this%stream_writer, x, y, dx, &
                                      dy, size, style)
    end subroutine draw_pdf_arrow_wrapper

    module subroutine draw_pdf_arrowhead_wrapper(this, x, y, dx, dy, size, style)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style

        call this%update_coord_context()
        call draw_pdf_arrowhead_at_coords(this%coord_ctx, this%stream_writer, x, y, &
                                          dx, dy, size, style)
    end subroutine draw_pdf_arrowhead_wrapper

    module function pdf_get_ascii_output(this) result(output)
        class(pdf_context), intent(in) :: this
        character(len=:), allocatable :: output
        output = "PDF output (non-ASCII format)"
    end function pdf_get_ascii_output

end submodule fortplot_pdf_draw
