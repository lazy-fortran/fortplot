submodule (fortplot_pdf) fortplot_pdf_draw

    !! PDF drawing, text rendering, fill operations, and markers
    !!
    !! Single Responsibility: Handle all drawing operations including lines,
    !! text, fills, markers, and arrows.

    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use fortplot_segment_clip, only: clip_segment
    use fortplot_polygon_clip, only: clip_polygon, clip_polygon_capacity
    implicit none

contains

    module subroutine draw_pdf_line(this, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: pdf_x1, pdf_y1, pdf_x2, pdf_y2
        real(wp) :: c_x1, c_y1, c_x2, c_y2, t_start, margin
        logical :: visible
        ! Ensure coordinate context reflects latest figure ranges and plot area
        call this%update_coord_context()

        ! Skip drawing if any coordinate is NaN (disconnected line segments)
        if (ieee_is_nan(x1) .or. ieee_is_nan(y1) .or. &
            ieee_is_nan(x2) .or. ieee_is_nan(y2)) then
            return
        end if

        call normalize_to_pdf_coords(this%coord_ctx, x1, y1, pdf_x1, pdf_y1)
        call normalize_to_pdf_coords(this%coord_ctx, x2, y2, pdf_x2, pdf_y2)
        ! Emit only the part on the page: a segment reaching 1e21 points
        ! makes viewers (and pdftoppm) dash or rasterise it forever.
        margin = this%stream_writer%current_state%line_width + 2.0_wp
        call clip_segment(pdf_x1, pdf_y1, pdf_x2, pdf_y2, [-margin, -margin], &
                          [real(this%coord_ctx%width, wp) + margin, &
                           real(this%coord_ctx%height, wp) + margin], &
                          c_x1, c_y1, c_x2, c_y2, t_start, visible)
        if (.not. visible) return
        call this%stream_writer%draw_vector_line(c_x1, c_y1, c_x2, c_y2)
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
        real(wp) :: dx, dy, theta
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
            ! Alignment offsets live in the text frame: rotate them with the
            ! text so a centred vertical label stays centred on its anchor.
            dx = x0 - x_pt
            dy = baseline_pt - y_pt
            theta = rotation*acos(-1.0_wp)/180.0_wp
            call draw_rotated_mixed_font_text(this%core_ctx, &
                                              x_pt + dx*cos(theta) - dy*sin(theta), &
                                              y_pt + dx*sin(theta) + dy*cos(theta), &
                                              trim(text), font_size, rotation)
        else
            call render_mixed_text(this%core_ctx, x0, baseline_pt, trim(text), &
                                   font_size)
        end if
    end subroutine draw_pdf_text_styled

    module subroutine fill_quad_wrapper(this, x_quad, y_quad)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp) :: px(4), py(4), lo(2), hi(2)
        real(wp) :: cx(clip_polygon_capacity(4)), cy(clip_polygon_capacity(4))
        character(len=512) :: cmd
        integer :: i, m
        real(wp) :: eps

        call this%update_coord_context()

        ! Convert to PDF coordinates
        do i = 1, 4
            call normalize_to_pdf_coords(this%coord_ctx, x_quad(i), y_quad(i), &
                                         px(i), py(i))
        end do

        ! Emit only the part on the page (grown by a few points): vertices
        ! at ~1e21 pt are dropped or mis-filled by viewers and pdftoppm.
        lo = [-2.0_wp, -2.0_wp]
        hi = [real(this%coord_ctx%width, wp), &
              real(this%coord_ctx%height, wp)] + 2.0_wp
        call clip_polygon(px, py, lo, hi, cx, cy, m)
        if (m == 0) return

        eps = 0.05_wp
        if ((abs(py(1)-py(2)) < 1.0e-6_wp .and. abs(px(2)-px(3)) < &
             1.0e-6_wp .and. &
             abs(py(3)-py(4)) < 1.0e-6_wp .and. abs(px(4)-px(1)) < &
             1.0e-6_wp)) then
            ! Axis-aligned: its clip is the bounding box of the clipped polygon.
            lo = [minval(cx(1:m)), minval(cy(1:m))] - eps
            hi = [maxval(cx(1:m)), maxval(cy(1:m))] + eps
            m = 4
            cx(1:4) = [lo(1), hi(1), hi(1), lo(1)]
            cy(1:4) = [lo(2), lo(2), hi(2), hi(2)]
        end if
        write (cmd, '(F0.3,1X,F0.3)') cx(1), cy(1)
        call this%stream_writer%add_to_stream(trim(cmd)//' m')
        do i = 2, m
            write (cmd, '(F0.3,1X,F0.3)') cx(i), cy(i)
            call this%stream_writer%add_to_stream(trim(cmd)//' l')
        end do
        call this%stream_writer%add_to_stream('h')
        ! Use B (fill and stroke) instead of f-star to eliminate anti-aliasing gaps
        call this%stream_writer%add_to_stream('B')
    end subroutine fill_quad_wrapper

    module subroutine fill_heatmap_wrapper(this, x_grid, y_grid, z_grid, &
                                            z_min, z_max, colormap_name)
        class(pdf_context), intent(inout) :: this
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: z_min, z_max
        character(len=*), intent(in), optional :: colormap_name

        integer :: nx, ny
        real(wp) :: x0, y0, x1, y1, dx, dy
        character(len=256) :: cmd
        character(len=:), allocatable :: img_data
        character(len=20) :: cmap

        nx = size(z_grid, 2)
        ny = size(z_grid, 1)
        if (size(x_grid) == nx .and. size(y_grid) == ny) then
            ! Retain the nodal-grid contract for existing heatmap callers.
            nx = nx - 1
            ny = ny - 1
        else if (size(x_grid) /= nx + 1 .or. size(y_grid) /= ny + 1) then
            return
        end if
        if (nx <= 0 .or. ny <= 0) return
        cmap = 'viridis'
        if (present(colormap_name)) cmap = colormap_name
        call this%update_coord_context()
        call this%stream_writer%add_to_stream('q')
        call clip_pdf_heatmap(this)
        call normalize_to_pdf_coords(this%coord_ctx, x_grid(1), y_grid(1), &
                                     x0, y0)
        call normalize_to_pdf_coords(this%coord_ctx, x_grid(nx + 1), &
                                     y_grid(ny + 1), x1, y1)
        ! One image cannot be placed beyond the PDF real-number range
        ! (+-32767); a mesh reaching that far is drawn as clipped cells.
        if (.not. uniform_mesh_edges(x_grid) .or. &
            .not. uniform_mesh_edges(y_grid) .or. &
            .not. all(abs([x0, y0, x1, y1]) < 32767.0_wp)) then
            call fill_pdf_mesh_cells(this, x_grid, y_grid, z_grid(:ny, :nx), &
                                     z_min, z_max, cmap)
        else
            dx = (x1 - x0)/real(nx, wp)
            dy = (y1 - y0)/real(ny, wp)
            ! Clip padding to the mesh extent, including meshes inside wider axes.
            write (cmd, '(4(F0.12,1X),A)') min(x0, x1), min(y0, y1), &
                abs(x1 - x0), abs(y1 - y0), 're W n'
            call this%stream_writer%add_to_stream(trim(cmd))
            write (cmd, '(6(F0.12,1X),A)') dx*real(nx + 2, wp), 0.0_wp, &
                0.0_wp, -dy*real(ny + 2, wp), x0 - dx, y1 + dy, 'cm'
            call this%stream_writer%add_to_stream(trim(cmd))
            call pdf_mesh_image(z_grid(:ny, :nx), z_min, z_max, cmap, img_data)
            call this%core_ctx%set_image(nx + 2, ny + 2, img_data)
            call this%stream_writer%add_to_stream('/Im1 Do')
        end if
        call this%stream_writer%add_to_stream('Q')
    end subroutine fill_heatmap_wrapper

    logical function uniform_mesh_edges(edges) result(uniform)
        real(wp), intent(in) :: edges(:)
        real(wp) :: step, tolerance
        integer :: i

        uniform = .true.
        if (size(edges) < 3) return
        step = (edges(size(edges)) - edges(1))/real(size(edges) - 1, wp)
        tolerance = 64.0_wp*epsilon(step)*max(1.0_wp, maxval(abs(edges)))
        do i = 2, size(edges)
            if (abs(edges(i) - edges(i - 1) - step) > tolerance) then
                uniform = .false.
                return
            end if
        end do
    end function uniform_mesh_edges

    module subroutine pdf_begin_plot_clip(this)
        !! Save the graphics state and clip to the axes rectangle.
        class(pdf_context), intent(inout) :: this

        if (this%clip_active) return
        call this%update_coord_context()
        call this%stream_writer%add_to_stream('q')
        call clip_pdf_heatmap(this)
        this%clip_saved_state = this%stream_writer%current_state
        this%clip_active = .true.
    end subroutine pdf_begin_plot_clip

    module subroutine pdf_end_plot_clip(this)
        !! Pop the clip; Q also restores colour and width, so the writer's
        !! cached state must follow it.
        class(pdf_context), intent(inout) :: this

        if (.not. this%clip_active) return
        call this%stream_writer%add_to_stream('Q')
        this%stream_writer%current_state = this%clip_saved_state
        this%clip_active = .false.
    end subroutine pdf_end_plot_clip

    subroutine clip_pdf_heatmap(this)
        class(pdf_context), intent(inout) :: this
        character(len=256) :: cmd

        associate (area => this%coord_ctx%plot_area)
            write (cmd, '(4(F0.12,1X),A)') real(area%left, wp), &
                real(area%bottom, wp), real(area%width, wp), &
                real(area%height, wp), 're W n'
        end associate
        call this%stream_writer%add_to_stream(trim(cmd))
    end subroutine clip_pdf_heatmap

    subroutine fill_pdf_mesh_cells(this, x, y, z, z_min, z_max, cmap)
        !! Unequal cell widths cannot be represented by one affine image.
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x(:), y(:), z(:, :), z_min, z_max
        character(len=*), intent(in) :: cmap
        real(wp) :: x0, y0, x1, y1, color(3), lo(2), hi(2)
        character(len=256) :: cmd
        integer :: i, j

        ! Cells are clipped to the page grown by a few points, so cells far
        ! off the page cost nothing and huge edges stay in PDF range.
        lo = [-2.0_wp, -2.0_wp]
        hi = [real(this%coord_ctx%width, wp), &
              real(this%coord_ctx%height, wp)] + 2.0_wp
        do i = 1, size(z, 2)
            do j = 1, size(z, 1)
                call normalize_to_pdf_coords(this%coord_ctx, x(i), y(j), x0, y0)
                call normalize_to_pdf_coords(this%coord_ctx, x(i + 1), &
                                             y(j + 1), x1, y1)
                if (.not. all(abs([x0, y0, x1, y1]) <= huge(1.0_wp))) cycle
                if (max(x0, x1) < lo(1) .or. min(x0, x1) > hi(1) .or. &
                    max(y0, y1) < lo(2) .or. min(y0, y1) > hi(2)) cycle
                x0 = min(max(x0, lo(1)), hi(1)); x1 = min(max(x1, lo(1)), hi(1))
                y0 = min(max(y0, lo(2)), hi(2)); y1 = min(max(y1, lo(2)), hi(2))
                call colormap_value_to_color(z(j, i), z_min, z_max, cmap, color)
                write (cmd, '(3(F0.6,1X),A)') color, 'rg'
                call this%stream_writer%add_to_stream(trim(cmd))
                ! Fill only: an implicit stroke changes both widths and colors.
                write (cmd, '(4(F0.12,1X),A)') min(x0, x1), min(y0, y1), &
                    abs(x1 - x0), abs(y1 - y0), 're f'
                call this%stream_writer%add_to_stream(trim(cmd))
            end do
        end do
    end subroutine fill_pdf_mesh_cells

    subroutine pdf_mesh_image(z, z_min, z_max, cmap, image_data)
        !! Encode every cell with a replicated one-pixel border for PDF viewers.
        use, intrinsic :: iso_fortran_env, only: int8
        real(wp), intent(in) :: z(:, :), z_min, z_max
        character(len=*), intent(in) :: cmap
        character(len=:), allocatable, intent(out) :: image_data
        integer(int8), allocatable :: input_bytes(:), output_bytes(:)
        real(wp) :: color(3)
        integer :: nx, ny, i, j, k, offset, output_length

        nx = size(z, 2)
        ny = size(z, 1)
        allocate (input_bytes(3*(nx + 2)*(ny + 2)))
        offset = 1
        do j = 0, ny + 1
            do i = 0, nx + 1
                call colormap_value_to_color(z(max(1, min(ny, j)), &
                    max(1, min(nx, i))), z_min, z_max, cmap, color)
                do k = 1, 3
                    input_bytes(offset) = int(nint(255.0_wp* &
                        max(0.0_wp, min(1.0_wp, color(k)))), int8)
                    offset = offset + 1
                end do
            end do
        end do
        call zlib_compress_into(input_bytes, size(input_bytes), output_bytes, &
                                output_length)
        image_data = repeat(' ', output_length)
        do k = 1, output_length
            image_data(k:k) = achar(iand(int(output_bytes(k)), 255))
        end do
    end subroutine pdf_mesh_image

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
