submodule (fortplot_raster) fortplot_raster_impl
    !! Implementation of drawing primitives for raster_context.
    !! Contains: line, color, line_width, line_style, marker, arrow.

    use fortplot_raster_core, only: pt2px
    use fortplot_3d_axes, only: draw_3d_axes

contains

    !! ── Drawing primitives ─────────────────────────────────────────────

    module subroutine raster_draw_line(this, x1, y1, x2, y2)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: px1, py1, px2, py2
        ! Transform coordinates to plot area (like matplotlib)
        ! Note: Raster Y=0 at top, so we need to flip Y coordinates
        px1 = (x1 - this%x_min)/(this%x_max - this%x_min)* &
              real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py1 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y1 - this%y_min)/(this%y_max - &
                                 this%y_min)*real(this%plot_area%height, wp)
        px2 = (x2 - this%x_min)/(this%x_max - this%x_min)* &
              real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py2 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y2 - this%y_min)/(this%y_max - &
                                 this%y_min)*real(this%plot_area%height, wp)

        ! Draw line with pattern support
        call draw_styled_line(this%raster%image_data, this%width, this%height, &
                              px1, py1, px2, py2, &
                              this%raster%current_r, this%raster%current_g, &
                              this%raster%current_b, &
                              this%raster%current_line_width, &
                              this%raster%line_style, this%raster%line_pattern, &
                              this%raster%pattern_size, this%raster%pattern_length, &
                              this%raster%pattern_distance)
    end subroutine raster_draw_line

    module subroutine raster_set_color_context(this, r, g, b)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        call this%raster%set_color(r, g, b)
    end subroutine raster_set_color_context

    module subroutine raster_set_line_width(this, width)
        !! Set line width for raster drawing with DPI-aware point-to-pixel scaling.
        !! Input width is in points (1pt = 1/72 inch). Converted to pixels via DPI.
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: width
        real(wp) :: px

        if (width <= 0.0_wp) then
            this%raster%current_line_width = 1.0_wp
        else
            px = pt2px(width, this%raster%dpi)
            this%raster%current_line_width = max(1.0_wp, px)
        end if
    end subroutine raster_set_line_width

    module subroutine raster_set_line_style_context(this, style)
        !! Set line style for raster context
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: style

        call this%raster%set_line_style(style)
    end subroutine raster_set_line_style_context

    !! ── Marker drawing ─────────────────────────────────────────────────

    module subroutine raster_draw_marker(this, x, y, style)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp) :: px, py
        integer(1) :: r, g, b
        associate (dsl => len_trim(style)); end associate

        ! Transform coordinates to plot area
        px = (x - this%x_min)/(this%x_max - this%x_min)*real(this%plot_area%width, wp) &
             + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min)/(this%y_max - this%y_min)*real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)

        ! Fixed marker centering for Issue #333: align markers with line centers
        ! Apply sub-pixel adjustment to match line drawing coordinate conventions
        call draw_raster_marker_by_style(this, px - 0.5_wp, py - 0.5_wp, style)
    end subroutine raster_draw_marker

    module subroutine draw_raster_marker_by_style(this, px, py, style)
        !! Draw marker using shared style dispatch logic (DRY compliance)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: px, py
        character(len=*), intent(in) :: style
        real(wp) :: marker_size

        marker_size = get_marker_size(style) * this%raster%dpi / REFERENCE_DPI

        select case (trim(style))
        case (MARKER_CIRCLE)
            call draw_circle_with_edge_face(this%raster%image_data, this%width, &
                                            this%height, px, py, marker_size, &
                                            this%raster%marker_edge_r, &
                                            this%raster%marker_edge_g, &
                                            this%raster%marker_edge_b, &
                                            this%raster%marker_edge_alpha, &
                                            this%raster%marker_face_r, &
                                            this%raster%marker_face_g, &
                                            this%raster%marker_face_b, &
                                            this%raster%marker_face_alpha, &
                                            this%raster%current_line_width)
        case (MARKER_SQUARE)
            call draw_square_with_edge_face(this%raster%image_data, this%width, &
                                            this%height, px, py, marker_size, &
                                            this%raster%marker_edge_r, &
                                            this%raster%marker_edge_g, &
                                            this%raster%marker_edge_b, &
                                            this%raster%marker_edge_alpha, &
                                            this%raster%marker_face_r, &
                                            this%raster%marker_face_g, &
                                            this%raster%marker_face_b, &
                                            this%raster%marker_face_alpha, &
                                            this%raster%current_line_width)
        case (MARKER_DIAMOND)
            call draw_diamond_with_edge_face(this%raster%image_data, this%width, &
                                             this%height, px, py, marker_size, &
                                             this%raster%marker_edge_r, &
                                             this%raster%marker_edge_g, &
                                             this%raster%marker_edge_b, &
                                             this%raster%marker_edge_alpha, &
                                             this%raster%marker_face_r, &
                                             this%raster%marker_face_g, &
                                             this%raster%marker_face_b, &
                                             this%raster%marker_face_alpha, &
                                             this%raster%current_line_width)
        case (MARKER_CROSS)
            call draw_x_marker(this%raster%image_data, this%width, &
                               this%height, px, &
                               py, marker_size, &
                               this%raster%marker_edge_r, &
                               this%raster%marker_edge_g, &
                               this%raster%marker_edge_b, &
                               this%raster%marker_edge_alpha, &
                               this%raster%current_line_width)
        end select
    end subroutine draw_raster_marker_by_style

    module subroutine raster_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, &
                                        face_g, face_b)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b

        this%raster%marker_edge_r = edge_r
        this%raster%marker_edge_g = edge_g
        this%raster%marker_edge_b = edge_b
        this%raster%marker_edge_alpha = 1.0_wp  ! Default to opaque
        this%raster%marker_face_r = face_r
        this%raster%marker_face_g = face_g
        this%raster%marker_face_b = face_b
        this%raster%marker_face_alpha = 1.0_wp  ! Default to opaque
    end subroutine raster_set_marker_colors

    module subroutine raster_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, &
                                                       edge_alpha, &
                                                       face_r, face_g, face_b, face_alpha)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha

        this%raster%marker_edge_r = edge_r
        this%raster%marker_edge_g = edge_g
        this%raster%marker_edge_b = edge_b
        this%raster%marker_edge_alpha = edge_alpha
        this%raster%marker_face_r = face_r
        this%raster%marker_face_g = face_g
        this%raster%marker_face_b = face_b
        this%raster%marker_face_alpha = face_alpha
    end subroutine raster_set_marker_colors_with_alpha

    !! ── Arrow drawing ──────────────────────────────────────────────────

    module subroutine raster_draw_arrow(this, x, y, dx, dy, size, style)
        !! Draw arrow head for streamplot arrows in raster backend
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style

        real(wp) :: arrow_length, arrow_width, norm_dx, norm_dy
        real(wp) :: perp_x, perp_y  ! Perpendicular vector for arrow width
        real(wp) :: x1, y1, x2, y2, x3, y3  ! Triangle vertices
        real(wp) :: shaft_base_x, shaft_base_y
        real(wp) :: magnitude
        integer(1) :: r, g, b
        real(wp) :: px, py, ddx, ddy
        associate (dsl => len_trim(style)); end associate

        ! Transform data coordinates to pixel coordinates
        px = (x - this%x_min)/(this%x_max - this%x_min)*real(this%plot_area%width, wp) &
             + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min)/(this%y_max - this%y_min)*real(this%plot_area%height, wp)

        ! Scale direction components to pixel units (account for inverted Y)
        ddx = dx*(real(this%plot_area%width, wp)/max(1.0_wp, (this%x_max - this%x_min)))
        ddy = -dy*(real(this%plot_area%height, wp)/max(1.0_wp, (this%y_max - &
                                                                this%y_min)))

        ! Normalize direction vector
        magnitude = sqrt(ddx*ddx + ddy*ddy)
        if (magnitude < 1e-10_wp) return  ! Avoid division by zero

        norm_dx = ddx/magnitude
        norm_dy = ddy/magnitude

        ! Calculate arrow dimensions based on size
        arrow_length = size*8.0_wp  ! Scale factor for reasonable arrow size
        arrow_width = arrow_length*0.5_wp

        ! Calculate perpendicular vector for arrow width
        perp_x = -norm_dy
        perp_y = norm_dx

        ! Calculate triangle vertices for arrow head (tip at pixel coords)
        x1 = px
        y1 = py

        ! Base vertices of arrow head
        x2 = px - arrow_length*norm_dx + arrow_width*perp_x
        y2 = py - arrow_length*norm_dy + arrow_width*perp_y

        x3 = px - arrow_length*norm_dx - arrow_width*perp_x
        y3 = py - arrow_length*norm_dy - arrow_width*perp_y

        ! Shaft base: vector start = tip - magnitude*norm (shaft = magnitude - arrow_length)
        shaft_base_x = px - magnitude*norm_dx
        shaft_base_y = py - magnitude*norm_dy

        ! Get current color for filling
        call this%raster%get_color_bytes(r, g, b)

        call draw_line_distance_aa(this%raster%image_data, this%width, this%height, &
                                   shaft_base_x, shaft_base_y, px, py, &
                                   real(r, wp)/255.0_wp, real(g, wp)/255.0_wp, &
                                   real(b, wp)/255.0_wp, 1.0_wp)

        ! Draw filled triangle arrow head
        call fill_triangle(this%raster%image_data, this%width, this%height, &
                           x1, y1, x2, y2, x3, y3, r, g, b)

        ! Mark that arrows have been rendered
        this%has_rendered_arrows = .true.
        this%uses_vector_arrows = .false.
        this%has_triangular_arrows = .true.
    end subroutine raster_draw_arrow

end submodule fortplot_raster_impl
