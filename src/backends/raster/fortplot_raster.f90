module fortplot_raster
    !! Main raster plotting context extracted into specialized modules
    use iso_c_binding
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_constants, only: EPSILON_COMPARE
    use fortplot_bitmap, only: composite_bitmap_to_raster_0, get_text_bitmap_metrics, &
                               render_text_to_bitmap_with_size, &
                               rotate_bitmap_about_anchor
    use fortplot_text, only: render_text_to_image, render_text_with_size, &
                             calculate_text_width, calculate_text_width_with_size, &
                             calculate_text_height
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_text_helpers, only: prepare_mathtext_if_needed
    use fortplot_unicode, only: escape_unicode_for_raster
    use fortplot_logging, only: log_error
    use fortplot_errors, only: fortplot_error_t, ERROR_INTERNAL
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, &
                                MARKER_DIAMOND, MARKER_CROSS
    use fortplot_raster_drawing, only: draw_line_distance_aa, blend_pixel, &
                                       distance_point_to_line_segment, &
                                       ipart, fpart, rfpart, color_to_byte, &
                                       draw_circle_antialiased, &
                                       draw_circle_outline_antialiased, &
                                       draw_circle_with_edge_face, &
                                       draw_square_with_edge_face, &
                                       draw_diamond_with_edge_face, draw_x_marker
    use fortplot_raster_line_styles, only: draw_styled_line, reset_pattern_distance, &
                                           set_raster_line_style
    use fortplot_raster_core, only: raster_image_t, create_raster_image, &
                                    destroy_raster_image
    use fortplot_raster_axes, only: raster_draw_axes_and_labels, raster_render_ylabel, &
                                    raster_draw_axes_lines_and_ticks, &
                                    raster_draw_axis_labels_only
    use fortplot_raster_labels, only: raster_draw_axis_labels
    use fortplot_raster_rendering, only: raster_fill_heatmap, raster_fill_quad, &
                                         fill_triangle, &
                                         raster_render_legend_specialized, &
                                         raster_calculate_legend_dimensions, &
                                         raster_set_legend_border_width, &
                                         raster_calculate_legend_position, &
                                         raster_extract_rgb_data, raster_get_png_data
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: raster_image_t, create_raster_image, destroy_raster_image
    public :: raster_context, create_raster_canvas
    public :: raster_draw_axes_and_labels, raster_render_ylabel
    public :: raster_draw_axes_lines_and_ticks, raster_draw_axis_labels_only

    integer, parameter :: DEFAULT_RASTER_LINE_WIDTH_SCALING = 10

    ! Raster plotting context - backend-agnostic bitmap operations
    type, extends(plot_context) :: raster_context
        type(raster_image_t) :: raster
        type(plot_margins_t) :: margins  ! Common margin functionality
        type(plot_area_t) :: plot_area
        character(len=16) :: last_xscale = 'linear'
        character(len=16) :: last_yscale = 'linear'
        real(wp) :: last_symlog_threshold = 1.0_wp
    contains
        procedure :: line => raster_draw_line
        procedure :: color => raster_set_color_context
        procedure :: text => raster_draw_text
        procedure :: draw_text_styled => raster_draw_text_styled
        procedure :: set_line_width => raster_set_line_width
        procedure :: set_line_style => raster_set_line_style_context
        procedure :: save => raster_save_dummy
        procedure :: draw_marker => raster_draw_marker
        procedure :: set_marker_colors => raster_set_marker_colors
        procedure :: set_marker_colors_with_alpha => raster_set_marker_colors_with_alpha
        procedure :: fill_quad => raster_fill_quad_context
        procedure :: draw_arrow => raster_draw_arrow
        procedure :: get_ascii_output => raster_get_ascii_output
        ! Polymorphic methods to eliminate SELECT TYPE
        procedure :: get_width_scale => raster_get_width_scale
        procedure :: get_height_scale => raster_get_height_scale
        procedure :: fill_heatmap => raster_fill_heatmap_context
        procedure :: render_legend_specialized => &
            raster_render_legend_specialized_context
        procedure :: calculate_legend_dimensions => &
            raster_calculate_legend_dimensions_context
        procedure :: set_legend_border_width => raster_set_legend_border_width_context
        procedure :: calculate_legend_position_backend => &
            raster_calculate_legend_position_context
        procedure :: extract_rgb_data => raster_extract_rgb_data_context
        procedure :: get_png_data_backend => raster_get_png_data_context
        procedure :: prepare_3d_data => raster_prepare_3d_data_context
        procedure :: render_ylabel => raster_render_ylabel_context
        procedure :: draw_axes_and_labels_backend => raster_draw_axes_and_labels_context
        procedure :: draw_axes_lines_and_ticks => &
            raster_draw_axes_lines_and_ticks_context
        procedure :: draw_axis_labels_only => raster_draw_axis_labels_only_context
        procedure :: save_coordinates => raster_save_coordinates
        procedure :: set_coordinates => raster_set_coordinates
        procedure :: render_axes => raster_render_axes
    end type raster_context

contains
    function create_raster_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(raster_context) :: ctx

        call setup_canvas(ctx, width, height)

        ctx%raster = create_raster_image(width, height)
        ctx%margins = plot_margins_t()  ! matplotlib-style margins
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
    end function create_raster_canvas

    subroutine raster_draw_line(this, x1, y1, x2, y2)
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

    subroutine raster_set_color_context(this, r, g, b)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        call this%raster%set_color(r, g, b)
    end subroutine raster_set_color_context

    subroutine raster_set_line_width(this, width)
        !! Set line width for raster drawing with proper pixel scaling
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: width

        ! Map line width to pixel thickness with reasonable scaling
        ! Use linear scaling: 1 point = 1 pixel for good visibility
        if (width <= 0.0_wp) then
            this%raster%current_line_width = 1.0_wp  ! Minimum visible width
        else if (width >= 10.0_wp) then
            this%raster%current_line_width = 10.0_wp  ! Maximum reasonable width
        else
            this%raster%current_line_width = width  ! Direct 1:1 mapping
        end if
    end subroutine raster_set_line_width

    subroutine raster_set_line_style_context(this, style)
        !! Set line style for raster context
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: style

        call this%raster%set_line_style(style)
    end subroutine raster_set_line_style_context

    subroutine raster_draw_text(this, x, y, text)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: px, py
        integer(1) :: r, g, b
        character(len=500) :: processed_text, escaped_text
        character(len=600) :: math_ready
        integer :: processed_len, math_len
        ! Process LaTeX commands
        call process_latex_in_text(text, processed_text, processed_len)
        ! Ensure mathtext gets parsed if superscripts/subscripts are present
        call prepare_mathtext_if_needed(processed_text(1:processed_len), &
                                        math_ready, math_len)
        ! Pass through Unicode (STB supports it)
        call escape_unicode_for_raster(math_ready(1:math_len), escaped_text)

        ! Transform coordinates to plot area (like matplotlib)
        ! Note: Raster Y=0 at top, so we need to flip Y coordinates
        px = (x - this%x_min)/(this%x_max - this%x_min)*real(this%plot_area%width, wp) &
             + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min)/(this%y_max - this%y_min)*real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)
        call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                  int(px), int(py), trim(escaped_text), r, g, b)
    end subroutine raster_draw_text

    subroutine raster_draw_text_styled(this, x_px, y_px, text, pixel_height, &
                                       rotation, ha, va, bbox, color)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_px, y_px
        character(len=*), intent(in) :: text
        real(wp), intent(in) :: pixel_height
        real(wp), intent(in) :: rotation
        character(len=*), intent(in) :: ha, va
        logical, intent(in) :: bbox
        real(wp), intent(in) :: color(3)

        integer :: x0, y0
        integer :: text_w, text_h
        integer(1) :: r, g, b
        real(wp) :: w_px, h_px
        real(wp) :: pad
        real(wp) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp) :: cr, cg, cb
        real(wp) :: ascent_px, descent_px
        integer :: baseline_y
        integer(1), allocatable :: text_bitmap(:, :, :)
        integer(1), allocatable :: rotated_bitmap(:, :, :)
        integer :: rot_w, rot_h
        real(wp) :: ax_src, ay_src
        real(wp) :: ax_dst, ay_dst
        logical :: have_metrics

        cr = max(0.0_wp, min(1.0_wp, color(1)))
        cg = max(0.0_wp, min(1.0_wp, color(2)))
        cb = max(0.0_wp, min(1.0_wp, color(3)))
        r = int(nint(cr*255.0_wp), kind=1)
        g = int(nint(cg*255.0_wp), kind=1)
        b = int(nint(cb*255.0_wp), kind=1)

        text_w = calculate_text_width_with_size(trim(text), pixel_height)
        text_h = max(1, int(pixel_height))
        ascent_px = 0.0_wp
        descent_px = 0.0_wp
        have_metrics = .false.
        call get_text_bitmap_metrics(pixel_height, ascent_px, descent_px, text_h, &
                                     have_metrics)
        if (.not. have_metrics) then
            text_h = max(1, int(1.2_wp*pixel_height))
            descent_px = 0.25_wp*pixel_height
        end if
        baseline_y = max(0, text_h - int(abs(descent_px)))
        w_px = real(text_w, wp)
        h_px = real(text_h, wp)

        ax_src = 0.0_wp
        select case (trim(ha))
        case ('center')
            ax_src = 0.5_wp*w_px
        case ('right')
            ax_src = w_px
        case default
            ax_src = 0.0_wp
        end select

        ay_src = 0.0_wp
        select case (trim(va))
        case ('center')
            ay_src = 0.5_wp*h_px
        case ('bottom')
            ay_src = h_px
        case ('top')
            ay_src = 0.0_wp
        case default
            ay_src = 0.0_wp
        end select

        if (bbox) then
            pad = max(2.0_wp, 0.2_wp*pixel_height)
        else
            pad = 0.0_wp
        end if

        if (abs(rotation) > 1.0e-6_wp) then
            allocate (text_bitmap(text_w, text_h, 3))
            text_bitmap = -1_1
            call render_text_to_bitmap_with_size(text_bitmap, text_w, text_h, 0, &
                                                 baseline_y, trim(text), r, g, b, &
                                                 pixel_height)

            ! `rotate_bitmap_about_anchor` uses the bitmap's pixel index coordinates
            ! (x right, y down). Passing `rotation` directly matches the 90-degree
            ! rotation behavior used for axis labels and aligns with Matplotlib's
            ! CCW-positive convention for `rotation`.
            call rotate_bitmap_about_anchor(text_bitmap, text_w, text_h, rotation, &
                                            ax_src, ay_src, rotated_bitmap, rot_w, &
                                            rot_h, ax_dst, ay_dst)

            x0 = int(nint(x_px - ax_dst))
            y0 = int(nint(y_px - ay_dst))

            if (bbox) then
                x1 = real(x0, wp) - pad
                y1 = real(y0, wp) - pad
                x2 = real(x0 + rot_w, wp) + pad
                y2 = y1
                x3 = x2
                y3 = real(y0 + rot_h, wp) + pad
                x4 = x1
                y4 = y3

                call fill_triangle(this%raster%image_data, this%width, this%height, &
                                   x1, y1, x2, y2, x3, y3, -1_1, -1_1, -1_1)
                call fill_triangle(this%raster%image_data, this%width, this%height, &
                                   x1, y1, x3, y3, x4, y4, -1_1, -1_1, -1_1)

                call draw_line_distance_aa(this%raster%image_data, this%width, &
                                           this%height, x1, y1, x2, y2, 0.0_wp, &
                                           0.0_wp, 0.0_wp, 1.0_wp)
                call draw_line_distance_aa(this%raster%image_data, this%width, &
                                           this%height, x2, y2, x3, y3, 0.0_wp, &
                                           0.0_wp, 0.0_wp, 1.0_wp)
                call draw_line_distance_aa(this%raster%image_data, this%width, &
                                           this%height, x3, y3, x4, y4, 0.0_wp, &
                                           0.0_wp, 0.0_wp, 1.0_wp)
                call draw_line_distance_aa(this%raster%image_data, this%width, &
                                           this%height, x4, y4, x1, y1, 0.0_wp, &
                                           0.0_wp, 0.0_wp, 1.0_wp)
            end if

            call composite_bitmap_to_raster_0(this%raster%image_data, this%width, &
                                              this%height, rotated_bitmap, rot_w, &
                                              rot_h, x0, y0)
        else
            block
                real(wp) :: baseline_px, top_px
                real(wp) :: descent_abs
                real(wp) :: h_box
                integer :: baseline_i

                x0 = int(nint(x_px - ax_src))

                descent_abs = abs(descent_px)
                h_box = max(1.0_wp, ascent_px + descent_abs)

                baseline_px = y_px
                select case (trim(va))
                case ('center')
                    baseline_px = y_px + 0.5_wp*(ascent_px - descent_abs)
                case ('top')
                    baseline_px = y_px + ascent_px
                case ('bottom')
                    baseline_px = y_px - descent_abs
                case default
                end select

                top_px = baseline_px - ascent_px
                y0 = int(nint(top_px))
                baseline_i = int(nint(baseline_px))

                if (bbox) then
                    x1 = real(x0, wp) - pad
                    y1 = real(y0, wp) - pad
                    x2 = real(x0, wp) + w_px + pad
                    y2 = y1
                    x3 = x2
                    y3 = real(y0, wp) + h_box + pad
                    x4 = x1
                    y4 = y3

                    call fill_triangle(this%raster%image_data, this%width, &
                                       this%height, x1, y1, x2, y2, x3, y3, -1_1, &
                                       -1_1, -1_1)
                    call fill_triangle(this%raster%image_data, this%width, &
                                       this%height, x1, y1, x3, y3, x4, y4, -1_1, &
                                       -1_1, -1_1)

                    call draw_line_distance_aa(this%raster%image_data, this%width, &
                                               this%height, x1, y1, x2, y2, 0.0_wp, &
                                               0.0_wp, 0.0_wp, 1.0_wp)
                    call draw_line_distance_aa(this%raster%image_data, this%width, &
                                               this%height, x2, y2, x3, y3, 0.0_wp, &
                                               0.0_wp, 0.0_wp, 1.0_wp)
                    call draw_line_distance_aa(this%raster%image_data, this%width, &
                                               this%height, x3, y3, x4, y4, 0.0_wp, &
                                               0.0_wp, 0.0_wp, 1.0_wp)
                    call draw_line_distance_aa(this%raster%image_data, this%width, &
                                               this%height, x4, y4, x1, y1, 0.0_wp, &
                                               0.0_wp, 0.0_wp, 1.0_wp)
                end if

                call render_text_with_size(this%raster%image_data, this%width, &
                                           this%height, x0, baseline_i, trim(text), &
                                           r, g, b, pixel_height)
            end block
        end if
    end subroutine raster_draw_text_styled

    subroutine raster_save_dummy(this, filename)
        !! Dummy save method - see issue #496 for implementation improvement roadmap
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        associate (dfl => len_trim(filename)); end associate

        ! This is a dummy implementation - concrete backends like PNG will override this
        ! Implementation improvement needed - see issue #496
        call log_error("raster_context cannot save files directly. Use a PNG backend.")
        ! Return instead of stopping - this allows graceful error handling
        return
    end subroutine raster_save_dummy

    subroutine raster_draw_marker(this, x, y, style)
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

    subroutine draw_raster_marker_by_style(this, px, py, style)
        !! Draw marker using shared style dispatch logic (DRY compliance)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: px, py
        character(len=*), intent(in) :: style
        real(wp) :: marker_size

        marker_size = get_marker_size(style)

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
	            call draw_x_marker(this%raster%image_data, this%width, this%height, px, &
	                               py, marker_size, &
	                               this%raster%marker_edge_r, this%raster%marker_edge_g, &
	                               this%raster%marker_edge_b, &
	                               this%raster%marker_edge_alpha, &
	                               this%raster%current_line_width)
	        end select
	    end subroutine draw_raster_marker_by_style

    subroutine raster_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, &
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

    subroutine raster_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, &
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

    subroutine raster_fill_quad_context(this, x_quad, y_quad)
        !! Fill quadrilateral with current color - delegate to specialized module
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)

        call raster_fill_quad(this%raster, this%width, this%height, this%plot_area, &
                              this%x_min, this%x_max, this%y_min, this%y_max, &
                              x_quad, y_quad)
    end subroutine raster_fill_quad_context

    subroutine raster_draw_arrow(this, x, y, dx, dy, size, style)
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

        shaft_base_x = px - arrow_length*norm_dx
        shaft_base_y = py - arrow_length*norm_dy

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

    function raster_get_ascii_output(this) result(output)
        !! Get ASCII output (not applicable for raster backend)
        class(raster_context), intent(in) :: this
        character(len=:), allocatable :: output

        output = ""  ! Raster backend doesn't produce ASCII output
    end function raster_get_ascii_output

    function raster_get_width_scale(this) result(scale)
        !! Get width scaling factor for coordinate transformation
        class(raster_context), intent(in) :: this
        real(wp) :: scale

        ! Calculate scaling from logical to pixel coordinates
        if (this%width > 0 .and. this%x_max > this%x_min) then
            scale = real(this%width, wp)/(this%x_max - this%x_min)
        else
            scale = 1.0_wp
        end if
    end function raster_get_width_scale

    function raster_get_height_scale(this) result(scale)
        !! Get height scaling factor for coordinate transformation
        class(raster_context), intent(in) :: this
        real(wp) :: scale

        ! Calculate scaling from logical to pixel coordinates
        if (this%height > 0 .and. this%y_max > this%y_min) then
            scale = real(this%height, wp)/(this%y_max - this%y_min)
        else
            scale = 1.0_wp
        end if
    end function raster_get_height_scale

    subroutine raster_fill_heatmap_context(this, x_grid, y_grid, z_grid, z_min, z_max)
        !! Fill contour plot - delegate to specialized rendering module
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: z_min, z_max

        call raster_fill_heatmap(this%raster, this%width, this%height, this%plot_area, &
                                 this%x_min, this%x_max, this%y_min, this%y_max, &
                                 x_grid, y_grid, z_grid, z_min, z_max)
    end subroutine raster_fill_heatmap_context

    subroutine raster_render_legend_specialized_context(this, legend, &
                                                        legend_x, legend_y)
        use fortplot_legend, only: legend_t
        class(raster_context), intent(inout) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y

        call raster_render_legend_specialized(legend, legend_x, legend_y)
    end subroutine raster_render_legend_specialized_context

    subroutine raster_calculate_legend_dimensions_context(this, legend, legend_width, &
                                                          legend_height)
        use fortplot_legend, only: legend_t
        class(raster_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: legend_width, legend_height

        call raster_calculate_legend_dimensions(legend, legend_width, legend_height)
    end subroutine raster_calculate_legend_dimensions_context

    subroutine raster_set_legend_border_width_context(this)
        class(raster_context), intent(inout) :: this

        call this%set_line_width(0.1_wp)  ! Thin border for PNG like axes
    end subroutine raster_set_legend_border_width_context

    subroutine raster_calculate_legend_position_context(this, legend, x, y)
        use fortplot_legend, only: legend_t
        class(raster_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: x, y

        call raster_calculate_legend_position(legend, x, y)
    end subroutine raster_calculate_legend_position_context

    subroutine raster_extract_rgb_data_context(this, width, height, rgb_data)
        class(raster_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)

        call raster_extract_rgb_data(this%raster, width, height, rgb_data)
    end subroutine raster_extract_rgb_data_context

    subroutine raster_get_png_data_context(this, width, height, png_data, status)
        class(raster_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status

        call raster_get_png_data(width, height, png_data, status)
    end subroutine raster_get_png_data_context

    subroutine raster_prepare_3d_data_context(this, plots)
        use fortplot_plot_data, only: plot_data_t
        class(raster_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)

        ! PNG backend does not use 3D data preparation; keep a benign reference
        ! to arguments to avoid unused warnings under stricter toolchains.
        if (this%width < 0) return
        if (size(plots) == 0) return
    end subroutine raster_prepare_3d_data_context

    subroutine raster_render_ylabel_context(this, ylabel)
        !! Render rotated Y-axis label - delegate to axes module
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel

        call raster_render_ylabel(this%raster, this%width, this%height, &
                                  this%plot_area, ylabel)
    end subroutine raster_render_ylabel_context

    subroutine raster_draw_axes_and_labels_context(this, xscale, yscale, &
                                                   symlog_threshold, &
                                                   x_min, x_max, y_min, y_max, &
                                                   title, xlabel, ylabel, &
                                                   x_date_format, y_date_format, &
                                                   z_min, z_max, has_3d_plots)
        !! Draw axes and labels - delegate to specialized axes module
        use fortplot_3d_axes, only: draw_3d_axes
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        character(len=:), allocatable :: title_str, xlabel_str, ylabel_str

        this%last_xscale = trim(xscale)
        this%last_yscale = trim(yscale)
        this%last_symlog_threshold = symlog_threshold

        ! Set color to black for axes and text
        call this%color(0.0_wp, 0.0_wp, 0.0_wp)

        if (has_3d_plots) then
            ! Draw simplified 3D axes frame using current data ranges
            call draw_3d_axes(this, x_min, x_max, y_min, y_max, &
                              merge(z_min, 0.0_wp, present(z_min)), &
                              merge(z_max, 1.0_wp, present(z_max)))
            ! Draw title/xlabel/ylabel without re-drawing 2D tick labels
            if (present(title) .or. present(xlabel) .or. present(ylabel)) then
                title_str = ""
                xlabel_str = ""
                ylabel_str = ""

                if (present(title)) then
                    if (allocated(title)) title_str = title
                end if
                if (present(xlabel)) then
                    if (allocated(xlabel)) xlabel_str = xlabel
                end if
                if (present(ylabel)) then
                    if (allocated(ylabel)) ylabel_str = ylabel
                end if

                call raster_draw_axis_labels(this%raster, this%width, this%height, &
                                             this%plot_area, &
                                             title_str, xlabel_str, ylabel_str)
            end if
        else
            ! Delegate to standard 2D axes module
            call raster_draw_axes_and_labels(this%raster, this%width, this%height, &
                                             this%plot_area, &
                                             xscale, yscale, symlog_threshold, &
                                             x_min, x_max, y_min, y_max, &
                                             title, xlabel, ylabel, &
                                             x_date_format=x_date_format, &
                                             y_date_format=y_date_format)
        end if
    end subroutine raster_draw_axes_and_labels_context

    subroutine raster_save_coordinates(this, x_min, x_max, y_min, y_max)
        !! Save current coordinate system
        class(raster_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max

        x_min = this%x_min
        x_max = this%x_max
        y_min = this%y_min
        y_max = this%y_max
    end subroutine raster_save_coordinates

    subroutine raster_set_coordinates(this, x_min, x_max, y_min, y_max)
        !! Set coordinate system
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max
    end subroutine raster_set_coordinates

    subroutine raster_render_axes(this, title_text, xlabel_text, ylabel_text)
        !! Render axes for raster context using the most recently configured scales.
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
        character(len=:), allocatable :: t, xl, yl
        real(wp) :: x_min, x_max, y_min, y_max

        t = ''
        xl = ''
        yl = ''
        if (present(title_text)) t = title_text
        if (present(xlabel_text)) xl = xlabel_text
        if (present(ylabel_text)) yl = ylabel_text

        call this%save_coordinates(x_min, x_max, y_min, y_max)
        call this%draw_axes_and_labels_backend(this%last_xscale, this%last_yscale, &
                                               this%last_symlog_threshold, &
                                               x_min, x_max, y_min, y_max, &
                                               t, xl, yl, has_3d_plots=.false.)
    end subroutine raster_render_axes

    subroutine raster_draw_axes_lines_and_ticks_context(this, xscale, yscale, &
                                                        symlog_threshold, &
                                                        x_min, x_max, y_min, y_max)
        !! Draw axes lines and tick marks WITHOUT labels (for proper drawing order)
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        this%last_xscale = trim(xscale)
        this%last_yscale = trim(yscale)
        this%last_symlog_threshold = symlog_threshold

        ! Set color to black for axes and ticks
        call this%color(0.0_wp, 0.0_wp, 0.0_wp)

        ! Delegate to axes module
        call raster_draw_axes_lines_and_ticks(this%raster, this%width, this%height, &
                                              this%plot_area, &
                                              xscale, yscale, symlog_threshold, &
                                              x_min, x_max, y_min, y_max)
    end subroutine raster_draw_axes_lines_and_ticks_context

    subroutine raster_draw_axis_labels_only_context(this, xscale, yscale, &
                                                    symlog_threshold, &
                                                    x_min, x_max, y_min, y_max, &
                                                    title, xlabel, ylabel, &
                                                    custom_xticks, &
                                                    custom_xtick_labels, &
                                                    custom_yticks, &
                                                    custom_ytick_labels, &
                                                    x_date_format, y_date_format)
        !! Draw ONLY axis labels and tick labels (for proper drawing order)
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
        character(len=*), intent(in), optional :: custom_xtick_labels(:)
        character(len=*), intent(in), optional :: custom_ytick_labels(:)
        character(len=*), intent(in), optional :: x_date_format, y_date_format

        this%last_xscale = trim(xscale)
        this%last_yscale = trim(yscale)
        this%last_symlog_threshold = symlog_threshold

        ! Set color to black for text
        call this%color(0.0_wp, 0.0_wp, 0.0_wp)

        ! Delegate to axes module
        call raster_draw_axis_labels_only(this%raster, this%width, this%height, &
                                          this%plot_area, &
                                          xscale, yscale, symlog_threshold, &
                                          x_min, x_max, y_min, y_max, &
                                          title, xlabel, ylabel, &
                                          custom_xticks, custom_xtick_labels, &
                                          custom_yticks, custom_ytick_labels, &
                                          x_date_format=x_date_format, &
                                          y_date_format=y_date_format)
    end subroutine raster_draw_axis_labels_only_context

end module fortplot_raster
