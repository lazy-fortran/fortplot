module fortplot_raster
    !! Main raster plotting context - reduced from 931 lines by extracting specialized modules
    use iso_c_binding
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_constants, only: EPSILON_COMPARE
    use fortplot_text, only: render_text_to_image, calculate_text_width, calculate_text_height
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_text_helpers, only: prepare_mathtext_if_needed
    use fortplot_unicode, only: escape_unicode_for_raster
    use fortplot_logging, only: log_error
    use fortplot_errors, only: fortplot_error_t, ERROR_INTERNAL
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_CROSS
    use fortplot_raster_drawing, only: draw_line_distance_aa, blend_pixel, distance_point_to_line_segment, &
                                       ipart, fpart, rfpart, color_to_byte, &
                                       draw_circle_antialiased, draw_circle_outline_antialiased, &
                                       draw_circle_with_edge_face, draw_square_with_edge_face, &
                                       draw_diamond_with_edge_face, draw_x_marker
    use fortplot_raster_line_styles, only: draw_styled_line, reset_pattern_distance, set_raster_line_style
    use fortplot_raster_core, only: raster_image_t, create_raster_image, destroy_raster_image
    use fortplot_raster_axes, only: raster_draw_axes_and_labels, raster_render_ylabel, &
                                        raster_draw_axes_lines_and_ticks, raster_draw_axis_labels_only
    use fortplot_raster_rendering, only: raster_fill_heatmap, raster_fill_quad, fill_triangle, &
                                        raster_render_legend_specialized, raster_calculate_legend_dimensions, &
                                        raster_set_legend_border_width, raster_calculate_legend_position, &
                                        raster_extract_rgb_data, raster_get_png_data, raster_prepare_3d_data
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
    contains
        procedure :: line => raster_draw_line
        procedure :: color => raster_set_color_context
        procedure :: text => raster_draw_text
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
        procedure :: render_legend_specialized => raster_render_legend_specialized_context
        procedure :: calculate_legend_dimensions => raster_calculate_legend_dimensions_context
        procedure :: set_legend_border_width => raster_set_legend_border_width_context
        procedure :: calculate_legend_position_backend => raster_calculate_legend_position_context
        procedure :: extract_rgb_data => raster_extract_rgb_data_context
        procedure :: get_png_data_backend => raster_get_png_data_context
        procedure :: prepare_3d_data => raster_prepare_3d_data_context
        procedure :: render_ylabel => raster_render_ylabel_context
        procedure :: draw_axes_and_labels_backend => raster_draw_axes_and_labels_context
        procedure :: draw_axes_lines_and_ticks => raster_draw_axes_lines_and_ticks_context
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
        px1 = (x1 - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py1 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y1 - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)
        px2 = (x2 - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py2 = real(this%plot_area%bottom + this%plot_area%height, wp) - &
              (y2 - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        ! Draw line with pattern support
        call draw_styled_line(this%raster%image_data, this%width, this%height, &
                             px1, py1, px2, py2, &
                             this%raster%current_r, this%raster%current_g, this%raster%current_b, &
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
        call prepare_mathtext_if_needed(processed_text(1:processed_len), math_ready, math_len)
        ! Pass through Unicode (STB supports it)
        call escape_unicode_for_raster(math_ready(1:math_len), escaped_text)

        ! Transform coordinates to plot area (like matplotlib)
        ! Note: Raster Y=0 at top, so we need to flip Y coordinates
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

        call this%raster%get_color_bytes(r, g, b)
        call render_text_to_image(this%raster%image_data, this%width, this%height, &
                                 int(px), int(py), trim(escaped_text), r, g, b)
    end subroutine raster_draw_text

    subroutine raster_save_dummy(this, filename)
        !! Dummy save method - see issue #496 for implementation improvement roadmap
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        associate(dfl=>len_trim(filename)); end associate
        
        ! This is a dummy implementation - concrete backends like PNG will override this
        ! Implementation improvement needed - see issue #496
        call log_error("raster_context cannot save files directly. Use a concrete backend like PNG.")
        ! Return instead of stopping - this allows graceful error handling
        return
    end subroutine raster_save_dummy

    subroutine raster_draw_marker(this, x, y, style)
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp) :: px, py
        integer(1) :: r, g, b
        associate(dsl=>len_trim(style)); end associate

        ! Transform coordinates to plot area
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)

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
            call draw_circle_with_edge_face(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                                           this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                           this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                           this%raster%marker_face_b, this%raster%marker_face_alpha)
        case (MARKER_SQUARE)
            call draw_square_with_edge_face(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                                           this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                           this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                           this%raster%marker_face_b, this%raster%marker_face_alpha)
        case (MARKER_DIAMOND)
            call draw_diamond_with_edge_face(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                                            this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b, &
                                            this%raster%marker_edge_alpha, this%raster%marker_face_r, this%raster%marker_face_g, &
                                            this%raster%marker_face_b, this%raster%marker_face_alpha)
        case (MARKER_CROSS)
            call draw_x_marker(this%raster%image_data, this%width, this%height, px, py, marker_size, &
                               this%raster%marker_edge_r, this%raster%marker_edge_g, this%raster%marker_edge_b)
        end select
    end subroutine draw_raster_marker_by_style

    subroutine raster_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
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

    subroutine raster_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, edge_alpha, &
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
                              this%x_min, this%x_max, this%y_min, this%y_max, x_quad, y_quad)
    end subroutine raster_fill_quad_context

    subroutine raster_draw_arrow(this, x, y, dx, dy, size, style)
        !! Draw arrow head for streamplot arrows in raster backend
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        
        real(wp) :: arrow_length, arrow_width, norm_dx, norm_dy
        real(wp) :: perp_x, perp_y  ! Perpendicular vector for arrow width
        real(wp) :: x1, y1, x2, y2, x3, y3  ! Triangle vertices
        real(wp) :: magnitude
        integer(1) :: r, g, b
        real(wp) :: px, py, ddx, ddy
        associate(dsl=>len_trim(style)); end associate
        
        ! Transform data coordinates to pixel coordinates
        px = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + real(this%plot_area%left, wp)
        py = real(this%plot_area%bottom + this%plot_area%height, wp) - &
             (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp)
        
        ! Scale direction components to pixel units (account for inverted Y)
        ddx = dx * ( real(this%plot_area%width, wp) / max(1.0_wp, (this%x_max - this%x_min)) )
        ddy = -dy * ( real(this%plot_area%height, wp) / max(1.0_wp, (this%y_max - this%y_min)) )
        
        ! Normalize direction vector
        magnitude = sqrt(ddx*ddx + ddy*ddy)
        if (magnitude < 1e-10_wp) return  ! Avoid division by zero
        
        norm_dx = ddx / magnitude
        norm_dy = ddy / magnitude
        
        ! Calculate arrow dimensions based on size
        arrow_length = size * 8.0_wp  ! Scale factor for reasonable arrow size
        arrow_width = arrow_length * 0.5_wp
        
        ! Calculate perpendicular vector for arrow width
        perp_x = -norm_dy
        perp_y = norm_dx
        
        ! Calculate triangle vertices for arrow head (tip at pixel coords)
        x1 = px
        y1 = py
        
        ! Base vertices of arrow head
        x2 = px - arrow_length * norm_dx + arrow_width * perp_x
        y2 = py - arrow_length * norm_dy + arrow_width * perp_y
        
        x3 = px - arrow_length * norm_dx - arrow_width * perp_x
        y3 = py - arrow_length * norm_dy - arrow_width * perp_y
        
        ! Get current color for filling
        call this%raster%get_color_bytes(r, g, b)
        
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
            scale = real(this%width, wp) / (this%x_max - this%x_min)
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
            scale = real(this%height, wp) / (this%y_max - this%y_min)
        else
            scale = 1.0_wp
        end if
    end function raster_get_height_scale

    subroutine raster_fill_heatmap_context(this, x_grid, y_grid, z_grid, z_min, z_max)
        !! Fill contour plot - delegate to specialized rendering module
        class(raster_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        
        call raster_fill_heatmap(this%raster, this%width, this%height, this%plot_area, &
                                this%x_min, this%x_max, this%y_min, this%y_max, &
                                x_grid, y_grid, z_grid, z_min, z_max)
    end subroutine raster_fill_heatmap_context

    subroutine raster_render_legend_specialized_context(this, legend, legend_x, legend_y)
        use fortplot_legend, only: legend_t
        class(raster_context), intent(inout) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y
        
        call raster_render_legend_specialized(legend, legend_x, legend_y)
    end subroutine raster_render_legend_specialized_context

    subroutine raster_calculate_legend_dimensions_context(this, legend, legend_width, legend_height)
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
        use, intrinsic :: iso_fortran_env, only: real64
        class(raster_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(real64), intent(out) :: rgb_data(width, height, 3)
        
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
        
        call raster_prepare_3d_data(plots)
    end subroutine raster_prepare_3d_data_context

    subroutine raster_render_ylabel_context(this, ylabel)
        !! Render rotated Y-axis label - delegate to axes module
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        
        call raster_render_ylabel(this%raster, this%width, this%height, this%plot_area, ylabel)
    end subroutine raster_render_ylabel_context

    subroutine raster_draw_axes_and_labels_context(this, xscale, yscale, symlog_threshold, &
                                                   x_min, x_max, y_min, y_max, &
                                                   title, xlabel, ylabel, &
                                                   z_min, z_max, has_3d_plots)
        !! Draw axes and labels - delegate to specialized axes module
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        
        ! Reference optional 3D parameters to keep interface stable
        associate(dzmin=>z_min, dzmax=>z_max, dh3d=>has_3d_plots); end associate
        ! Set color to black for axes and text
        call this%color(0.0_wp, 0.0_wp, 0.0_wp)
        
        ! Delegate to axes module
        call raster_draw_axes_and_labels(this%raster, this%width, this%height, this%plot_area, &
                                        xscale, yscale, symlog_threshold, &
                                        x_min, x_max, y_min, y_max, &
                                        title, xlabel, ylabel)
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
        !! Render axes for raster context - see issue #495 for implementation roadmap
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
        if (present(title_text)) then; associate(dt=>len_trim(title_text)); end associate; end if
        if (present(xlabel_text)) then; associate(dx=>len_trim(xlabel_text)); end associate; end if
        if (present(ylabel_text)) then; associate(dy=>len_trim(ylabel_text)); end associate; end if
        
        ! Raster axes are rendered as part of draw_axes_and_labels_backend
        ! Implementation needed - see issue #495
    end subroutine raster_render_axes

    subroutine raster_draw_axes_lines_and_ticks_context(this, xscale, yscale, symlog_threshold, &
                                                       x_min, x_max, y_min, y_max)
        !! Draw axes lines and tick marks WITHOUT labels (for proper drawing order)
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        ! Set color to black for axes and ticks
        call this%color(0.0_wp, 0.0_wp, 0.0_wp)
        
        ! Delegate to axes module
        call raster_draw_axes_lines_and_ticks(this%raster, this%width, this%height, this%plot_area, &
                                            xscale, yscale, symlog_threshold, &
                                            x_min, x_max, y_min, y_max)
    end subroutine raster_draw_axes_lines_and_ticks_context

    subroutine raster_draw_axis_labels_only_context(this, xscale, yscale, symlog_threshold, &
                                                   x_min, x_max, y_min, y_max, &
                                                   title, xlabel, ylabel)
        !! Draw ONLY axis labels and tick labels (for proper drawing order)
        class(raster_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        
        ! Set color to black for text
        call this%color(0.0_wp, 0.0_wp, 0.0_wp)
        
        ! Delegate to axes module
        call raster_draw_axis_labels_only(this%raster, this%width, this%height, this%plot_area, &
                                        xscale, yscale, symlog_threshold, &
                                        x_min, x_max, y_min, y_max, &
                                        title, xlabel, ylabel)
    end subroutine raster_draw_axis_labels_only_context

end module fortplot_raster
