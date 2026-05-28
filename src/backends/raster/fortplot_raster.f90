module fortplot_raster
    !! Main raster plotting context - type definition and constructor only.
    !! All bound method implementations are in submodules:
    !!   fortplot_raster_impl        - drawing methods (line, arrow, marker, quad)
    !!   fortplot_raster_text_impl   - text rendering methods (text, bbox, rotated)
    !!
    !! This module provides the raster_context type and create_raster_canvas
    !! constructor. See submodules for all method implementations.

    use iso_c_binding
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_constants, only: EPSILON_COMPARE, REFERENCE_DPI
    use fortplot_bitmap, only: composite_bitmap_to_raster_0, get_text_bitmap_metrics, &
                               render_text_to_bitmap_with_size, &
                               rotate_bitmap_about_anchor
    use fortplot_text, only: calculate_text_width, calculate_text_width_with_size, &
                             calculate_text_height
    use fortplot_text_rendering, only: render_text_to_image, render_text_with_size
    use fortplot_text_helpers, only: prepare_text_for_raster
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
                                    destroy_raster_image, pt2px, scale_px
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
    use fortplot_legend, only: legend_t
    use fortplot_plot_data, only: plot_data_t
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
        procedure :: draw_arrowhead => raster_draw_arrowhead
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

    ! Abstract interfaces for submodule-implemented procedures
    interface
        module subroutine raster_draw_line(this, x1, y1, x2, y2)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: x1, y1, x2, y2
        end subroutine raster_draw_line

        module subroutine raster_set_color_context(this, r, g, b)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: r, g, b
        end subroutine raster_set_color_context

        module subroutine raster_set_line_width(this, width)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: width
        end subroutine raster_set_line_width

        module subroutine raster_set_line_style_context(this, style)
            class(raster_context), intent(inout) :: this
            character(len=*), intent(in) :: style
        end subroutine raster_set_line_style_context

        module subroutine raster_draw_text(this, x, y, text)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: x, y
            character(len=*), intent(in) :: text
        end subroutine raster_draw_text

        module subroutine raster_draw_text_styled(this, x_px, y_px, text, pixel_height, &
                                                  rotation, ha, va, bbox, color)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: x_px, y_px
            character(len=*), intent(in) :: text
            real(wp), intent(in) :: pixel_height
            real(wp), intent(in) :: rotation
            character(len=*), intent(in) :: ha, va
            logical, intent(in) :: bbox
            real(wp), intent(in) :: color(3)
        end subroutine raster_draw_text_styled

        module subroutine raster_save_dummy(this, filename)
            class(raster_context), intent(inout) :: this
            character(len=*), intent(in) :: filename
        end subroutine raster_save_dummy

        module subroutine raster_draw_marker(this, x, y, style)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: x, y
            character(len=*), intent(in) :: style
        end subroutine raster_draw_marker

        module subroutine draw_raster_marker_by_style(this, px, py, style)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: px, py
            character(len=*), intent(in) :: style
        end subroutine draw_raster_marker_by_style

        module subroutine raster_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, &
                                                   face_g, face_b)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: edge_r, edge_g, edge_b
            real(wp), intent(in) :: face_r, face_g, face_b
        end subroutine raster_set_marker_colors

        module subroutine raster_set_marker_colors_with_alpha(this, edge_r, edge_g, &
                                                              edge_b, edge_alpha, &
                                                              face_r, face_g, face_b, &
                                                              face_alpha)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
            real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        end subroutine raster_set_marker_colors_with_alpha

        module subroutine raster_fill_quad_context(this, x_quad, y_quad)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: x_quad(4), y_quad(4)
        end subroutine raster_fill_quad_context

        module subroutine raster_draw_arrow(this, x, y, dx, dy, size, style)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: x, y, dx, dy, size
            character(len=*), intent(in) :: style
        end subroutine raster_draw_arrow

        module subroutine raster_draw_arrowhead(this, x, y, dx, dy, size, style)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: x, y, dx, dy, size
            character(len=*), intent(in) :: style
        end subroutine raster_draw_arrowhead

        module function raster_get_ascii_output(this) result(output)
            class(raster_context), intent(in) :: this
            character(len=:), allocatable :: output
        end function raster_get_ascii_output

        module function raster_get_width_scale(this) result(scale)
            class(raster_context), intent(in) :: this
            real(wp) :: scale
        end function raster_get_width_scale

        module function raster_get_height_scale(this) result(scale)
            class(raster_context), intent(in) :: this
            real(wp) :: scale
        end function raster_get_height_scale

        module subroutine raster_fill_heatmap_context(this, x_grid, y_grid, z_grid, &
                                                      z_min, z_max, colormap_name)
            class(raster_context), intent(inout) :: this
            real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
            real(wp), intent(in) :: z_min, z_max
            character(len=*), intent(in), optional :: colormap_name
        end subroutine raster_fill_heatmap_context

        module subroutine raster_render_legend_specialized_context(this, legend, &
                                                                   legend_x, legend_y)
            class(raster_context), intent(inout) :: this
            type(legend_t), intent(in) :: legend
            real(wp), intent(in) :: legend_x, legend_y
        end subroutine raster_render_legend_specialized_context

        module subroutine raster_calculate_legend_dimensions_context(this, legend, &
                                                                     legend_width, &
                                                                     legend_height)
            class(raster_context), intent(in) :: this
            type(legend_t), intent(in) :: legend
            real(wp), intent(out) :: legend_width, legend_height
        end subroutine raster_calculate_legend_dimensions_context

        module subroutine raster_set_legend_border_width_context(this)
            class(raster_context), intent(inout) :: this
        end subroutine raster_set_legend_border_width_context

        module subroutine raster_calculate_legend_position_context(this, legend, x, y)
            class(raster_context), intent(in) :: this
            type(legend_t), intent(in) :: legend
            real(wp), intent(out) :: x, y
        end subroutine raster_calculate_legend_position_context

        module subroutine raster_extract_rgb_data_context(this, width, height, rgb_data)
            class(raster_context), intent(in) :: this
            integer, intent(in) :: width, height
            real(wp), intent(out) :: rgb_data(width, height, 3)
        end subroutine raster_extract_rgb_data_context

        module subroutine raster_get_png_data_context(this, width, height, png_data, &
                                                      status)
            class(raster_context), intent(in) :: this
            integer, intent(in) :: width, height
            integer(1), allocatable, intent(out) :: png_data(:)
            integer, intent(out) :: status
        end subroutine raster_get_png_data_context

        module subroutine raster_prepare_3d_data_context(this, plots)
            class(raster_context), intent(inout) :: this
            type(plot_data_t), intent(in) :: plots(:)
        end subroutine raster_prepare_3d_data_context

        module subroutine raster_render_ylabel_context(this, ylabel)
            class(raster_context), intent(inout) :: this
            character(len=*), intent(in) :: ylabel
        end subroutine raster_render_ylabel_context

        module subroutine raster_draw_axes_and_labels_context(this, xscale, yscale, &
                                                              symlog_threshold, &
                                                              x_min, x_max, y_min, y_max, &
                                                              title, xlabel, ylabel, &
                                                              x_date_format, y_date_format, &
                                                              z_min, z_max, has_3d_plots)
            class(raster_context), intent(inout) :: this
            character(len=*), intent(in) :: xscale, yscale
            real(wp), intent(in) :: symlog_threshold
            real(wp), intent(in) :: x_min, x_max, y_min, y_max
            character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
            character(len=*), intent(in), optional :: x_date_format, y_date_format
            real(wp), intent(in), optional :: z_min, z_max
            logical, intent(in) :: has_3d_plots
        end subroutine raster_draw_axes_and_labels_context

        module subroutine raster_draw_axes_lines_and_ticks_context(this, xscale, yscale, &
                                                                   symlog_threshold, &
                                                                   x_min, x_max, y_min, y_max)
            class(raster_context), intent(inout) :: this
            character(len=*), intent(in) :: xscale, yscale
            real(wp), intent(in) :: symlog_threshold
            real(wp), intent(in) :: x_min, x_max, y_min, y_max
        end subroutine raster_draw_axes_lines_and_ticks_context

        module subroutine raster_draw_axis_labels_only_context(this, xscale, yscale, &
                                                               symlog_threshold, &
                                                               x_min, x_max, y_min, y_max, &
                                                               title, xlabel, ylabel, &
                                                               custom_xticks, &
                                                               custom_xtick_labels, &
                                                               custom_yticks, &
                                                               custom_ytick_labels, &
                                                               x_date_format, y_date_format)
            class(raster_context), intent(inout) :: this
            character(len=*), intent(in) :: xscale, yscale
            real(wp), intent(in) :: symlog_threshold
            real(wp), intent(in) :: x_min, x_max, y_min, y_max
            character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
            real(wp), intent(in), optional :: custom_xticks(:), custom_yticks(:)
            character(len=*), intent(in), optional :: custom_xtick_labels(:)
            character(len=*), intent(in), optional :: custom_ytick_labels(:)
            character(len=*), intent(in), optional :: x_date_format, y_date_format
        end subroutine raster_draw_axis_labels_only_context

        module subroutine raster_save_coordinates(this, x_min, x_max, y_min, y_max)
            class(raster_context), intent(in) :: this
            real(wp), intent(out) :: x_min, x_max, y_min, y_max
        end subroutine raster_save_coordinates

        module subroutine raster_set_coordinates(this, x_min, x_max, y_min, y_max)
            class(raster_context), intent(inout) :: this
            real(wp), intent(in) :: x_min, x_max, y_min, y_max
        end subroutine raster_set_coordinates

        module subroutine raster_render_axes(this, title_text, xlabel_text, ylabel_text)
             class(raster_context), intent(inout) :: this
             character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
         end subroutine raster_render_axes

         ! Helper procedures for text rendering (not bound methods)
         module subroutine raster_draw_text_with_bbox(image_data, width, height, x_px, y_px, &
                                               text, r, g, b, pixel_height, ha, va, &
                                               bbox, text_w, text_h, ascent_px, &
                                               descent_px, pad)
             integer(1), intent(inout) :: image_data(:)
             integer, intent(in) :: width, height
             real(wp), intent(in) :: x_px, y_px
             character(len=*), intent(in) :: text
             integer(1), intent(in) :: r, g, b
             real(wp), intent(in) :: pixel_height
             character(len=*), intent(in) :: ha, va
             logical, intent(in) :: bbox
             integer, intent(in) :: text_w, text_h
             real(wp), intent(in) :: ascent_px, descent_px
             real(wp), intent(in) :: pad
         end subroutine raster_draw_text_with_bbox

         module subroutine raster_draw_rotated_text_with_bbox(image_data, width, height, x_px, &
                                               y_px, text, r, g, b, pixel_height, &
                                               rotation, ha, va, bbox, ax_src, &
                                               ay_src, pad)
             integer(1), intent(inout) :: image_data(:)
             integer, intent(in) :: width, height
             real(wp), intent(in) :: x_px, y_px
             character(len=*), intent(in) :: text
             integer(1), intent(in) :: r, g, b
             real(wp), intent(in) :: pixel_height
             real(wp), intent(in) :: rotation
             character(len=*), intent(in) :: ha, va
             logical, intent(in) :: bbox
             real(wp), intent(in) :: ax_src, ay_src, pad
         end subroutine raster_draw_rotated_text_with_bbox
     end interface

contains

    function create_raster_canvas(width, height, dpi) result(ctx)
        integer, intent(in) :: width, height
        real(wp), intent(in), optional :: dpi
        type(raster_context) :: ctx

        call setup_canvas(ctx, width, height)

        ctx%raster = create_raster_image(width, height, dpi)
        ctx%margins = plot_margins_t()  ! matplotlib-style margins
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
    end function create_raster_canvas

end module fortplot_raster
