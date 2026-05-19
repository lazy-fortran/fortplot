module fortplot_pdf
    !! PDF backend main interface (unified coordinates using plot area)

    use fortplot_pdf_core
    use fortplot_pdf_text
    use fortplot_pdf_drawing
    use fortplot_zlib_core, only: zlib_compress_into
    use fortplot_pdf_axes, only: draw_pdf_axes_and_labels, render_mixed_text
    use fortplot_pdf_secondary_axes, only: draw_pdf_secondary_y_axis, &
                                           draw_pdf_secondary_x_axis_top
    use fortplot_pdf_io
    use fortplot_pdf_coordinate
    use fortplot_pdf_markers

    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_plot_data, only: plot_data_t
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_constants, only: EPSILON_COMPARE, REFERENCE_DPI
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_colormap, only: colormap_value_to_color
    use fortplot_logging, only: log_error, log_info
    implicit none

    private

    public :: pdf_context, create_pdf_canvas
    public :: draw_pdf_axes_and_labels, draw_mixed_font_text
    public :: pdf_stream_writer

    type, extends(plot_context) :: pdf_context
        type(pdf_stream_writer) :: stream_writer
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        type(pdf_context_core), private :: core_ctx
        type(pdf_context_handle), private :: coord_ctx
        integer :: x_tick_count = 0
        integer :: y_tick_count = 0
        logical, private :: axes_rendered = .false.
        ! Custom tick support (set_xticks / set_yticks)
        real(wp), allocatable :: custom_xtick_positions(:)
        real(wp), allocatable :: custom_ytick_positions(:)
        character(len=50), allocatable :: custom_xtick_labels(:)
        character(len=50), allocatable :: custom_ytick_labels(:)
    contains
        procedure :: line => draw_pdf_line
        procedure :: color => set_pdf_color
        procedure :: text => draw_pdf_text_wrapper
        procedure :: draw_text_styled => draw_pdf_text_styled
        procedure :: save => write_pdf_file_facade
        procedure :: set_line_width => set_pdf_line_width
        procedure :: set_line_style => set_pdf_line_style
        procedure :: draw_marker => draw_pdf_marker_wrapper
        procedure :: set_marker_colors => set_marker_colors_wrapper
        procedure :: set_marker_colors_with_alpha => &
            set_marker_colors_with_alpha_wrapper
        procedure :: draw_arrow => draw_pdf_arrow_wrapper
        procedure :: get_ascii_output => pdf_get_ascii_output

        procedure :: get_width_scale => get_width_scale_wrapper
        procedure :: get_height_scale => get_height_scale_wrapper
        procedure :: fill_quad => fill_quad_wrapper
        procedure :: fill_heatmap => fill_heatmap_wrapper
        procedure :: extract_rgb_data => extract_rgb_data_wrapper
        procedure :: get_png_data_backend => get_png_data_wrapper
        procedure :: prepare_3d_data => prepare_3d_data_wrapper
        procedure :: render_ylabel => render_ylabel_wrapper
        procedure :: draw_axes_and_labels_backend => &
            draw_axes_and_labels_backend_wrapper
        procedure :: save_coordinates => pdf_save_coordinates
        procedure :: set_coordinates => pdf_set_coordinates
        procedure :: render_axes => render_pdf_axes_wrapper
        procedure :: draw_secondary_y_axis => pdf_draw_secondary_y_axis_wrapper
        procedure :: draw_secondary_x_axis_top => pdf_draw_secondary_x_axis_top_wrapper

        procedure, private :: update_coord_context
        procedure, private :: make_coord_context
    end type pdf_context

    interface
        module function create_pdf_canvas(width, height) result(ctx)
            integer, intent(in) :: width, height
            type(pdf_context) :: ctx
        end function create_pdf_canvas

        module subroutine draw_pdf_line(this, x1, y1, x2, y2)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: x1, y1, x2, y2
        end subroutine draw_pdf_line

        module subroutine set_pdf_color(this, r, g, b)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: r, g, b
        end subroutine set_pdf_color

        module subroutine set_pdf_line_width(this, width)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: width
        end subroutine set_pdf_line_width

        module subroutine set_pdf_line_style(this, style)
            class(pdf_context), intent(inout) :: this
            character(len=*), intent(in) :: style
        end subroutine set_pdf_line_style

        module subroutine draw_pdf_text_wrapper(this, x, y, text)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: x, y
            character(len=*), intent(in) :: text
        end subroutine draw_pdf_text_wrapper

        module subroutine draw_pdf_text_styled(this, x_pt, y_pt, text, font_size, rotation, &
                                        ha, va, bbox, color)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: x_pt, y_pt
            character(len=*), intent(in) :: text
            real(wp), intent(in) :: font_size
            real(wp), intent(in) :: rotation
            character(len=*), intent(in) :: ha, va
            logical, intent(in) :: bbox
            real(wp), intent(in) :: color(3)
        end subroutine draw_pdf_text_styled

        module subroutine write_pdf_file_facade(this, filename)
            class(pdf_context), intent(inout) :: this
            character(len=*), intent(in) :: filename
        end subroutine write_pdf_file_facade

        module subroutine update_coord_context(this)
            class(pdf_context), intent(inout) :: this
        end subroutine update_coord_context

        module function make_coord_context(this) result(ctx)
            class(pdf_context), intent(in) :: this
            type(pdf_context_handle) :: ctx
        end function make_coord_context

        module subroutine draw_pdf_marker_wrapper(this, x, y, style)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: x, y
            character(len=*), intent(in) :: style
        end subroutine draw_pdf_marker_wrapper

        module subroutine set_marker_colors_wrapper(this, edge_r, edge_g, edge_b, face_r, &
                                             face_g, face_b)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: edge_r, edge_g, edge_b, face_r, face_g, face_b
        end subroutine set_marker_colors_wrapper

        module subroutine set_marker_colors_with_alpha_wrapper(this, edge_r, edge_g, edge_b, &
                                                         edge_alpha, &
                                                         face_r, face_g, face_b, &
                                                         face_alpha)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
            real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        end subroutine set_marker_colors_with_alpha_wrapper

        module subroutine draw_pdf_arrow_wrapper(this, x, y, dx, dy, size, style)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: x, y, dx, dy, size
            character(len=*), intent(in) :: style
        end subroutine draw_pdf_arrow_wrapper

        module function pdf_get_ascii_output(this) result(output)
            class(pdf_context), intent(in) :: this
            character(len=:), allocatable :: output
        end function pdf_get_ascii_output

        module function get_width_scale_wrapper(this) result(scale)
            class(pdf_context), intent(in) :: this
            real(wp) :: scale
        end function get_width_scale_wrapper

        module function get_height_scale_wrapper(this) result(scale)
            class(pdf_context), intent(in) :: this
            real(wp) :: scale
        end function get_height_scale_wrapper

        module subroutine fill_quad_wrapper(this, x_quad, y_quad)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: x_quad(4), y_quad(4)
        end subroutine fill_quad_wrapper

        module subroutine fill_heatmap_wrapper(this, x_grid, y_grid, z_grid, z_min, z_max, colormap_name)
            class(pdf_context), intent(inout) :: this
            real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
            real(wp), intent(in) :: z_min, z_max
            character(len=*), intent(in), optional :: colormap_name
        end subroutine fill_heatmap_wrapper

        module subroutine extract_rgb_data_wrapper(this, width, height, rgb_data)
            class(pdf_context), intent(in) :: this
            integer, intent(in) :: width, height
            real(wp), intent(out) :: rgb_data(width, height, 3)
        end subroutine extract_rgb_data_wrapper

        module subroutine get_png_data_wrapper(this, width, height, png_data, status)
            class(pdf_context), intent(in) :: this
            integer, intent(in) :: width, height
            integer(1), allocatable, intent(out) :: png_data(:)
            integer, intent(out) :: status
        end subroutine get_png_data_wrapper

        module subroutine prepare_3d_data_wrapper(this, plots)
            class(pdf_context), intent(inout) :: this
            type(plot_data_t), intent(in) :: plots(:)
        end subroutine prepare_3d_data_wrapper

        module subroutine render_ylabel_wrapper(this, ylabel)
            class(pdf_context), intent(inout) :: this
            character(len=*), intent(in) :: ylabel
        end subroutine render_ylabel_wrapper

        module subroutine draw_axes_and_labels_backend_wrapper(this, xscale, yscale, &
                                                        symlog_threshold, &
                                                        x_min, x_max, y_min, y_max, &
                                                        title, xlabel, ylabel, &
                                                        x_date_format, y_date_format, &
                                                        z_min, z_max, has_3d_plots)
            class(pdf_context), intent(inout) :: this
            character(len=*), intent(in) :: xscale, yscale
            real(wp), intent(in) :: symlog_threshold
            real(wp), intent(in) :: x_min, x_max, y_min, y_max
            character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
            character(len=*), intent(in), optional :: x_date_format, y_date_format
            real(wp), intent(in), optional :: z_min, z_max
            logical, intent(in) :: has_3d_plots
        end subroutine draw_axes_and_labels_backend_wrapper

        module subroutine pdf_save_coordinates(this, x_min, x_max, y_min, y_max)
            class(pdf_context), intent(in) :: this
            real(wp), intent(out) :: x_min, x_max, y_min, y_max
        end subroutine pdf_save_coordinates

        module subroutine pdf_set_coordinates(this, x_min, x_max, y_min, y_max)
            class(pdf_context), intent(inout) :: this
            real(wp), intent(in) :: x_min, x_max, y_min, y_max
        end subroutine pdf_set_coordinates

        module subroutine render_pdf_axes_wrapper(this, title_text, xlabel_text, ylabel_text)
            class(pdf_context), intent(inout) :: this
            character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
        end subroutine render_pdf_axes_wrapper

        module subroutine pdf_draw_secondary_y_axis_wrapper(this, yscale, symlog_threshold, &
                                                     y_min, y_max, ylabel, date_format)
            class(pdf_context), intent(inout) :: this
            character(len=*), intent(in) :: yscale
            real(wp), intent(in) :: symlog_threshold
            real(wp), intent(in) :: y_min, y_max
            character(len=:), allocatable, intent(in), optional :: ylabel
            character(len=*), intent(in), optional :: date_format
        end subroutine pdf_draw_secondary_y_axis_wrapper

        module subroutine pdf_draw_secondary_x_axis_top_wrapper(this, xscale, symlog_threshold, &
                                                         x_min, x_max, xlabel, date_format)
            class(pdf_context), intent(inout) :: this
            character(len=*), intent(in) :: xscale
            real(wp), intent(in) :: symlog_threshold
            real(wp), intent(in) :: x_min, x_max
            character(len=:), allocatable, intent(in), optional :: xlabel
            character(len=*), intent(in), optional :: date_format
        end subroutine pdf_draw_secondary_x_axis_top_wrapper
    end interface

end module fortplot_pdf
