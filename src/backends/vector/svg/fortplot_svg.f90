module fortplot_svg
    !! SVG backend main interface
    !!
    !! Provides Scalable Vector Graphics output format alongside PNG, PDF, ASCII.
    !! SVG advantages: web-native, infinitely scalable, editable, CSS styling.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_plot_data, only: plot_data_t
    use fortplot_legend, only: legend_entry_t
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_svg_markers, only: svg_draw_marker_impl
    use fortplot_markers, only: marker_size_scale
    use fortplot_svg_legend, only: svg_render_legend_impl, svg_calc_legend_dims_impl, &
                                    svg_set_legend_border_impl, svg_calc_legend_pos_impl
    use fortplot_svg_axes, only: svg_render_ylabel_impl, svg_draw_axes_labels_impl
    use fortplot_svg_draw, only: svg_draw_line_impl, svg_draw_arrow_impl, &
                                  svg_draw_arrowhead_impl, &
                                  svg_fill_quad_impl, svg_fill_heatmap_impl, &
                                  svg_write_file_impl, svg_add_to_stream
    implicit none

    private
    public :: svg_context, create_svg_canvas

    type, extends(plot_context) :: svg_context
        character(len=:), allocatable :: content_stream
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        real(wp) :: current_r = 0.0_wp, current_g = 0.0_wp, current_b = 0.0_wp
        real(wp) :: current_line_width = 1.0_wp
        character(len=32) :: current_dash_pattern = ''
        real(wp) :: marker_edge_r = 0.0_wp, marker_edge_g = 0.0_wp
        real(wp) :: marker_edge_b = 0.0_wp
        real(wp) :: marker_face_r = 0.0_wp, marker_face_g = 0.0_wp
        real(wp) :: marker_face_b = 1.0_wp
        real(wp) :: marker_edge_alpha = 1.0_wp, marker_face_alpha = 1.0_wp
        logical, private :: axes_rendered = .false.
    contains
        procedure :: line => draw_svg_line
        procedure :: color => set_svg_color
        procedure :: text => draw_svg_text
        procedure :: save => write_svg_file
        procedure :: set_line_width => set_svg_line_width
        procedure :: set_line_style => set_svg_line_style
        procedure :: draw_marker => draw_svg_marker
        procedure :: set_marker_colors => set_svg_marker_colors
        procedure :: set_marker_colors_with_alpha => set_svg_marker_colors_alpha
        procedure :: draw_arrow => draw_svg_arrow
        procedure :: draw_arrowhead => draw_svg_arrowhead
        procedure :: get_ascii_output => svg_get_ascii_output
        procedure :: get_width_scale => svg_get_width_scale
        procedure :: get_height_scale => svg_get_height_scale
        procedure :: fill_quad => svg_fill_quad
        procedure :: fill_heatmap => svg_fill_heatmap
        procedure :: render_legend_specialized => svg_render_legend
        procedure :: calculate_legend_dimensions => svg_calc_legend_dims
        procedure :: set_legend_border_width => svg_set_legend_border
        procedure :: calculate_legend_position_backend => svg_calc_legend_pos
        procedure :: extract_rgb_data => svg_extract_rgb_data
        procedure :: get_png_data_backend => svg_get_png_data
        procedure :: prepare_3d_data => svg_prepare_3d_data
        procedure :: render_ylabel => svg_render_ylabel
        procedure :: draw_axes_and_labels_backend => svg_draw_axes_labels
        procedure :: save_coordinates => svg_save_coordinates
        procedure :: set_coordinates => svg_set_coordinates
        procedure :: render_axes => svg_render_axes
        procedure, private :: add_to_stream
        procedure, private :: normalize_to_svg
    end type svg_context

contains

    function create_svg_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(svg_context) :: ctx

        call setup_canvas(ctx, width, height)
        ctx%content_stream = ''
        ctx%margins = plot_margins_t()
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
    end function create_svg_canvas

    subroutine add_to_stream(this, content)
        class(svg_context), intent(inout) :: this
        character(len=*), intent(in) :: content

        call svg_add_to_stream(this%content_stream, content)
    end subroutine add_to_stream

    subroutine normalize_to_svg(this, data_x, data_y, svg_x, svg_y)
        class(svg_context), intent(in) :: this
        real(wp), intent(in) :: data_x, data_y
        real(wp), intent(out) :: svg_x, svg_y
        real(wp) :: x_range, y_range, x_norm, y_norm

        x_range = this%x_max - this%x_min
        y_range = this%y_max - this%y_min
        if (abs(x_range) < 1.0e-12_wp) x_range = 1.0_wp
        if (abs(y_range) < 1.0e-12_wp) y_range = 1.0_wp

        x_norm = (data_x - this%x_min)/x_range
        y_norm = (data_y - this%y_min)/y_range

        svg_x = real(this%plot_area%left, wp) + x_norm*real(this%plot_area%width, wp)
        svg_y = real(this%plot_area%bottom + this%plot_area%height, wp) - &
                y_norm*real(this%plot_area%height, wp)
    end subroutine normalize_to_svg

   subroutine draw_svg_line(this, x1, y1, x2, y2)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2

        call svg_draw_line_impl(x1, y1, x2, y2, &
            real(this%plot_area%left, wp), real(this%plot_area%bottom, wp), &
            real(this%plot_area%width, wp), real(this%plot_area%height, wp), &
            this%x_min, this%x_max, this%y_min, this%y_max, &
            this%current_r, this%current_g, this%current_b, &
            this%current_line_width, this%current_dash_pattern, &
            this%content_stream)
    end subroutine draw_svg_line

    subroutine set_svg_color(this, r, g, b)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        this%current_r = max(0.0_wp, min(1.0_wp, r))
        this%current_g = max(0.0_wp, min(1.0_wp, g))
        this%current_b = max(0.0_wp, min(1.0_wp, b))
    end subroutine set_svg_color

    subroutine set_svg_line_width(this, width)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: width

        this%current_line_width = max(0.1_wp, width)
    end subroutine set_svg_line_width

    subroutine set_svg_line_style(this, style)
        class(svg_context), intent(inout) :: this
        character(len=*), intent(in) :: style

        select case (trim(style))
        case ('-', 'solid')
            this%current_dash_pattern = ''
        case ('--', 'dashed')
            this%current_dash_pattern = '6,3'
        case (':', 'dotted')
            this%current_dash_pattern = '2,3'
        case ('-.', 'dashdot')
            this%current_dash_pattern = '6,3,2,3'
        case default
            this%current_dash_pattern = ''
        end select
    end subroutine set_svg_line_style

    subroutine draw_svg_text(this, x, y, text)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: sx, sy
        character(len=1024) :: text_elem

        call this%normalize_to_svg(x, y, sx, sy)
        write (text_elem, '(A,F0.3,A,F0.3,A,A,A)') &
            '<text x="', sx, '" y="', sy, &
            '" font-family="sans-serif" font-size="12">', &
            trim(text), '</text>'
        call this%add_to_stream(trim(text_elem))
    end subroutine draw_svg_text

    subroutine draw_svg_marker(this, x, y, style, size)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp), intent(in), optional :: size
        real(wp) :: sx, sy, scale
        character(len=:), allocatable :: elem
        character(len=64) :: fill_color, edge_color
        character(len=64) :: fill_opacity, stroke_opacity, stroke_width

        scale = 1.0_wp
        if (present(size)) scale = marker_size_scale(size)

        call this%normalize_to_svg(x, y, sx, sy)

        write (fill_color, '(A,F0.1,A,F0.1,A,F0.1,A)') 'rgb(', &
            this%marker_face_r*255.0_wp, ',', &
            this%marker_face_g*255.0_wp, ',', &
            this%marker_face_b*255.0_wp, ')'
        write (edge_color, '(A,F0.1,A,F0.1,A,F0.1,A)') 'rgb(', &
            this%marker_edge_r*255.0_wp, ',', &
            this%marker_edge_g*255.0_wp, ',', &
            this%marker_edge_b*255.0_wp, ')'

        write (fill_opacity, '(A,F0.3,A)') ' fill-opacity="', &
            max(0.0_wp, min(1.0_wp, this%marker_face_alpha)), '"'
        write (stroke_opacity, '(A,F0.3,A)') ' stroke-opacity="', &
            max(0.0_wp, min(1.0_wp, this%marker_edge_alpha)), '"'
        write (stroke_width, '(A,F0.3,A)') ' stroke-width="', &
            max(0.0_wp, this%current_line_width), '"'

        call svg_draw_marker_impl(sx, sy, style, fill_color, edge_color, &
                                  fill_opacity, stroke_opacity, stroke_width, elem, &
                                  scale)
        call this%add_to_stream(trim(elem))
    end subroutine draw_svg_marker

    subroutine set_svg_marker_colors(this, edge_r, edge_g, edge_b, &
                                     face_r, face_g, face_b)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, face_r, face_g, face_b

        this%marker_edge_r = max(0.0_wp, min(1.0_wp, edge_r))
        this%marker_edge_g = max(0.0_wp, min(1.0_wp, edge_g))
        this%marker_edge_b = max(0.0_wp, min(1.0_wp, edge_b))
        this%marker_face_r = max(0.0_wp, min(1.0_wp, face_r))
        this%marker_face_g = max(0.0_wp, min(1.0_wp, face_g))
        this%marker_face_b = max(0.0_wp, min(1.0_wp, face_b))
    end subroutine set_svg_marker_colors

    subroutine set_svg_marker_colors_alpha(this, edge_r, edge_g, edge_b, &
                                           edge_alpha, face_r, face_g, face_b, &
                                           face_alpha)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha

        call this%set_marker_colors(edge_r, edge_g, edge_b, face_r, face_g, face_b)
        this%marker_edge_alpha = max(0.0_wp, min(1.0_wp, edge_alpha))
        this%marker_face_alpha = max(0.0_wp, min(1.0_wp, face_alpha))
    end subroutine set_svg_marker_colors_alpha

  subroutine draw_svg_arrow(this, x, y, dx, dy, size, style)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style

        call svg_draw_arrow_impl(x, y, dx, dy, size, style, &
            real(this%plot_area%left, wp), real(this%plot_area%bottom, wp), &
            real(this%plot_area%width, wp), real(this%plot_area%height, wp), &
            this%x_min, this%x_max, this%y_min, this%y_max, &
            this%current_r, this%current_g, this%current_b, &
            this%content_stream)
    end subroutine draw_svg_arrow

  subroutine draw_svg_arrowhead(this, x, y, dx, dy, size, style)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style

        call svg_draw_arrowhead_impl(x, y, dx, dy, size, style, &
            real(this%plot_area%left, wp), real(this%plot_area%bottom, wp), &
            real(this%plot_area%width, wp), real(this%plot_area%height, wp), &
            this%x_min, this%x_max, this%y_min, this%y_max, &
            this%current_r, this%current_g, this%current_b, &
            this%content_stream)
    end subroutine draw_svg_arrowhead

  subroutine svg_fill_quad(this, x_quad, y_quad)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)

        call svg_fill_quad_impl(x_quad, y_quad, &
            real(this%plot_area%left, wp), real(this%plot_area%bottom, wp), &
            real(this%plot_area%width, wp), real(this%plot_area%height, wp), &
            this%x_min, this%x_max, this%y_min, this%y_max, &
            this%current_r, this%current_g, this%current_b, &
            this%content_stream)
    end subroutine svg_fill_quad

    subroutine svg_fill_heatmap(this, x_grid, y_grid, z_grid, z_min, z_max, colormap_name)
        class(svg_context), intent(inout) :: this
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: z_min, z_max
        character(len=*), intent(in), optional :: colormap_name

        call svg_fill_heatmap_impl(x_grid, y_grid, z_grid, z_min, z_max, &
            colormap_name, &
            real(this%plot_area%left, wp), real(this%plot_area%bottom, wp), &
            real(this%plot_area%width, wp), real(this%plot_area%height, wp), &
            this%x_min, this%x_max, this%y_min, this%y_max, &
            this%content_stream)
    end subroutine svg_fill_heatmap

    subroutine write_svg_file(this, filename)
        class(svg_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer :: ios

        call svg_write_file_impl(filename, this%content_stream, &
            this%width, this%height, ios)
    end subroutine write_svg_file

    function svg_get_ascii_output(this) result(output)
        class(svg_context), intent(in) :: this
        character(len=:), allocatable :: output
        output = "SVG output (non-ASCII format)"
    end function svg_get_ascii_output

    real(wp) function svg_get_width_scale(this) result(scale)
        class(svg_context), intent(in) :: this
        real(wp) :: x_range
        x_range = this%x_max - this%x_min
        if (abs(x_range) < 1.0e-12_wp) x_range = 1.0_wp
        scale = real(this%plot_area%width, wp)/x_range
    end function svg_get_width_scale

    real(wp) function svg_get_height_scale(this) result(scale)
        class(svg_context), intent(in) :: this
        real(wp) :: y_range
        y_range = this%y_max - this%y_min
        if (abs(y_range) < 1.0e-12_wp) y_range = 1.0_wp
        scale = real(this%plot_area%height, wp)/y_range
    end function svg_get_height_scale

    subroutine svg_render_legend(this, entries, x, y, width, height)
        class(svg_context), intent(inout) :: this
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(in) :: x, y, width, height
        real(wp) :: lx, ly, lw, lh

        call svg_calc_legend_pos_impl( &
            real(this%plot_area%left, wp), &
            real(this%plot_area%bottom, wp), &
            real(this%plot_area%width, wp), &
            real(this%plot_area%height, wp), &
            'upper right', lx, ly)
        call svg_calc_legend_dims_impl(entries, lw, lh)
        call svg_render_legend_impl(this%content_stream, entries, lx, ly, lw, lh)
    end subroutine svg_render_legend

    subroutine svg_calc_legend_dims(this, entries, width, height)
        class(svg_context), intent(in) :: this
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(out) :: width, height

        call svg_calc_legend_dims_impl(entries, width, height)
    end subroutine svg_calc_legend_dims

    subroutine svg_set_legend_border(this, width)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: width

        call svg_set_legend_border_impl(width)
    end subroutine svg_set_legend_border

    subroutine svg_calc_legend_pos(this, loc, x, y)
        class(svg_context), intent(in) :: this
        character(len=*), intent(in) :: loc
        real(wp), intent(out) :: x, y

        call svg_calc_legend_pos_impl( &
            real(this%plot_area%left, wp), &
            real(this%plot_area%bottom, wp), &
            real(this%plot_area%width, wp), &
            real(this%plot_area%height, wp), loc, x, y)
    end subroutine svg_calc_legend_pos

    subroutine svg_extract_rgb_data(this, width, height, rgb_data)
        class(svg_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)
        rgb_data = 1.0_wp
    end subroutine svg_extract_rgb_data

    subroutine svg_get_png_data(this, width, height, png_data, status)
        class(svg_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        associate (w => width, h => height, s => this); end associate
        allocate (png_data(0))
        status = 1
    end subroutine svg_get_png_data

    subroutine svg_prepare_3d_data(this, plots)
        class(svg_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)
        associate (p => plots, s => this); end associate
    end subroutine svg_prepare_3d_data

    subroutine svg_render_ylabel(this, ylabel)
        class(svg_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        character(len=:), allocatable :: elem

        call svg_render_ylabel_impl( &
            real(this%plot_area%left, wp), &
            real(this%plot_area%bottom, wp), &
            real(this%plot_area%height, wp), &
            ylabel, elem)
        call this%add_to_stream(trim(elem))
    end subroutine svg_render_ylabel

    subroutine svg_draw_axes_labels(this, xscale, yscale, symlog_threshold, &
                                    x_min, x_max, y_min, y_max, &
                                    title, xlabel, ylabel, &
                                    x_date_format, y_date_format, &
                                    z_min, z_max, has_3d_plots)
        class(svg_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        real(wp) :: left, right, bottom, top

        left = real(this%plot_area%left, wp)
        right = left + real(this%plot_area%width, wp)
        bottom = real(this%plot_area%bottom + this%plot_area%height, wp)
        top = real(this%plot_area%bottom, wp)

        call svg_draw_axes_labels_impl(this%content_stream, left, right, bottom, top, &
                                       xscale, yscale, symlog_threshold, &
                                       x_min, x_max, y_min, y_max, &
                                       title, xlabel, ylabel, &
                                       x_date_format, y_date_format, &
                                       z_min, z_max, has_3d_plots)
    end subroutine svg_draw_axes_labels

    subroutine svg_save_coordinates(this, x_min, x_max, y_min, y_max)
        class(svg_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max
        x_min = this%x_min
        x_max = this%x_max
        y_min = this%y_min
        y_max = this%y_max
    end subroutine svg_save_coordinates

    subroutine svg_set_coordinates(this, x_min, x_max, y_min, y_max)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max
        this%axes_rendered = .false.
    end subroutine svg_set_coordinates

    subroutine svg_render_axes(this, title_text, xlabel_text, ylabel_text)
        class(svg_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
        character(len=:), allocatable :: t, xl, yl

        if (this%axes_rendered) return
        if (abs(this%x_max - this%x_min) <= epsilon(1.0_wp) .or. &
            abs(this%y_max - this%y_min) <= epsilon(1.0_wp)) return

        t = ''
        xl = ''
        yl = ''
        if (present(title_text)) t = title_text
        if (present(xlabel_text)) xl = xlabel_text
        if (present(ylabel_text)) yl = ylabel_text

        call this%draw_axes_and_labels_backend('linear', 'linear', 1.0_wp, &
                                               this%x_min, this%x_max, &
                                               this%y_min, this%y_max, &
                                               t, xl, yl, has_3d_plots=.false.)
        this%axes_rendered = .true.
    end subroutine svg_render_axes

end module fortplot_svg
