module fortplot_svg
    !! SVG backend main interface
    !!
    !! Provides Scalable Vector Graphics output format alongside PNG, PDF, ASCII.
    !! SVG advantages: web-native, infinitely scalable, editable, CSS styling.

    use, intrinsic :: iso_fortran_env, only: wp => real64
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_plot_data, only: plot_data_t
    use fortplot_legend, only: legend_entry_t
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_logging, only: log_error, log_info
    use fortplot_colormap, only: colormap_value_to_color
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

        if (allocated(this%content_stream)) then
            this%content_stream = this%content_stream//content//new_line('a')
        else
            this%content_stream = content//new_line('a')
        end if
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
        real(wp) :: sx1, sy1, sx2, sy2
        character(len=512) :: line_elem
        character(len=128) :: stroke_dasharray

        if (ieee_is_nan(x1) .or. ieee_is_nan(y1) .or. &
            ieee_is_nan(x2) .or. ieee_is_nan(y2)) return

        call this%normalize_to_svg(x1, y1, sx1, sy1)
        call this%normalize_to_svg(x2, y2, sx2, sy2)

        stroke_dasharray = ''
        if (len_trim(this%current_dash_pattern) > 0) then
            stroke_dasharray = ' stroke-dasharray="'// &
                               trim(this%current_dash_pattern)//'"'
        end if

        write (line_elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.1,A,F0.1,A,F0.1,A, &
&           F0.3,A)') &
            '<line x1="', sx1, '" y1="', sy1, '" x2="', sx2, '" y2="', sy2, &
            '" stroke="rgb(', this%current_r*255.0_wp, ',', &
            this%current_g*255.0_wp, ',', this%current_b*255.0_wp, &
            ')" stroke-width="', this%current_line_width, &
            '"'//trim(stroke_dasharray)//'/>'
        call this%add_to_stream(trim(line_elem))
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

	    subroutine draw_svg_marker(this, x, y, style)
	        class(svg_context), intent(inout) :: this
	        real(wp), intent(in) :: x, y
	        character(len=*), intent(in) :: style
	        real(wp) :: sx, sy, r
	        character(len=512) :: elem
	        character(len=64) :: fill_color, edge_color
	        character(len=64) :: fill_opacity, stroke_opacity, stroke_width
	        real(wp) :: half

	        call this%normalize_to_svg(x, y, sx, sy)
	        r = 4.0_wp

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

	        select case (trim(style))
	        case ('o', 'circle')
	            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A,A,A,A)') &
	                '<circle cx="', sx, '" cy="', sy, '" r="', r, &
	                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
	                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
	        case ('s', 'square')
	            half = r
	            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A,A,A,A)') &
	                '<rect x="', sx - half, '" y="', sy - half, &
	                '" width="', 2.0_wp*half, '" height="', 2.0_wp*half, &
	                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
	                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
	        case ('^', 'triangle_up')
	            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A, &
&               A,A,A,A)') &
	                '<polygon points="', sx, ',', sy - r, ' ', sx - r, ',', sy + r, &
	                ' ', sx + r, ',', sy + r, &
	                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
	                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
	        case ('v', 'triangle_down')
	            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A, &
&               A,A,A,A)') &
	                '<polygon points="', sx, ',', sy + r, ' ', sx - r, ',', sy - r, &
	                ' ', sx + r, ',', sy - r, &
	                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
	                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
	        case ('D', 'diamond')
	            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3, &
&               A,A,A,A,A,A,A,A,A)') &
	                '<polygon points="', sx, ',', sy - r, ' ', sx + r, ',', sy, &
	                ' ', sx, ',', sy + r, ' ', sx - r, ',', sy, &
	                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
	                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
	        case ('+', 'plus')
	            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A,A,A,A, &
&               F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A)') &
	                '<line x1="', sx - r, '" y1="', sy, '" x2="', sx + r, '" y2="', sy, &
	                '" stroke="', trim(edge_color), trim(stroke_opacity), &
	                trim(stroke_width), '/><line x1="', sx, '" y1="', sy - r, &
	                '" x2="', sx, '" y2="', sy + r, '" stroke="', &
	                trim(edge_color), trim(stroke_opacity), trim(stroke_width), '/>'
	        case ('x', 'cross')
	            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A,A,A,A, &
&               F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A)') &
	                '<line x1="', sx - r, '" y1="', sy - r, '" x2="', sx + r, &
	                '" y2="', sy + r, '" stroke="', trim(edge_color), &
	                trim(stroke_opacity), trim(stroke_width), '/><line x1="', sx - r, &
	                '" y1="', sy + r, '" x2="', sx + r, '" y2="', sy - r, &
	                '" stroke="', trim(edge_color), trim(stroke_opacity), &
	                trim(stroke_width), '/>'
	        case ('.', 'point')
	            write (elem, '(A,F0.3,A,F0.3,A,A,A,A,A,A)') &
	                '<circle cx="', sx, '" cy="', sy, '" r="2" fill="', &
	                trim(fill_color), trim(fill_opacity), '/>'
	        case default
	            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,A,A,A,A,A,A,A,A)') &
	                '<circle cx="', sx, '" cy="', sy, '" r="', r, &
	                '" fill="', trim(fill_color), '" stroke="', trim(edge_color), &
	                trim(fill_opacity), trim(stroke_opacity), trim(stroke_width), '/>'
	        end select
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
        real(wp) :: sx, sy, mag, nx, ny, px, py
        real(wp) :: arrow_len, arrow_w, base_x, base_y
        real(wp) :: left_x, left_y, right_x, right_y
        character(len=512) :: elem

        call this%normalize_to_svg(x, y, sx, sy)
        mag = sqrt(dx*dx + dy*dy)
        if (mag < 1.0e-12_wp) return
        nx = dx/mag
        ny = -dy/mag
        px = -ny
        py = nx

        arrow_len = max(4.0_wp, 1.5_wp*size)
        arrow_w = 0.55_wp*arrow_len
        base_x = sx - arrow_len*nx
        base_y = sy - arrow_len*ny
        left_x = base_x + arrow_w*px
        left_y = base_y + arrow_w*py
        right_x = base_x - arrow_w*px
        right_y = base_y - arrow_w*py

        if (index(style, '>') > 0 .or. index(style, '<') > 0 .or. &
            style == 'filled' .or. style == 'open') then
            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.1,A, &
&               F0.1,A,F0.1,A)') &
                '<polygon points="', sx, ',', sy, ' ', left_x, ',', left_y, &
                ' ', right_x, ',', right_y, '" fill="rgb(', &
                this%current_r*255.0_wp, ',', this%current_g*255.0_wp, &
                ',', this%current_b*255.0_wp, ')"/>'
            call this%add_to_stream(trim(elem))
        end if
    end subroutine draw_svg_arrow

    subroutine svg_fill_quad(this, x_quad, y_quad)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp) :: sx(4), sy(4)
        character(len=512) :: elem
        integer :: i

        do i = 1, 4
            call this%normalize_to_svg(x_quad(i), y_quad(i), sx(i), sy(i))
        end do

        write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.3, &
&           A,F0.1,A,F0.1,A,F0.1,A)') &
            '<polygon points="', sx(1), ',', sy(1), ' ', sx(2), ',', sy(2), &
            ' ', sx(3), ',', sy(3), ' ', sx(4), ',', sy(4), &
            '" fill="rgb(', this%current_r*255.0_wp, ',', &
            this%current_g*255.0_wp, ',', this%current_b*255.0_wp, &
            ')" stroke="none"/>'
        call this%add_to_stream(trim(elem))
    end subroutine svg_fill_quad

    subroutine svg_fill_heatmap(this, x_grid, y_grid, z_grid, z_min, z_max)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: z_min, z_max
        integer :: i, j, nx, ny
        real(wp) :: x_quad(4), y_quad(4), value
        real(wp), dimension(3) :: clr

        nx = size(x_grid)
        ny = size(y_grid)
        if (nx < 2 .or. ny < 2) return
        if (size(z_grid, 1) /= ny .or. size(z_grid, 2) /= nx) return

        do j = 1, ny - 1
            do i = 1, nx - 1
                value = z_grid(j, i)
                call colormap_value_to_color(value, z_min, z_max, 'viridis', clr)
                call this%color(clr(1), clr(2), clr(3))

                x_quad(1) = x_grid(i)
                x_quad(2) = x_grid(i + 1)
                x_quad(3) = x_grid(i + 1)
                x_quad(4) = x_grid(i)
                y_quad(1) = y_grid(j)
                y_quad(2) = y_grid(j)
                y_quad(3) = y_grid(j + 1)
                y_quad(4) = y_grid(j + 1)
                call this%fill_quad(x_quad, y_quad)
            end do
        end do
    end subroutine svg_fill_heatmap

    subroutine write_svg_file(this, filename)
        use fortplot_system_viewer, only: launch_system_viewer, &
                                          has_graphical_session, &
                                          get_temp_filename
        class(svg_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        integer :: unit, ios
        character(len=1024) :: actual_filename
        logical :: viewer_success

        if (trim(filename) == 'terminal') then
            if (has_graphical_session()) then
                call get_temp_filename('.svg', actual_filename)
            else
                call log_info("No graphical session detected for SVG display")
                return
            end if
        else
            actual_filename = filename
        end if

        open (newunit=unit, file=trim(actual_filename), status='replace', &
              form='formatted', action='write', iostat=ios)
        if (ios /= 0) then
            call log_error('SVG: failed to open file: '//trim(actual_filename))
            return
        end if

        write (unit, '(A)') '<?xml version="1.0" encoding="UTF-8"?>'
        write (unit, '(A,I0,A,I0,A)') &
            '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ', &
            this%width, ' ', this%height, '">'

        write (unit, '(A,I0,A,I0,A)') &
            '<rect width="', this%width, '" height="', this%height, &
            '" fill="white"/>'

        if (allocated(this%content_stream)) then
            write (unit, '(A)') trim(this%content_stream)
        end if

        write (unit, '(A)') '</svg>'
        close (unit)

        if (trim(filename) == 'terminal' .and. has_graphical_session()) then
            call launch_system_viewer(actual_filename, viewer_success)
            if (.not. viewer_success) then
                call log_error("Failed to launch SVG viewer")
            end if
        end if
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
        integer :: i, n
        real(wp) :: entry_h, lx, ly
        character(len=1024) :: elem

        n = size(entries)
        if (n == 0) return
        entry_h = height/real(n, wp)

        write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A)') &
            '<rect x="', x, '" y="', y, '" width="', width, &
            '" height="', height, '" fill="white" stroke="black"/>'
        call this%add_to_stream(trim(elem))

        do i = 1, n
            ly = y + real(i - 1, wp)*entry_h + entry_h*0.5_wp
            lx = x + 5.0_wp
            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,F0.1,A,F0.1,A,F0.1,A)') &
                '<line x1="', lx, '" y1="', ly, '" x2="', lx + 20.0_wp, &
                '" y2="', ly, '" stroke="rgb(', &
                entries(i)%color(1)*255.0_wp, ',', &
                entries(i)%color(2)*255.0_wp, ',', &
                entries(i)%color(3)*255.0_wp, ')" stroke-width="2"/>'
            call this%add_to_stream(trim(elem))

            if (allocated(entries(i)%label)) then
                write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
                    '<text x="', lx + 25.0_wp, '" y="', ly + 4.0_wp, &
                    '" font-family="sans-serif" font-size="10">', &
                    trim(entries(i)%label), '</text>'
                call this%add_to_stream(trim(elem))
            end if
        end do
    end subroutine svg_render_legend

    subroutine svg_calc_legend_dims(this, entries, width, height)
        class(svg_context), intent(in) :: this
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(out) :: width, height
        integer :: n, i, max_len

        n = size(entries)
        max_len = 0
        do i = 1, n
            if (allocated(entries(i)%label)) then
                max_len = max(max_len, len_trim(entries(i)%label))
            end if
        end do
        width = 30.0_wp + real(max_len, wp)*7.0_wp
        height = real(n, wp)*18.0_wp + 10.0_wp
    end subroutine svg_calc_legend_dims

    subroutine svg_set_legend_border(this, width)
        class(svg_context), intent(inout) :: this
        real(wp), intent(in) :: width
        associate (w => width); end associate
    end subroutine svg_set_legend_border

    subroutine svg_calc_legend_pos(this, loc, x, y)
        class(svg_context), intent(in) :: this
        character(len=*), intent(in) :: loc
        real(wp), intent(out) :: x, y

        select case (trim(loc))
        case ('upper left')
            x = real(this%plot_area%left, wp) + 10.0_wp
            y = real(this%plot_area%bottom, wp) + 10.0_wp
        case ('upper right')
            x = real(this%plot_area%left + this%plot_area%width, wp) - 110.0_wp
            y = real(this%plot_area%bottom, wp) + 10.0_wp
        case ('lower left')
            x = real(this%plot_area%left, wp) + 10.0_wp
            y = real(this%plot_area%bottom + this%plot_area%height, wp) - 60.0_wp
        case ('lower right')
            x = real(this%plot_area%left + this%plot_area%width, wp) - 110.0_wp
            y = real(this%plot_area%bottom + this%plot_area%height, wp) - 60.0_wp
        case default
            x = real(this%plot_area%left + this%plot_area%width, wp) - 110.0_wp
            y = real(this%plot_area%bottom, wp) + 10.0_wp
        end select
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
        real(wp) :: x, y
        character(len=1024) :: elem

        x = real(this%plot_area%left, wp) - 35.0_wp
        y = real(this%plot_area%bottom, wp) + &
            real(this%plot_area%height, wp)*0.5_wp

        write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
            '<text x="', x, '" y="', y, &
            '" font-family="sans-serif" font-size="12" '// &
            'text-anchor="middle" transform="rotate(-90 ', x, ' ', y, ')">', &
            trim(ylabel), '</text>'
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
        character(len=1024) :: elem
        real(wp) :: left, right, top, bottom, mid_x, mid_y
        integer :: i
        real(wp) :: tick_x, tick_y, val
        character(len=32) :: val_str

        associate (xs => xscale, ys => yscale, st => symlog_threshold, &
                   zmi => z_min, zma => z_max, h3d => has_3d_plots)
        end associate
        if (present(x_date_format)) then
            associate (unused_xfmt => len_trim(x_date_format)); end associate
        end if
        if (present(y_date_format)) then
            associate (unused_yfmt => len_trim(y_date_format)); end associate
        end if

        left = real(this%plot_area%left, wp)
        right = left + real(this%plot_area%width, wp)
        bottom = real(this%plot_area%bottom + this%plot_area%height, wp)
        top = real(this%plot_area%bottom, wp)

        write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A)') &
            '<rect x="', left, '" y="', top, '" width="', right - left, &
            '" height="', bottom - top, '" fill="none" stroke="black"/>'
        call this%add_to_stream(trim(elem))

        do i = 0, 4
            tick_x = left + real(i, wp)/4.0_wp*(right - left)
            val = x_min + real(i, wp)/4.0_wp*(x_max - x_min)
            write (val_str, '(G10.3)') val
            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A)') &
                '<line x1="', tick_x, '" y1="', bottom, &
                '" x2="', tick_x, '" y2="', bottom + 5.0_wp, '" stroke="black"/>'
            call this%add_to_stream(trim(elem))
            write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
                '<text x="', tick_x, '" y="', bottom + 18.0_wp, &
                '" font-family="sans-serif" font-size="10" text-anchor="middle">', &
                trim(adjustl(val_str)), '</text>'
            call this%add_to_stream(trim(elem))
        end do

        do i = 0, 4
            tick_y = top + real(i, wp)/4.0_wp*(bottom - top)
            val = y_max - real(i, wp)/4.0_wp*(y_max - y_min)
            write (val_str, '(G10.3)') val
            write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A)') &
                '<line x1="', left - 5.0_wp, '" y1="', tick_y, &
                '" x2="', left, '" y2="', tick_y, '" stroke="black"/>'
            call this%add_to_stream(trim(elem))
            write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
                '<text x="', left - 8.0_wp, '" y="', tick_y + 4.0_wp, &
                '" font-family="sans-serif" font-size="10" text-anchor="end">', &
                trim(adjustl(val_str)), '</text>'
            call this%add_to_stream(trim(elem))
        end do

        mid_x = (left + right)/2.0_wp
        mid_y = (top + bottom)/2.0_wp

        if (present(title)) then
            if (len_trim(title) > 0) then
                write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
                    '<text x="', mid_x, '" y="', top - 10.0_wp, &
                    '" font-family="sans-serif" font-size="14" '// &
                    'font-weight="bold" text-anchor="middle">', &
                    trim(title), '</text>'
                call this%add_to_stream(trim(elem))
            end if
        end if

        if (present(xlabel)) then
            if (len_trim(xlabel) > 0) then
                write (elem, '(A,F0.3,A,F0.3,A,A,A)') &
                    '<text x="', mid_x, '" y="', bottom + 35.0_wp, &
                    '" font-family="sans-serif" font-size="12" text-anchor="middle">', &
                    trim(xlabel), '</text>'
                call this%add_to_stream(trim(elem))
            end if
        end if

        if (present(ylabel)) then
            if (len_trim(ylabel) > 0) then
                write (elem, '(A,F0.3,A,F0.3,A,F0.3,A,F0.3,A,A,A)') &
                    '<text x="', left - 45.0_wp, '" y="', mid_y, &
                    '" font-family="sans-serif" font-size="12" '// &
                    'text-anchor="middle" transform="rotate(-90 ', &
                    left - 45.0_wp, ' ', mid_y, ')">', trim(ylabel), '</text>'
                call this%add_to_stream(trim(elem))
            end if
        end if
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
