module fortplot_ascii
    !! ASCII terminal plotting backend

    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_constants, only: ASCII_CHAR_ASPECT
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_ascii_mathtext, only: sanitize_ascii_text
    use fortplot_ascii_utils, only: text_element_t
    use fortplot_ascii_elements, only: draw_ascii_marker, fill_ascii_heatmap, &
                                       draw_ascii_arrow, draw_ascii_vector_arrow
    use fortplot_ascii_drawing, only: draw_ascii_stream_segment
    use fortplot_ascii_legend, only: ascii_render_legend_impl, ascii_calc_legend_dims_impl, &
                                     ascii_set_legend_border_impl, ascii_calc_legend_pos_impl, &
                                     ascii_add_legend_entry_impl, ascii_clear_legend_impl, &
                                     ascii_clear_pie_legend_impl, ascii_register_pie_legend_impl, &
                                     decode_ascii_legend_line
    use fortplot_ascii_backend_ops, only: ascii_extract_rgb_impl, ascii_get_png_impl, &
                                          ascii_prepare_3d_impl, ascii_render_ylabel_impl, &
                                          ascii_draw_axes_impl, ascii_save_coord_impl, &
                                          ascii_set_coord_impl, ascii_render_axes_impl
    use fortplot_ascii_rendering, only: ascii_finalize => ascii_finalize, &
                                        ascii_get_output, output_to_file
    use fortplot_ascii_primitives, only: ascii_draw_line_primitive, &
                                         ascii_fill_quad_primitive
    use fortplot_3d_axes, only: draw_3d_axes
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none

    private
    public :: ascii_context, create_ascii_canvas, ASCII_CHAR_ASPECT

    !! Upper bound on tick labels one axis can request; four axes (primary and
    !! secondary x/y) size the text-element headroom for twin plots.
    integer, parameter :: MAX_ASCII_TICKS = 20

    type, extends(plot_context) :: ascii_context
        character(len=1), allocatable :: canvas(:, :)
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        character(len=:), allocatable :: title_text
        character(len=:), allocatable :: xlabel_text
        character(len=:), allocatable :: ylabel_text
        logical :: title_set = .false.  ! Track if title was explicitly set
        type(text_element_t), allocatable :: text_elements(:)
        integer :: num_text_elements = 0
        type(text_element_t), allocatable :: arrow_elements(:)
        integer :: num_arrow_elements = 0
        real(wp) :: current_r, current_g, current_b
        logical :: stream_mode = .false.
        integer :: plot_width = 80
        integer :: plot_height = 24
        character(len=96), allocatable :: legend_lines(:)
        integer :: num_legend_lines = 0
        logical :: capturing_legend = .false.
        real(wp) :: stored_y_min = 0.0_wp
        real(wp) :: stored_y_max = 0.0_wp
        logical :: has_stored_y_range = .false.
        character(len=16) :: last_xscale = 'linear'
        character(len=16) :: last_yscale = 'linear'
        real(wp) :: last_symlog_threshold = 1.0_wp
        !! Optional custom x-tick positions/labels forwarded by the render
        !! engine so the ASCII axis honours ``set_xticks`` (issue #1714).
        real(wp), allocatable :: custom_xtick_positions(:)
        character(len=64), allocatable :: custom_xtick_labels(:)
    contains
        procedure :: line => ascii_draw_line
        procedure :: color => ascii_set_color
        procedure :: text => ascii_draw_text
        procedure :: clear_text_background => ascii_clear_text_background
        procedure :: draw_text_overlay => ascii_draw_text_overlay
        procedure :: set_line_width => ascii_set_line_width
        procedure :: set_line_style => ascii_set_line_style
        procedure :: save => ascii_save
        procedure :: save_to_unit => ascii_save_to_unit
        procedure :: set_title => ascii_set_title
        procedure :: draw_marker => ascii_draw_marker
        procedure :: set_marker_colors => ascii_set_marker_colors
        procedure :: set_marker_colors_with_alpha => ascii_set_marker_colors_with_alpha
        procedure :: fill_heatmap => ascii_fill_heatmap
        procedure :: draw_arrow => ascii_draw_arrow
        procedure :: draw_arrowhead => ascii_draw_arrowhead
        procedure :: draw_quiver_arrow => ascii_draw_quiver_arrow
        procedure :: get_ascii_output => ascii_get_output_method

        !! New polymorphic methods to eliminate SELECT TYPE
        procedure :: get_width_scale => ascii_get_width_scale
        procedure :: get_height_scale => ascii_get_height_scale
        procedure :: fill_quad => ascii_fill_quad
        procedure :: render_legend_specialized => ascii_render_legend_specialized
        procedure :: calculate_legend_dimensions => ascii_calculate_legend_dimensions
        procedure :: set_legend_border_width => ascii_set_legend_border_width
        procedure :: calculate_legend_position_backend => &
            ascii_calculate_legend_position
        procedure :: extract_rgb_data => ascii_extract_rgb_data
        procedure :: get_png_data_backend => ascii_get_png_data
        procedure :: prepare_3d_data => ascii_prepare_3d_data
        procedure :: render_ylabel => ascii_render_ylabel
        procedure :: draw_axes_and_labels_backend => ascii_draw_axes_and_labels
        procedure :: save_coordinates => ascii_save_coordinates
        procedure :: set_coordinates => ascii_set_coordinates
        procedure :: render_axes => ascii_render_axes
        procedure :: clear_ascii_legend => ascii_clear_legend_lines
        procedure :: add_ascii_legend_entry => ascii_add_legend_entry
        procedure :: clear_pie_legend_entries => ascii_clear_pie_legend_entries
        procedure :: register_pie_legend_entry => ascii_register_pie_legend_entry
    end type ascii_context

  
contains

    function create_ascii_canvas(width, height) result(ctx)
        integer, intent(in), optional :: width, height
        type(ascii_context) :: ctx
        integer :: w, h

        if (present(width) .and. width > 0) then
            w = merge(max(80, min(120, nint(real(width, wp) / 10.0_wp))), width, width > 200)
        else
            w = 80
        end if
        if (present(height) .and. height > 0) then
            h = merge(max(20, min(30, nint(real(height, wp) / 20.0_wp))), height, height > 60)
        else
            h = 24
        end if

        call setup_canvas(ctx, w, h)
        ctx%plot_width = w
        ctx%plot_height = h
        ctx%margins%left = 0.0_wp
        ctx%margins%right = 0.0_wp
        ctx%margins%bottom = 0.0_wp
        ctx%margins%top = 0.0_wp
        call calculate_plot_area(w, h, ctx%margins, ctx%plot_area)
        allocate (ctx%canvas(h, w))
        ctx%canvas = ' '
        allocate (ctx%text_elements(20))
        ctx%num_text_elements = 0
        allocate (ctx%arrow_elements(1000))
        ctx%num_arrow_elements = 0
        ctx%title_set = .false.
        allocate (ctx%legend_lines(0))
        ctx%num_legend_lines = 0
        ctx%capturing_legend = .false.
        ctx%current_r = 0.0_wp
        ctx%current_g = 0.0_wp
        ctx%current_b = 1.0_wp
    end function create_ascii_canvas

    subroutine ascii_draw_line(this, x1, y1, x2, y2)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2

        if (this%stream_mode) then
            call draw_ascii_stream_segment(this%canvas, x1, y1, x2, y2, &
                                           this%x_min, this%x_max, &
                                           this%y_min, this%y_max, this%plot_area, &
                                           this%plot_width, this%plot_height)
            return
        end if

        call ascii_draw_line_primitive(this%canvas, x1, y1, x2, y2, &
                                       this%x_min, this%x_max, this%y_min, this%y_max, &
                                       this%plot_area, this%plot_width, this%plot_height, &
                                       this%current_r, this%current_g, this%current_b)
    end subroutine ascii_draw_line

    subroutine ascii_set_color(this, r, g, b)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        this%current_r = r
        this%current_g = g
        this%current_b = b
    end subroutine ascii_set_color

   subroutine ascii_set_line_width(this, width)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: width
        associate(u => this%width, v => width); end associate
    end subroutine ascii_set_line_width

    subroutine ascii_set_line_style(this, style)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: style
        associate(u => this%width, v => style); end associate
    end subroutine ascii_set_line_style

    subroutine ascii_draw_text(this, x, y, text)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text

        character(len=500) :: processed_text
        integer :: processed_len, text_x, text_y, pw, ph
        logical :: screen_coords, use_plot_area

        call ensure_text_capacity(this, this%num_text_elements + 1)
        call sanitize_ascii_text(text, processed_text, processed_len)

        pw = this%plot_width
        ph = this%plot_height
        use_plot_area = this%plot_area%width > 0 .and. this%plot_area%height > 0
        screen_coords = .false.
        if (x >= 1.0_wp .and. x <= real(pw, wp) .and. &
            y >= 1.0_wp .and. y <= real(ph, wp)) then
            text_x = nint(x)
            text_y = nint(y)
            screen_coords = .true.
        else if (this%x_max > this%x_min .and. this%y_max > this%y_min) then
            if (use_plot_area) then
                text_x = this%plot_area%left + nint((x - this%x_min)/(this%x_max - this%x_min)* &
                         real(max(1, this%plot_area%width), wp))
                text_y = this%plot_area%bottom + this%plot_area%height - &
                         nint((y - this%y_min)/(this%y_max - this%y_min)* &
                         real(max(1, this%plot_area%height), wp))
            else
                text_x = nint((x - this%x_min)/(this%x_max - this%x_min)*real(pw, wp))
                text_y = nint((this%y_max - y)/(this%y_max - this%y_min)*real(ph, wp))
            end if
        else
            text_x = nint(x)
            text_y = nint(y)
        end if

        if (screen_coords) then
            text_x = max(1, min(text_x, pw))
            text_y = max(1, min(text_y, ph))
        else if (use_plot_area) then
            text_x = max(this%plot_area%left + 1, &
                         min(text_x, this%plot_area%left + max(1, this%plot_area%width) - 1))
            text_y = max(this%plot_area%bottom + 1, &
                         min(text_y, this%plot_area%bottom + max(1, this%plot_area%height) - 1))
        else
            text_x = max(2, min(text_x, max(2, pw - processed_len - 1)))
            text_y = max(1, min(text_y, ph))
        end if

        this%num_text_elements = this%num_text_elements + 1
        this%text_elements(this%num_text_elements)%text = processed_text(1:processed_len)
        this%text_elements(this%num_text_elements)%x = text_x
        this%text_elements(this%num_text_elements)%y = text_y
        this%text_elements(this%num_text_elements)%color_r = this%current_r
        this%text_elements(this%num_text_elements)%color_g = this%current_g
        this%text_elements(this%num_text_elements)%color_b = this%current_b
    end subroutine ascii_draw_text

    subroutine ensure_text_capacity(this, needed)
        !! Grow the stored text-element buffer so tick and axis labels are never
        !! silently dropped. Twin (secondary) axes add a second set of tick
        !! labels, easily exceeding the original fixed capacity (issue #2066).
        class(ascii_context), intent(inout) :: this
        integer, intent(in) :: needed
        type(text_element_t), allocatable :: grown(:)
        integer :: cap

        if (.not. allocated(this%text_elements)) then
            allocate (this%text_elements(max(needed, 20)))
            return
        end if
        cap = size(this%text_elements)
        if (needed <= cap) return
        allocate (grown(max(needed, 2*cap)))
        if (this%num_text_elements > 0) then
            grown(1:this%num_text_elements) = &
                this%text_elements(1:this%num_text_elements)
        end if
        call move_alloc(grown, this%text_elements)
    end subroutine ensure_text_capacity

    subroutine ascii_clear_text_background(this, x, y, width)
        !! Blank a horizontal run of canvas cells so a text overlay (legend
        !! block) reads against clear background instead of the plotted curves
        !! and axis lines underneath it.
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        integer, intent(in) :: width

        integer :: col, row, first_col, last_col

        row = nint(y)
        if (row < 1 .or. row > this%plot_height) return

        first_col = max(2, nint(x))
        last_col = min(this%plot_width - 1, first_col + width - 1)
        do col = first_col, last_col
            this%canvas(row, col) = ' '
        end do
    end subroutine ascii_clear_text_background

    subroutine ascii_draw_text_overlay(this, x, y, text)
        !! Place overlay text (legend rows) directly at the given cell so stacked
        !! entries keep their own rows. The buffered text path shifts colliding
        !! rows to dodge earlier text, which blends adjacent legend labels when
        !! the block lands over tick labels; a direct write avoids that.
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text

        character(len=500) :: processed
        integer :: processed_len, row, col, k, j

        call sanitize_ascii_text(text, processed, processed_len)
        row = nint(y)
        if (row < 1 .or. row > this%plot_height) return

        col = nint(x)
        do k = 1, processed_len
            j = col + k - 1
            if (j >= 2 .and. j <= this%plot_width - 1) then
                this%canvas(row, j) = processed(k:k)
            end if
        end do
    end subroutine ascii_draw_text_overlay

   subroutine ascii_set_title(this, title)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: title
        character(len=500) :: processed_title
        integer :: processed_len

        call sanitize_ascii_text(title, processed_title, processed_len)
        this%title_text = processed_title(1:processed_len)
        this%title_set = .true.
    end subroutine ascii_set_title

    subroutine ascii_save(this, filename)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: filename

        call ascii_finalize(this%canvas, this%text_elements, this%num_text_elements, &
                            this%arrow_elements, this%num_arrow_elements, &
                            this%plot_width, this%plot_height, &
                            this%title_text, this%xlabel_text, this%ylabel_text, &
                            this%legend_lines, this%num_legend_lines, filename)
    end subroutine ascii_save

    subroutine ascii_save_to_unit(this, unit)
        class(ascii_context), intent(inout) :: this
        integer, intent(in) :: unit

        call output_to_file(this%canvas, this%text_elements, &
                            this%num_text_elements, &
                            this%arrow_elements, this%num_arrow_elements, &
                            this%plot_width, this%plot_height, &
                            this%title_text, this%xlabel_text, this%ylabel_text, &
                            this%legend_lines, this%num_legend_lines, unit)
    end subroutine ascii_save_to_unit

    subroutine ascii_draw_marker(this, x, y, style, size)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp), intent(in), optional :: size

        ! ASCII markers are single glyphs; per-point size has no raster meaning.
        if (present(size)) then
            associate (unused => size); end associate
        end if

        call draw_ascii_marker(this%canvas, x, y, style, &
                               this%x_min, this%x_max, this%y_min, this%y_max, &
                               this%plot_area, this%plot_width, this%plot_height)
    end subroutine ascii_draw_marker

subroutine ascii_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, &
                                        face_g, face_b)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b
        associate(u => this%width + edge_r + edge_g + edge_b + face_r + face_g + face_b); end associate
    end subroutine ascii_set_marker_colors

subroutine ascii_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, &
                                                   edge_alpha, &
                                                   face_r, face_g, face_b, face_alpha)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        associate(u => this%width + edge_r + edge_g + edge_b + edge_alpha + &
                           face_r + face_g + face_b + face_alpha); end associate
    end subroutine ascii_set_marker_colors_with_alpha

subroutine ascii_fill_heatmap(this, x_grid, y_grid, z_grid, z_min, z_max, colormap_name)
        class(ascii_context), intent(inout) :: this
        real(wp), contiguous, intent(in) :: x_grid(:), y_grid(:), z_grid(:, :)
        real(wp), intent(in) :: z_min, z_max
        character(len=*), intent(in), optional :: colormap_name
        integer :: cdummy = 0

        if (present(colormap_name)) cdummy = len_trim(colormap_name)
        call fill_ascii_heatmap(this%canvas, x_grid, y_grid, z_grid, z_min, z_max, &
                                this%x_min, this%x_max, this%y_min, this%y_max, &
                                this%plot_area, this%plot_width, this%plot_height)
    end subroutine ascii_fill_heatmap

    subroutine ascii_draw_arrow(this, x, y, dx, dy, size, style)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style

        integer :: px, py, inner_width, inner_height
        character(len=1) :: arrow_char

        call draw_ascii_arrow(this%canvas, x, y, dx, dy, size, style, &
                              this%x_min, this%x_max, this%y_min, this%y_max, &
                              this%plot_area, this%width, this%height, &
                              this%has_rendered_arrows, this%uses_vector_arrows, &
                              this%has_triangular_arrows)

        if (this%num_arrow_elements < ubound(this%arrow_elements, 1)) then
            inner_width = max(1, this%plot_area%width - 2)
            inner_height = max(1, this%plot_area%height - 2)
            px = this%plot_area%left + 1 + nint((x - this%x_min)/(this%x_max - this%x_min)* &
                 real(inner_width, wp))
            py = this%plot_area%bottom + this%plot_area%height - 1 - &
                 nint((y - this%y_min)/(this%y_max - this%y_min)* &
                 real(inner_height, wp))
            if (px >= this%plot_area%left + 1 .and. px <= this%plot_area%left + this%plot_area%width - 1 .and. &
                py >= this%plot_area%bottom + 1 .and. py <= this%plot_area%bottom + this%plot_area%height - 1) then
                arrow_char = infer_ascii_arrow_char(dx, dy)
                this%num_arrow_elements = this%num_arrow_elements + 1
                this%arrow_elements(this%num_arrow_elements)%text = arrow_char
                this%arrow_elements(this%num_arrow_elements)%x = px
                this%arrow_elements(this%num_arrow_elements)%y = py
                this%arrow_elements(this%num_arrow_elements)%color_r = this%current_r
                this%arrow_elements(this%num_arrow_elements)%color_g = this%current_g
                this%arrow_elements(this%num_arrow_elements)%color_b = this%current_b
            end if
        end if
    end subroutine ascii_draw_arrow

    subroutine ascii_draw_arrowhead(this, x, y, dx, dy, size, style)
        !! ASCII has no separate head-only glyph; reuse draw_arrow.
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style

        call draw_ascii_arrow(this%canvas, x, y, dx, dy, size, style, &
                              this%x_min, this%x_max, this%y_min, this%y_max, &
                              this%plot_area, this%width, this%height, &
                              this%has_rendered_arrows, this%uses_vector_arrows, &
                              this%has_triangular_arrows)
    end subroutine ascii_draw_arrowhead

    subroutine ascii_draw_quiver_arrow(this, x, y, u, v)
        !! Stamp a quiver arrow directly onto the canvas via the axis-policy
        !! layer path so it clips to the plot interior and never corrupts axis
        !! spines, tick marks, or tick/axis labels (issue #2071).
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, u, v

        call draw_ascii_vector_arrow(this%canvas, x, y, u, v, &
                                     this%x_min, this%x_max, this%y_min, this%y_max, &
                                     this%plot_area, this%plot_width, this%plot_height)
    end subroutine ascii_draw_quiver_arrow

    function ascii_get_output_method(this) result(output)
        class(ascii_context), intent(in) :: this
        character(len=:), allocatable :: output

        output = ascii_get_output(this%canvas, this%width, this%height)
    end function ascii_get_output_method

    pure character(len=1) function infer_ascii_arrow_char(dx, dy) result(arrow_char)
        real(wp), intent(in) :: dx, dy
        real(wp) :: angle

        angle = atan2(dy / ASCII_CHAR_ASPECT, dx)

        if (abs(angle) < 0.393_wp) then
            arrow_char = '>'
        else if (angle >= 0.393_wp .and. angle < 1.178_wp) then
            arrow_char = '/'
        else if (angle >= 1.178_wp .and. angle < 1.963_wp) then
            arrow_char = '^'
        else if (angle >= 1.963_wp .and. angle < 2.749_wp) then
            arrow_char = '\'
        else if (abs(angle) >= 2.749_wp) then
            arrow_char = '<'
        else if (angle <= -0.393_wp .and. angle > -1.178_wp) then
            arrow_char = '\'
        else if (angle <= -1.178_wp .and. angle > -1.963_wp) then
            arrow_char = 'v'
        else
            arrow_char = '/'
        end if
    end function infer_ascii_arrow_char

   function ascii_get_width_scale(this) result(scale)
        class(ascii_context), intent(in) :: this
        real(wp) :: scale

        if (this%plot_width > 0 .and. this%x_max > this%x_min) then
            scale = real(this%plot_width, wp)/(this%x_max - this%x_min)
        else
            scale = 1.0_wp
        end if
    end function ascii_get_width_scale

    function ascii_get_height_scale(this) result(scale)
        class(ascii_context), intent(in) :: this
        real(wp) :: scale

        if (this%plot_height > 0 .and. this%y_max > this%y_min) then
            scale = real(this%plot_height, wp)/(this%y_max - this%y_min)
        else
            scale = 1.0_wp
        end if
    end function ascii_get_height_scale

    subroutine ascii_fill_quad(this, x_quad, y_quad)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp) :: y_lo, y_hi

        ! Map quad vertices through the same data y range as the axis frame and
        ! ticks. The stored range preserves compatibility with older saves.
        if (this%has_stored_y_range) then
            y_lo = this%stored_y_min
            y_hi = this%stored_y_max
        else
            y_lo = this%y_min
            y_hi = this%y_max
        end if

        call ascii_fill_quad_primitive(this%canvas, x_quad, y_quad, &
                                       this%x_min, this%x_max, y_lo, y_hi, &
                                       this%plot_area, this%plot_width, this%plot_height, &
                                       this%current_r, this%current_g, this%current_b)
    end subroutine ascii_fill_quad

 subroutine ascii_render_legend_specialized(this, legend, legend_x, legend_y)
        use fortplot_legend, only: legend_t
        class(ascii_context), intent(inout) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y

        associate(unused_x => legend_x, unused_y => legend_y); end associate
        call ascii_render_legend_impl(legend, this%legend_lines, this%num_legend_lines)
    end subroutine ascii_render_legend_specialized

 subroutine ascii_calculate_legend_dimensions(this, legend, legend_width, &
                                                  legend_height)
        use fortplot_legend, only: legend_t
        class(ascii_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: legend_width, legend_height

        call ascii_calc_legend_dims_impl(legend, this%width, legend_width, legend_height)
    end subroutine ascii_calculate_legend_dimensions

    subroutine ascii_set_legend_border_width(this)
        class(ascii_context), intent(inout) :: this
        call ascii_set_legend_border_impl()
    end subroutine ascii_set_legend_border_width

  subroutine ascii_calculate_legend_position(this, legend, x, y)
        use fortplot_legend, only: legend_t
        class(ascii_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: x, y
        real(wp) :: lw, lh

        call this%calculate_legend_dimensions(legend, lw, lh)
        call ascii_calc_legend_pos_impl(legend, this%width, this%height, lw, lh, x, y)
    end subroutine ascii_calculate_legend_position

    subroutine ascii_extract_rgb_data(this, width, height, rgb_data)
        class(ascii_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)

        associate(unused_w => this%width); end associate
        call ascii_extract_rgb_impl(width, height, rgb_data)
    end subroutine ascii_extract_rgb_data

   subroutine ascii_get_png_data(this, width, height, png_data, status)
        class(ascii_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status

        associate(unused_w => this%width); end associate
        call ascii_get_png_impl(width, height, png_data, status)
    end subroutine ascii_get_png_data

   subroutine ascii_prepare_3d_data(this, plots)
        use fortplot_plot_data, only: plot_data_t
        class(ascii_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)

        associate(unused_w => this%width); end associate
        call ascii_prepare_3d_impl(plots)
    end subroutine ascii_prepare_3d_data

   subroutine ascii_render_ylabel(this, ylabel)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel

        associate(unused_w => this%width); end associate
        call ascii_render_ylabel_impl(ylabel)
    end subroutine ascii_render_ylabel

    subroutine ascii_draw_axes_and_labels(this, xscale, yscale, symlog_threshold, &
                                          x_min, x_max, y_min, y_max, &
                                          title, xlabel, ylabel, &
                                          x_date_format, y_date_format, &
                                          z_min, z_max, has_3d_plots)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        character(len=*), intent(in), optional :: x_date_format, y_date_format
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        logical :: has_custom_ticks

        this%last_xscale = trim(xscale)
        this%last_yscale = trim(yscale)
        this%last_symlog_threshold = symlog_threshold

        if (has_3d_plots) then
            call ascii_draw_projected_3d_axes(this, x_min, x_max, y_min, y_max, &
                                              z_min, z_max)
            return
        end if

        has_custom_ticks = allocated(this%custom_xtick_positions) .and. &
                           allocated(this%custom_xtick_labels)
        call ensure_text_capacity(this, this%num_text_elements + 4*MAX_ASCII_TICKS)
        call ascii_draw_axes_impl(this%canvas, xscale, yscale, symlog_threshold, &
                               x_min, x_max, y_min, y_max, &
                               title, xlabel, ylabel, x_date_format, y_date_format, &
                               z_min, z_max, has_3d_plots, &
                               this%current_r, this%current_g, this%current_b, &
                               this%plot_area, this%plot_width, this%plot_height, &
                               this%title_text, this%xlabel_text, this%ylabel_text, &
                               this%text_elements, this%num_text_elements, &
                               has_custom_ticks, &
                               this%custom_xtick_positions, this%custom_xtick_labels)
    end subroutine ascii_draw_axes_and_labels

    subroutine ascii_draw_projected_3d_axes(this, x_min, x_max, y_min, y_max, &
                                            z_min, z_max)
        !! Render the projected 3D box, ticks, and tick labels for ASCII output.
        !! Replaces the static 2D axis frame so the ASCII axes rotate with the
        !! stored view angles, matching matplotlib mplot3d (refs #2054).
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        real(wp), intent(in), optional :: z_min, z_max

        real(wp) :: zlo, zhi

        zlo = 0.0_wp
        zhi = 1.0_wp
        if (present(z_min)) zlo = z_min
        if (present(z_max)) zhi = z_max

        call draw_3d_axes(this, x_min, x_max, y_min, y_max, zlo, zhi, &
                          fill_panes=.false., label_gap_px=10.0_wp)
    end subroutine ascii_draw_projected_3d_axes

    subroutine ascii_save_coordinates(this, x_min, x_max, y_min, y_max)
        class(ascii_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max

        call ascii_save_coord_impl(this%x_min, this%x_max, this%y_min, this%y_max, &
                                    this%has_stored_y_range, this%stored_y_min, this%stored_y_max, &
                                    x_min, x_max, y_min, y_max)
    end subroutine ascii_save_coordinates

    subroutine ascii_set_coordinates(this, x_min, x_max, y_min, y_max)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        call ascii_set_coord_impl(x_min, x_max, y_min, y_max, &
                                   this%x_min, this%x_max, this%y_min, this%y_max, &
                                   this%stored_y_min, this%stored_y_max, this%has_stored_y_range)
    end subroutine ascii_set_coordinates

    subroutine ascii_render_axes(this, title_text, xlabel_text, ylabel_text)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text

        character(len=:), allocatable :: t, xl, yl

        t = ''; xl = ''; yl = ''
        if (present(title_text)) t = title_text
        if (present(xlabel_text)) xl = xlabel_text
        if (present(ylabel_text)) yl = ylabel_text

        this%title_text = t
        this%xlabel_text = xl
        this%ylabel_text = yl

        call ensure_text_capacity(this, this%num_text_elements + 4*MAX_ASCII_TICKS)
        call ascii_render_axes_impl(this%x_min, this%x_max, this%y_min, this%y_max, &
                                     this%has_stored_y_range, this%stored_y_min, this%stored_y_max, &
                                     this%last_xscale, this%last_yscale, this%last_symlog_threshold, &
                                     this%canvas, this%plot_area, this%plot_width, this%plot_height, &
                                     this%title_text, this%xlabel_text, this%ylabel_text, &
                                     this%text_elements, this%num_text_elements, &
                                     this%custom_xtick_positions, this%custom_xtick_labels)
    end subroutine ascii_render_axes

subroutine ascii_clear_legend_lines(this, header)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in), optional :: header

        call ascii_clear_legend_impl(this%legend_lines, this%num_legend_lines, header)
        this%capturing_legend = .false.
    end subroutine ascii_clear_legend_lines

   
subroutine ascii_add_legend_entry(this, label, value_text)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: label
        character(len=*), intent(in), optional :: value_text

        call ascii_add_legend_entry_impl(label, value_text, this%legend_lines, this%num_legend_lines)
    end subroutine ascii_add_legend_entry

    subroutine ascii_clear_pie_legend_entries(this)
        class(ascii_context), intent(inout) :: this
        call ascii_clear_pie_legend_impl(this%legend_lines, this%num_legend_lines)
    end subroutine ascii_clear_pie_legend_entries

   subroutine ascii_register_pie_legend_entry(this, label, value_text)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: label
        character(len=*), intent(in) :: value_text

        call ascii_register_pie_legend_impl(label, value_text, this%legend_lines, this%num_legend_lines)
    end subroutine ascii_register_pie_legend_entry

end module fortplot_ascii
