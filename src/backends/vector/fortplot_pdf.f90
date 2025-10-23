module fortplot_pdf
    !! PDF backend main interface (unified coordinates using plot area)

    use fortplot_pdf_core
    use fortplot_pdf_text
    use fortplot_pdf_drawing
    use fortplot_zlib_core, only: zlib_compress_into
    use fortplot_pdf_axes, only: draw_pdf_axes_and_labels, render_mixed_text
    use fortplot_pdf_io
    use fortplot_pdf_coordinate
    use fortplot_pdf_markers

    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_plot_data, only: plot_data_t
    use fortplot_legend, only: legend_entry_t
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area
    use fortplot_constants, only: EPSILON_COMPARE
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
    contains
        procedure :: line => draw_pdf_line
        procedure :: color => set_pdf_color
        procedure :: text => draw_pdf_text_wrapper
        procedure :: save => write_pdf_file_facade
        procedure :: set_line_width => set_pdf_line_width
        procedure :: set_line_style => set_pdf_line_style
        procedure :: draw_marker => draw_pdf_marker_wrapper
        procedure :: set_marker_colors => set_marker_colors_wrapper
        procedure :: set_marker_colors_with_alpha => set_marker_colors_with_alpha_wrapper
        procedure :: draw_arrow => draw_pdf_arrow_wrapper
        procedure :: get_ascii_output => pdf_get_ascii_output

        procedure :: get_width_scale => get_width_scale_wrapper
        procedure :: get_height_scale => get_height_scale_wrapper
        procedure :: fill_quad => fill_quad_wrapper
        procedure :: fill_heatmap => fill_heatmap_wrapper
        procedure :: render_legend_specialized => render_legend_specialized_wrapper
        procedure :: calculate_legend_dimensions => calculate_legend_dimensions_wrapper
        procedure :: set_legend_border_width => set_legend_border_width_wrapper
        procedure :: calculate_legend_position_backend => calculate_legend_position_wrapper
        procedure :: extract_rgb_data => extract_rgb_data_wrapper
        procedure :: get_png_data_backend => get_png_data_wrapper
        procedure :: prepare_3d_data => prepare_3d_data_wrapper
        procedure :: render_ylabel => render_ylabel_wrapper
        procedure :: draw_axes_and_labels_backend => draw_axes_and_labels_backend_wrapper
        procedure :: save_coordinates => pdf_save_coordinates
        procedure :: set_coordinates => pdf_set_coordinates
        procedure :: render_axes => render_pdf_axes_wrapper

        procedure, private :: update_coord_context
        procedure, private :: make_coord_context
    end type pdf_context

contains

    function create_pdf_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(pdf_context) :: ctx
        ! Align PDF canvas size with matplotlib's inches/DPI semantics.
        ! Our figure dimensions are in pixels at a default DPI of 100.
        ! PDF units are points (1 pt = 1/72 inch). Convert pixels -> points
        ! so that an 800x600px figure maps to a 8x6 inch PDF page (576x432 pt).
        real(wp) :: width_pts, height_pts
        integer :: width_pts_i, height_pts_i

        call setup_canvas(ctx, width, height)

        width_pts  = real(width,  wp) * 72.0_wp / 100.0_wp
        height_pts = real(height, wp) * 72.0_wp / 100.0_wp
        ! Use integer canvas for downstream plot-area computations
        width_pts_i  = max(1, nint(width_pts))
        height_pts_i = max(1, nint(height_pts))

        ctx%core_ctx = create_pdf_canvas_core(real(width_pts_i, wp), real(height_pts_i, wp))

        call ctx%stream_writer%initialize_stream()
        call ctx%stream_writer%add_to_stream("q")
        call ctx%stream_writer%add_to_stream("1 w")
        call ctx%stream_writer%add_to_stream("1 J")
        call ctx%stream_writer%add_to_stream("1 j")
        call ctx%stream_writer%add_to_stream("0 0 0 RG")
        call ctx%stream_writer%add_to_stream("0 0 0 rg")

        ctx%margins = plot_margins_t()
        call calculate_pdf_plot_area(width_pts_i, height_pts_i, ctx%margins, ctx%plot_area)

        call ctx%update_coord_context()
    end function create_pdf_canvas

    subroutine draw_pdf_line(this, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: pdf_x1, pdf_y1, pdf_x2, pdf_y2
        ! Ensure coordinate context reflects latest figure ranges and plot area
        call this%update_coord_context()

        call normalize_to_pdf_coords(this%coord_ctx, x1, y1, pdf_x1, pdf_y1)
        call normalize_to_pdf_coords(this%coord_ctx, x2, y2, pdf_x2, pdf_y2)
        call this%stream_writer%draw_vector_line(pdf_x1, pdf_y1, pdf_x2, pdf_y2)
    end subroutine draw_pdf_line

    subroutine set_pdf_color(this, r, g, b)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b

        call this%stream_writer%set_vector_color(r, g, b)
        call this%core_ctx%set_color(r, g, b)
    end subroutine set_pdf_color

    subroutine set_pdf_line_width(this, width)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: width

        call this%stream_writer%set_vector_line_width(width)
        call this%core_ctx%set_line_width(width)
    end subroutine set_pdf_line_width

    subroutine set_pdf_line_style(this, style)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: style
        character(len=64) :: dash_pattern

        ! Convert line style to PDF dash pattern
        select case (trim(style))
        case ('-', 'solid')
            dash_pattern = '[] 0 d'  ! Solid line (empty dash array)
        case ('--', 'dashed')
            dash_pattern = '[6 3] 0 d'  ! 6 on, 3 off (approx. Matplotlib)
        case (':', 'dotted')
            dash_pattern = '[1 3] 0 d'  ! 1 on, 3 off
        case ('-.', 'dashdot')
            dash_pattern = '[6 3 1 3] 0 d'  ! dash-dot pattern
        case default
            dash_pattern = '[] 0 d'  ! Default to solid
        end select

        call this%stream_writer%add_to_stream(trim(dash_pattern))
    end subroutine set_pdf_line_style

    subroutine draw_pdf_text_wrapper(this, x, y, text)
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

    subroutine write_pdf_file_facade(this, filename)
        use fortplot_system_viewer, only: launch_system_viewer, has_graphical_session
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        logical :: file_success
        character(len=1024) :: temp_file, actual_filename
        logical :: viewer_success
        integer :: pid

        ! Handle terminal display
        if (trim(filename) == 'terminal') then
            if (has_graphical_session()) then
                call get_environment_variable('USER', temp_file)
                if (len_trim(temp_file) == 0) temp_file = 'user'
                call get_environment_variable('PID', temp_file)
                if (len_trim(temp_file) == 0) then
                    pid = 0
                else
                    read(temp_file, *) pid
                end if
                write(actual_filename, '(A,I0,A)') '/tmp/fortplot_show_', pid, '.pdf'
            else
                call log_info("No graphical session detected, cannot display PDF")
                return
            end if
        else
            actual_filename = filename
        end if

        ! Do not re-render axes here. The main rendering pipeline has already
        ! produced the complete `core_ctx%stream_data`, including axes, tick labels,
        ! titles/axis labels, legend text, and annotations. Re-rendering would
        ! clear or overwrite that state and can drop labels/legend.

        ! Merge vector drawing stream (lines, markers, etc.) with the core text
        ! stream. Keep existing `core_ctx%stream_data` intact to preserve labels
        ! and legend text that were rendered earlier in the pipeline.
        if (len_trim(this%stream_writer%content_stream) > 0) then
            if (len_trim(this%core_ctx%stream_data) > 0) then
                this%core_ctx%stream_data = trim(this%stream_writer%content_stream)//new_line('a')//trim(this%core_ctx%stream_data)
            else
                this%core_ctx%stream_data = this%stream_writer%content_stream
            end if
        end if

        ! Ensure a solid dash reset exists in the final content stream so that
        ! axes frame and tick marks are rendered with solid strokes regardless
        ! of prior plot linestyle state. This is harmless if plots later set a
        ! different dash pattern; the presence of this operator guarantees the
        ! PDF stream contains an explicit solid dash command.
        this%core_ctx%stream_data = '[] 0 d' // new_line('a') // trim(this%core_ctx%stream_data)
        call write_pdf_file(this%core_ctx, actual_filename, file_success)
        if (.not. file_success) return

        ! Launch viewer if displaying to terminal
        if (trim(filename) == 'terminal' .and. has_graphical_session()) then
            call launch_system_viewer(actual_filename, viewer_success)
            if (.not. viewer_success) then
                call log_error("Failed to launch PDF viewer")
            end if
        end if
    end subroutine write_pdf_file_facade

    subroutine update_coord_context(this)
        class(pdf_context), intent(inout) :: this

        this%coord_ctx%x_min = this%x_min
        this%coord_ctx%x_max = this%x_max
        this%coord_ctx%y_min = this%y_min
        this%coord_ctx%y_max = this%y_max
        ! Coordinate context should operate in the same units as the PDF page
        ! dimensions (points). Keep plot area (already computed in points) and
        ! propagate the converted canvas size by recomputing from core context.
        this%coord_ctx%width = int(this%core_ctx%width)
        this%coord_ctx%height = int(this%core_ctx%height)
        this%coord_ctx%plot_area = this%plot_area
        this%coord_ctx%core_ctx = this%core_ctx
    end subroutine update_coord_context

    function make_coord_context(this) result(ctx)
        class(pdf_context), intent(in) :: this
        type(pdf_context_handle) :: ctx

        ctx%x_min = this%x_min
        ctx%x_max = this%x_max
        ctx%y_min = this%y_min
        ctx%y_max = this%y_max
        ctx%width = int(this%core_ctx%width)
        ctx%height = int(this%core_ctx%height)
        ctx%plot_area = this%plot_area
        ctx%core_ctx = this%core_ctx
    end function make_coord_context

    subroutine draw_pdf_marker_wrapper(this, x, y, style)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style

        call this%update_coord_context()
        call draw_pdf_marker_at_coords(this%coord_ctx, this%stream_writer, x, y, style)
    end subroutine draw_pdf_marker_wrapper

    subroutine set_marker_colors_wrapper(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, face_r, face_g, face_b

        call pdf_set_marker_colors(this%core_ctx, edge_r, edge_g, edge_b, face_r, face_g, face_b)
    end subroutine set_marker_colors_wrapper

    subroutine set_marker_colors_with_alpha_wrapper(this, edge_r, edge_g, edge_b, edge_alpha, &
                                                   face_r, face_g, face_b, face_alpha)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha

        call pdf_set_marker_colors_with_alpha(this%core_ctx, edge_r, edge_g, edge_b, edge_alpha, &
                                             face_r, face_g, face_b, face_alpha)
    end subroutine set_marker_colors_with_alpha_wrapper

    subroutine draw_pdf_arrow_wrapper(this, x, y, dx, dy, size, style)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style

        call this%update_coord_context()
        call draw_pdf_arrow_at_coords(this%coord_ctx, this%stream_writer, x, y, dx, dy, size, style)
    end subroutine draw_pdf_arrow_wrapper

    function pdf_get_ascii_output(this) result(output)
        class(pdf_context), intent(in) :: this
        character(len=:), allocatable :: output
        output = "PDF output (non-ASCII format)"
    end function pdf_get_ascii_output
    real(wp) function get_width_scale_wrapper(this) result(scale)
        class(pdf_context), intent(in) :: this
        type(pdf_context_handle) :: local_ctx
        local_ctx = this%make_coord_context()
        scale = pdf_get_width_scale(local_ctx)
    end function get_width_scale_wrapper
    real(wp) function get_height_scale_wrapper(this) result(scale)
        class(pdf_context), intent(in) :: this
        type(pdf_context_handle) :: local_ctx
        local_ctx = this%make_coord_context()
        scale = pdf_get_height_scale(local_ctx)
    end function get_height_scale_wrapper
    subroutine fill_quad_wrapper(this, x_quad, y_quad)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp) :: px(4), py(4)
        character(len=512) :: cmd
        integer :: i
        real(wp) :: minx, maxx, miny, maxy, eps

        call this%update_coord_context()

        ! Convert to PDF coordinates
        do i = 1, 4
            call normalize_to_pdf_coords(this%coord_ctx, x_quad(i), y_quad(i), px(i), py(i))
        end do

        ! Slightly expand axis-aligned quads to overlap neighbors and avoid hairline seams
        minx = min(min(px(1),px(2)), min(px(3),px(4)))
        maxx = max(max(px(1),px(2)), max(px(3),px(4)))
        miny = min(min(py(1),py(2)), min(py(3),py(4)))
        maxy = max(max(py(1),py(2)), max(py(3),py(4)))
        eps = 0.05_wp  ! expand by small amount in PDF points

        ! If the quad is axis-aligned (common for pcolormesh), use expanded bbox
        if ( (abs(py(1)-py(2)) < 1.0e-6_wp .and. abs(px(2)-px(3)) < 1.0e-6_wp .and. &
              abs(py(3)-py(4)) < 1.0e-6_wp .and. abs(px(4)-px(1)) < 1.0e-6_wp) ) then
            write(cmd, '(F0.3,1X,F0.3)') minx-eps, miny-eps; call this%stream_writer%add_to_stream(trim(cmd)//' m')
            write(cmd, '(F0.3,1X,F0.3)') maxx+eps, miny-eps; call this%stream_writer%add_to_stream(trim(cmd)//' l')
            write(cmd, '(F0.3,1X,F0.3)') maxx+eps, maxy+eps; call this%stream_writer%add_to_stream(trim(cmd)//' l')
            write(cmd, '(F0.3,1X,F0.3)') minx-eps, maxy+eps; call this%stream_writer%add_to_stream(trim(cmd)//' l')
            call this%stream_writer%add_to_stream('h')
            call this%stream_writer%add_to_stream('f')
        else
            ! Fallback: draw original quad
            write(cmd, '(F0.3,1X,F0.3)') px(1), py(1); call this%stream_writer%add_to_stream(trim(cmd)//' m')
            write(cmd, '(F0.3,1X,F0.3)') px(2), py(2); call this%stream_writer%add_to_stream(trim(cmd)//' l')
            write(cmd, '(F0.3,1X,F0.3)') px(3), py(3); call this%stream_writer%add_to_stream(trim(cmd)//' l')
            write(cmd, '(F0.3,1X,F0.3)') px(4), py(4); call this%stream_writer%add_to_stream(trim(cmd)//' l')
            call this%stream_writer%add_to_stream('h')
            call this%stream_writer%add_to_stream('f')
        end if
    end subroutine fill_quad_wrapper

    subroutine fill_heatmap_wrapper(this, x_grid, y_grid, z_grid, z_min, z_max)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max

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

        W = nx - 1; H = ny - 1
        if (W <= 0 .or. H <= 0) return

        ! Build RGB image with 1-pixel replicated border padding to avoid
        ! sampling outside the image at arbitrary zoom levels.
        block
            integer :: WP, HP
            integer, allocatable :: img(:,:,:)
            integer :: ii, jj, src_i, src_j
            WP = W + 2; HP = H + 2
            allocate(img(3, WP, HP))
            do jj = 1, HP
                do ii = 1, WP
                    src_i = max(1, min(W, ii-1))
                    src_j = max(1, min(H, jj-1))
                    value = z_grid(src_j, src_i)
                    call colormap_value_to_color(value, z_min, z_max, 'viridis', color)
                    v1 = max(0.0d0, min(1.0d0, color(1)))
                    v2 = max(0.0d0, min(1.0d0, color(2)))
                    v3 = max(0.0d0, min(1.0d0, color(3)))
                    img(1, ii, jj) = int(nint(v1 * 255.0d0), kind=4)
                    img(2, ii, jj) = int(nint(v2 * 255.0d0), kind=4)
                    img(3, ii, jj) = int(nint(v3 * 255.0d0), kind=4)
                end do
            end do
            allocate(rgb_u8(WP*HP*3))
            idx = 1
            do j = 1, HP
                do i = 1, WP
                    rgb_u8(idx) = img(1, i, j); idx = idx + 1
                    rgb_u8(idx) = img(2, i, j); idx = idx + 1
                    rgb_u8(idx) = img(3, i, j); idx = idx + 1
                end do
            end do
            W = WP; H = HP
        end block

        block
            use, intrinsic :: iso_fortran_env, only: int8
            integer(int8), allocatable :: in_bytes(:), out_bytes(:)
            integer :: k, n
            n = size(rgb_u8)
            allocate(in_bytes(n))
            do k = 1, n
                in_bytes(k) = int(iand(rgb_u8(k),255))
            end do
            call zlib_compress_into(in_bytes, n, out_bytes, out_len)
            img_data = repeat(' ', out_len)
            do k = 1, out_len
                img_data(k:k) = achar(iand(int(out_bytes(k), kind=4), 255))
            end do
        end block

        ! Align placement to the exact PDF plot area (consistent with PNG backend)
        pdf_x0   = real(this%coord_ctx%plot_area%left,   wp)
        pdf_y0   = real(this%coord_ctx%plot_area%bottom, wp)
        width_pt = real(this%coord_ctx%plot_area%width,  wp)
        height_pt= real(this%coord_ctx%plot_area%height, wp)

        ! Compute a half-pixel bleed in user-space and clip to the exact plot area
        px_w = width_pt / real(W, wp)
        px_h = height_pt / real(H, wp)
        bleed_x = 0.5_wp * px_w
        bleed_y = 0.5_wp * px_h

        call this%stream_writer%add_to_stream('q')
        ! Clip to the exact target rectangle to keep padded borders inside
        write(cmd,'(F0.12,1X,F0.12,1X,F0.12,1X,F0.12,1X,A)') pdf_x0, pdf_y0, width_pt, height_pt, ' re W n'
        call this%stream_writer%add_to_stream(trim(cmd))
        ! Compute pixel scale and place padded image so that the extra 1px ring
        ! lies just outside the clip region
        px_w = width_pt / real(W-2, wp)
        px_h = height_pt / real(H-2, wp)
        write(cmd,'(F0.12,1X,F0.12,1X,F0.12,1X,F0.12,1X,F0.12,1X,F0.12,1X,A)') &
            px_w*real(W,wp), 0.0_wp, 0.0_wp, -(px_h*real(H,wp)), &
            pdf_x0 - px_w, (pdf_y0 + height_pt) + px_h, ' cm'
        call this%stream_writer%add_to_stream(trim(cmd))
        ! Place image XObject instead of inline image
        call this%core_ctx%set_image(W, H, img_data)
        call this%stream_writer%add_to_stream('/Im1 Do')
        call this%stream_writer%add_to_stream('Q')
    end subroutine fill_heatmap_wrapper
    subroutine render_legend_specialized_wrapper(this, entries, x, y, width, height)
        class(pdf_context), intent(inout) :: this
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(in) :: x, y, width, height

        call this%update_coord_context()
        call pdf_render_legend_specialized(this%coord_ctx, entries, x, y, width, height)
    end subroutine render_legend_specialized_wrapper

    subroutine calculate_legend_dimensions_wrapper(this, entries, width, height)
        class(pdf_context), intent(in) :: this
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(out) :: width, height
        type(pdf_context_handle) :: local_ctx

        local_ctx = this%make_coord_context()
        call pdf_calculate_legend_dimensions(local_ctx, entries, width, height)
    end subroutine calculate_legend_dimensions_wrapper

    subroutine set_legend_border_width_wrapper(this, width)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: width

        call this%update_coord_context()
        call pdf_set_legend_border_width(this%coord_ctx, width)
    end subroutine set_legend_border_width_wrapper

    subroutine calculate_legend_position_wrapper(this, loc, x, y)
        class(pdf_context), intent(in) :: this
        character(len=*), intent(in) :: loc
        real(wp), intent(out) :: x, y
        type(pdf_context_handle) :: local_ctx

        local_ctx = this%make_coord_context()
        call pdf_calculate_legend_position(local_ctx, loc, x, y)
    end subroutine calculate_legend_position_wrapper

    subroutine extract_rgb_data_wrapper(this, width, height, rgb_data)
        class(pdf_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)
        type(pdf_context_handle) :: local_ctx

        local_ctx = this%make_coord_context()
        call pdf_extract_rgb_data(local_ctx, width, height, rgb_data)
    end subroutine extract_rgb_data_wrapper

    subroutine get_png_data_wrapper(this, width, height, png_data, status)
        class(pdf_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        type(pdf_context_handle) :: local_ctx

        local_ctx = this%make_coord_context()
        call pdf_get_png_data(local_ctx, width, height, png_data, status)
    end subroutine get_png_data_wrapper

    subroutine prepare_3d_data_wrapper(this, plots)
        class(pdf_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)

        call this%update_coord_context()
        call pdf_prepare_3d_data(this%coord_ctx, plots)
    end subroutine prepare_3d_data_wrapper

    subroutine render_ylabel_wrapper(this, ylabel)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel

        call this%update_coord_context()
        call pdf_render_ylabel(this%coord_ctx, ylabel)
    end subroutine render_ylabel_wrapper

    subroutine draw_axes_and_labels_backend_wrapper(this, xscale, yscale, symlog_threshold, &
                                                   x_min, x_max, y_min, y_max, &
                                                   title, xlabel, ylabel, &
                                                   z_min, z_max, has_3d_plots)
        use fortplot_3d_axes, only: draw_3d_axes
        use fortplot_pdf_axes, only: draw_pdf_title_and_labels
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots

        character(len=256) :: title_str, xlabel_str, ylabel_str
        associate(dzmin => z_min, dzmax => z_max, dh3d => has_3d_plots); end associate

        title_str = ""; xlabel_str = ""; ylabel_str = ""
        if (present(title))  title_str  = title
        if (present(xlabel)) xlabel_str = xlabel
        if (present(ylabel)) ylabel_str = ylabel

        if (has_3d_plots) then
            call draw_3d_axes(this, x_min, x_max, y_min, y_max, &
                              merge(z_min, 0.0_wp, present(z_min)), &
                              merge(z_max, 1.0_wp, present(z_max)))
            ! Draw only title/xlabel/ylabel using PDF helpers (avoid 2D axes duplication)
            call draw_pdf_title_and_labels(this%core_ctx, title_str, xlabel_str, ylabel_str, &
                                           real(this%plot_area%left, wp), &
                                           real(this%plot_area%bottom, wp), &
                                           real(this%plot_area%width, wp), &
                                           real(this%plot_area%height, wp))
        else
            call draw_pdf_axes_and_labels(this%core_ctx, xscale, yscale, symlog_threshold, &
                                         x_min, x_max, y_min, y_max, &
                                         title_str, xlabel_str, ylabel_str, &
                                         real(this%plot_area%left, wp), &
                                         real(this%plot_area%bottom, wp), &
                                         real(this%plot_area%width, wp), &
                                         real(this%plot_area%height, wp), &
                                         real(this%height, wp))
        end if
    end subroutine draw_axes_and_labels_backend_wrapper

    subroutine pdf_save_coordinates(this, x_min, x_max, y_min, y_max)
        class(pdf_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max

        ! Return current coordinate bounds
        x_min = this%x_min
        x_max = this%x_max
        y_min = this%y_min
        y_max = this%y_max
    end subroutine pdf_save_coordinates

    subroutine pdf_set_coordinates(this, x_min, x_max, y_min, y_max)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max

        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max

        ! Reset axes flag when coordinates change
        this%axes_rendered = .false.
    end subroutine pdf_set_coordinates

    subroutine render_pdf_axes_wrapper(this, title_text, xlabel_text, ylabel_text)
        !! Explicitly render axes with optional labels
        !! This allows low-level PDF users to add proper axes to their plots
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text

        character(len=256) :: title_str, xlabel_str, ylabel_str

        ! Only render axes once unless coordinates change
        if (this%axes_rendered) return

        ! Ensure coordinate system is set
        if (abs(this%x_max - this%x_min) <= epsilon(1.0_wp) .or. &
            abs(this%y_max - this%y_min) <= epsilon(1.0_wp)) then
            ! No valid coordinate system - skip axes
            return
        end if

        ! Set default empty strings for labels
        title_str = ""
        xlabel_str = ""
        ylabel_str = ""

        ! Use provided labels if present
        if (present(title_text)) title_str = title_text
        if (present(xlabel_text)) xlabel_str = xlabel_text
        if (present(ylabel_text)) ylabel_str = ylabel_text

        ! Clear any previous axes data in core context
        this%core_ctx%stream_data = ""

        ! Draw axes and labels with current coordinate system
        call draw_pdf_axes_and_labels(this%core_ctx, "linear", "linear", 1.0_wp, &
                                     this%x_min, this%x_max, this%y_min, this%y_max, &
                                     title_str, xlabel_str, ylabel_str, &
                                     real(this%plot_area%left, wp), &
                                     real(this%plot_area%bottom, wp), &
                                     real(this%plot_area%width, wp), &
                                     real(this%plot_area%height, wp), &
                                     real(this%height, wp))

        ! Add axes content to the stream
        call this%stream_writer%add_to_stream(this%core_ctx%stream_data)

        ! Mark axes as rendered
        this%axes_rendered = .true.
    end subroutine render_pdf_axes_wrapper


end module fortplot_pdf
