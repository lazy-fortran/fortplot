module fortplot_pdf
    !! PDF backend facade module (< 1000 lines)
    !! Maintains backward compatibility while delegating to specialized submodules
    !! This module serves as the main interface, re-exporting functionality
    !! from refactored submodules to comply with size limits
    
    ! Import and re-export core PDF functionality
    use fortplot_pdf_core
    use fortplot_pdf_text
    use fortplot_pdf_drawing
    use fortplot_pdf_axes
    use fortplot_pdf_io
    
    ! Original dependencies still needed for pdf_context type
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_plot_data, only: plot_data_t
    use fortplot_legend, only: legend_entry_t
    use fortplot_vector, only: vector_stream_writer, vector_graphics_state
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_unicode, only: utf8_char_length, utf8_to_codepoint
    use fortplot_logging, only: log_info, log_error
    use fortplot_margins, only: plot_margins_t, plot_area_t, calculate_plot_area, get_axis_tick_positions
    use fortplot_ticks, only: generate_scale_aware_tick_labels, find_nice_tick_locations, format_tick_value_smart
    use fortplot_scales, only: apply_scale_transform
    use fortplot_label_positioning, only: calculate_x_label_position, calculate_y_label_position, &
                                         calculate_x_axis_label_position, calculate_y_axis_label_position, &
                                         calculate_x_tick_label_position_pdf, calculate_y_tick_label_position_pdf
    use fortplot_text, only: calculate_text_width
    use fortplot_markers, only: get_marker_size, MARKER_CIRCLE, MARKER_SQUARE, MARKER_DIAMOND, MARKER_CROSS
    use fortplot_colormap, only: colormap_value_to_color
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    
    ! Re-export main types and procedures
    public :: pdf_context, create_pdf_canvas
    
    ! Re-export from submodules for backward compatibility
    public :: draw_pdf_axes_and_labels, draw_mixed_font_text
    public :: pdf_stream_writer
    
    type, extends(plot_context) :: pdf_context
        type(pdf_stream_writer) :: stream_writer
        type(plot_margins_t) :: margins
        type(plot_area_t) :: plot_area
        ! Core PDF context from refactored module
        type(pdf_context_core), private :: core_ctx
    contains
        procedure :: line => draw_pdf_line
        procedure :: color => set_pdf_color
        procedure :: text => draw_pdf_text_wrapper
        procedure :: save => write_pdf_file_facade
        procedure :: set_line_width => set_pdf_line_width
        procedure :: save_graphics_state => pdf_save_graphics_state
        procedure :: restore_graphics_state => pdf_restore_graphics_state
        procedure :: draw_marker => draw_pdf_marker
        procedure :: set_marker_colors => pdf_set_marker_colors
        procedure :: set_marker_colors_with_alpha => pdf_set_marker_colors_with_alpha
        procedure :: draw_arrow => draw_pdf_arrow_facade
        procedure :: get_ascii_output => pdf_get_ascii_output
        
        ! Polymorphic methods to eliminate SELECT TYPE
        procedure :: get_width_scale => pdf_get_width_scale
        procedure :: get_height_scale => pdf_get_height_scale
        procedure :: fill_quad => pdf_fill_quad
        procedure :: fill_heatmap => pdf_fill_heatmap
        procedure :: render_legend_specialized => pdf_render_legend_specialized
        procedure :: calculate_legend_dimensions => pdf_calculate_legend_dimensions
        procedure :: set_legend_border_width => pdf_set_legend_border_width
        procedure :: calculate_legend_position_backend => pdf_calculate_legend_position
        procedure :: extract_rgb_data => pdf_extract_rgb_data
        procedure :: get_png_data_backend => pdf_get_png_data
        procedure :: prepare_3d_data => pdf_prepare_3d_data
        procedure :: render_ylabel => pdf_render_ylabel
        procedure :: draw_axes_and_labels_backend => pdf_draw_axes_stub
        procedure :: save_coordinates => pdf_save_coordinates
        procedure :: set_coordinates => pdf_set_coordinates
        
        ! Internal helper
        procedure, private :: normalize_coords => normalize_to_pdf_coords_facade
    end type pdf_context
    
contains

    function create_pdf_canvas(width, height) result(ctx)
        integer, intent(in) :: width, height
        type(pdf_context) :: ctx
        
        call setup_canvas(ctx, width, height)
        
        ! Initialize core PDF context
        ctx%core_ctx = create_pdf_canvas_core(real(width, wp), real(height, wp))
        
        ! Initialize stream writer
        call ctx%stream_writer%initialize_stream()
        call ctx%stream_writer%add_to_stream("q")
        call ctx%stream_writer%add_to_stream("1 w")
        call ctx%stream_writer%add_to_stream("1 J")
        call ctx%stream_writer%add_to_stream("1 j")
        call ctx%stream_writer%add_to_stream("0 0 1 RG")
        
        ! Set up matplotlib-style margins
        ctx%margins = plot_margins_t()
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
    end function create_pdf_canvas
    
    subroutine draw_pdf_line(this, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: pdf_x1, pdf_y1, pdf_x2, pdf_y2
        
        call this%normalize_coords(x1, y1, pdf_x1, pdf_y1)
        call this%normalize_coords(x2, y2, pdf_x2, pdf_y2)
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
    
    subroutine draw_pdf_text_wrapper(this, x, y, text)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        real(wp) :: pdf_x, pdf_y
        character(len=500) :: processed_text
        integer :: processed_len
        
        ! Process LaTeX commands to Unicode
        call process_latex_in_text(text, processed_text, processed_len)
        
        call this%normalize_coords(x, y, pdf_x, pdf_y)
        
        ! Delegate to text module
        call draw_mixed_font_text(this%core_ctx, pdf_x, pdf_y, processed_text(1:processed_len))
    end subroutine draw_pdf_text_wrapper
    
    subroutine write_pdf_file_facade(this, filename)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        ! Merge stream data
        this%core_ctx%stream_data = this%stream_writer%content_stream
        
        ! Delegate to I/O module
        call write_pdf_file(this%core_ctx, filename)
    end subroutine write_pdf_file_facade
    
    subroutine normalize_to_pdf_coords_facade(this, x, y, pdf_x, pdf_y)
        class(pdf_context), intent(in) :: this
        real(wp), intent(in) :: x, y
        real(wp), intent(out) :: pdf_x, pdf_y
        
        ! Proper data coordinate to PDF coordinate transformation
        ! This restores the original algorithm from before the refactoring
        
        ! Transform coordinates to plot area (like matplotlib)
        ! Note: PDF coordinates have Y=0 at bottom (same as plot coordinates)
        pdf_x = (x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_area%width, wp) + &
                real(this%plot_area%left, wp)
        pdf_y = (y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_area%height, wp) + &
                real(this%height - this%plot_area%bottom - this%plot_area%height, wp)
    end subroutine normalize_to_pdf_coords_facade
    
    subroutine pdf_save_graphics_state(this)
        class(pdf_context), intent(inout) :: this
        
        call this%stream_writer%save_state()
    end subroutine pdf_save_graphics_state
    
    subroutine pdf_restore_graphics_state(this)
        class(pdf_context), intent(inout) :: this
        
        call this%stream_writer%restore_state()
    end subroutine pdf_restore_graphics_state
    
    subroutine draw_pdf_marker(this, x, y, style)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        real(wp) :: pdf_x, pdf_y
        real(wp) :: size
        
        size = 5.0_wp  ! Default marker size
        call this%normalize_coords(x, y, pdf_x, pdf_y)
        
        ! Save state for marker drawing
        call this%save_graphics_state()
        
        ! Draw marker based on style
        select case(trim(style))
        case('o', 'circle')
            call draw_pdf_circle_with_outline(this%stream_writer, pdf_x, pdf_y, size)
        case('s', 'square')
            call draw_pdf_square_with_outline(this%stream_writer, pdf_x, pdf_y, size)
        case('d', 'diamond')
            call draw_pdf_diamond_with_outline(this%stream_writer, pdf_x, pdf_y, size)
        case('x', 'cross')
            call draw_pdf_x_marker(this%stream_writer, pdf_x, pdf_y, size)
        end select
        
        ! Restore state
        call this%restore_graphics_state()
    end subroutine draw_pdf_marker
    
    subroutine pdf_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b
        
        ! Set edge color for stroking
        call this%color(edge_r, edge_g, edge_b)
    end subroutine pdf_set_marker_colors
    
    subroutine pdf_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, edge_alpha, face_r, face_g, face_b, face_alpha)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        
        ! PDF doesn't support alpha directly, just use the colors
        call pdf_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
    end subroutine pdf_set_marker_colors_with_alpha
    
    subroutine draw_pdf_arrow_facade(this, x, y, dx, dy, size, style)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        real(wp) :: pdf_x, pdf_y
        
        call this%normalize_coords(x, y, pdf_x, pdf_y)
        call draw_pdf_arrow(this%stream_writer, pdf_x, pdf_y, dx, dy, size, style)
    end subroutine draw_pdf_arrow_facade
    
    function pdf_get_ascii_output(this) result(output)
        class(pdf_context), intent(in) :: this
        character(len=:), allocatable :: output
        
        output = "PDF output (non-ASCII format)"
    end function pdf_get_ascii_output
    
    real(wp) function pdf_get_width_scale(this) result(scale)
        class(pdf_context), intent(in) :: this
        scale = real(this%width, wp) / PDF_WIDTH
    end function pdf_get_width_scale
    
    real(wp) function pdf_get_height_scale(this) result(scale)
        class(pdf_context), intent(in) :: this
        scale = real(this%height, wp) / PDF_HEIGHT
    end function pdf_get_height_scale
    
    subroutine pdf_fill_quad(this, x_quad, y_quad)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        real(wp) :: px(4), py(4)
        character(len=512) :: cmd
        integer :: i
        
        ! Convert to PDF coordinates
        do i = 1, 4
            call this%normalize_coords(x_quad(i), y_quad(i), px(i), py(i))
        end do
        
        ! Use current color (should be set before calling)
        
        ! Draw filled quadrilateral
        write(cmd, '(8(F0.3, 1X), "m l l l h f")') px(1), py(1), px(2), py(2), px(3), py(3), px(4), py(4)
        call this%stream_writer%add_to_stream(trim(cmd))
    end subroutine pdf_fill_quad
    
    subroutine pdf_fill_heatmap(this, x_grid, y_grid, z_grid, z_min, z_max)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        
        integer :: i, j
        real(wp) :: x_quad(4), y_quad(4)
        real(wp) :: value, norm_value
        real(wp), dimension(3) :: color
        character(len=32) :: cmd
        
        do i = 1, size(z_grid, 1) - 1
            do j = 1, size(z_grid, 2) - 1
                ! Get normalized value
                value = z_grid(i, j)
                if (z_max > z_min) then
                    norm_value = (value - z_min) / (z_max - z_min)
                else
                    norm_value = 0.5_wp
                end if
                
                ! Simple grayscale color
                color = [norm_value, norm_value, norm_value]
                
                ! Set fill color
                write(cmd, '(3(F0.3, 1X), "rg")') color
                call this%stream_writer%add_to_stream(trim(cmd))
                
                ! Define quad corners
                x_quad = [x_grid(i), x_grid(i+1), x_grid(i+1), x_grid(i)]
                y_quad = [y_grid(j), y_grid(j), y_grid(j+1), y_grid(j+1)]
                
                ! Fill cell
                call pdf_fill_quad(this, x_quad, y_quad)
            end do
        end do
    end subroutine pdf_fill_heatmap
    
    subroutine pdf_render_legend_specialized(this, entries, x, y, width, height)
        class(pdf_context), intent(inout) :: this
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(in) :: x, y, width, height
        
        ! Simplified legend rendering
        integer :: i
        real(wp) :: y_pos
        
        y_pos = y
        do i = 1, size(entries)
            call this%text(x, y_pos, entries(i)%label)
            y_pos = y_pos - 20.0_wp
        end do
    end subroutine pdf_render_legend_specialized
    
    subroutine pdf_calculate_legend_dimensions(this, entries, width, height)
        class(pdf_context), intent(in) :: this
        type(legend_entry_t), dimension(:), intent(in) :: entries
        real(wp), intent(out) :: width, height
        
        width = 100.0_wp
        height = real(size(entries), wp) * 20.0_wp
    end subroutine pdf_calculate_legend_dimensions
    
    subroutine pdf_set_legend_border_width(this, width)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: width
        
        call this%set_line_width(width)
    end subroutine pdf_set_legend_border_width
    
    subroutine pdf_calculate_legend_position(this, loc, x, y)
        class(pdf_context), intent(in) :: this
        character(len=*), intent(in) :: loc
        real(wp), intent(out) :: x, y
        
        select case(trim(loc))
        case('upper right')
            x = real(this%plot_area%left + this%plot_area%width - 100, wp)
            y = real(this%plot_area%bottom + this%plot_area%height - 20, wp)
        case('upper left')
            x = real(this%plot_area%left + 20, wp)
            y = real(this%plot_area%bottom + this%plot_area%height - 20, wp)
        case('lower right')
            x = real(this%plot_area%left + this%plot_area%width - 100, wp)
            y = real(this%plot_area%bottom + 100, wp)
        case('lower left')
            x = real(this%plot_area%left + 20, wp)
            y = real(this%plot_area%bottom + 100, wp)
        case default
            x = real(this%plot_area%left + this%plot_area%width - 100, wp)
            y = real(this%plot_area%bottom + this%plot_area%height - 20, wp)
        end select
    end subroutine pdf_calculate_legend_position
    
    subroutine pdf_extract_rgb_data(this, width, height, rgb_data)
        class(pdf_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)
        
        ! PDF doesn't have RGB pixel data - return white
        rgb_data = 1.0_wp
    end subroutine pdf_extract_rgb_data
    
    subroutine pdf_get_png_data(this, width, height, png_data, status)
        class(pdf_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        ! PDF doesn't generate PNG data
        allocate(png_data(0))
        status = -1  ! Indicate not supported
    end subroutine pdf_get_png_data
    
    subroutine pdf_prepare_3d_data(this, plots)
        class(pdf_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)
        
        ! PDF doesn't support 3D - stub implementation
        ! Nothing to prepare for 2D PDF output
    end subroutine pdf_prepare_3d_data
    
    subroutine pdf_render_ylabel(this, ylabel)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        
        real(wp) :: x, y
        
        x = real(this%plot_area%left - 40, wp)
        y = real(this%plot_area%bottom + this%plot_area%height / 2, wp)
        
        ! Draw rotated Y-axis label
        call draw_rotated_mixed_font_text(this%core_ctx, x, y, ylabel)
    end subroutine pdf_render_ylabel
    
    subroutine pdf_draw_axes_stub(this, xscale, yscale, symlog_threshold, &
                                   x_min, x_max, y_min, y_max, &
                                   title, xlabel, ylabel, &
                                   z_min, z_max, has_3d_plots)
        ! Implementation that properly handles plot area for axes drawing
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        
        ! Draw axes using the full PDF context with proper plot area
        character(len=256) :: title_str, xlabel_str, ylabel_str
        logical :: enable_grid
        
        title_str = ""
        xlabel_str = ""
        ylabel_str = ""
        enable_grid = .false.
        if (allocated(title)) title_str = title
        if (allocated(xlabel)) xlabel_str = xlabel
        if (allocated(ylabel)) ylabel_str = ylabel
        
        ! Use the proper plot area-aware axes drawing
        call pdf_draw_axes_with_plot_area(this, xscale, yscale, symlog_threshold, &
                                         x_min, x_max, y_min, y_max, &
                                         title_str, xlabel_str, ylabel_str, enable_grid)
    end subroutine pdf_draw_axes_stub
    
    subroutine pdf_draw_axes_and_labels_facade(this, xscale, yscale, symlog_threshold, &
                                              x_min, x_max, y_min, y_max, &
                                              title, xlabel, ylabel, enable_grid)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: title, xlabel, ylabel
        logical, intent(in), optional :: enable_grid
        
        ! Delegate to axes module
        call draw_pdf_axes_and_labels(this%core_ctx, xscale, yscale, symlog_threshold, &
                                     x_min, x_max, y_min, y_max, &
                                     title, xlabel, ylabel, enable_grid)
    end subroutine pdf_draw_axes_and_labels_facade
    
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
        
        ! Set new coordinate bounds
        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max
    end subroutine pdf_set_coordinates
    
    subroutine pdf_draw_axes_with_plot_area(this, xscale, yscale, symlog_threshold, &
                                           x_min, x_max, y_min, y_max, &
                                           title, xlabel, ylabel, enable_grid)
        !! Draw axes using the actual plot area from the PDF context
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: title, xlabel, ylabel
        logical, intent(in) :: enable_grid
        
        ! Draw plot frame using actual plot area
        call pdf_draw_plot_frame(this)
        
        ! Draw tick marks and labels using data coordinates and plot area  
        call pdf_draw_ticks_and_labels(this, x_min, x_max, y_min, y_max, xscale, yscale)
        
        ! Draw title and axis labels if provided
        if (len_trim(title) > 0) call pdf_draw_title(this, title)
        if (len_trim(xlabel) > 0) call pdf_draw_xlabel(this, xlabel) 
        if (len_trim(ylabel) > 0) call pdf_draw_ylabel(this, ylabel)
        
        ! Draw grid if enabled
        if (enable_grid) call pdf_draw_grid(this, x_min, x_max, y_min, y_max)
    end subroutine pdf_draw_axes_with_plot_area
    
    subroutine pdf_draw_plot_frame(this)
        !! Draw plot frame using the actual plot area
        class(pdf_context), intent(inout) :: this
        character(len=256) :: frame_cmd
        real(wp) :: x1, y1, width, height
        
        x1 = real(this%plot_area%left, wp)
        y1 = real(this%height - this%plot_area%bottom - this%plot_area%height, wp)
        width = real(this%plot_area%width, wp)
        height = real(this%plot_area%height, wp)
        
        ! Set line color to black
        call this%core_ctx%set_color(0.0_wp, 0.0_wp, 0.0_wp)
        call this%core_ctx%set_line_width(1.0_wp)
        
        ! Draw rectangle frame
        write(frame_cmd, '(F0.3, 1X, F0.3, " ", F0.3, 1X, F0.3, " re S")') x1, y1, width, height
        this%core_ctx%stream_data = this%core_ctx%stream_data // trim(adjustl(frame_cmd)) // new_line('a')
    end subroutine pdf_draw_plot_frame
    
    subroutine pdf_draw_ticks_and_labels(this, x_min, x_max, y_min, y_max, xscale, yscale)
        !! Draw tick marks and labels using proper coordinate transformation
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: xscale, yscale
        
        real(wp) :: x_range, y_range, x_step, y_step
        real(wp) :: tick_x, tick_y, pdf_x, pdf_y
        integer :: i, num_ticks
        character(len=32) :: tick_label
        character(len=256) :: tick_cmd, text_cmd
        real(wp), parameter :: TICK_LENGTH = 5.0_wp
        
        x_range = x_max - x_min
        y_range = y_max - y_min
        num_ticks = 6
        
        ! Draw X-axis ticks and labels
        if (x_range > 0.0_wp) then
            x_step = x_range / real(num_ticks - 1, wp)
            do i = 1, num_ticks
                tick_x = x_min + real(i - 1, wp) * x_step
                call this%normalize_coords(tick_x, y_min, pdf_x, pdf_y)
                
                ! Draw tick mark
                write(tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                    pdf_x, pdf_y, pdf_x, pdf_y - TICK_LENGTH
                this%core_ctx%stream_data = this%core_ctx%stream_data // trim(adjustl(tick_cmd)) // new_line('a')
                
                ! Draw tick label
                write(tick_label, '(F0.1)') tick_x
                tick_label = adjustl(tick_label)
                call draw_mixed_font_text(this%core_ctx, pdf_x - 10.0_wp, pdf_y - 20.0_wp, trim(tick_label))
            end do
        end if
        
        ! Draw Y-axis ticks and labels
        if (y_range > 0.0_wp) then
            y_step = y_range / real(num_ticks - 1, wp)
            do i = 1, num_ticks
                tick_y = y_min + real(i - 1, wp) * y_step
                call this%normalize_coords(x_min, tick_y, pdf_x, pdf_y)
                
                ! Draw tick mark
                write(tick_cmd, '(F0.3, 1X, F0.3, " m ", F0.3, 1X, F0.3, " l S")') &
                    pdf_x, pdf_y, pdf_x - TICK_LENGTH, pdf_y
                this%core_ctx%stream_data = this%core_ctx%stream_data // trim(adjustl(tick_cmd)) // new_line('a')
                
                ! Draw tick label
                write(tick_label, '(F0.1)') tick_y
                tick_label = adjustl(tick_label)
                call draw_mixed_font_text(this%core_ctx, pdf_x - 30.0_wp, pdf_y - 5.0_wp, trim(tick_label))
            end do
        end if
    end subroutine pdf_draw_ticks_and_labels
    
    subroutine pdf_draw_title(this, title)
        !! Draw plot title
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: title
        real(wp) :: x, y
        
        x = real(this%plot_area%left + this%plot_area%width / 2, wp)
        y = real(this%height - this%plot_area%bottom - this%plot_area%height - 30, wp)
        
        call draw_mixed_font_text(this%core_ctx, x - 50.0_wp, y, title)
    end subroutine pdf_draw_title
    
    subroutine pdf_draw_xlabel(this, xlabel)
        !! Draw X-axis label
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: xlabel
        real(wp) :: x, y
        
        x = real(this%plot_area%left + this%plot_area%width / 2, wp)
        y = real(this%height - this%plot_area%bottom + 50, wp)
        
        call draw_mixed_font_text(this%core_ctx, x - 30.0_wp, y, xlabel)
    end subroutine pdf_draw_xlabel
    
    subroutine pdf_draw_ylabel(this, ylabel)
        !! Draw Y-axis label (rotated)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        real(wp) :: x, y
        
        x = real(this%plot_area%left - 60, wp)
        y = real(this%height - this%plot_area%bottom - this%plot_area%height / 2, wp)
        
        call draw_rotated_mixed_font_text(this%core_ctx, x, y, ylabel)
    end subroutine pdf_draw_ylabel
    
    subroutine pdf_draw_grid(this, x_min, x_max, y_min, y_max)
        !! Draw grid lines (stub for now)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        ! Grid implementation can be added later if needed
        ! For now, just a stub to satisfy the interface
    end subroutine pdf_draw_grid

end module fortplot_pdf