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
    
    ! Temporary type definition (should be imported from proper module)
    type :: legend_entry_t
        character(len=100) :: label
        real(wp), dimension(3) :: color
        character(len=20) :: linestyle
        character(len=10) :: marker
    end type legend_entry_t
    
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
        
        ! Delegate to core module
        call normalize_to_pdf_coords(this%core_ctx, x, y, pdf_x, pdf_y)
    end subroutine normalize_to_pdf_coords_facade
    
    subroutine pdf_save_graphics_state(this)
        class(pdf_context), intent(inout) :: this
        
        call this%stream_writer%save_state()
    end subroutine pdf_save_graphics_state
    
    subroutine pdf_restore_graphics_state(this)
        class(pdf_context), intent(inout) :: this
        
        call this%stream_writer%restore_state()
    end subroutine pdf_restore_graphics_state
    
    subroutine draw_pdf_marker(this, x, y, marker_type, size, fill_color, edge_color)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, size
        character(len=*), intent(in) :: marker_type
        real(wp), dimension(3), intent(in), optional :: fill_color, edge_color
        real(wp) :: pdf_x, pdf_y
        
        call this%normalize_coords(x, y, pdf_x, pdf_y)
        
        ! Save state for marker drawing
        call this%save_graphics_state()
        
        ! Set colors if provided
        if (present(fill_color)) then
            call pdf_set_marker_colors(this, fill_color, edge_color)
        end if
        
        ! Draw marker based on type
        select case(trim(marker_type))
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
    
    subroutine pdf_set_marker_colors(this, fill_color, edge_color)
        class(pdf_context), intent(inout) :: this
        real(wp), dimension(3), intent(in) :: fill_color
        real(wp), dimension(3), intent(in), optional :: edge_color
        
        if (present(edge_color)) then
            call this%color(edge_color(1), edge_color(2), edge_color(3))
        else
            call this%color(fill_color(1), fill_color(2), fill_color(3))
        end if
    end subroutine pdf_set_marker_colors
    
    subroutine pdf_set_marker_colors_with_alpha(this, fill_color, edge_color, alpha)
        class(pdf_context), intent(inout) :: this
        real(wp), dimension(3), intent(in) :: fill_color
        real(wp), dimension(3), intent(in), optional :: edge_color
        real(wp), intent(in) :: alpha
        
        ! PDF doesn't support alpha directly, just use the colors
        call pdf_set_marker_colors(this, fill_color, edge_color)
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
    
    subroutine pdf_fill_quad(this, x1, y1, x2, y2, x3, y3, x4, y4, color)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2, x3, y3, x4, y4
        real(wp), dimension(3), intent(in) :: color
        real(wp) :: px1, py1, px2, py2, px3, py3, px4, py4
        character(len=512) :: cmd
        
        ! Convert to PDF coordinates
        call this%normalize_coords(x1, y1, px1, py1)
        call this%normalize_coords(x2, y2, px2, py2)
        call this%normalize_coords(x3, y3, px3, py3)
        call this%normalize_coords(x4, y4, px4, py4)
        
        ! Set fill color
        write(cmd, '(3(F0.3, 1X), "rg")') color
        call this%stream_writer%add_to_stream(trim(cmd))
        
        ! Draw filled quadrilateral
        write(cmd, '(8(F0.3, 1X), "m l l l h f")') px1, py1, px2, py2, px3, py3, px4, py4
        call this%stream_writer%add_to_stream(trim(cmd))
    end subroutine pdf_fill_quad
    
    subroutine pdf_fill_heatmap(this, data, x_min, x_max, y_min, y_max, colormap_name)
        class(pdf_context), intent(inout) :: this
        real(wp), dimension(:,:), intent(in) :: data
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=*), intent(in) :: colormap_name
        
        integer :: i, j
        real(wp) :: cell_width, cell_height
        real(wp) :: x, y, value
        real(wp), dimension(3) :: color
        
        cell_width = (x_max - x_min) / real(size(data, 1), wp)
        cell_height = (y_max - y_min) / real(size(data, 2), wp)
        
        do i = 1, size(data, 1)
            do j = 1, size(data, 2)
                x = x_min + (i - 0.5_wp) * cell_width
                y = y_min + (j - 0.5_wp) * cell_height
                value = data(i, j)
                
                ! Get color from colormap
                call colormap_value_to_color(value, colormap_name, color)
                
                ! Fill cell
                call pdf_fill_quad(this, &
                    x - cell_width/2, y - cell_height/2, &
                    x + cell_width/2, y - cell_height/2, &
                    x + cell_width/2, y + cell_height/2, &
                    x - cell_width/2, y + cell_height/2, &
                    color)
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
    
    subroutine pdf_extract_rgb_data(this, rgb_data)
        class(pdf_context), intent(in) :: this
        integer, dimension(:,:,:), allocatable, intent(out) :: rgb_data
        
        ! PDF doesn't have RGB pixel data
        allocate(rgb_data(1, 1, 3))
        rgb_data = 255
    end subroutine pdf_extract_rgb_data
    
    subroutine pdf_get_png_data(this, png_data)
        class(pdf_context), intent(in) :: this
        integer(1), dimension(:), allocatable, intent(out) :: png_data
        
        ! PDF doesn't generate PNG data
        allocate(png_data(0))
    end subroutine pdf_get_png_data
    
    subroutine pdf_prepare_3d_data(this, x, y, z, points_2d)
        class(pdf_context), intent(in) :: this
        real(wp), dimension(:), intent(in) :: x, y, z
        real(wp), dimension(:,:), allocatable, intent(out) :: points_2d
        
        integer :: n
        
        n = size(x)
        allocate(points_2d(2, n))
        
        ! Simple orthographic projection
        points_2d(1, :) = x - 0.3_wp * y
        points_2d(2, :) = z - 0.3_wp * y
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
                                   title, xlabel, ylabel, enable_grid)
        ! Stub implementation to match parent signature
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        logical, intent(in) :: enable_grid
        
        ! Delegate to actual implementation with fixed strings
        character(len=256) :: title_str, xlabel_str, ylabel_str
        
        title_str = ""
        xlabel_str = ""
        ylabel_str = ""
        if (allocated(title)) title_str = title
        if (allocated(xlabel)) xlabel_str = xlabel
        if (allocated(ylabel)) ylabel_str = ylabel
        
        call draw_pdf_axes_and_labels(this%core_ctx, xscale, yscale, symlog_threshold, &
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
    
    subroutine pdf_save_coordinates(this)
        class(pdf_context), intent(inout) :: this
        
        ! Save current transformation matrix
        call this%save_graphics_state()
    end subroutine pdf_save_coordinates
    
    subroutine pdf_set_coordinates(this, x_offset, y_offset, x_scale, y_scale)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_offset, y_offset, x_scale, y_scale
        character(len=256) :: transform_cmd
        
        ! Apply coordinate transformation
        write(transform_cmd, '(4(F0.3, 1X), "0 0 cm")') x_scale, 0.0_wp, 0.0_wp, y_scale
        call this%stream_writer%add_to_stream(trim(transform_cmd))
        
        write(transform_cmd, '("1 0 0 1 ", 2(F0.3, 1X), "cm")') x_offset, y_offset
        call this%stream_writer%add_to_stream(trim(transform_cmd))
    end subroutine pdf_set_coordinates

end module fortplot_pdf