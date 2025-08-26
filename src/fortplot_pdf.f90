module fortplot_pdf
    !! PDF backend main interface - consolidated single coordinate system
    !! Eliminates dual coordinate systems and function duplication
    !! Uses plot area approach consistently throughout
    
    use fortplot_pdf_core
    use fortplot_pdf_text
    use fortplot_pdf_drawing
    use fortplot_pdf_axes
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
        procedure :: save_graphics_state => save_graphics_state_wrapper
        procedure :: restore_graphics_state => restore_graphics_state_wrapper
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
        
        call setup_canvas(ctx, width, height)
        
        ctx%core_ctx = create_pdf_canvas_core(real(width, wp), real(height, wp))
        
        call ctx%stream_writer%initialize_stream()
        call ctx%stream_writer%add_to_stream("q")
        call ctx%stream_writer%add_to_stream("1 w")
        call ctx%stream_writer%add_to_stream("1 J")
        call ctx%stream_writer%add_to_stream("1 j")
        call ctx%stream_writer%add_to_stream("0 0 1 RG")
        
        ctx%margins = plot_margins_t()
        call calculate_plot_area(width, height, ctx%margins, ctx%plot_area)
        
        call ctx%update_coord_context()
    end function create_pdf_canvas
    
    subroutine draw_pdf_line(this, x1, y1, x2, y2)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        real(wp) :: pdf_x1, pdf_y1, pdf_x2, pdf_y2
        
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
            dash_pattern = '[15 5] 0 d'  ! 15 units on, 5 units off
        case (':', 'dotted')
            dash_pattern = '[2 5] 0 d'  ! 2 units on, 5 units off
        case ('-.', 'dashdot')
            dash_pattern = '[15 5 2 5] 0 d'  ! dash-dot pattern
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
        character(len=500) :: processed_text
        integer :: processed_len
        
        call process_latex_in_text(text, processed_text, processed_len)
        call normalize_to_pdf_coords(this%coord_ctx, x, y, pdf_x, pdf_y)
        call draw_mixed_font_text(this%core_ctx, pdf_x, pdf_y, processed_text(1:processed_len))
    end subroutine draw_pdf_text_wrapper
    
    subroutine write_pdf_file_facade(this, filename)
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        ! Automatically render axes if they haven't been rendered yet
        ! This provides better UX for low-level PDF API users
        call this%render_axes()
        
        this%core_ctx%stream_data = this%stream_writer%content_stream
        call write_pdf_file(this%core_ctx, filename)
    end subroutine write_pdf_file_facade
    
    subroutine update_coord_context(this)
        class(pdf_context), intent(inout) :: this
        
        this%coord_ctx%x_min = this%x_min
        this%coord_ctx%x_max = this%x_max
        this%coord_ctx%y_min = this%y_min
        this%coord_ctx%y_max = this%y_max
        this%coord_ctx%width = this%width
        this%coord_ctx%height = this%height
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
        ctx%width = this%width
        ctx%height = this%height
        ctx%plot_area = this%plot_area
        ctx%core_ctx = this%core_ctx
    end function make_coord_context
    
    subroutine save_graphics_state_wrapper(this)
        class(pdf_context), intent(inout) :: this
        call pdf_save_graphics_state(this%stream_writer)
    end subroutine save_graphics_state_wrapper
    
    subroutine restore_graphics_state_wrapper(this)
        class(pdf_context), intent(inout) :: this
        call pdf_restore_graphics_state(this%stream_writer)
    end subroutine restore_graphics_state_wrapper
    
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
        
        call this%update_coord_context()
        
        ! Convert to PDF coordinates
        do i = 1, 4
            call normalize_to_pdf_coords(this%coord_ctx, x_quad(i), y_quad(i), px(i), py(i))
        end do
        
        ! Draw filled quadrilateral
        write(cmd, '(8(F0.3, 1X), "m l l l h f")') px(1), py(1), px(2), py(2), px(3), py(3), px(4), py(4)
        call this%stream_writer%add_to_stream(trim(cmd))
    end subroutine fill_quad_wrapper
    
    subroutine fill_heatmap_wrapper(this, x_grid, y_grid, z_grid, z_min, z_max)
        class(pdf_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        
        integer :: i, j
        real(wp) :: x_quad(4), y_quad(4)
        real(wp) :: value, norm_value
        real(wp), dimension(3) :: color
        
        call this%update_coord_context()
        
        do i = 1, size(z_grid, 1) - 1
            do j = 1, size(z_grid, 2) - 1
                ! Get cell value
                value = z_grid(i, j)
                
                ! Handle edge case where z_max == z_min
                if (abs(z_max - z_min) > EPSILON_COMPARE) then
                    norm_value = (value - z_min) / (z_max - z_min)
                else
                    norm_value = 0.5_wp
                end if
                
                ! Clamp to [0, 1] range for safety
                norm_value = max(0.0_wp, min(1.0_wp, norm_value))
                
                ! Simple grayscale color (colormaps should be handled at higher level)
                color = [norm_value, norm_value, norm_value]
                
                ! Set fill color using validated method
                call this%stream_writer%write_color(color(1), color(2), color(3))
                
                ! Define quad corners
                x_quad = [x_grid(i), x_grid(i+1), x_grid(i+1), x_grid(i)]
                y_quad = [y_grid(j), y_grid(j), y_grid(j+1), y_grid(j+1)]
                
                ! Fill cell
                call this%fill_quad(x_quad, y_quad)
            end do
        end do
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
        class(pdf_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        
        character(len=256) :: title_str, xlabel_str, ylabel_str
        
        title_str = ""; xlabel_str = ""; ylabel_str = ""
        if (allocated(title)) title_str = title
        if (allocated(xlabel)) xlabel_str = xlabel
        if (allocated(ylabel)) ylabel_str = ylabel
        
        call draw_pdf_axes_and_labels(this%core_ctx, xscale, yscale, symlog_threshold, &
                                     x_min, x_max, y_min, y_max, &
                                     title_str, xlabel_str, ylabel_str, &
                                     real(this%plot_area%left, wp), &
                                     real(this%plot_area%bottom, wp), &
                                     real(this%plot_area%width, wp), &
                                     real(this%plot_area%height, wp), &
                                     real(this%height, wp))
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
        if (this%x_min == this%x_max .or. this%y_min == this%y_max) then
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
