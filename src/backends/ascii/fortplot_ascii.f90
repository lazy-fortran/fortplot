module fortplot_ascii
    !! ASCII terminal plotting backend
    !!
    !! This module implements text-based plotting for terminal output using
    !! ASCII characters and Unicode box drawing characters. Provides basic
    !! line plotting with character density mapping for visualization.
    !!
    !! Author: fortplot contributors
    
    use fortplot_context, only: plot_context, setup_canvas
    use fortplot_logging, only: log_info, log_error
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_constants, only: EPSILON_COMPARE
    use fortplot_ascii_utils, only: text_element_t, get_char_density, get_blend_char, ASCII_CHARS
    use fortplot_ascii_utils, only: render_text_elements_to_canvas, print_centered_title, write_centered_title
    use fortplot_ascii_elements, only: draw_ascii_marker, fill_ascii_heatmap, draw_ascii_arrow
    use fortplot_ascii_elements, only: render_ascii_legend_specialized, calculate_ascii_legend_dimensions
    use fortplot_ascii_elements, only: set_ascii_legend_border_width, calculate_ascii_legend_position
    use fortplot_ascii_elements, only: draw_ascii_axes_and_labels
    use fortplot_ascii_backend_support, only: extract_ascii_rgb_data, get_ascii_png_data, prepare_ascii_3d_data
    use fortplot_ascii_backend_support, only: render_ascii_ylabel, render_ascii_axes
    use fortplot_ascii_rendering, only: ascii_finalize => ascii_finalize, ascii_get_output
    use fortplot_ascii_primitives, only: ascii_draw_line_primitive, ascii_fill_quad_primitive
    use fortplot_ascii_primitives, only: ascii_draw_text_primitive
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: ascii_context, create_ascii_canvas

    real(wp), parameter :: ASCII_CHAR_ASPECT = 2.0_wp

    type, extends(plot_context) :: ascii_context
        character(len=1), allocatable :: canvas(:,:)
        character(len=:), allocatable :: title_text
        character(len=:), allocatable :: xlabel_text
        character(len=:), allocatable :: ylabel_text
        logical :: title_set = .false.  ! Track if title was explicitly set
        type(text_element_t), allocatable :: text_elements(:)
        integer :: num_text_elements = 0
        real(wp) :: current_r, current_g, current_b
        integer :: plot_width = 80
        integer :: plot_height = 24
        character(len=96), allocatable :: legend_lines(:)
        integer :: num_legend_lines = 0
        logical :: capturing_legend = .false.
        character(len=64), allocatable :: pie_legend_labels(:)
        character(len=32), allocatable :: pie_legend_values(:)
        integer :: pie_legend_count = 0
        character(len=32), allocatable :: pie_autopct_queue(:)
        integer :: pie_autopct_count = 0
        integer, allocatable :: legend_entry_indices(:)
        logical, allocatable :: legend_entry_has_autopct(:)
        integer :: legend_entry_count = 0
        integer :: legend_autopct_cursor = 1
        character(len=64), allocatable :: legend_entry_labels(:)
        real(wp) :: stored_y_min = 0.0_wp
        real(wp) :: stored_y_max = 0.0_wp
        logical :: has_stored_y_range = .false.
    contains
        procedure :: line => ascii_draw_line
        procedure :: color => ascii_set_color
        procedure :: text => ascii_draw_text
        procedure :: set_line_width => ascii_set_line_width
        procedure :: set_line_style => ascii_set_line_style
        procedure :: save => ascii_save
        procedure :: set_title => ascii_set_title
        procedure :: draw_marker => ascii_draw_marker
        procedure :: set_marker_colors => ascii_set_marker_colors
        procedure :: set_marker_colors_with_alpha => ascii_set_marker_colors_with_alpha
        procedure :: fill_heatmap => ascii_fill_heatmap
        procedure :: draw_arrow => ascii_draw_arrow
        procedure :: get_ascii_output => ascii_get_output_method
        
        !! New polymorphic methods to eliminate SELECT TYPE
        procedure :: get_width_scale => ascii_get_width_scale
        procedure :: get_height_scale => ascii_get_height_scale
        procedure :: fill_quad => ascii_fill_quad
        procedure :: render_legend_specialized => ascii_render_legend_specialized
        procedure :: calculate_legend_dimensions => ascii_calculate_legend_dimensions
        procedure :: set_legend_border_width => ascii_set_legend_border_width
        procedure :: calculate_legend_position_backend => ascii_calculate_legend_position
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
        procedure :: register_pie_legend_entry => ascii_register_pie_legend_entry
        procedure :: clear_pie_legend_entries => ascii_clear_pie_legend_entries
        procedure :: get_pie_autopct => ascii_get_pie_autopct
    end type ascii_context
    
    character(len=*), parameter :: DENSITY_CHARS = ' ░▒▓█'
    character(len=*), parameter :: BOX_CHARS = '-|+++++++'
    
contains

    function create_ascii_canvas(width, height) result(ctx)
        integer, intent(in), optional :: width, height
        type(ascii_context) :: ctx
        integer :: w, h
        
        ! Suppress unused parameter warnings
        associate(unused_w => width, unused_h => height); end associate
        
        ! ASCII backend uses 4:3 aspect ratio accounting for terminal character dimensions
        ! Terminal characters are taller than they are wide; a 24-row canvas keeps the
        ! legend heuristics inside ASCII mode while preserving enough vertical space.
        w = 80
        h = 24
        
        call setup_canvas(ctx, w, h)
        
        ctx%plot_width = w
        ctx%plot_height = h
        
        allocate(ctx%canvas(h, w))
        ctx%canvas = ' '
        
        ! Initialize text elements storage (start with capacity for 20 text elements)
        allocate(ctx%text_elements(20))
        ctx%num_text_elements = 0
        ctx%title_set = .false.
        allocate(ctx%legend_lines(0))
        ctx%num_legend_lines = 0
        ctx%capturing_legend = .false.
        allocate(ctx%pie_legend_labels(0))
        allocate(ctx%pie_legend_values(0))
        ctx%pie_legend_count = 0
        allocate(ctx%pie_autopct_queue(0))
        ctx%pie_autopct_count = 0
        allocate(ctx%legend_entry_indices(0))
        allocate(ctx%legend_entry_has_autopct(0))
        allocate(ctx%legend_entry_labels(0))
        ctx%legend_entry_count = 0
        ctx%legend_autopct_cursor = 1

        ctx%current_r = 0.0_wp
        ctx%current_g = 0.0_wp 
        ctx%current_b = 1.0_wp
    end function create_ascii_canvas
    
    subroutine ascii_draw_line(this, x1, y1, x2, y2)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        
        call ascii_draw_line_primitive(this%canvas, x1, y1, x2, y2, &
                                      this%x_min, this%x_max, this%y_min, this%y_max, &
                                      this%plot_width, this%plot_height, &
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
        !! Set line width for ASCII context (no-op as ASCII uses fixed character width)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: width
        
        ! Suppress unused parameter warnings
        associate(unused_int => this%width, unused_real => width); end associate
        
        ! ASCII context doesn't support variable line widths
        ! This is a no-op to satisfy the interface
    end subroutine ascii_set_line_width
    
    subroutine ascii_set_line_style(this, style)
        !! Set line style for ASCII context (no-op as ASCII uses fixed characters)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: style
        
        ! Suppress unused parameter warnings
        associate(unused_int => this%width, unused_style => style); end associate
        
        ! ASCII context doesn't support different line styles
        ! All lines are rendered as continuous ASCII characters
        ! This is a no-op to satisfy the interface
    end subroutine ascii_set_line_style
    
    subroutine ascii_draw_text(this, x, y, text)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        integer :: text_x, text_y
        character(len=:), allocatable :: processed_text
        character(len=:), allocatable :: trimmed_text
        character(len=96) :: formatted_line
        character(len=64) :: entry_label
        character(len=32) :: autopct_value

        trimmed_text = trim(adjustl(text))

        if (trimmed_text == 'ASCII Legend') then
            call reset_ascii_legend_lines(this)
            call append_ascii_legend_line(this, 'Legend:')
            this%capturing_legend = .true.
            this%legend_entry_count = 0
            this%legend_autopct_cursor = 1
            return
        end if

        if (is_autopct_text(trimmed_text)) then
            call enqueue_pie_autopct(this, trimmed_text)
            call assign_pending_autopct(this)
            return
        end if

        if (this%capturing_legend) then
            if (len_trim(trimmed_text) == 0) then
                this%capturing_legend = .false.
                call this%clear_pie_legend_entries()
                return
            end if

            if (.not. is_legend_entry_text(trimmed_text)) then
                this%capturing_legend = .false.
                return
            end if

            call decode_ascii_legend_line(trimmed_text, formatted_line, entry_label)
            if (len_trim(entry_label) > 0 .and. len_trim(formatted_line) > 0) then
                autopct_value = ''
                if (this%pie_autopct_count > 0) then
                    autopct_value = dequeue_pie_autopct(this)
                else
                    autopct_value = this%get_pie_autopct(entry_label)
                end if
                if (len_trim(autopct_value) > 0) then
                    formatted_line = trim(formatted_line) // ' (' // trim(autopct_value) // ')'
                end if
                call append_ascii_legend_line(this, trim(formatted_line))
                call register_legend_entry(this, this%num_legend_lines, entry_label, &
                                            len_trim(autopct_value) > 0)
                call assign_pending_autopct(this)
                return
            end if

            ! Non-legend content encountered; end capture and fall through to regular rendering
            this%capturing_legend = .false.
            call this%clear_pie_legend_entries()
        end if

        if (this%legend_entry_count > 0) then
            if (is_registered_legend_label(this, trimmed_text)) return
        end if

        ! Store text element for later rendering
        if (this%num_text_elements < size(this%text_elements)) then
            this%num_text_elements = this%num_text_elements + 1
            
            call ascii_draw_text_primitive(text_x, text_y, processed_text, &
                                          x, y, text, &
                                          this%x_min, this%x_max, this%y_min, this%y_max, &
                                          this%plot_width, this%plot_height, &
                                          this%current_r, this%current_g, this%current_b)
            
            this%text_elements(this%num_text_elements)%text = processed_text
            this%text_elements(this%num_text_elements)%x = text_x
            this%text_elements(this%num_text_elements)%y = text_y
            this%text_elements(this%num_text_elements)%color_r = this%current_r
            this%text_elements(this%num_text_elements)%color_g = this%current_g
            this%text_elements(this%num_text_elements)%color_b = this%current_b
        end if
    end subroutine ascii_draw_text
    
    subroutine ascii_set_title(this, title)
        !! Explicitly set title for ASCII backend
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: title
        character(len=500) :: processed_title
        integer :: processed_len
        
        ! Process LaTeX commands in title
        call process_latex_in_text(title, processed_title, processed_len)
        this%title_text = processed_title(1:processed_len)
        this%title_set = .true.
    end subroutine ascii_set_title
    
    subroutine ascii_save(this, filename)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        call ascii_finalize(this%canvas, this%text_elements, this%num_text_elements, &
                           this%plot_width, this%plot_height, &
                           this%title_text, this%xlabel_text, this%ylabel_text, &
                           this%legend_lines, this%num_legend_lines, filename)
    end subroutine ascii_save

    subroutine ascii_draw_marker(this, x, y, style)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        
        call draw_ascii_marker(this%canvas, x, y, style, &
                              this%x_min, this%x_max, this%y_min, this%y_max, &
                              this%plot_width, this%plot_height)
    end subroutine ascii_draw_marker

    subroutine ascii_set_marker_colors(this, edge_r, edge_g, edge_b, face_r, face_g, face_b)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b
        real(wp), intent(in) :: face_r, face_g, face_b
        
        ! Suppress unused parameter warnings
        associate(unused_int => this%width, &
                  unused_real => edge_r + edge_g + edge_b + face_r + face_g + face_b); end associate
        
        ! ASCII backend doesn't support separate marker colors
        ! This is a stub implementation for interface compliance
    end subroutine ascii_set_marker_colors

    subroutine ascii_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, edge_alpha, &
                                                  face_r, face_g, face_b, face_alpha)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha
        real(wp), intent(in) :: face_r, face_g, face_b, face_alpha
        
        ! Suppress unused parameter warnings  
        associate(unused_int => this%width, &
                  unused_real => edge_r + edge_g + edge_b + edge_alpha + &
                                face_r + face_g + face_b + face_alpha); end associate
        
        ! ASCII backend doesn't support separate marker colors or transparency
        ! This is a stub implementation for interface compliance
    end subroutine ascii_set_marker_colors_with_alpha
    
    subroutine ascii_fill_heatmap(this, x_grid, y_grid, z_grid, z_min, z_max)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        
        call fill_ascii_heatmap(this%canvas, x_grid, y_grid, z_grid, z_min, z_max, &
                               this%x_min, this%x_max, this%y_min, this%y_max, &
                               this%plot_width, this%plot_height)
    end subroutine ascii_fill_heatmap

    subroutine ascii_draw_arrow(this, x, y, dx, dy, size, style)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        
        call draw_ascii_arrow(this%canvas, x, y, dx, dy, size, style, &
                             this%x_min, this%x_max, this%y_min, this%y_max, &
                             this%width, this%height, &
                             this%has_rendered_arrows, this%uses_vector_arrows, this%has_triangular_arrows)
    end subroutine ascii_draw_arrow

    function ascii_get_output_method(this) result(output)
        !! Get the complete ASCII canvas as a string
        class(ascii_context), intent(in) :: this
        character(len=:), allocatable :: output
        
        output = ascii_get_output(this%canvas, this%width, this%height)
    end function ascii_get_output_method

    function ascii_get_width_scale(this) result(scale)
        !! Get width scaling factor for coordinate transformation
        class(ascii_context), intent(in) :: this
        real(wp) :: scale
        
        ! Calculate scaling from logical to ASCII coordinates
        if (this%plot_width > 0 .and. this%x_max > this%x_min) then
            scale = real(this%plot_width, wp) / (this%x_max - this%x_min)
        else
            scale = 1.0_wp
        end if
    end function ascii_get_width_scale

    function ascii_get_height_scale(this) result(scale)
        !! Get height scaling factor for coordinate transformation  
        class(ascii_context), intent(in) :: this
        real(wp) :: scale
        
        ! Calculate scaling from logical to ASCII coordinates
        if (this%plot_height > 0 .and. this%y_max > this%y_min) then
            scale = real(this%plot_height, wp) / (this%y_max - this%y_min)
        else
            scale = 1.0_wp
        end if
    end function ascii_get_height_scale

    subroutine ascii_fill_quad(this, x_quad, y_quad)
        !! Fill quadrilateral using character mapping based on current color
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        
        call ascii_fill_quad_primitive(this%canvas, x_quad, y_quad, &
                                      this%x_min, this%x_max, this%y_min, this%y_max, &
                                      this%plot_width, this%plot_height, &
                                      this%current_r, this%current_g, this%current_b)
    end subroutine ascii_fill_quad

    subroutine ascii_render_legend_specialized(this, legend, legend_x, legend_y)
        use fortplot_legend, only: legend_t
        class(ascii_context), intent(inout) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y

        integer :: i
        character(len=96) :: line_buffer
        character(len=:), allocatable :: label_text
        character(len=1) :: marker_char

        ! Suppress unused parameters (legend positions are handled outside canvas)
        associate(unused_x => legend_x, unused_y => legend_y); end associate

        call reset_ascii_legend_lines(this)

        if (legend%num_entries <= 0) return

        call append_ascii_legend_line(this, 'Legend:')

        do i = 1, legend%num_entries
            if (allocated(legend%entries(i)%label)) then
                label_text = trim(legend%entries(i)%label)
            else
                label_text = ''
            end if

            if (len_trim(label_text) == 0) then
                write(line_buffer, '("Series ",I0)') i
                label_text = trim(line_buffer)
            end if

            if (allocated(legend%entries(i)%marker)) then
                if (trim(legend%entries(i)%marker) /= 'None' .and. &
                    len_trim(legend%entries(i)%marker) > 0) then
                    marker_char = ascii_marker_char(legend%entries(i)%marker)
                    line_buffer = '  ' // marker_char // ' ' // label_text
                    call append_ascii_legend_line(this, trim(line_buffer))
                    cycle
                end if
            end if

            line_buffer = '  - ' // label_text
            call append_ascii_legend_line(this, trim(line_buffer))
        end do
    end subroutine ascii_render_legend_specialized

    subroutine ascii_calculate_legend_dimensions(this, legend, legend_width, legend_height)
        use fortplot_legend, only: legend_t
        class(ascii_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: legend_width, legend_height
        
        call calculate_ascii_legend_dimensions(legend, this%width, legend_width, legend_height)
    end subroutine ascii_calculate_legend_dimensions

    subroutine ascii_set_legend_border_width(this)
        class(ascii_context), intent(inout) :: this
        
        ! Suppress unused parameter warning
        if (this%width < 0) then
        end if
        
        call set_ascii_legend_border_width()
    end subroutine ascii_set_legend_border_width

    subroutine ascii_calculate_legend_position(this, legend, x, y)
        !! Calculate ASCII-specific legend position using character coordinates
        use fortplot_legend, only: legend_t, LEGEND_UPPER_LEFT, LEGEND_UPPER_RIGHT, LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT
        class(ascii_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: x, y
        real(wp) :: legend_width, legend_height, margin_x, margin_y
        
        ! Get ASCII-specific dimensions
        call this%calculate_legend_dimensions(legend, legend_width, legend_height)
        
        margin_x = 2.0_wp      ! 2 character margin
        margin_y = 1.0_wp      ! 1 line margin
        
        select case (legend%position)
        case (LEGEND_UPPER_LEFT)
            x = margin_x
            y = margin_y
        case (LEGEND_UPPER_RIGHT)
            ! Position legend so its text fits within the canvas
            ! For ASCII, be more conservative to avoid clipping
            x = real(this%width, wp) - legend_width - margin_x - 5.0_wp
            x = max(margin_x, x)  ! But not too far left
            y = margin_y + 2.0_wp  ! Start lower to leave room for multiple entries
        case (LEGEND_LOWER_LEFT)
            x = margin_x
            y = real(this%height, wp) - legend_height - margin_y
        case (LEGEND_LOWER_RIGHT)
            x = real(this%width, wp) - legend_width - margin_x
            y = real(this%height, wp) - legend_height - margin_y
        case default
            ! Default to upper right corner  
            x = real(this%width, wp) - legend_width - margin_x
            y = margin_y
        end select
    end subroutine ascii_calculate_legend_position

    subroutine ascii_extract_rgb_data(this, width, height, rgb_data)
        !! Extract RGB data from ASCII backend (not supported - dummy data)
        class(ascii_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(wp), intent(out) :: rgb_data(width, height, 3)
        
        ! Reference otherwise-unused member without unreachable branch
        associate(unused_w => this%width); end associate
        
        ! ASCII backend doesn't have RGB data for animation - fill with dummy data
        rgb_data = 0.0_wp  ! Black background
    end subroutine ascii_extract_rgb_data

    subroutine ascii_get_png_data(this, width, height, png_data, status)
        !! Get PNG data from ASCII backend (not supported)
        class(ascii_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        ! Reference otherwise-unused parameters without unreachable branches
        associate(unused_w => this%width, unused_pw => width, unused_ph => height); end associate
        
        ! ASCII backend doesn't provide PNG data
        allocate(png_data(0))
        status = -1
    end subroutine ascii_get_png_data

    subroutine ascii_prepare_3d_data(this, plots)
        !! Prepare 3D data for ASCII backend (no-op - ASCII doesn't use 3D data)
        use fortplot_plot_data, only: plot_data_t
        class(ascii_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)
        
        ! Reference otherwise-unused parameters without unreachable branches
        associate(unused_w => this%width, unused_n => size(plots)); end associate
        
        ! ASCII backend doesn't need 3D data preparation - no-op
    end subroutine ascii_prepare_3d_data

    subroutine ascii_render_ylabel(this, ylabel)
        !! Render Y-axis label for ASCII backend (no-op - handled elsewhere)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        
        ! Reference otherwise-unused parameters without unreachable branches
        associate(unused_w => this%width, unused_l => len_trim(ylabel)); end associate
        
        ! ASCII backend handles Y-axis labels differently - no-op
    end subroutine ascii_render_ylabel

    subroutine ascii_draw_axes_and_labels(this, xscale, yscale, symlog_threshold, &
                                         x_min, x_max, y_min, y_max, &
                                         title, xlabel, ylabel, &
                                         z_min, z_max, has_3d_plots)
        !! Draw axes and labels for ASCII backend
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        
        ! Call the module version with all required parameters
        call draw_ascii_axes_and_labels(this%canvas, xscale, yscale, symlog_threshold, &
                                       x_min, x_max, y_min, y_max, &
                                       title, xlabel, ylabel, &
                                       z_min, z_max, has_3d_plots, &
                                       this%current_r, this%current_g, this%current_b, &
                                       this%plot_width, this%plot_height, &
                                       this%title_text, this%xlabel_text, this%ylabel_text, &
                                       this%text_elements, this%num_text_elements)
    end subroutine ascii_draw_axes_and_labels

    subroutine ascii_save_coordinates(this, x_min, x_max, y_min, y_max)
        !! Save current coordinate system
        class(ascii_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max
        
        x_min = this%x_min
        x_max = this%x_max
        if (this%has_stored_y_range) then
            y_min = this%stored_y_min
            y_max = this%stored_y_max
        else
            y_min = this%y_min
            y_max = this%y_max
        end if
    end subroutine ascii_save_coordinates

    subroutine ascii_set_coordinates(this, x_min, x_max, y_min, y_max)
        !! Set coordinate system
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        this%x_min = x_min
        this%x_max = x_max
        this%stored_y_min = y_min
        this%stored_y_max = y_max
        this%has_stored_y_range = .true.
        this%y_min = y_min * ASCII_CHAR_ASPECT
        this%y_max = y_max * ASCII_CHAR_ASPECT
    end subroutine ascii_set_coordinates

    subroutine ascii_render_axes(this, title_text, xlabel_text, ylabel_text)
        !! Render axes for ASCII context (stub implementation)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
        
        ! Reference otherwise-unused members/optionals without unreachable branches
        associate(unused_w => this%width); end associate
        if (present(title_text)) then; associate(unused_lt => len_trim(title_text)); end associate; end if
        if (present(xlabel_text)) then; associate(unused_lx => len_trim(xlabel_text)); end associate; end if
        if (present(ylabel_text)) then; associate(unused_ly => len_trim(ylabel_text)); end associate; end if
        
        ! ASCII axes are rendered as part of draw_axes_and_labels_backend
        ! This is a stub to satisfy the interface
    end subroutine ascii_render_axes

    subroutine reset_ascii_legend_lines(this)
        class(ascii_context), intent(inout) :: this

        if (.not. allocated(this%legend_lines)) then
            allocate(character(len=96) :: this%legend_lines(0))
        end if

        this%num_legend_lines = 0
    end subroutine reset_ascii_legend_lines

    subroutine append_ascii_legend_line(this, raw_line)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: raw_line

        integer :: current_size, new_size
        character(len=96), allocatable :: tmp(:)
        character(len=96) :: line_buffer

        if (.not. allocated(this%legend_lines)) then
            allocate(character(len=96) :: this%legend_lines(0))
        end if

        current_size = size(this%legend_lines)
        if (this%num_legend_lines == current_size) then
            new_size = max(4, max(1, current_size) * 2)
            allocate(tmp(new_size))
            tmp = ' '
            if (this%num_legend_lines > 0) tmp(1:this%num_legend_lines) = this%legend_lines
            call move_alloc(tmp, this%legend_lines)
        end if

        this%num_legend_lines = this%num_legend_lines + 1
        line_buffer = adjustl(raw_line)
        this%legend_lines(this%num_legend_lines) = line_buffer
    end subroutine append_ascii_legend_line

    subroutine ascii_register_pie_legend_entry(this, label, value_text)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: label
        character(len=*), intent(in), optional :: value_text

        integer :: current_size, new_size
        character(len=64), allocatable :: label_tmp(:)
        character(len=32), allocatable :: value_tmp(:)

        if (.not. allocated(this%pie_legend_labels)) then
            allocate(character(len=64) :: this%pie_legend_labels(0))
        end if
        if (.not. allocated(this%pie_legend_values)) then
            allocate(character(len=32) :: this%pie_legend_values(0))
        end if

        current_size = size(this%pie_legend_labels)
        if (this%pie_legend_count == current_size) then
            new_size = max(4, max(1, current_size) * 2)

            allocate(label_tmp(new_size))
            label_tmp = ''
            if (this%pie_legend_count > 0) label_tmp(1:this%pie_legend_count) = this%pie_legend_labels
            call move_alloc(label_tmp, this%pie_legend_labels)

            allocate(value_tmp(new_size))
            value_tmp = ''
            if (this%pie_legend_count > 0) value_tmp(1:this%pie_legend_count) = this%pie_legend_values
            call move_alloc(value_tmp, this%pie_legend_values)
        end if

        this%pie_legend_count = this%pie_legend_count + 1
        this%pie_legend_labels(this%pie_legend_count) = adjustl(trim(label))
        if (present(value_text)) then
            this%pie_legend_values(this%pie_legend_count) = adjustl(trim(value_text))
        else
            this%pie_legend_values(this%pie_legend_count) = ''
        end if
    end subroutine ascii_register_pie_legend_entry

    subroutine ascii_clear_pie_legend_entries(this)
        class(ascii_context), intent(inout) :: this

        if (.not. allocated(this%pie_legend_labels)) then
            allocate(character(len=64) :: this%pie_legend_labels(0))
        else
            if (size(this%pie_legend_labels) > 0) this%pie_legend_labels = ''
        end if

        if (.not. allocated(this%pie_legend_values)) then
            allocate(character(len=32) :: this%pie_legend_values(0))
        else
            if (size(this%pie_legend_values) > 0) this%pie_legend_values = ''
        end if

        this%pie_legend_count = 0
    end subroutine ascii_clear_pie_legend_entries

    function ascii_get_pie_autopct(this, label) result(value)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: label
        character(len=32) :: value

        integer :: i
        character(len=:), allocatable :: normalized

        value = ''
        if (.not. allocated(this%pie_legend_labels)) return
        if (this%pie_legend_count <= 0) return

        normalized = adjustl(trim(label))

        do i = 1, this%pie_legend_count
            if (adjustl(trim(this%pie_legend_labels(i))) == normalized) then
                value = trim(this%pie_legend_values(i))
                this%pie_legend_values(i) = ''
                return
            end if
        end do
    end function ascii_get_pie_autopct

    subroutine enqueue_pie_autopct(this, value)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: value

        integer :: current_size, new_size
        character(len=32), allocatable :: tmp(:)

        if (.not. allocated(this%pie_autopct_queue)) then
            allocate(character(len=32) :: this%pie_autopct_queue(0))
        end if

        current_size = size(this%pie_autopct_queue)
        if (this%pie_autopct_count == current_size) then
            new_size = max(4, max(1, current_size) * 2)
            allocate(tmp(new_size))
            tmp = ''
            if (this%pie_autopct_count > 0) tmp(1:this%pie_autopct_count) = this%pie_autopct_queue
            call move_alloc(tmp, this%pie_autopct_queue)
        end if

        this%pie_autopct_count = this%pie_autopct_count + 1
        this%pie_autopct_queue(this%pie_autopct_count) = adjustl(trim(value))
    end subroutine enqueue_pie_autopct

    function dequeue_pie_autopct(this) result(value)
        class(ascii_context), intent(inout) :: this
        character(len=32) :: value

        integer :: idx

        if (.not. allocated(this%pie_autopct_queue)) then
            allocate(character(len=32) :: this%pie_autopct_queue(0))
        end if

        if (this%pie_autopct_count <= 0) then
            value = ''
            return
        end if

        value = this%pie_autopct_queue(1)
        if (this%pie_autopct_count > 1) then
            do idx = 1, this%pie_autopct_count - 1
                this%pie_autopct_queue(idx) = this%pie_autopct_queue(idx + 1)
            end do
        end if
        this%pie_autopct_queue(this%pie_autopct_count) = ''
        this%pie_autopct_count = this%pie_autopct_count - 1
    end function dequeue_pie_autopct

    subroutine register_legend_entry(this, line_idx, label, has_autopct)
        class(ascii_context), intent(inout) :: this
        integer, intent(in) :: line_idx
        character(len=*), intent(in) :: label
        logical, intent(in) :: has_autopct

        integer :: current_size, new_size
        integer, allocatable :: idx_tmp(:)
        logical, allocatable :: flag_tmp(:)
        character(len=64), allocatable :: label_tmp(:)

        if (.not. allocated(this%legend_entry_indices)) then
            allocate(this%legend_entry_indices(0))
            allocate(this%legend_entry_has_autopct(0))
            allocate(this%legend_entry_labels(0))
        end if

        current_size = size(this%legend_entry_indices)
        if (this%legend_entry_count == current_size) then
            new_size = max(4, max(1, current_size) * 2)
            allocate(idx_tmp(new_size))
            idx_tmp = 0
            if (this%legend_entry_count > 0) then
                idx_tmp(1:this%legend_entry_count) = this%legend_entry_indices
            end if
            call move_alloc(idx_tmp, this%legend_entry_indices)

            allocate(flag_tmp(new_size))
            flag_tmp = .false.
            if (this%legend_entry_count > 0) then
                flag_tmp(1:this%legend_entry_count) = this%legend_entry_has_autopct
            end if
            call move_alloc(flag_tmp, this%legend_entry_has_autopct)

            allocate(label_tmp(new_size))
            label_tmp = ''
            if (this%legend_entry_count > 0) then
                label_tmp(1:this%legend_entry_count) = this%legend_entry_labels
            end if
            call move_alloc(label_tmp, this%legend_entry_labels)
        end if

        this%legend_entry_count = this%legend_entry_count + 1
        this%legend_entry_indices(this%legend_entry_count) = line_idx
        this%legend_entry_has_autopct(this%legend_entry_count) = has_autopct
        this%legend_entry_labels(this%legend_entry_count) = adjustl(trim(label))

        if (has_autopct) then
            if (this%legend_autopct_cursor == this%legend_entry_count) then
                this%legend_autopct_cursor = this%legend_autopct_cursor + 1
            end if
        end if
    end subroutine register_legend_entry

    subroutine assign_pending_autopct(this)
        class(ascii_context), intent(inout) :: this

        character(len=32) :: value
        integer :: target_idx

        do while (this%pie_autopct_count > 0 .and. this%legend_autopct_cursor <= this%legend_entry_count)
            if (this%legend_entry_has_autopct(this%legend_autopct_cursor)) then
                this%legend_autopct_cursor = this%legend_autopct_cursor + 1
                cycle
            end if

            value = dequeue_pie_autopct(this)
            target_idx = this%legend_entry_indices(this%legend_autopct_cursor)
            call append_autopct_to_line(this, target_idx, value)
            this%legend_entry_has_autopct(this%legend_autopct_cursor) = .true.
            this%legend_autopct_cursor = this%legend_autopct_cursor + 1
        end do
    end subroutine assign_pending_autopct

    subroutine append_autopct_to_line(this, line_idx, value)
        class(ascii_context), intent(inout) :: this
        integer, intent(in) :: line_idx
        character(len=*), intent(in) :: value

        character(len=96) :: updated_line

        if (len_trim(value) == 0) return
        if (line_idx < 1 .or. line_idx > this%num_legend_lines) return

        updated_line = trim(this%legend_lines(line_idx))
        if (index(updated_line, '(') > 0) then
            this%legend_lines(line_idx) = updated_line
        else
            updated_line = trim(updated_line) // ' (' // trim(value) // ')'
            this%legend_lines(line_idx) = adjustl(updated_line)
        end if
    end subroutine append_autopct_to_line

    subroutine ascii_clear_legend_lines(this, header)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in), optional :: header

        call reset_ascii_legend_lines(this)

        if (present(header)) then
            if (len_trim(header) > 0) then
                call append_ascii_legend_line(this, trim(header))
            end if
        end if

        this%capturing_legend = .false.
        this%pie_autopct_count = 0
        if (allocated(this%pie_autopct_queue)) then
            if (size(this%pie_autopct_queue) > 0) this%pie_autopct_queue = ''
        end if
        this%legend_entry_count = 0
        this%legend_autopct_cursor = 1
        if (allocated(this%legend_entry_has_autopct)) then
            if (size(this%legend_entry_has_autopct) > 0) then
                this%legend_entry_has_autopct = .false.
            end if
        end if

    end subroutine ascii_clear_legend_lines

    subroutine decode_ascii_legend_line(raw_text, formatted_line, entry_label)
        character(len=*), intent(in) :: raw_text
        character(len=96), intent(out) :: formatted_line
        character(len=64), intent(out) :: entry_label

        character(len=:), allocatable :: trimmed_text
        integer :: first_space

        formatted_line = ''
        entry_label = ''

        trimmed_text = trim(adjustl(raw_text))
        if (len_trim(trimmed_text) == 0) return

        if (len(trimmed_text) >= 3 .and. trimmed_text(1:3) == '-- ') then
            if (len(trimmed_text) > 3) then
                entry_label = trim(adjustl(trimmed_text(4:)))
            else
                entry_label = ''
            end if
            formatted_line = '  - ' // trim(entry_label)
        else
            formatted_line = '  ' // trim(trimmed_text)
            first_space = index(trimmed_text, ' ')
            if (first_space > 0 .and. first_space < len(trimmed_text)) then
                entry_label = trim(adjustl(trimmed_text(first_space + 1:)))
            else
                entry_label = trim(trimmed_text)
            end if
        end if
    end subroutine decode_ascii_legend_line

    subroutine ascii_add_legend_entry(this, label, value_text)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: label
        character(len=*), intent(in), optional :: value_text

        character(len=96) :: line_buffer
        character(len=:), allocatable :: value_trimmed

        line_buffer = '  ' // trim(label)

        if (present(value_text)) then
            value_trimmed = trim(value_text)
            if (len_trim(value_trimmed) > 0) then
                line_buffer = trim(line_buffer) // ' (' // value_trimmed // ')'
            end if
        end if

        call append_ascii_legend_line(this, trim(line_buffer))
    end subroutine ascii_add_legend_entry

    pure logical function is_legend_entry_text(text) result(is_entry)
        character(len=*), intent(in) :: text

        character(len=:), allocatable :: trimmed_text
        character(len=1) :: first_char

        trimmed_text = trim(adjustl(text))
        if (len_trim(trimmed_text) == 0) then
            is_entry = .false.
            return
        end if

        if (len(trimmed_text) >= 3) then
            if (trimmed_text(1:3) == '-- ') then
                is_entry = .true.
                return
            end if
        end if

        first_char = trimmed_text(1:1)
        if (index('o#%x+*^v<>PH', first_char) > 0) then
            if (len(trimmed_text) >= 2) then
                if (trimmed_text(2:2) == ' ') then
                    is_entry = .true.
                    return
                end if
            end if
        end if

        is_entry = .false.
    end function is_legend_entry_text

    pure logical function is_registered_legend_label(this, text) result(found)
        class(ascii_context), intent(in) :: this
        character(len=*), intent(in) :: text

        character(len=:), allocatable :: trimmed
        integer :: i

        trimmed = trim(adjustl(text))
        if (len_trim(trimmed) == 0) then
            found = .false.
            return
        end if

        if (.not. allocated(this%legend_entry_labels)) then
            found = .false.
            return
        end if

        do i = 1, this%legend_entry_count
            if (adjustl(trim(this%legend_entry_labels(i))) == trimmed) then
                found = .true.
                return
            end if
        end do

        found = .false.
    end function is_registered_legend_label

    pure logical function is_autopct_text(text) result(is_percent)
        character(len=*), intent(in) :: text

        character(len=:), allocatable :: trimmed
        integer :: idx, last
        character(len=1) :: ch

        trimmed = trim(text)
        last = len_trim(trimmed)
        if (last <= 1) then
            is_percent = .false.
            return
        end if

        if (trimmed(last:last) /= '%') then
            is_percent = .false.
            return
        end if

        is_percent = .true.
        do idx = 1, last - 1
            ch = trimmed(idx:idx)
            select case (ch)
            case ('0':'9', '.', '+', '-', ' ')
                cycle
            case default
                is_percent = .false.
                return
            end select
        end do
    end function is_autopct_text

    pure function ascii_marker_char(marker_style) result(marker_char)
        character(len=*), intent(in) :: marker_style
        character(len=1) :: marker_char

        select case (trim(marker_style))
        case ('o')
            marker_char = 'o'
        case ('s')
            marker_char = '#'
        case ('D', 'd')
            marker_char = '%'
        case ('x')
            marker_char = 'x'
        case ('+')
            marker_char = '+'
        case ('*')
            marker_char = '*'
        case ('^')
            marker_char = '^'
        case ('v')
            marker_char = 'v'
        case ('<')
            marker_char = '<'
        case ('>')
            marker_char = '>'
        case ('p')
            marker_char = 'P'
        case ('h', 'H')
            marker_char = 'H'
        case default
            marker_char = '*'
        end select
    end function ascii_marker_char

end module fortplot_ascii
