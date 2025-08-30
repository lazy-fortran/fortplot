module fortplot_ascii
    !! ASCII terminal plotting backend
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
    use fortplot_ascii_elements, only: extract_ascii_rgb_data, get_ascii_png_data, prepare_ascii_3d_data
    use fortplot_ascii_elements, only: render_ascii_ylabel, draw_ascii_axes_and_labels, render_ascii_axes
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: ascii_context, create_ascii_canvas

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
    contains
        procedure :: line => ascii_draw_line
        procedure :: color => ascii_set_color
        procedure :: text => ascii_draw_text
        procedure :: set_line_width => ascii_set_line_width
        procedure :: set_line_style => ascii_set_line_style
        procedure :: save => ascii_finalize
        procedure :: set_title => ascii_set_title
        procedure :: draw_marker => ascii_draw_marker
        procedure :: set_marker_colors => ascii_set_marker_colors
        procedure :: set_marker_colors_with_alpha => ascii_set_marker_colors_with_alpha
        procedure :: fill_heatmap => ascii_fill_heatmap
        procedure :: draw_arrow => ascii_draw_arrow
        procedure :: get_ascii_output => ascii_get_output
        
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
    end type ascii_context
    
    character(len=*), parameter :: DENSITY_CHARS = ' ░▒▓█'
    character(len=*), parameter :: BOX_CHARS = '-|+++++++'
    
    ! Color filtering thresholds
    real(wp), parameter :: LIGHT_COLOR_THRESHOLD = 0.8_wp
    real(wp), parameter :: MEDIUM_COLOR_THRESHOLD = 0.7_wp
    
contains

    function create_ascii_canvas(width, height) result(ctx)
        integer, intent(in), optional :: width, height
        type(ascii_context) :: ctx
        integer :: w, h
        
        associate(unused_w => width, unused_h => height); end associate
        w = 80; h = 30
        call setup_canvas(ctx, w, h)
        ctx%plot_width = w; ctx%plot_height = h
        allocate(ctx%canvas(h, w)); ctx%canvas = ' '
        allocate(ctx%text_elements(20)); ctx%num_text_elements = 0; ctx%title_set = .false.
        ctx%current_r = 0.0_wp; ctx%current_g = 0.0_wp; ctx%current_b = 1.0_wp
    end function create_ascii_canvas
    
    subroutine ascii_draw_line(this, x1, y1, x2, y2)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        
        real(wp) :: dx, dy, length, step_x, step_y, x, y
        integer :: steps, i, px, py
        character(len=1) :: line_char
        real(wp) :: luminance
        
        luminance = 0.299_wp * this%current_r + 0.587_wp * this%current_g + 0.114_wp * this%current_b
        if (luminance > 0.9_wp) then; line_char = ':'
        else if (this%current_g > 0.7_wp) then; line_char = '@'
        else if (this%current_g > 0.3_wp) then; line_char = '#'
        else if (this%current_b > 0.7_wp) then; line_char = '*'
        else if (this%current_b > 0.3_wp) then; line_char = 'o'
        else if (this%current_r > 0.7_wp) then; line_char = '%'
        else if (this%current_r > 0.3_wp) then; line_char = '+'
        else; line_char = '.'; end if
        dx = x2 - x1; dy = y2 - y1; length = sqrt(dx*dx + dy*dy)
        if (length < 1e-6_wp) return
        steps = max(int(length * 4), max(abs(int(dx)), abs(int(dy)))) + 1
        step_x = dx / real(steps, wp); step_y = dy / real(steps, wp)
        x = x1; y = y1
        
        do i = 0, steps
            px = int((x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_width - 3, wp)) + 2
            py = (this%plot_height - 1) - int((y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_height - 3, wp))
            if (px >= 2 .and. px <= this%plot_width - 1 .and. py >= 2 .and. py <= this%plot_height - 1) then
                if (this%canvas(py, px) == ' ') then; this%canvas(py, px) = line_char
                else if (this%canvas(py, px) /= line_char) then; this%canvas(py, px) = get_blend_char(this%canvas(py, px), line_char); end if
            end if
            x = x + step_x; y = y + step_y
        end do
    end subroutine ascii_draw_line
    
    subroutine ascii_set_color(this, r, g, b)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: r, g, b
        this%current_r = r; this%current_g = g; this%current_b = b
    end subroutine ascii_set_color
    
    subroutine ascii_set_line_width(this, width)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: width
        associate(unused_int => this%width, unused_real => width); end associate
    end subroutine ascii_set_line_width
    
    subroutine ascii_set_line_style(this, style)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: style
        associate(unused_int => this%width, unused_style => style); end associate
    end subroutine ascii_set_line_style
    
    subroutine ascii_draw_text(this, x, y, text)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        integer :: text_x, text_y
        character(len=500) :: processed_text
        integer :: processed_len
        
        call process_latex_in_text(text, processed_text, processed_len)
        if (this%num_text_elements < size(this%text_elements)) then
            this%num_text_elements = this%num_text_elements + 1
            if (x >= 1.0_wp .and. x <= real(this%plot_width, wp) .and. &
                y >= 1.0_wp .and. y <= real(this%plot_height, wp)) then
                text_x = nint(x); text_y = nint(y)
            else
                text_x = nint((x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_width, wp))
                text_y = nint((this%y_max - y) / (this%y_max - this%y_min) * real(this%plot_height, wp))
            end if
            if (x >= 1.0_wp .and. x <= real(this%plot_width, wp) .and. &
                y >= 1.0_wp .and. y <= real(this%plot_height, wp)) then
                text_x = max(1, min(text_x, this%plot_width))
            else; text_x = max(1, min(text_x, this%plot_width - processed_len)); end if
            text_y = max(1, min(text_y, this%plot_height))
            this%text_elements(this%num_text_elements)%text = processed_text(1:processed_len)
            this%text_elements(this%num_text_elements)%x = text_x; this%text_elements(this%num_text_elements)%y = text_y
            this%text_elements(this%num_text_elements)%color_r = this%current_r
            this%text_elements(this%num_text_elements)%color_g = this%current_g
            this%text_elements(this%num_text_elements)%color_b = this%current_b
        end if
    end subroutine ascii_draw_text
    
    subroutine ascii_set_title(this, title)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: title
        character(len=500) :: processed_title
        integer :: processed_len
        call process_latex_in_text(title, processed_title, processed_len)
        this%title_text = processed_title(1:processed_len); this%title_set = .true.
    end subroutine ascii_set_title
    
    subroutine ascii_finalize(this, filename)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        integer :: unit, ios
        character(len=512) :: error_msg
        
        if (len_trim(filename) == 0 .or. trim(filename) == "terminal") then
            call output_to_terminal(this)
        else
            open(newunit=unit, file=filename, status='replace', iostat=ios, iomsg=error_msg)
            
            if (ios /= 0) then
                call log_error("Cannot save ASCII file '" // trim(filename) // "': " // trim(error_msg))
                ! Fall back to terminal output
                call log_info("Falling back to terminal output due to file error")
                call output_to_terminal(this)
                return
            end if
            
            call output_to_file(this, unit)
            close(unit)
            call log_info("Unicode plot saved to '" // trim(filename) // "'")
        end if
    end subroutine ascii_finalize
    
    subroutine output_to_terminal(this)
        class(ascii_context), intent(inout) :: this
        integer :: i, j
        
        call render_text_elements_to_canvas(this%canvas, this%text_elements, &
                                            this%num_text_elements, this%plot_width, this%plot_height)
        if (allocated(this%title_text)) then
            print '(A)', ''; call print_centered_title(this%title_text, this%plot_width)
        end if
        print '(A)', '+' // repeat('-', this%plot_width) // '+'
        do i = 1, this%plot_height
            write(*, '(A)', advance='no') '|'
            do j = 1, this%plot_width; write(*, '(A)', advance='no') this%canvas(i, j); end do
            print '(A)', '|'
        end do
        print '(A)', '+' // repeat('-', this%plot_width) // '+'
        if (allocated(this%xlabel_text)) call print_centered_title(this%xlabel_text, this%plot_width)
        if (allocated(this%ylabel_text)) print '(A)', this%ylabel_text
    end subroutine output_to_terminal
    
    subroutine output_to_file(this, unit)
        class(ascii_context), intent(inout) :: this
        integer, intent(in) :: unit
        integer :: i, j
        
        call render_text_elements_to_canvas(this%canvas, this%text_elements, &
                                            this%num_text_elements, this%plot_width, this%plot_height)
        if (allocated(this%title_text)) then
            write(unit, '(A)') ''; call write_centered_title(unit, this%title_text, this%plot_width)
        end if
        write(unit, '(A)') '+' // repeat('-', this%plot_width) // '+'
        do i = 1, this%plot_height
            write(unit, '(A)', advance='no') '|'
            do j = 1, this%plot_width; write(unit, '(A)', advance='no') this%canvas(i, j); end do
            write(unit, '(A)') '|'
        end do
        write(unit, '(A)') '+' // repeat('-', this%plot_width) // '+'
        if (allocated(this%xlabel_text)) call write_centered_title(unit, this%xlabel_text, this%plot_width)
        if (allocated(this%ylabel_text)) write(unit, '(A)') this%ylabel_text
    end subroutine output_to_file


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
        real(wp), intent(in) :: edge_r, edge_g, edge_b, face_r, face_g, face_b
        associate(unused_int => this%width, &
                  unused_real => edge_r + edge_g + edge_b + face_r + face_g + face_b); end associate
    end subroutine ascii_set_marker_colors

    subroutine ascii_set_marker_colors_with_alpha(this, edge_r, edge_g, edge_b, edge_alpha, &
                                                  face_r, face_g, face_b, face_alpha)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: edge_r, edge_g, edge_b, edge_alpha, face_r, face_g, face_b, face_alpha
        associate(unused_int => this%width, &
                  unused_real => edge_r + edge_g + edge_b + edge_alpha + face_r + face_g + face_b + face_alpha); end associate
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

    function ascii_get_output(this) result(output)
        class(ascii_context), intent(in) :: this
        character(len=:), allocatable :: output
        character(len=1000) :: line_buffer
        integer :: i, j, total_len, line_len
        total_len = this%height * (this%width + 1)
        allocate(character(len=total_len) :: output); output = ""
        do i = 1, this%height
            line_buffer = ""
            do j = 1, this%width; line_buffer(j:j) = this%canvas(i, j); end do
            line_len = len_trim(line_buffer(:this%width)); if (line_len == 0) line_len = 1
            output = output // line_buffer(1:line_len) // new_line('a')
        end do
    end function ascii_get_output

    function ascii_get_width_scale(this) result(scale)
        class(ascii_context), intent(in) :: this
        real(wp) :: scale
        if (this%plot_width > 0 .and. this%x_max > this%x_min) then
            scale = real(this%plot_width, wp) / (this%x_max - this%x_min)
        else; scale = 1.0_wp; end if
    end function ascii_get_width_scale

    function ascii_get_height_scale(this) result(scale)
        class(ascii_context), intent(in) :: this
        real(wp) :: scale
        if (this%plot_height > 0 .and. this%y_max > this%y_min) then
            scale = real(this%plot_height, wp) / (this%y_max - this%y_min)
        else; scale = 1.0_wp; end if
    end function ascii_get_height_scale

    subroutine ascii_fill_quad(this, x_quad, y_quad)
        !! Fill quadrilateral using character mapping based on current color
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        
        integer :: px(4), py(4), i, j, min_x, max_x, min_y, max_y
        character(len=1) :: fill_char
        real(wp) :: color_intensity
        integer :: char_index
        
        do i = 1, 4
            px(i) = int((x_quad(i) - this%x_min) / &
                (this%x_max - this%x_min) * real(this%plot_width - 3, wp)) + 2
            py(i) = (this%plot_height - 1) - int((y_quad(i) - this%y_min) / &
                (this%y_max - this%y_min) * real(this%plot_height - 3, wp))
        end do
        color_intensity = 0.299_wp * this%current_r + 0.587_wp * this%current_g + 0.114_wp * this%current_b
        if (color_intensity <= 0.001_wp) then; char_index = 1
        else; char_index = min(len(ASCII_CHARS), max(1, int(color_intensity * len(ASCII_CHARS)) + 1)); end if
        fill_char = ASCII_CHARS(char_index:char_index)
        min_x = max(2, min(minval(px), this%plot_width - 1)); max_x = max(2, min(maxval(px), this%plot_width - 1))
        min_y = max(2, min(minval(py), this%plot_height - 1)); max_y = max(2, min(maxval(py), this%plot_height - 1))
        do j = min_y, max_y
            do i = min_x, max_x
                if (this%canvas(j, i) == ' ' .or. get_char_density(fill_char) > get_char_density(this%canvas(j, i))) &
                    this%canvas(j, i) = fill_char
            end do
        end do
    end subroutine ascii_fill_quad

    subroutine ascii_render_legend_specialized(this, legend, legend_x, legend_y)
        use fortplot_legend, only: legend_t
        class(ascii_context), intent(inout) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y
        
        call render_ascii_legend_specialized(legend, this, legend_x, legend_y)
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
        if (this%width < 0) then; end if
        call set_ascii_legend_border_width()
    end subroutine ascii_set_legend_border_width

    subroutine ascii_calculate_legend_position(this, legend, x, y)
        use fortplot_legend, only: legend_t, LEGEND_UPPER_LEFT, LEGEND_UPPER_RIGHT, LEGEND_LOWER_LEFT, LEGEND_LOWER_RIGHT
        class(ascii_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: x, y
        real(wp) :: legend_width, legend_height, margin_x, margin_y
        call this%calculate_legend_dimensions(legend, legend_width, legend_height)
        margin_x = 2.0_wp; margin_y = 1.0_wp
        select case (legend%position)
        case (LEGEND_UPPER_LEFT); x = margin_x; y = margin_y
        case (LEGEND_UPPER_RIGHT)
            x = real(this%width, wp) - legend_width - margin_x - 5.0_wp; x = max(margin_x, x)
            y = margin_y + 2.0_wp
        case (LEGEND_LOWER_LEFT); x = margin_x; y = real(this%height, wp) - legend_height - margin_y
        case (LEGEND_LOWER_RIGHT); x = real(this%width, wp) - legend_width - margin_x; y = real(this%height, wp) - legend_height - margin_y
        case default; x = real(this%width, wp) - legend_width - margin_x; y = margin_y
        end select
    end subroutine ascii_calculate_legend_position

    subroutine ascii_extract_rgb_data(this, width, height, rgb_data)
        use, intrinsic :: iso_fortran_env, only: real64
        class(ascii_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(real64), intent(out) :: rgb_data(width, height, 3)
        if (this%width < 0) then; end if
        rgb_data = 0.0_real64
    end subroutine ascii_extract_rgb_data

    subroutine ascii_get_png_data(this, width, height, png_data, status)
        class(ascii_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        if (this%width < 0 .or. width < 0 .or. height < 0) then; end if
        allocate(png_data(0)); status = -1
    end subroutine ascii_get_png_data

    subroutine ascii_prepare_3d_data(this, plots)
        use fortplot_plot_data, only: plot_data_t
        class(ascii_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)
        if (this%width < 0 .or. size(plots) < 0) then; end if
    end subroutine ascii_prepare_3d_data

    subroutine ascii_render_ylabel(this, ylabel)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        if (this%width < 0 .or. len_trim(ylabel) < 0) then; end if
    end subroutine ascii_render_ylabel

    subroutine ascii_draw_axes_and_labels(this, xscale, yscale, symlog_threshold, &
                                         x_min, x_max, y_min, y_max, &
                                         title, xlabel, ylabel, &
                                         z_min, z_max, has_3d_plots)
        !! Draw axes and labels for ASCII backend
        use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: xscale, yscale
        real(wp), intent(in) :: symlog_threshold
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        character(len=:), allocatable, intent(in), optional :: title, xlabel, ylabel
        real(wp), intent(in), optional :: z_min, z_max
        logical, intent(in) :: has_3d_plots
        
        real(wp) :: x_tick_positions(MAX_TICKS), y_tick_positions(MAX_TICKS)
        integer :: num_x_ticks, num_y_ticks, i
        character(len=50) :: tick_label
        real(wp) :: tick_x, tick_y
        
        if (present(z_min) .and. present(z_max)) then
            if (z_min < -huge(z_min) .or. z_max > huge(z_max)) then; end if
        end if
        if (.not. has_3d_plots) then; end if
        if (present(title)) then
            if (allocated(title)) call this%set_title(title)
        end if
        call this%line(x_min, y_min, x_max, y_min); call this%line(x_min, y_min, x_min, y_max)
        call compute_scale_ticks(xscale, x_min, x_max, symlog_threshold, x_tick_positions, num_x_ticks)
        do i = 1, num_x_ticks
            tick_x = x_tick_positions(i); tick_label = format_tick_label(tick_x, xscale)
            call this%text(tick_x, y_min - 0.05_wp * (y_max - y_min), trim(tick_label))
        end do
        call compute_scale_ticks(yscale, y_min, y_max, symlog_threshold, y_tick_positions, num_y_ticks)
        do i = 1, num_y_ticks
            tick_y = y_tick_positions(i); tick_label = format_tick_label(tick_y, yscale)
            call this%text(x_min - 0.1_wp * (x_max - x_min), tick_y, trim(tick_label))
        end do
        if (present(xlabel)) then
            if (allocated(xlabel)) this%xlabel_text = xlabel
        end if
        if (present(ylabel)) then
            if (allocated(ylabel)) this%ylabel_text = ylabel
        end if
    end subroutine ascii_draw_axes_and_labels

    subroutine ascii_save_coordinates(this, x_min, x_max, y_min, y_max)
        class(ascii_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max
        x_min = this%x_min; x_max = this%x_max; y_min = this%y_min; y_max = this%y_max
    end subroutine ascii_save_coordinates

    subroutine ascii_set_coordinates(this, x_min, x_max, y_min, y_max)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        this%x_min = x_min; this%x_max = x_max; this%y_min = y_min; this%y_max = y_max
    end subroutine ascii_set_coordinates

    subroutine ascii_render_axes(this, title_text, xlabel_text, ylabel_text)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in), optional :: title_text, xlabel_text, ylabel_text
        if (this%width < 0) then; end if
        if (present(title_text) .and. present(xlabel_text) .and. present(ylabel_text)) then
            if (len_trim(title_text) < 0 .or. len_trim(xlabel_text) < 0 .or. len_trim(ylabel_text) < 0) then; end if
        end if
    end subroutine ascii_render_axes

end module fortplot_ascii