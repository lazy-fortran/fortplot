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
    ! use fortplot_unicode, only: unicode_to_ascii
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: ascii_context, create_ascii_canvas
    type :: text_element_t
        character(len=:), allocatable :: text
        integer :: x, y
        real(wp) :: color_r, color_g, color_b
    end type text_element_t

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
    end type ascii_context
    
    ! ASCII plotting constants
    character(len=*), parameter :: ASCII_CHARS = ' .:-=+*#%@'
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
        
        ! Suppress unused parameter warnings
        associate(unused_w => width, unused_h => height); end associate
        
        ! ASCII backend uses 4:3 aspect ratio accounting for terminal character dimensions
        ! Terminal chars are ~1.5x taller than wide, so for 4:3 visual ratio:
        ! 80 chars wide × (3/4) × (1/1.5) = 80 × 0.75 × 0.67 ≈ 40 → 30 chars high
        w = 80
        h = 30
        
        call setup_canvas(ctx, w, h)
        
        ctx%plot_width = w
        ctx%plot_height = h
        
        allocate(ctx%canvas(h, w))
        ctx%canvas = ' '
        
        ! Initialize text elements storage (start with capacity for 20 text elements)
        allocate(ctx%text_elements(20))
        ctx%num_text_elements = 0
        ctx%title_set = .false.
        
        ctx%current_r = 0.0_wp
        ctx%current_g = 0.0_wp 
        ctx%current_b = 1.0_wp
    end function create_ascii_canvas
    
    subroutine ascii_draw_line(this, x1, y1, x2, y2)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x1, y1, x2, y2
        
        real(wp) :: dx, dy, length, step_x, step_y, x, y
        integer :: steps, i, px, py
        character(len=1) :: line_char
        
        
        if (this%current_r > 0.8_wp .and. this%current_g > 0.8_wp .and. this%current_b > 0.8_wp) then
            return
        end if
        
        if (this%current_g > 0.7_wp) then
            line_char = '@'
        else if (this%current_g > 0.3_wp) then
            line_char = '#'
        else if (this%current_b > 0.7_wp) then
            line_char = '*'
        else if (this%current_b > 0.3_wp) then
            line_char = 'o'
        else if (this%current_r > 0.7_wp) then
            line_char = '%'
        else if (this%current_r > 0.3_wp) then
            line_char = '+'
        else
            line_char = '.'
        end if
        
        dx = x2 - x1
        dy = y2 - y1
        length = sqrt(dx*dx + dy*dy)
        
        if (length < 1e-6_wp) return
        
        steps = max(int(length * 4), max(abs(int(dx)), abs(int(dy)))) + 1
        step_x = dx / real(steps, wp)
        step_y = dy / real(steps, wp)
        
        x = x1
        y = y1
        
        do i = 0, steps
            ! Map to usable plot area (excluding 1-char border on each side)
            px = int((x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_width - 3, wp)) + 2
            py = (this%plot_height - 1) - int((y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_height - 3, wp))
            
            
            if (px >= 2 .and. px <= this%plot_width - 1 .and. py >= 2 .and. py <= this%plot_height - 1) then
                if (this%canvas(py, px) == ' ') then
                    this%canvas(py, px) = line_char
                else if (this%canvas(py, px) /= line_char) then
                    this%canvas(py, px) = get_blend_char(this%canvas(py, px), line_char)
                end if
            end if
            
            x = x + step_x
            y = y + step_y
        end do
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
    
    subroutine ascii_draw_text(this, x, y, text)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        integer :: text_x, text_y
        character(len=500) :: processed_text
        integer :: processed_len
        
        ! Process LaTeX commands to Unicode
        call process_latex_in_text(text, processed_text, processed_len)
        
        ! Store text element for later rendering
        if (this%num_text_elements < size(this%text_elements)) then
            this%num_text_elements = this%num_text_elements + 1
            
            ! Convert coordinates - check if already in screen coordinates
            if (x >= 1.0_wp .and. x <= real(this%plot_width, wp) .and. &
                y >= 1.0_wp .and. y <= real(this%plot_height, wp)) then
                ! Already in screen coordinates (e.g., from legend)
                text_x = nint(x)
                text_y = nint(y)
            else
                ! Convert from data coordinates to canvas coordinates
                text_x = nint((x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_width, wp))
                text_y = nint((this%y_max - y) / (this%y_max - this%y_min) * real(this%plot_height, wp))
            end if
            
            ! Clamp to canvas bounds
            ! For legend text (already in screen coordinates), don't truncate based on length
            if (x >= 1.0_wp .and. x <= real(this%plot_width, wp) .and. &
                y >= 1.0_wp .and. y <= real(this%plot_height, wp)) then
                ! For legend text, only clamp starting position, let text extend as needed
                text_x = max(1, min(text_x, this%plot_width))
            else
                ! For other text, prevent overflow
                text_x = max(1, min(text_x, this%plot_width - processed_len))
            end if
            text_y = max(1, min(text_y, this%plot_height))
            
            this%text_elements(this%num_text_elements)%text = processed_text(1:processed_len)
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
        
        ! Render text elements to canvas before output
        call render_text_elements_to_canvas(this)
        
        if (allocated(this%title_text)) then
            print '(A)', ''  ! Empty line before title
            call print_centered_title(this%title_text, this%plot_width)
        end if
        
        print '(A)', '+' // repeat('-', this%plot_width) // '+'
        do i = 1, this%plot_height
            write(*, '(A)', advance='no') '|'
            do j = 1, this%plot_width
                write(*, '(A)', advance='no') this%canvas(i, j)
            end do
            print '(A)', '|'
        end do
        
        print '(A)', '+' // repeat('-', this%plot_width) // '+'
        
        ! Print xlabel below the plot if present
        if (allocated(this%xlabel_text)) then
            call print_centered_title(this%xlabel_text, this%plot_width)
        end if
    end subroutine output_to_terminal
    
    subroutine output_to_file(this, unit)
        class(ascii_context), intent(inout) :: this
        integer, intent(in) :: unit
        integer :: i, j
        
        ! Render text elements to canvas before output
        call render_text_elements_to_canvas(this)
        
        if (allocated(this%title_text)) then
            write(unit, '(A)') ''  ! Empty line before title
            call write_centered_title(unit, this%title_text, this%plot_width)
        end if
        
        write(unit, '(A)') '+' // repeat('-', this%plot_width) // '+'
        do i = 1, this%plot_height
            write(unit, '(A)', advance='no') '|'
            do j = 1, this%plot_width
                write(unit, '(A)', advance='no') this%canvas(i, j)
            end do
            write(unit, '(A)') '|'
        end do
        
        write(unit, '(A)') '+' // repeat('-', this%plot_width) // '+'
        
        ! Write xlabel below the plot if present
        if (allocated(this%xlabel_text)) then
            call write_centered_title(unit, this%xlabel_text, this%plot_width)
        end if
    end subroutine output_to_file

    integer function get_char_density(char)
        character(len=1), intent(in) :: char
        
        select case (char)
        case (' ')
            get_char_density = 0
        case ('.')
            get_char_density = 1
        case (':')
            get_char_density = 2
        case ('-')
            get_char_density = 2
        case ('=')
            get_char_density = 3
        case ('+')
            get_char_density = 3
        case ('o')
            get_char_density = 4
        case ('*')
            get_char_density = 5
        case ('#')
            get_char_density = 6
        case ('%')
            get_char_density = 7
        case ('@')
            get_char_density = 8
        case default
            get_char_density = 9
        end select
    end function get_char_density

    character(len=1) function get_blend_char(char1, char2)
        character(len=1), intent(in) :: char1, char2
        
        select case (char1)
        case ('*')
            if (char2 == '#' .or. char2 == '@') then
                get_blend_char = '%'
            else
                get_blend_char = char1
            end if
        case ('#')
            if (char2 == '*' .or. char2 == 'o') then
                get_blend_char = '%'
            else
                get_blend_char = char1
            end if
        case ('@')
            if (char2 == '*' .or. char2 == 'o') then
                get_blend_char = '%'
            else
                get_blend_char = char1
            end if
        case default
            if (get_char_density(char2) > get_char_density(char1)) then
                get_blend_char = char2
            else
                get_blend_char = char1
            end if
        end select
    end function get_blend_char

    subroutine render_text_elements_to_canvas(this)
        !! Render stored text elements onto the ASCII canvas
        class(ascii_context), intent(inout) :: this
        integer :: i, j, text_len, char_idx
        character(len=1) :: text_char
        
        ! Render each stored text element
        do i = 1, this%num_text_elements
            text_len = len_trim(this%text_elements(i)%text)
            
            ! Draw each character of the text
            do char_idx = 1, text_len
                j = this%text_elements(i)%x + char_idx - 1
                
                ! Check bounds
                if (j >= 1 .and. j <= this%plot_width .and. &
                    this%text_elements(i)%y >= 1 .and. this%text_elements(i)%y <= this%plot_height) then
                    
                    text_char = this%text_elements(i)%text(char_idx:char_idx)
                    
                    ! Choose character based on text color (simple color mapping)
                    if (this%text_elements(i)%color_r > 0.7_wp) then
                        text_char = text_char  ! Keep original for red text
                    else if (this%text_elements(i)%color_g > 0.7_wp) then
                        text_char = text_char  ! Keep original for green text
                    else if (this%text_elements(i)%color_b > 0.7_wp) then
                        text_char = text_char  ! Keep original for blue text
                    end if
                    
                    ! Only overwrite space or lower density characters
                    if (this%canvas(this%text_elements(i)%y, j) == ' ' .or. &
                        get_char_density(text_char) > get_char_density(this%canvas(this%text_elements(i)%y, j))) then
                        this%canvas(this%text_elements(i)%y, j) = text_char
                    end if
                end if
            end do
        end do
    end subroutine render_text_elements_to_canvas

    subroutine print_centered_title(title, width)
        !! Print centered title to terminal
        character(len=*), intent(in) :: title
        integer, intent(in) :: width
        integer :: padding, title_len
        character(len=:), allocatable :: centered_title
        
        title_len = len_trim(title)
        if (title_len >= width) then
            print '(A)', trim(title)
        else
            padding = (width - title_len) / 2
            centered_title = repeat(' ', padding) // trim(title)
            print '(A)', centered_title
        end if
    end subroutine print_centered_title

    subroutine write_centered_title(unit, title, width)
        !! Write centered title to file
        integer, intent(in) :: unit
        character(len=*), intent(in) :: title
        integer, intent(in) :: width
        integer :: padding, title_len
        character(len=:), allocatable :: centered_title
        
        title_len = len_trim(title)
        if (title_len >= width) then
            write(unit, '(A)') trim(title)
        else
            padding = (width - title_len) / 2
            centered_title = repeat(' ', padding) // trim(title)
            write(unit, '(A)') centered_title
        end if
    end subroutine write_centered_title

    subroutine ascii_draw_marker(this, x, y, style)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: style
        integer :: px, py
        character(len=1) :: marker_char

        ! Map to usable plot area (excluding 1-char border on each side)
        px = int((x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_width - 3, wp)) + 2
        py = (this%plot_height - 1) - int((y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_height - 3, wp))

        ! Map marker styles to distinct ASCII characters for visual differentiation
        select case (trim(style))
        case ('o')
            marker_char = 'o'  ! Circle
        case ('s')
            marker_char = '#'  ! Square
        case ('D', 'd')
            marker_char = '%'  ! Diamond (ASCII representation)
        case ('x')
            marker_char = 'x'  ! Cross
        case ('+')
            marker_char = '+'  ! Plus
        case ('*')
            marker_char = '*'  ! Star
        case ('^')
            marker_char = '^'  ! Triangle up
        case ('v')
            marker_char = 'v'  ! Triangle down
        case ('<')
            marker_char = '<'  ! Triangle left
        case ('>')
            marker_char = '>'  ! Triangle right
        case ('p')
            marker_char = 'P'  ! Pentagon
        case ('h', 'H')
            marker_char = 'H'  ! Hexagon
        case default
            marker_char = '*'  ! Default fallback
        end select

        if (px >= 2 .and. px <= this%plot_width - 1 .and. py >= 2 .and. py <= this%plot_height - 1) then
            this%canvas(py, px) = marker_char
        end if
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
        !! Fill ASCII canvas with heatmap representation of 2D data
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_grid(:), y_grid(:), z_grid(:,:)
        real(wp), intent(in) :: z_min, z_max
        
        integer :: nx, ny, i, j, px, py
        real(wp) :: x_min, x_max, y_min, y_max
        real(wp) :: z_normalized
        integer :: char_idx
        
        nx = size(x_grid)
        ny = size(y_grid)
        
        if (size(z_grid, 1) /= nx .or. size(z_grid, 2) /= ny) return
        
        x_min = minval(x_grid)
        x_max = maxval(x_grid)
        y_min = minval(y_grid)
        y_max = maxval(y_grid)
        
        ! Fill the canvas with density characters based on z values
        do i = 1, nx
            do j = 1, ny
                ! Map grid coordinates to canvas coordinates
                px = int((x_grid(i) - this%x_min) / (this%x_max - this%x_min) * &
                        real(this%plot_width - 3, wp)) + 2
                py = (this%plot_height - 1) - int((y_grid(j) - this%y_min) / &
                        (this%y_max - this%y_min) * real(this%plot_height - 3, wp))
                
                ! Check bounds
                if (px >= 2 .and. px <= this%plot_width - 1 .and. py >= 2 .and. py <= this%plot_height - 1) then
                    ! Normalize z value to character index
                    if (abs(z_max - z_min) > 1e-10_wp) then
                        z_normalized = (z_grid(i, j) - z_min) / (z_max - z_min)
                    else
                        z_normalized = 0.5_wp
                    end if
                    
                    ! Map to character index (1 to len(ASCII_CHARS))
                    char_idx = min(len(ASCII_CHARS), max(1, int(z_normalized * real(len(ASCII_CHARS) - 1, wp)) + 1))
                    
                    ! Only overwrite if current position is empty or has lower density
                    if (this%canvas(py, px) == ' ' .or. char_idx > index(ASCII_CHARS, this%canvas(py, px))) then
                        this%canvas(py, px) = ASCII_CHARS(char_idx:char_idx)
                    end if
                end if
            end do
        end do
    end subroutine ascii_fill_heatmap

    subroutine ascii_draw_arrow(this, x, y, dx, dy, size, style)
        !! Draw arrow using Unicode directional characters for ASCII backend
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y, dx, dy, size
        character(len=*), intent(in) :: style
        
        integer :: px, py
        character(len=1) :: arrow_char
        real(wp) :: angle
        
        ! Convert world coordinates to pixel coordinates
        px = int((x - this%x_min) / (this%x_max - this%x_min) * real(this%width, wp))
        py = int((y - this%y_min) / (this%y_max - this%y_min) * real(this%height, wp))
        
        ! Ensure coordinates are within bounds
        if (px < 1 .or. px > this%width .or. py < 1 .or. py > this%height) return
        
        ! Calculate angle for direction
        angle = atan2(dy, dx)
        
        ! Choose arrow character based on direction
        if (abs(angle) < 0.393_wp) then          ! 0 ± 22.5 degrees (right)
            arrow_char = '→'
        else if (angle >= 0.393_wp .and. angle < 1.178_wp) then  ! 22.5-67.5 degrees (up-right)
            arrow_char = '↗'
        else if (angle >= 1.178_wp .and. angle < 1.963_wp) then  ! 67.5-112.5 degrees (up)
            arrow_char = '↑'
        else if (angle >= 1.963_wp .and. angle < 2.749_wp) then  ! 112.5-157.5 degrees (up-left)
            arrow_char = '↖'
        else if (abs(angle) >= 2.749_wp) then    ! 157.5-180 degrees (left)
            arrow_char = '←'
        else if (angle <= -0.393_wp .and. angle > -1.178_wp) then  ! -22.5 to -67.5 degrees (down-right)
            arrow_char = '↘'
        else if (angle <= -1.178_wp .and. angle > -1.963_wp) then  ! -67.5 to -112.5 degrees (down)
            arrow_char = '↓'
        else  ! -112.5 to -157.5 degrees (down-left)
            arrow_char = '↙'
        end if
        
        ! Place arrow character on canvas
        this%canvas(py, px) = arrow_char
        
        ! Mark that arrows have been rendered
        this%has_rendered_arrows = .true.
        this%uses_vector_arrows = .false.
        this%has_triangular_arrows = .false.
    end subroutine ascii_draw_arrow

    function ascii_get_output(this) result(output)
        !! Get the complete ASCII canvas as a string
        class(ascii_context), intent(in) :: this
        character(len=:), allocatable :: output
        character(len=1000) :: line_buffer
        integer :: i, j, total_len, line_len
        
        ! Calculate total length needed
        total_len = this%height * (this%width + 1)  ! +1 for newline per row
        allocate(character(len=total_len) :: output)
        
        output = ""
        do i = 1, this%height
            line_buffer = ""
            do j = 1, this%width
                line_buffer(j:j) = this%canvas(i, j)
            end do
            line_len = len_trim(line_buffer(:this%width))
            if (line_len == 0) line_len = 1  ! Ensure at least one character
            output = output // line_buffer(1:line_len) // new_line('a')
        end do
    end function ascii_get_output

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
        !! Fill quadrilateral using polymorphic interface (approximation)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_quad(4), y_quad(4)
        
        integer :: px(4), py(4), i, j, min_x, max_x, min_y, max_y
        
        ! Convert coordinates to ASCII canvas coordinates
        do i = 1, 4
            px(i) = nint((x_quad(i) - this%x_min) / (this%x_max - this%x_min) * this%plot_width) + 1
            py(i) = nint((y_quad(i) - this%y_min) / (this%y_max - this%y_min) * this%plot_height) + 1
        end do
        
        ! Simple approximation: fill bounding rectangle
        min_x = max(1, min(minval(px), this%plot_width))
        max_x = max(1, min(maxval(px), this%plot_width))  
        min_y = max(1, min(minval(py), this%plot_height))
        max_y = max(1, min(maxval(py), this%plot_height))
        
        do j = min_y, max_y
            do i = min_x, max_x
                this%canvas(j, i) = '#'
            end do
        end do
    end subroutine ascii_fill_quad

    subroutine ascii_render_legend_specialized(this, legend, legend_x, legend_y)
        !! Render legend using ASCII-specific compact layout
        use fortplot_legend, only: legend_t, render_ascii_legend
        class(ascii_context), intent(inout) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(in) :: legend_x, legend_y
        
        ! Use ASCII-specific legend rendering
        call render_ascii_legend(legend, this, legend_x, legend_y)
    end subroutine ascii_render_legend_specialized

    subroutine ascii_calculate_legend_dimensions(this, legend, legend_width, legend_height)
        !! Calculate ASCII-specific legend dimensions
        use fortplot_legend, only: legend_t
        class(ascii_context), intent(in) :: this
        type(legend_t), intent(in) :: legend
        real(wp), intent(out) :: legend_width, legend_height
        integer :: i
        
        ! Calculate actual legend width based on longest entry
        legend_width = 15.0_wp  ! Default minimum width
        do i = 1, legend%num_entries
            legend_width = max(legend_width, real(len_trim(legend%entries(i)%label) + 5, wp))  ! +5 for "-- " prefix and margin
        end do
        
        ! For ASCII backend, limit legend width to prevent overflow  
        if (legend_width > real(this%width, wp) * 0.3) then
            legend_width = real(this%width, wp) * 0.3
        end if
        
        legend_height = real(legend%num_entries + 2, wp)  ! Each entry + border
    end subroutine ascii_calculate_legend_dimensions

    subroutine ascii_set_legend_border_width(this)
        !! ASCII doesn't use line widths - no-op
        class(ascii_context), intent(inout) :: this
        
        ! ASCII backend doesn't have line widths - no operation needed
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
        use, intrinsic :: iso_fortran_env, only: real64
        class(ascii_context), intent(in) :: this
        integer, intent(in) :: width, height
        real(real64), intent(out) :: rgb_data(width, height, 3)
        
        ! ASCII backend doesn't have RGB data for animation - fill with dummy data
        rgb_data = 0.0_real64  ! Black background
    end subroutine ascii_extract_rgb_data

    subroutine ascii_get_png_data(this, width, height, png_data, status)
        !! Get PNG data from ASCII backend (not supported)
        class(ascii_context), intent(in) :: this
        integer, intent(in) :: width, height
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        ! ASCII backend doesn't provide PNG data
        allocate(png_data(0))
        status = -1
    end subroutine ascii_get_png_data

    subroutine ascii_prepare_3d_data(this, plots)
        !! Prepare 3D data for ASCII backend (no-op - ASCII doesn't use 3D data)
        use fortplot_plot_data, only: plot_data_t
        class(ascii_context), intent(inout) :: this
        type(plot_data_t), intent(in) :: plots(:)
        
        ! ASCII backend doesn't need 3D data preparation - no-op
    end subroutine ascii_prepare_3d_data

    subroutine ascii_render_ylabel(this, ylabel)
        !! Render Y-axis label for ASCII backend (no-op - handled elsewhere)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: ylabel
        
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
        
        real(wp) :: label_x, label_y
        
        ! ASCII backend: explicitly set title and draw simple axes
        if (present(title)) then
            if (allocated(title)) then
                call this%set_title(title)
            end if
        end if
        
        ! Draw axes
        call this%line(x_min, y_min, x_max, y_min)
        call this%line(x_min, y_min, x_min, y_max)
        
        ! Store xlabel and ylabel for rendering outside the plot frame
        if (present(xlabel)) then
            if (allocated(xlabel)) then
                this%xlabel_text = xlabel
            end if
        end if
        
        if (present(ylabel)) then
            if (allocated(ylabel)) then
                this%ylabel_text = ylabel
            end if
        end if
    end subroutine ascii_draw_axes_and_labels

    subroutine ascii_save_coordinates(this, x_min, x_max, y_min, y_max)
        !! Save current coordinate system
        class(ascii_context), intent(in) :: this
        real(wp), intent(out) :: x_min, x_max, y_min, y_max
        
        x_min = this%x_min
        x_max = this%x_max
        y_min = this%y_min
        y_max = this%y_max
    end subroutine ascii_save_coordinates

    subroutine ascii_set_coordinates(this, x_min, x_max, y_min, y_max)
        !! Set coordinate system
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x_min, x_max, y_min, y_max
        
        this%x_min = x_min
        this%x_max = x_max
        this%y_min = y_min
        this%y_max = y_max
    end subroutine ascii_set_coordinates

end module fortplot_ascii