module fortplot_ascii
    !! ASCII terminal plotting backend
    !!
    !! This module implements text-based plotting for terminal output using
    !! ASCII characters and Unicode box drawing characters. Provides basic
    !! line plotting with character density mapping for visualization.
    !!
    !! Author: fortplot contributors
    
    use fortplot_context
    use fortplot_latex_parser
    use fortplot_unicode
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
        
        ! print *, "ASCII DRAW: Called with", x1, y1, "to", x2, y2
        
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
            
            ! Debug first few points
            ! if (i < 3) then
            !     print *, "ASCII DEBUG: px,py=", px, py, "canvas:", this%plot_width, "x", this%plot_height
            ! end if
            
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
        
        ! Only store as title if not explicitly set and no title allocated yet
        if (.not. this%title_set .and. .not. allocated(this%title_text)) then
            this%title_text = processed_text(1:processed_len)
        end if
        
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
        
        integer :: unit
        
        if (len_trim(filename) == 0 .or. trim(filename) == "terminal") then
            call output_to_terminal(this)
        else
            open(newunit=unit, file=filename, status='replace')
            call output_to_file(this, unit)
            close(unit)
            print *, "Unicode plot saved to '", trim(filename), "'"
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
        
        ! print *, "ASCII OUTPUT: Using width=", this%plot_width, "height=", this%plot_height
        write(unit, '(A)') '+' // repeat('-', this%plot_width) // '+'
        do i = 1, this%plot_height
            write(unit, '(A)', advance='no') '|'
            do j = 1, this%plot_width
                write(unit, '(A)', advance='no') this%canvas(i, j)
            end do
            write(unit, '(A)') '|'
        end do
        
        write(unit, '(A)') '+' // repeat('-', this%plot_width) // '+'
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

end module fortplot_ascii