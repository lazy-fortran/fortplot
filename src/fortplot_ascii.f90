module fortplot_ascii
    !! ASCII terminal plotting backend
    !!
    !! This module implements text-based plotting for terminal output using
    !! ASCII characters and Unicode box drawing characters. Provides basic
    !! line plotting with character density mapping for visualization.
    !!
    !! Author: fortplotlib contributors
    
    use fortplot_context
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: ascii_context, create_ascii_canvas
    type, extends(plot_context) :: ascii_context
        character(len=1), allocatable :: canvas(:,:)
        character(len=:), allocatable :: title_text
        real(wp) :: current_r, current_g, current_b
        integer :: plot_width = 80
        integer :: plot_height = 24
    contains
        procedure :: line => ascii_draw_line
        procedure :: color => ascii_set_color
        procedure :: text => ascii_draw_text
        procedure :: save => ascii_finalize
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
    
    subroutine ascii_draw_text(this, x, y, text)
        class(ascii_context), intent(inout) :: this
        real(wp), intent(in) :: x, y
        character(len=*), intent(in) :: text
        
        if (.not. allocated(this%title_text)) then
            this%title_text = text
        end if
    end subroutine ascii_draw_text
    
    subroutine ascii_finalize(this, filename)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        integer :: i, j, unit
        
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
        
        if (allocated(this%title_text)) then
            print '(A)', this%title_text
            print '(A)', repeat('=', len_trim(this%title_text))
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
        
        if (allocated(this%title_text)) then
            write(unit, '(A)') this%title_text
            write(unit, '(A)') repeat('=', len_trim(this%title_text))
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

end module fortplot_ascii