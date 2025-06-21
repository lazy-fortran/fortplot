module fortplot_ascii
    use fortplot_context
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: ascii_context, create_ascii_canvas
    
    ! ASCII plotting context for terminal output
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
    
    ! Enhanced ASCII characters for plotting
    character(len=*), parameter :: ASCII_CHARS = ' .:-=+*#%@'
    character(len=*), parameter :: DENSITY_CHARS = ' ░▒▓█'
    character(len=*), parameter :: BOX_CHARS = '-|+++++++'
    
contains

    function create_ascii_canvas(width, height) result(ctx)
        integer, intent(in), optional :: width, height
        type(ascii_context) :: ctx
        integer :: w, h
        
        w = 80
        h = 40
        if (present(width)) w = width
        if (present(height)) h = height
        
        call setup_canvas(ctx, w, h)
        
        ctx%plot_width = w
        ctx%plot_height = h
        
        ! Initialize canvas
        allocate(ctx%canvas(h, w))
        ctx%canvas = ' '
        
        ! Default color (will be ignored for terminal output)
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
        
        ! Skip drawing if color is too light (grid lines, background elements)
        ! Grid lines typically use light gray like (0.9, 0.9, 0.9)
        if (this%current_r > 0.8_wp .and. this%current_g > 0.8_wp .and. this%current_b > 0.8_wp) then
            return  ! Don't draw light-colored grid lines in ASCII
        end if
        
        ! Choose line character based on color with better mapping
        if (this%current_g > 0.7_wp) then
            line_char = '@'  ! Bright green gets dense character
        else if (this%current_g > 0.3_wp) then
            line_char = '#'  ! Medium green gets hash
        else if (this%current_b > 0.7_wp) then
            line_char = '*'  ! Bright blue gets asterisk
        else if (this%current_b > 0.3_wp) then
            line_char = 'o'  ! Medium blue gets circle
        else if (this%current_r > 0.7_wp) then
            line_char = '%'  ! Red gets percent
        else if (this%current_r > 0.3_wp) then
            line_char = '+'  ! Medium red gets plus
        else
            line_char = '.'  ! Default to dots
        end if
        
        dx = x2 - x1
        dy = y2 - y1
        length = sqrt(dx*dx + dy*dy)
        
        if (length < 1e-6_wp) return
        
        ! Use higher resolution line drawing with antialiasing effect
        steps = max(int(length * 4), max(abs(int(dx)), abs(int(dy)))) + 1
        step_x = dx / real(steps, wp)
        step_y = dy / real(steps, wp)
        
        x = x1
        y = y1
        
        do i = 0, steps
            ! Convert world coordinates to canvas coordinates
            px = int((x - this%x_min) / (this%x_max - this%x_min) * real(this%plot_width - 1, wp)) + 1
            py = this%plot_height - int((y - this%y_min) / (this%y_max - this%y_min) * real(this%plot_height - 1, wp))
            
            ! Check bounds and set character - allow overwriting for better multiple plot support
            if (px >= 1 .and. px <= this%plot_width .and. py >= 1 .and. py <= this%plot_height) then
                ! If space is empty, use the new character
                if (this%canvas(py, px) == ' ') then
                    this%canvas(py, px) = line_char
                ! If space is occupied, use a blend character to show overlap
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
        
        ! Store title for later display
        if (.not. allocated(this%title_text)) then
            this%title_text = text
        end if
    end subroutine ascii_draw_text
    
    subroutine ascii_finalize(this, filename)
        class(ascii_context), intent(inout) :: this
        character(len=*), intent(in) :: filename
        
        integer :: i, j, unit
        
        ! Output to terminal if filename is empty or "terminal"
        if (len_trim(filename) == 0 .or. trim(filename) == "terminal") then
            call output_to_terminal(this)
        else
            ! Save to file
            open(newunit=unit, file=filename, status='replace')
            call output_to_file(this, unit)
            close(unit)
            print *, "Unicode plot saved to '", trim(filename), "'"
        end if
    end subroutine ascii_finalize
    
    subroutine output_to_terminal(this)
        class(ascii_context), intent(inout) :: this
        integer :: i, j
        
        ! Print title if available
        if (allocated(this%title_text)) then
            print '(A)', this%title_text
            print '(A)', repeat('=', len_trim(this%title_text))
        end if
        
        ! Print top border
        print '(A)', '+' // repeat('-', this%plot_width) // '+'
        
        ! Print canvas with side borders
        do i = 1, this%plot_height
            write(*, '(A)', advance='no') '|'
            do j = 1, this%plot_width
                write(*, '(A)', advance='no') this%canvas(i, j)
            end do
            print '(A)', '|'
        end do
        
        ! Print bottom border
        print '(A)', '+' // repeat('-', this%plot_width) // '+'
    end subroutine output_to_terminal
    
    subroutine output_to_file(this, unit)
        class(ascii_context), intent(inout) :: this
        integer, intent(in) :: unit
        integer :: i, j
        
        ! Write title if available
        if (allocated(this%title_text)) then
            write(unit, '(A)') this%title_text
            write(unit, '(A)') repeat('=', len_trim(this%title_text))
        end if
        
        ! Write top border
        write(unit, '(A)') '+' // repeat('-', this%plot_width) // '+'
        
        ! Write canvas with side borders
        do i = 1, this%plot_height
            write(unit, '(A)', advance='no') '|'
            do j = 1, this%plot_width
                write(unit, '(A)', advance='no') this%canvas(i, j)
            end do
            write(unit, '(A)') '|'
        end do
        
        ! Write bottom border
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
            get_char_density = 9  ! Unicode blocks get highest priority
        end select
    end function get_char_density

    character(len=1) function get_blend_char(char1, char2)
        character(len=1), intent(in) :: char1, char2
        
        ! When two different plot characters overlap, use a distinctive blend character
        ! This prevents one plot from completely overwriting another
        select case (char1)
        case ('*')  ! Blue sine
            if (char2 == '#' .or. char2 == '@') then
                get_blend_char = '%'  ! Blend of sine and cosine
            else
                get_blend_char = char1
            end if
        case ('#')  ! Green cosine  
            if (char2 == '*' .or. char2 == 'o') then
                get_blend_char = '%'  ! Blend of sine and cosine
            else
                get_blend_char = char1
            end if
        case ('@')  ! Bright green
            if (char2 == '*' .or. char2 == 'o') then
                get_blend_char = '%'  ! Blend character
            else
                get_blend_char = char1
            end if
        case default
            ! For other combinations, keep the higher density character
            if (get_char_density(char2) > get_char_density(char1)) then
                get_blend_char = char2
            else
                get_blend_char = char1
            end if
        end select
    end function get_blend_char

end module fortplot_ascii