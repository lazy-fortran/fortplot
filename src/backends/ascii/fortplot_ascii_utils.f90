module fortplot_ascii_utils
    !! ASCII terminal plotting backend - Utility Functions
    !!
    !! This module contains utility functions used by the ASCII plotting backend,
    !! including character manipulation, text rendering, and output formatting.
    !!
    !! Author: fortplot contributors
    
    use fortplot_unicode, only: escape_unicode_for_ascii
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: get_char_density, get_blend_char
    public :: render_text_elements_to_canvas
    public :: print_centered_title, write_centered_title
    public :: text_element_t, ASCII_CHARS
    
    ! ASCII plotting constants
    character(len=*), parameter :: ASCII_CHARS = ' .:-=+*#%@'
    
    type :: text_element_t
        character(len=:), allocatable :: text
        integer :: x, y
        real(wp) :: color_r, color_g, color_b
    end type text_element_t
    
contains

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

    logical function is_graphics_character(char)
        !! Check if character is a plot graphics element (not text)
        !! Returns .true. for characters used in ASCII plots (lines, markers, etc.)
        !! Returns .false. for text characters (letters, digits, most punctuation)
        character(len=1), intent(in) :: char
        
        select case (char)
        case ('.', ':', '-', '=', '+', 'o', '*', '#', '%', '@')
            ! These are plot graphics characters that can be overwritten by text
            is_graphics_character = .true.
        case default
            ! All other characters (letters, digits, most punctuation) are text
            ! Text should not overwrite other text to prevent scrambling
            is_graphics_character = .false.
        end select
    end function is_graphics_character

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

    subroutine render_text_elements_to_canvas(canvas, text_elements, num_text_elements, plot_width, plot_height)
        !! Render stored text elements onto the ASCII canvas with Unicode-to-ASCII conversion
        character(len=1), intent(inout) :: canvas(:,:)
        type(text_element_t), intent(in) :: text_elements(:)
        integer, intent(in) :: num_text_elements, plot_width, plot_height
        integer :: i, j, text_len, char_idx
        integer :: current_y, base_y, attempt, candidate_y
        logical :: conflict
        character(len=1) :: text_char
        character(len=500) :: ascii_text  ! Buffer for converted text
        integer, parameter :: MAX_ROW_SHIFT = 3
        
        ! Render each stored text element
        do i = 1, num_text_elements
            ! Convert Unicode text to ASCII-compatible form for Issue #853 fix
            call escape_unicode_for_ascii(text_elements(i)%text, ascii_text)
            text_len = len_trim(ascii_text)

            base_y = max(1, min(text_elements(i)%y, plot_height))
            current_y = base_y

            do attempt = 0, MAX_ROW_SHIFT
                candidate_y = base_y + attempt
                if (candidate_y <= plot_height) then
                    conflict = .false.
                    do char_idx = 1, text_len
                        j = text_elements(i)%x + char_idx - 1
                        if (j < 1 .or. j > plot_width) cycle
                        if (canvas(candidate_y, j) /= ' ' .and. &
                            .not. is_graphics_character(canvas(candidate_y, j))) then
                            conflict = .true.
                            exit
                        end if
                    end do
                    if (.not. conflict) then
                        current_y = candidate_y
                        exit
                    end if
                end if

                if (attempt == 0) cycle

                candidate_y = base_y - attempt
                if (candidate_y >= 1) then
                    conflict = .false.
                    do char_idx = 1, text_len
                        j = text_elements(i)%x + char_idx - 1
                        if (j < 1 .or. j > plot_width) cycle
                        if (canvas(candidate_y, j) /= ' ' .and. &
                            .not. is_graphics_character(canvas(candidate_y, j))) then
                            conflict = .true.
                            exit
                        end if
                    end do
                    if (.not. conflict) then
                        current_y = candidate_y
                        exit
                    end if
                end if
            end do

            ! Draw each character of the text at the chosen row
            do char_idx = 1, text_len
                j = text_elements(i)%x + char_idx - 1

                if (j >= 1 .and. j <= plot_width .and. &
                    current_y >= 1 .and. current_y <= plot_height) then

                    text_char = ascii_text(char_idx:char_idx)

                    if (text_elements(i)%color_r > 0.7_wp .or. &
                        text_elements(i)%color_g > 0.7_wp .or. &
                        text_elements(i)%color_b > 0.7_wp) then
                        ! Placeholder: keep original character for color emphasis
                        text_char = text_char
                    end if

                    if (canvas(current_y, j) == ' ') then
                        canvas(current_y, j) = text_char
                    else if (is_graphics_character(canvas(current_y, j))) then
                        canvas(current_y, j) = text_char
                    end if
                end if
            end do
        end do
    end subroutine render_text_elements_to_canvas

    subroutine print_centered_title(title, width)
        !! Print centered title to terminal with Unicode-to-ASCII conversion
        character(len=*), intent(in) :: title
        integer, intent(in) :: width
        integer :: padding, title_len
        character(len=:), allocatable :: centered_title
        character(len=500) :: ascii_title  ! Buffer for converted title
        
        ! Convert Unicode title to ASCII-compatible form for Issue #853 fix
        call escape_unicode_for_ascii(title, ascii_title)
        
        title_len = len_trim(ascii_title)
        if (title_len >= width) then
            print '(A)', trim(ascii_title)
        else
            padding = (width - title_len) / 2
            centered_title = repeat(' ', padding) // trim(ascii_title)
            print '(A)', centered_title
        end if
    end subroutine print_centered_title

    subroutine write_centered_title(unit, title, width)
        !! Write centered title to file with Unicode-to-ASCII conversion
        integer, intent(in) :: unit
        character(len=*), intent(in) :: title
        integer, intent(in) :: width
        integer :: padding, title_len
        character(len=:), allocatable :: centered_title
        character(len=500) :: ascii_title  ! Buffer for converted title
        
        ! Convert Unicode title to ASCII-compatible form for Issue #853 fix
        call escape_unicode_for_ascii(title, ascii_title)
        
        title_len = len_trim(ascii_title)
        if (title_len >= width) then
            write(unit, '(A)') trim(ascii_title)
        else
            padding = (width - title_len) / 2
            centered_title = repeat(' ', padding) // trim(ascii_title)
            write(unit, '(A)') centered_title
        end if
    end subroutine write_centered_title

end module fortplot_ascii_utils
