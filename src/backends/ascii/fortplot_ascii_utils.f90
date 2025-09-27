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
        logical :: override_existing = .false.
        integer :: clear_width = 0
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
        integer :: i, j, text_len, char_idx, fill_end, pass
        character(len=1) :: text_char
        character(len=500) :: ascii_text  ! Buffer for converted text
        
        ! Render stored text elements, processing lower-priority text first
        do pass = 1, 2
            do i = 1, num_text_elements
                if (pass == 1 .and. text_elements(i)%override_existing) cycle
                if (pass == 2 .and. .not. text_elements(i)%override_existing) cycle
            ! Convert Unicode text to ASCII-compatible form for Issue #853 fix
            call escape_unicode_for_ascii(text_elements(i)%text, ascii_text)
            text_len = len_trim(ascii_text)
            
            ! Draw each character of the text
            do char_idx = 1, text_len
                j = text_elements(i)%x + char_idx - 1
                
                ! Check bounds
                if (j >= 1 .and. j <= plot_width .and. &
                    text_elements(i)%y >= 1 .and. text_elements(i)%y <= plot_height) then
                    
                    text_char = ascii_text(char_idx:char_idx)
                    
                    ! Choose character based on text color (simple color mapping)
                    if (text_elements(i)%color_r > 0.7_wp) then
                        text_char = text_char  ! Keep original for red text
                    else if (text_elements(i)%color_g > 0.7_wp) then
                        text_char = text_char  ! Keep original for green text
                    else if (text_elements(i)%color_b > 0.7_wp) then
                        text_char = text_char  ! Keep original for blue text
                    end if
                    
                    ! Fix for issue #852: Prevent text scrambling by implementing proper text layering
                    ! Text elements should only overwrite empty spaces or plot graphics
                    ! Never allow text to overwrite other text to prevent character mixing
                    if (text_elements(i)%override_existing) then
                        ! Legend or high-priority text can replace existing characters
                        canvas(text_elements(i)%y, j) = text_char
                    else if (canvas(text_elements(i)%y, j) == ' ') then
                        ! Always write text to empty spaces
                        canvas(text_elements(i)%y, j) = text_char
                    else if (is_graphics_character(canvas(text_elements(i)%y, j))) then
                        ! Text has higher priority than plot graphics, can overwrite
                        canvas(text_elements(i)%y, j) = text_char
                    end if
                    ! Existing text characters are preserved unless override is requested
                end if
            end do

            if (text_elements(i)%override_existing) then
                fill_end = max(text_len, text_elements(i)%clear_width)
                if (fill_end > text_len) then
                    do char_idx = text_len + 1, fill_end
                        j = text_elements(i)%x + char_idx - 1
                        if (j >= 1 .and. j <= plot_width .and. &
                            text_elements(i)%y >= 1 .and. text_elements(i)%y <= plot_height) then
                            canvas(text_elements(i)%y, j) = ' '
                        end if
                    end do
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
