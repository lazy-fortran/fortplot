module fortplot_ascii_utils
    !! ASCII terminal plotting backend - Utility Functions
    !!
    !! This module contains utility functions used by the ASCII plotting backend,
    !! including character manipulation, text rendering, and output formatting.
    !!
    !! Author: fortplot contributors
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    private
    public :: get_char_density, get_blend_char
    public :: render_text_elements_to_canvas
    public :: print_centered_title, write_centered_title
    public :: text_element_t
    
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
        !! Render stored text elements onto the ASCII canvas
        character(len=1), intent(inout) :: canvas(:,:)
        type(text_element_t), intent(in) :: text_elements(:)
        integer, intent(in) :: num_text_elements, plot_width, plot_height
        integer :: i, j, text_len, char_idx
        character(len=1) :: text_char
        
        ! Render each stored text element
        do i = 1, num_text_elements
            text_len = len_trim(text_elements(i)%text)
            
            ! Draw each character of the text
            do char_idx = 1, text_len
                j = text_elements(i)%x + char_idx - 1
                
                ! Check bounds
                if (j >= 1 .and. j <= plot_width .and. &
                    text_elements(i)%y >= 1 .and. text_elements(i)%y <= plot_height) then
                    
                    text_char = text_elements(i)%text(char_idx:char_idx)
                    
                    ! Choose character based on text color (simple color mapping)
                    if (text_elements(i)%color_r > 0.7_wp) then
                        text_char = text_char  ! Keep original for red text
                    else if (text_elements(i)%color_g > 0.7_wp) then
                        text_char = text_char  ! Keep original for green text
                    else if (text_elements(i)%color_b > 0.7_wp) then
                        text_char = text_char  ! Keep original for blue text
                    end if
                    
                    ! Only overwrite space or lower density characters
                    if (canvas(text_elements(i)%y, j) == ' ' .or. &
                        get_char_density(text_char) > get_char_density(canvas(text_elements(i)%y, j))) then
                        canvas(text_elements(i)%y, j) = text_char
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

end module fortplot_ascii_utils