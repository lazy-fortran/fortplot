module fortplot_format_parser
    implicit none
    
    private
    
    public :: parse_format_string, contains_format_chars
    
contains

    subroutine parse_format_string(format_str, marker, linestyle)
        character(len=*), intent(in) :: format_str
        character(len=*), intent(out) :: marker, linestyle
        
        integer :: i, format_len
        character(len=1) :: char
        logical :: found_marker, found_line_start
        character(len=20) :: temp_marker, temp_linestyle
        
        ! Initialize outputs
        marker = ''
        linestyle = ''
        temp_marker = ''
        temp_linestyle = ''
        
        format_len = len_trim(format_str)
        found_marker = .false.
        found_line_start = .false.
        
        ! Parse character by character
        do i = 1, format_len
            char = format_str(i:i)
            
            ! Check for markers
            if (is_marker_char(char)) then
                temp_marker = char
                found_marker = .true.
            ! Check for line styles
            else if (char == '-') then
                if (.not. found_line_start) then
                    temp_linestyle = '-'
                    found_line_start = .true.
                else
                    ! Second dash makes it dashed line
                    temp_linestyle = '--'
                end if
            end if
        end do
        
        marker = trim(temp_marker)
        linestyle = trim(temp_linestyle)
        
        ! If we found a marker but no line style, set linestyle to 'None' for scatter plots
        if (found_marker .and. len_trim(linestyle) == 0) then
            linestyle = 'None'
        end if
    end subroutine

    pure function is_marker_char(char) result(is_marker)
        character(len=1), intent(in) :: char
        logical :: is_marker
        
        ! Common matplotlib markers
        select case (char)
        case ('o', 'x', '+', '*', 's', '^', 'v', '<', '>', 'd', 'D', 'p', 'h', 'H')
            is_marker = .true.
        case default
            is_marker = .false.
        end select
    end function

    pure function contains_format_chars(style_str) result(has_format)
        !! Check if a style string contains matplotlib format characters
        character(len=*), intent(in) :: style_str
        logical :: has_format
        
        integer :: i
        character(len=1) :: char
        
        has_format = .false.
        
        do i = 1, len_trim(style_str)
            char = style_str(i:i)
            if (is_marker_char(char)) then
                has_format = .true.
                return
            end if
        end do
    end function

end module