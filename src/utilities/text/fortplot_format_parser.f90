module fortplot_format_parser
    implicit none
    
    private
    
    public :: parse_format_string, contains_format_chars
    
contains

    subroutine parse_format_string(format_str, marker, linestyle, color)
        character(len=*), intent(in) :: format_str
        character(len=*), intent(out) :: marker, linestyle
        character(len=*), intent(out), optional :: color

        character(len=20) :: trimmed_str, stripped
        integer :: i, j
        character(len=1) :: ch

        ! Initialize outputs
        marker = ''
        linestyle = ''
        if (present(color)) color = ''

        trimmed_str = trim(adjustl(format_str))

        ! Handle empty or whitespace strings
        if (len_trim(trimmed_str) == 0) then
            return
        end if

        ! Check for named linestyles first
        if (translate_named_linestyle(trimmed_str, linestyle)) then
            marker = ''  ! Named linestyles have no markers
            return
        end if

        ! Extract color character and build stripped string without it
        j = 0
        stripped = ''
        do i = 1, len_trim(trimmed_str)
            ch = trimmed_str(i:i)
            if (is_color_char(ch)) then
                if (present(color)) color = ch
            else
                j = j + 1
                stripped(j:j) = ch
            end if
        end do

        ! Parse the remaining (color-stripped) string for marker and linestyle
        if (j == 0) then
            return
        end if

        if (extract_linestyle_pattern(stripped, linestyle)) then
            call extract_marker_from_remaining(stripped, marker)
        else
            call extract_marker_only(stripped, marker)
            if (len_trim(marker) > 0) then
                linestyle = 'None'
            end if
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

    pure function is_color_char(char) result(is_color)
        !! Recognize the 8 matplotlib/MATLAB single-letter color codes.
        !! No overlap with marker chars (b,g,r,c,m,y,k,w vs o,x,+,*,s,...).
        character(len=1), intent(in) :: char
        logical :: is_color

        select case (char)
        case ('b', 'g', 'r', 'c', 'm', 'y', 'k', 'w')
            is_color = .true.
        case default
            is_color = .false.
        end select
    end function

    pure function contains_format_chars(style_str) result(has_format)
        !! Check if a style string contains matplotlib format characters
        !! Returns false for named linestyles (solid, dashed, etc)
        character(len=*), intent(in) :: style_str
        logical :: has_format
        
        character(len=20) :: trimmed_str
        
        has_format = .false.
        trimmed_str = trim(adjustl(style_str))
        
        ! Empty string has no format
        if (len_trim(trimmed_str) == 0) then
            return
        end if
        
        ! Named linestyles don't count as format strings
        if (is_named_linestyle(trimmed_str)) then
            has_format = .false.
            return
        end if
        
        ! Check for actual matplotlib syntax patterns
        if (has_linestyle_pattern(trimmed_str) &
            .or. has_valid_marker_syntax(trimmed_str) &
            .or. has_color_char(trimmed_str)) then
            has_format = .true.
        end if
    end function

    function translate_named_linestyle(name_str, linestyle) result(translated)
        !! Translate named linestyle to matplotlib syntax
        character(len=*), intent(in) :: name_str
        character(len=*), intent(out) :: linestyle
        logical :: translated
        
        select case (trim(name_str))
        case ('solid')
            linestyle = '-'
            translated = .true.
        case ('dashed')
            linestyle = '--'
            translated = .true.
        case ('dotted')
            linestyle = ':'
            translated = .true.
        case ('dashdot')
            linestyle = '-.'
            translated = .true.
        case ('None')
            linestyle = 'None'
            translated = .true.
        case default
            linestyle = ''
            translated = .false.
        end select
    end function

    function extract_linestyle_pattern(format_str, linestyle) result(found)
        !! Extract linestyle pattern from format string
        character(len=*), intent(in) :: format_str
        character(len=*), intent(out) :: linestyle
        logical :: found
        
        found = .false.
        linestyle = ''
        
        ! Look for specific patterns in order of length (longest first)
        if (index(format_str, '--') > 0) then
            linestyle = '--'
            found = .true.
        else if (index(format_str, '-.') > 0) then
            linestyle = '-.'
            found = .true.
        else if (index(format_str, '-') > 0) then
            linestyle = '-'
            found = .true.
        else if (index(format_str, ':') > 0) then
            linestyle = ':'
            found = .true.
        else if (trim(format_str) == 'None') then
            linestyle = 'None'
            found = .true.
        end if
    end function

    subroutine extract_marker_from_remaining(format_str, marker)
        !! Extract marker from format string after removing linestyle
        character(len=*), intent(in) :: format_str
        character(len=*), intent(out) :: marker
        
        integer :: i
        character(len=1) :: char
        
        marker = ''
        
        ! Find first valid marker character
        do i = 1, len_trim(format_str)
            char = format_str(i:i)
            if (is_marker_char(char)) then
                marker = char
                return
            end if
        end do
    end subroutine

    subroutine extract_marker_only(format_str, marker)
        !! Extract marker when no linestyle is present
        character(len=*), intent(in) :: format_str
        character(len=*), intent(out) :: marker
        
        integer :: i
        character(len=1) :: char
        
        marker = ''
        
        ! Find first valid marker character
        do i = 1, len_trim(format_str)
            char = format_str(i:i)
            if (is_marker_char(char)) then
                marker = char
                return
            end if
        end do
    end subroutine


    pure function is_named_linestyle(style_str) result(is_named)
        !! Check if string is a named linestyle parameter
        character(len=*), intent(in) :: style_str
        logical :: is_named
        
        select case (trim(style_str))
        case ('solid', 'dashed', 'dotted', 'dashdot', 'None')
            is_named = .true.
        case default
            is_named = .false.
        end select
    end function

    pure function has_linestyle_pattern(style_str) result(has_pattern)
        !! Check if string contains linestyle pattern characters
        character(len=*), intent(in) :: style_str
        logical :: has_pattern
        
        has_pattern = .false.
        
        ! Check for specific linestyle patterns
        if (index(style_str, '--') > 0 .or. &
            index(style_str, '-.') > 0 .or. &
            index(style_str, ':') > 0 .or. &
            (index(style_str, '-') > 0 .and. index(style_str, '--') == 0 .and. index(style_str, '-.') == 0)) then
            has_pattern = .true.
        end if
    end function

    pure function has_valid_marker_syntax(style_str) result(has_marker)
        !! Check if string contains valid standalone marker syntax
        character(len=*), intent(in) :: style_str
        logical :: has_marker
        
        integer :: i, marker_count
        character(len=1) :: char
        
        has_marker = .false.
        marker_count = 0
        
        ! Count valid marker characters
        do i = 1, len_trim(style_str)
            char = style_str(i:i)
            if (is_marker_char(char)) then
                marker_count = marker_count + 1
            end if
        end do
        
        ! Valid marker syntax: exactly one marker, possibly with linestyle
        if (marker_count == 1) then
            has_marker = .true.
        end if
    end function

    pure function has_color_char(style_str) result(has_color)
        !! Check if string contains a single-letter color code
        character(len=*), intent(in) :: style_str
        logical :: has_color

        integer :: i

        has_color = .false.
        do i = 1, len_trim(style_str)
            if (is_color_char(style_str(i:i))) then
                has_color = .true.
                return
            end if
        end do
    end function

end module
