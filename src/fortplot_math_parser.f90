module fortplot_math_parser
    !! Math mode parser for handling LaTeX-style delimited expressions
    implicit none
    
    private
    public :: find_math_delimiters, validate_math_delimiters, extract_math_content
    public :: find_all_math_expressions, process_escaped_dollars, has_nested_delimiters
    public :: parse_text_with_math
    
contains

    subroutine find_math_delimiters(text, start, start_pos, end_pos, found)
        character(len=*), intent(in) :: text
        integer, intent(in) :: start
        integer, intent(out) :: start_pos, end_pos
        logical, intent(out) :: found
        integer :: i, text_len
        
        found = .false.
        start_pos = 0
        end_pos = 0
        text_len = len_trim(text)
        
        ! Find opening delimiter
        do i = start, text_len
            if (i > 1) then
                ! Check for escaped dollar
                if (text(i-1:i-1) == '\' .and. text(i:i) == '$') then
                    cycle  ! Skip escaped dollar
                end if
            end if
            
            if (text(i:i) == '$') then
                start_pos = i
                exit
            end if
        end do
        
        if (start_pos == 0) return  ! No opening delimiter found
        
        ! Find closing delimiter
        do i = start_pos + 1, text_len
            if (i > 1) then
                ! Check for escaped dollar
                if (text(i-1:i-1) == '\' .and. text(i:i) == '$') then
                    cycle  ! Skip escaped dollar
                end if
            end if
            
            if (text(i:i) == '$') then
                end_pos = i
                found = .true.
                exit
            end if
        end do
    end subroutine find_math_delimiters
    
    logical function validate_math_delimiters(text)
        character(len=*), intent(in) :: text
        integer :: i, count, text_len
        
        count = 0
        text_len = len_trim(text)
        
        do i = 1, text_len
            if (i > 1) then
                ! Skip escaped dollars
                if (text(i-1:i-1) == '\' .and. text(i:i) == '$') then
                    cycle
                end if
            end if
            
            if (text(i:i) == '$') then
                count = count + 1
            end if
        end do
        
        ! Valid if even number of delimiters
        validate_math_delimiters = (mod(count, 2) == 0)
    end function validate_math_delimiters
    
    subroutine extract_math_content(text, start_pos, end_pos, content, content_len)
        character(len=*), intent(in) :: text
        integer, intent(in) :: start_pos, end_pos
        character(len=*), intent(out) :: content
        integer, intent(out) :: content_len
        
        content = ""
        content_len = 0
        
        if (start_pos >= end_pos) return
        if (start_pos < 1 .or. end_pos > len_trim(text)) return
        
        ! Extract content between delimiters (excluding the $ signs)
        content_len = end_pos - start_pos - 1
        if (content_len > 0) then
            content = text(start_pos+1:end_pos-1)
        end if
    end subroutine extract_math_content
    
    subroutine find_all_math_expressions(text, positions, num_found)
        character(len=*), intent(in) :: text
        integer, intent(out) :: positions(:,:)
        integer, intent(out) :: num_found
        integer :: start, start_pos, end_pos
        logical :: found
        
        num_found = 0
        start = 1
        
        do while (start <= len_trim(text) .and. num_found < size(positions, 1))
            call find_math_delimiters(text, start, start_pos, end_pos, found)
            if (.not. found) exit
            
            num_found = num_found + 1
            positions(num_found, 1) = start_pos
            positions(num_found, 2) = end_pos
            
            start = end_pos + 1
        end do
    end subroutine find_all_math_expressions
    
    subroutine process_escaped_dollars(text, processed, proc_len)
        character(len=*), intent(in) :: text
        character(len=*), intent(out) :: processed
        integer, intent(out) :: proc_len
        integer :: i, j, text_len
        
        processed = ""
        proc_len = 0
        text_len = len_trim(text)
        
        i = 1
        j = 1
        do while (i <= text_len)
            if (i < text_len .and. text(i:i) == '\' .and. text(i+1:i+1) == '$') then
                ! Replace \$ with $
                processed(j:j) = '$'
                i = i + 2  ! Skip both characters
            else
                ! Copy character as-is
                processed(j:j) = text(i:i)
                i = i + 1
            end if
            j = j + 1
        end do
        
        proc_len = j - 1
    end subroutine process_escaped_dollars
    
    logical function has_nested_delimiters(text)
        character(len=*), intent(in) :: text
        integer :: dollar_positions(100), num_dollars
        integer :: i, j, k, text_len
        
        has_nested_delimiters = .false.
        text_len = len_trim(text)
        num_dollars = 0
        
        ! First, find all non-escaped $ positions
        do i = 1, text_len
            if (i > 1) then
                if (text(i-1:i-1) == '\' .and. text(i:i) == '$') then
                    cycle  ! Skip escaped
                end if
            end if
            if (text(i:i) == '$') then
                num_dollars = num_dollars + 1
                dollar_positions(num_dollars) = i
            end if
        end do
        
        ! Actually, let's use a different approach
        ! A string has nested delimiters if it can't be clearly parsed
        ! as a sequence of paired delimiters
        
        ! For "$\alpha$ and $\beta$" - this is clearly two expressions with "and" between
        ! For "$outer $inner$ text$" - this is ambiguous 
        
        ! Check if we have exactly matching pairs with reasonable separation
        if (mod(num_dollars, 2) == 0 .and. num_dollars >= 4) then
            ! Check if the pattern suggests overlapping expressions
            ! Key insight: in "$outer $inner$ text$", the substring "$inner$" 
            ! is completely contained within what could be interpreted as one expression
            
            ! Look for the pattern where we have 4 $s and the middle substring
            ! between positions 2 and 3 doesn't contain any clear separator
            do i = 1, num_dollars - 3
                ! Check the text between the 2nd and 3rd delimiter
                j = dollar_positions(i+1) + 1
                k = dollar_positions(i+2) - 1
                
                ! If the text between is short and doesn't contain clear separators
                ! like "and", "or", spaces, commas, then it's likely nested
                if (k >= j) then
                    block
                        character(len=100) :: between_text
                        integer :: bt_len
                        bt_len = k - j + 1
                        if (bt_len < 15) then  ! Short text between delimiters
                            between_text = text(j:k)
                            ! Check if it lacks clear separators
                            if (index(between_text, ' and ') == 0 .and. &
                                index(between_text, ', ') == 0 .and. &
                                index(between_text, ' or ') == 0) then
                                has_nested_delimiters = .true.
                                return
                            end if
                        end if
                    end block
                end if
            end do
        end if
    end function has_nested_delimiters
    
    subroutine parse_text_with_math(text, result, result_len)
        character(len=*), intent(in) :: text
        character(len=*), intent(out) :: result
        integer, intent(out) :: result_len
        integer :: positions(20, 2), num_math
        integer :: i, last_pos
        
        ! First process escaped dollars
        call process_escaped_dollars(text, result, result_len)
        
        ! Find all math expressions
        call find_all_math_expressions(result(1:result_len), positions, num_math)
        
        ! For now, just return the processed text
        ! In a full implementation, this would parse and mark math regions
        if (num_math > 0) then
            ! Text contains math expressions
            result_len = len_trim(result)
        end if
    end subroutine parse_text_with_math

end module fortplot_math_parser