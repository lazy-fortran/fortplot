program test_math_mode_parser
    use fortplot_math_parser
    implicit none
    
    call test_find_math_delimiters()
    call test_paired_delimiter_validation()
    call test_extract_math_content()
    call test_multiple_math_expressions()
    call test_escaped_dollar_signs()
    call test_nested_delimiters()
    call test_mixed_text_and_math()
    
    print *, "All math mode parser tests passed!"
    
contains

    subroutine test_find_math_delimiters()
        character(len=100) :: text
        integer :: start_pos, end_pos
        logical :: found
        
        ! Test simple math expression
        text = "The value is $x = 2$"
        call find_math_delimiters(text, 1, start_pos, end_pos, found)
        if (.not. found) then
            print *, "ERROR: Failed to find math delimiters in simple case"
            stop 1
        end if
        if (start_pos /= 14 .or. end_pos /= 20) then
            print *, "ERROR: Wrong delimiter positions:", start_pos, end_pos
            stop 1
        end if
        
        ! Test no delimiters
        text = "No math here"
        call find_math_delimiters(text, 1, start_pos, end_pos, found)
        if (found) then
            print *, "ERROR: Found delimiters where none exist"
            stop 1
        end if
        
        ! Test unclosed delimiter
        text = "Unclosed $math"
        call find_math_delimiters(text, 1, start_pos, end_pos, found)
        if (found) then
            print *, "ERROR: Should not find complete pair with unclosed delimiter"
            stop 1
        end if
        
        print *, "test_find_math_delimiters: PASSED"
    end subroutine
    
    subroutine test_paired_delimiter_validation()
        character(len=100) :: text
        logical :: is_valid
        
        ! Valid pairs
        text = "$\alpha$"
        is_valid = validate_math_delimiters(text)
        if (.not. is_valid) then
            print *, "ERROR: Simple math expression marked invalid"
            stop 1
        end if
        
        text = "Text $\mu$ and $\pi$"
        is_valid = validate_math_delimiters(text)
        if (.not. is_valid) then
            print *, "ERROR: Multiple math expressions marked invalid"
            stop 1
        end if
        
        ! Invalid - odd number of delimiters
        text = "Bad $math$ extra $"
        is_valid = validate_math_delimiters(text)
        if (is_valid) then
            print *, "ERROR: Odd number of delimiters marked valid"
            stop 1
        end if
        
        print *, "test_paired_delimiter_validation: PASSED"
    end subroutine
    
    subroutine test_extract_math_content()
        character(len=100) :: text, math_content
        integer :: content_len
        
        ! Extract simple expression
        text = "Value is $\alpha = 2\pi$"
        call extract_math_content(text, 10, 24, math_content, content_len)
        if (content_len /= 13) then
            print *, "ERROR: Wrong content length:", content_len
            stop 1
        end if
        if (math_content(1:content_len) /= "\alpha = 2\pi") then
            print *, "ERROR: Wrong content: '", math_content(1:content_len), "'"
            stop 1
        end if
        
        ! Extract empty math
        text = "Empty $$"
        call extract_math_content(text, 7, 8, math_content, content_len)
        if (content_len /= 0) then
            print *, "ERROR: Empty math should have length 0"
            stop 1
        end if
        
        print *, "test_extract_math_content: PASSED"
    end subroutine
    
    subroutine test_multiple_math_expressions()
        character(len=100) :: text
        integer :: positions(10, 2), num_found
        
        ! Multiple expressions
        text = "First $\alpha$ then $\beta$ and $\gamma$"
        call find_all_math_expressions(text, positions, num_found)
        if (num_found /= 3) then
            print *, "ERROR: Expected 3 expressions, found", num_found
            stop 1
        end if
        
        ! Check positions
        if (positions(1, 1) /= 7 .or. positions(1, 2) /= 14) then
            print *, "ERROR: Wrong first expression position"
            stop 1
        end if
        if (positions(2, 1) /= 21 .or. positions(2, 2) /= 27) then
            print *, "ERROR: Wrong second expression position"
            stop 1
        end if
        if (positions(3, 1) /= 33 .or. positions(3, 2) /= 40) then
            print *, "ERROR: Wrong third expression position"
            stop 1
        end if
        
        print *, "test_multiple_math_expressions: PASSED"
    end subroutine
    
    subroutine test_escaped_dollar_signs()
        character(len=100) :: text
        character(len=100) :: processed
        integer :: proc_len
        
        ! Test escaped dollar signs
        text = "Cost is \$10 and $\alpha$ = 0.5"
        call process_escaped_dollars(text, processed, proc_len)
        
        ! Should have replaced \$ with just $
        if (index(processed(1:proc_len), "Cost is $10") == 0) then
            print *, "ERROR: Escaped dollar not processed correctly"
            stop 1
        end if
        
        ! Math delimiter should remain
        if (index(processed(1:proc_len), "$\alpha$") == 0) then
            print *, "ERROR: Math delimiters affected by escape processing"
            stop 1
        end if
        
        print *, "test_escaped_dollar_signs: PASSED"
    end subroutine
    
    subroutine test_nested_delimiters()
        character(len=100) :: text
        logical :: has_nested
        
        ! No nesting
        text = "$\alpha$ and $\beta$"
        has_nested = has_nested_delimiters(text)
        if (has_nested) then
            print *, "ERROR: False positive for nested delimiters"
            stop 1
        end if
        
        ! Nested delimiters (should be detected as error)
        ! The interpretation is: math starts at first $, but then we encounter
        ! another $ before closing, which would be nesting
        text = "$outer $inner$ text$"
        
        has_nested = has_nested_delimiters(text)
        if (.not. has_nested) then
            print *, "ERROR: Failed to detect nested delimiters"
            stop 1
        end if
        
        print *, "test_nested_delimiters: PASSED"
    end subroutine
    
    subroutine test_mixed_text_and_math()
        character(len=200) :: text, result
        integer :: result_len
        
        ! Mix of normal text and math
        text = "The formula $E = mc^2$ shows that energy equals mass times $c^2$"
        call parse_text_with_math(text, result, result_len)
        
        ! Should preserve structure
        if (result_len == 0) then
            print *, "ERROR: Failed to parse mixed text"
            stop 1
        end if
        
        ! Test complex case
        text = "Greek: $\alpha$, $\beta$, $\gamma$ are common"
        call parse_text_with_math(text, result, result_len)
        if (result_len == 0) then
            print *, "ERROR: Failed to parse text with multiple math expressions"
            stop 1
        end if
        
        print *, "test_mixed_text_and_math: PASSED"
    end subroutine
    

end program test_math_mode_parser