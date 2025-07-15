program debug_mixed_font_detailed
    use fortplot_pdf, only: pdf_context, create_pdf_canvas
    use fortplot_latex_parser, only: process_latex_in_text
    use fortplot_unicode, only: utf8_char_length, utf8_to_codepoint
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(pdf_context) :: ctx
    character(len=100) :: test_text, processed_text
    integer :: processed_len, i, char_len, codepoint
    character(len=1) :: current_char
    
    ! Test the exact problematic case
    test_text = "A \\alpha B"
    
    print *, "=== Manual font switching simulation ==="
    print *, "Original: '", trim(test_text), "'"
    
    call process_latex_in_text(test_text, processed_text, processed_len)
    print *, "Processed: '", processed_text(1:processed_len), "'"
    print *, "Length: ", processed_len
    print *, ""
    
    ! Simulate the exact logic from draw_mixed_font_text
    i = 1
    do while (i <= processed_len)
        current_char = processed_text(i:i)
        
        print *, "Position ", i, ": '", current_char, "' (", iachar(current_char), ")"
        
        if (iachar(current_char) > 127) then
            ! Unicode character
            char_len = utf8_char_length(current_char)
            print *, "  -> Unicode character, length: ", char_len
            
            if (char_len > 0 .and. i + char_len - 1 <= processed_len) then
                codepoint = utf8_to_codepoint(processed_text, i)
                print *, "  -> Codepoint: ", codepoint
                
                if (codepoint == 945) then  ! α
                    print *, "  -> Greek alpha detected!"
                    print *, "  -> Would switch to Symbol font here"
                end if
                
                i = i + char_len
            else
                print *, "  -> Invalid UTF-8, skipping"
                i = i + 1
            end if
        else
            ! ASCII character
            if (current_char == ' ') then
                print *, "  -> SPACE character detected"
                print *, "  -> Would be in Helvetica font"
            else
                print *, "  -> ASCII character: '", current_char, "'"
            end if
            i = i + 1
        end if
        print *, ""
    end do
    
end program debug_mixed_font_detailed