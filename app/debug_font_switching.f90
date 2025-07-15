program debug_font_switching
    use fortplot_pdf, only: pdf_context, draw_mixed_font_text
    use fortplot_latex_parser, only: process_latex_in_text
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    type(pdf_context) :: ctx
    character(len=100) :: test_text, processed_text
    integer :: processed_len, i
    
    ! Test cases that should reveal the space issue
    test_text = "A \\alpha B"
    
    print *, "=== Testing: '", trim(test_text), "' ==="
    
    call process_latex_in_text(test_text, processed_text, processed_len)
    
    print *, "Processed: '", processed_text(1:processed_len), "'"
    print *, "Length: ", processed_len
    
    ! Check each character
    print *, "Character by character:"
    do i = 1, processed_len
        print *, "  ", i, ": '", processed_text(i:i), "' (", iachar(processed_text(i:i)), ")"
    end do
    
end program debug_font_switching