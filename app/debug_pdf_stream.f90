program debug_pdf_stream
    use fortplot_latex_parser, only: process_latex_in_text
    implicit none
    
    character(len=100) :: test_text, processed_text
    integer :: processed_len
    
    print *, "=== Debug PDF Text Processing ==="
    
    ! Test LaTeX to Unicode conversion
    test_text = "sin(\omega t)"
    call process_latex_in_text(test_text, processed_text, processed_len)
    print *, "Original: '", trim(test_text), "'"
    print *, "Processed: '", processed_text(1:processed_len), "'"
    print *, "Processed length:", processed_len
    
    ! Check character by character
    block
        integer :: i
        do i = 1, processed_len
            print *, "  Char", i, ": '", processed_text(i:i), "' ASCII:", iachar(processed_text(i:i))
        end do
    end block
    
    ! Test another example
    print *, ""
    test_text = "f(\alpha, \beta)"
    call process_latex_in_text(test_text, processed_text, processed_len)
    print *, "Original: '", trim(test_text), "'"
    print *, "Processed: '", processed_text(1:processed_len), "'"
    
end program debug_pdf_stream