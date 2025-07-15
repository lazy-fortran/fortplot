program debug_text_processing
    use fortplot_latex_parser
    implicit none
    
    character(len=100) :: input_text, result_text
    integer :: result_len
    
    ! Test the specific problematic text
    input_text = "Time \\tau (normalized: \\tau = \\omega t / 2\\pi)"
    
    print *, "Original text: '", trim(input_text), "'"
    print *, "Length: ", len_trim(input_text)
    
    call process_latex_in_text(input_text, result_text, result_len)
    
    print *, "Processed text: '", result_text(1:result_len), "'"
    print *, "Processed length: ", result_len
    print *, "len_trim of processed: ", len_trim(result_text(1:result_len))
    
end program debug_text_processing