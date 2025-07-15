program debug_pdf_escape
    implicit none
    
    character(len=20) :: input, output
    integer :: output_len, i, j
    
    input = "A "  ! Text with trailing space
    
    print *, "=== PDF escape test ==="
    print *, "Input: '", input, "'"
    print *, "Input length: ", len_trim(input)
    
    ! Simulate the exact escape logic
    j = 1
    do i = 1, len_trim(input)
        print *, "Processing char ", i, ": '", input(i:i), "'"
        
        ! Escape parentheses and backslashes in PDF strings
        if (input(i:i) == '(' .or. input(i:i) == ')' .or. input(i:i) == '\') then
            output(j:j) = '\'
            j = j + 1
        end if
        output(j:j) = input(i:i)
        j = j + 1
    end do
    output_len = j - 1
    
    print *, "Output: '", output(1:output_len), "'"
    print *, "Output length: ", output_len
    
    ! Check each character
    print *, "Output characters:"
    do i = 1, output_len
        print *, "  ", i, ": '", output(i:i), "' (", iachar(output(i:i)), ")"
    end do
    
end program debug_pdf_escape