program test_pdf_escape
    implicit none
    
    character(len=100) :: input, output
    integer :: i, j
    
    ! Test escaping parentheses
    input = "sin(omega)"
    output = ""
    j = 1
    
    do i = 1, len_trim(input)
        if (input(i:i) == '(' .or. input(i:i) == ')' .or. input(i:i) == '\') then
            output(j:j) = '\'
            j = j + 1
        end if
        output(j:j) = input(i:i)
        j = j + 1
    end do
    
    print *, "Original: '", trim(input), "'"
    print *, "Escaped:  '", output(1:j-1), "'"
    
end program test_pdf_escape