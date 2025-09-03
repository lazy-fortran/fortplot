program test_unicode_issue_1138
    !! Test for issue #1138: \Psi with capital P not working
    use fortplot_latex_parser, only: process_latex_in_text
    implicit none
    
    character(len=200) :: input_text, result_text
    integer :: result_len
    integer :: i
    
    ! Test various Greek letters including \Psi
    print *, "=== Testing LaTeX to Unicode conversion ==="
    print *, ""
    
    ! Test 1: Lowercase psi
    input_text = "Wave function: \psi"
    call process_latex_in_text(input_text, result_text, result_len)
    print *, "Input:  '", trim(input_text), "'"
    print *, "Output: '", result_text(1:result_len), "'"
    print *, ""
    
    ! Test 2: Uppercase Psi
    input_text = "Wave function: \Psi"
    call process_latex_in_text(input_text, result_text, result_len)
    print *, "Input:  '", trim(input_text), "'"
    print *, "Output: '", result_text(1:result_len), "'"
    print *, ""
    
    ! Test 3: Mix of uppercase Greek letters
    input_text = "Uppercase: \Alpha \Beta \Gamma \Delta \Psi \Omega"
    call process_latex_in_text(input_text, result_text, result_len)
    print *, "Input:  '", trim(input_text), "'"
    print *, "Output: '", result_text(1:result_len), "'"
    print *, ""
    
    ! Test 4: From the demo - the exact text that's failing
    input_text = "Amplitude \Psi (V)"
    call process_latex_in_text(input_text, result_text, result_len)
    print *, "Input:  '", trim(input_text), "'"
    print *, "Output: '", result_text(1:result_len), "'"
    
    ! Show the bytes
    print *, "Output bytes:"
    do i = 1, result_len
        print '(I3, ": ", A1, " (", I3, ")")', i, result_text(i:i), iachar(result_text(i:i))
    end do
    print *, ""
    
    ! Test 5: Another from the demo
    input_text = "Wave Functions: \psi(\omega t) = A e^{-\lambda t} sin(\omega t + \phi)"
    call process_latex_in_text(input_text, result_text, result_len)
    print *, "Input:  '", trim(input_text), "'"
    print *, "Output: '", result_text(1:result_len), "'"
    print *, ""
    
end program test_unicode_issue_1138
