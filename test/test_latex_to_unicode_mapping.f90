program test_latex_to_unicode_mapping
    use fortplot_latex_parser
    use fortplot_unicode
    implicit none
    
    call test_basic_greek_letter_mapping()
    call test_all_lowercase_greek_mapping()
    call test_all_uppercase_greek_mapping()
    call test_mixed_latex_and_text()
    call test_invalid_commands_ignored()
    call test_complete_text_processing()
    
    print *, "All LaTeX to Unicode mapping tests passed!"
    
contains

    subroutine test_basic_greek_letter_mapping()
        character(len=20) :: latex_cmd, unicode_char
        integer :: codepoint
        logical :: success
        
        ! Test alpha mapping
        latex_cmd = "alpha"
        call latex_to_unicode(latex_cmd, unicode_char, success)
        if (.not. success) then
            print *, "ERROR: Failed to map alpha to Unicode"
            stop 1
        end if
        
        ! Check that we get the correct Unicode character
        codepoint = utf8_to_codepoint(unicode_char, 1)
        if (codepoint /= 945) then  ! U+03B1 = 945
            print *, "ERROR: Alpha mapping wrong codepoint:", codepoint
            stop 1
        end if
        
        ! Test Omega mapping
        latex_cmd = "Omega"
        call latex_to_unicode(latex_cmd, unicode_char, success)
        if (.not. success) then
            print *, "ERROR: Failed to map Omega to Unicode"
            stop 1
        end if
        
        codepoint = utf8_to_codepoint(unicode_char, 1)
        if (codepoint /= 937) then  ! U+03A9 = 937
            print *, "ERROR: Omega mapping wrong codepoint:", codepoint
            stop 1
        end if
        
        print *, "test_basic_greek_letter_mapping: PASSED"
    end subroutine
    
    subroutine test_all_lowercase_greek_mapping()
        character(len=20), parameter :: greek_letters(5) = [ &
            "alpha   ", "beta    ", "gamma   ", "delta   ", "epsilon " ]
        integer, parameter :: expected_codepoints(5) = [945, 946, 947, 948, 949]
        character(len=20) :: unicode_char
        integer :: i, codepoint
        logical :: success
        
        do i = 1, 5
            call latex_to_unicode(trim(greek_letters(i)), unicode_char, success)
            if (.not. success) then
                print *, "ERROR: Failed to map", trim(greek_letters(i))
                stop 1
            end if
            
            codepoint = utf8_to_codepoint(unicode_char, 1)
            if (codepoint /= expected_codepoints(i)) then
                print *, "ERROR: Wrong codepoint for", trim(greek_letters(i)), codepoint
                stop 1
            end if
        end do
        
        print *, "test_all_lowercase_greek_mapping: PASSED"
    end subroutine
    
    subroutine test_all_uppercase_greek_mapping()
        character(len=20), parameter :: greek_letters(3) = [ &
            "Alpha   ", "Beta    ", "Gamma   " ]
        integer, parameter :: expected_codepoints(3) = [913, 914, 915]
        character(len=20) :: unicode_char
        integer :: i, codepoint
        logical :: success
        
        do i = 1, 3
            call latex_to_unicode(trim(greek_letters(i)), unicode_char, success)
            if (.not. success) then
                print *, "ERROR: Failed to map", trim(greek_letters(i))
                stop 1
            end if
            
            codepoint = utf8_to_codepoint(unicode_char, 1)
            if (codepoint /= expected_codepoints(i)) then
                print *, "ERROR: Wrong codepoint for", trim(greek_letters(i)), codepoint
                stop 1
            end if
        end do
        
        print *, "test_all_uppercase_greek_mapping: PASSED"
    end subroutine
    
    subroutine test_mixed_latex_and_text()
        character(len=200) :: input_text, result_text
        integer :: result_len
        
        ! Process text with LaTeX commands
        input_text = "The value $\alpha = 2\pi$ is important"
        call process_latex_in_text(input_text, result_text, result_len)
        print *, 'DEBUG mixed:', result_text(1:result_len)
        
        if (result_len == 0) then
            print *, "ERROR: Failed to process mixed text"
            stop 1
        end if
        
        ! Result should contain Unicode characters
        if (.not. contains_unicode(result_text(1:result_len))) then
            print *, "ERROR: Processed text should contain Unicode"
            stop 1
        end if
        
        print *, "test_mixed_latex_and_text: PASSED"
    end subroutine
    
    subroutine test_invalid_commands_ignored()
        character(len=20) :: unicode_char
        logical :: success
        
        ! Test invalid command
        call latex_to_unicode("notgreek", unicode_char, success)
        if (success) then
            print *, "ERROR: Invalid command was mapped"
            stop 1
        end if
        
        ! Test empty command
        call latex_to_unicode("", unicode_char, success)
        if (success) then
            print *, "ERROR: Empty command was mapped"
            stop 1
        end if
        
        print *, "test_invalid_commands_ignored: PASSED"
    end subroutine
    
    subroutine test_complete_text_processing()
        character(len=200) :: input_text, result_text
        integer :: result_len
        
        ! Complex text with multiple commands
        input_text = "Equation: $\alpha + \beta = \gamma$"
        call process_latex_in_text(input_text, result_text, result_len)
        print *, 'DEBUG complex:', result_text(1:result_len)
        
        if (result_len == 0) then
            print *, "ERROR: Failed to process complex text"
            stop 1
        end if
        
        ! Should start with "Equation: " and contain Unicode
        if (result_text(1:10) /= "Equation: ") then
            print *, "ERROR: Text processing altered non-LaTeX content"
            stop 1
        end if
        
        if (.not. contains_unicode(result_text(1:result_len))) then
            print *, "ERROR: No Unicode found in processed text"
            stop 1
        end if
        
        print *, "test_complete_text_processing: PASSED"
    end subroutine

end program test_latex_to_unicode_mapping
