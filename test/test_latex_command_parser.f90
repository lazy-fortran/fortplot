program test_latex_command_parser
    use fortplot_latex_parser
    implicit none
    
    call test_basic_greek_letter_recognition()
    call test_all_lowercase_greek_letters()
    call test_all_uppercase_greek_letters()
    call test_invalid_command_handling()
    call test_mixed_text_and_commands()
    call test_multiple_commands_in_sequence()
    call test_command_at_boundaries()
    
    print *, "All LaTeX command parser tests passed!"
    
contains

    subroutine test_basic_greek_letter_recognition()
        character(len=100) :: text, command
        integer :: start_pos, end_pos
        logical :: found
        
        ! Test basic alpha command
        text = "\alpha"
        call find_latex_command(text, 1, start_pos, end_pos, found)
        if (.not. found) then
            print *, "ERROR: Failed to find \\alpha command"
            stop 1
        end if
        if (start_pos /= 1 .or. end_pos /= 6) then
            print *, "ERROR: Wrong positions for \\alpha:", start_pos, end_pos
            stop 1
        end if
        
        call extract_latex_command(text, start_pos, end_pos, command)
        if (trim(command) /= "alpha") then
            print *, "ERROR: Wrong command extracted: '", trim(command), "'"
            stop 1
        end if
        
        ! Test beta command in middle of text
        text = "The \beta value"
        call find_latex_command(text, 1, start_pos, end_pos, found)
        if (.not. found) then
            print *, "ERROR: Failed to find \\beta command in text"
            stop 1
        end if
        if (start_pos /= 5 .or. end_pos /= 9) then
            print *, "ERROR: Wrong positions for \\beta:", start_pos, end_pos
            stop 1
        end if
        
        print *, "test_basic_greek_letter_recognition: PASSED"
    end subroutine
    
    subroutine test_all_lowercase_greek_letters()
        character(len=20), parameter :: greek_letters(24) = [ &
            "alpha   ", "beta    ", "gamma   ", "delta   ", &
            "epsilon ", "zeta    ", "eta     ", "theta   ", &
            "iota    ", "kappa   ", "lambda  ", "mu      ", &
            "nu      ", "xi      ", "omicron ", "pi      ", &
            "rho     ", "sigma   ", "tau     ", "upsilon ", &
            "phi     ", "chi     ", "psi     ", "omega   " ]
        character(len=100) :: text
        integer :: i, start_pos, end_pos
        logical :: found
        
        do i = 1, 24
            text = "\" // trim(greek_letters(i))
            call find_latex_command(text, 1, start_pos, end_pos, found)
            if (.not. found) then
                print *, "ERROR: Failed to find command: \\", trim(greek_letters(i))
                stop 1
            end if
            if (start_pos /= 1) then
                print *, "ERROR: Wrong start position for \\", trim(greek_letters(i))
                stop 1
            end if
        end do
        
        print *, "test_all_lowercase_greek_letters: PASSED"
    end subroutine
    
    subroutine test_all_uppercase_greek_letters()
        character(len=20), parameter :: greek_letters(24) = [ &
            "Alpha   ", "Beta    ", "Gamma   ", "Delta   ", &
            "Epsilon ", "Zeta    ", "Eta     ", "Theta   ", &
            "Iota    ", "Kappa   ", "Lambda  ", "Mu      ", &
            "Nu      ", "Xi      ", "Omicron ", "Pi      ", &
            "Rho     ", "Sigma   ", "Tau     ", "Upsilon ", &
            "Phi     ", "Chi     ", "Psi     ", "Omega   " ]
        character(len=100) :: text
        integer :: i, start_pos, end_pos
        logical :: found
        
        do i = 1, 24
            text = "\" // trim(greek_letters(i))
            call find_latex_command(text, 1, start_pos, end_pos, found)
            if (.not. found) then
                print *, "ERROR: Failed to find command: \\", trim(greek_letters(i))
                stop 1
            end if
        end do
        
        print *, "test_all_uppercase_greek_letters: PASSED"
    end subroutine
    
    subroutine test_invalid_command_handling()
        character(len=100) :: text
        integer :: start_pos, end_pos
        logical :: found
        
        ! Test invalid command
        text = "\notgreek"
        call find_latex_command(text, 1, start_pos, end_pos, found)
        if (found) then
            print *, "ERROR: Invalid command \\notgreek was recognized"
            stop 1
        end if
        
        ! Test backslash without command
        text = "Just \\ backslash"
        call find_latex_command(text, 1, start_pos, end_pos, found)
        if (found) then
            print *, "ERROR: Lone backslash was recognized as command"
            stop 1
        end if
        
        ! Test incomplete command at end
        text = "Ends with \alph"
        call find_latex_command(text, 1, start_pos, end_pos, found)
        if (found) then
            print *, "ERROR: Incomplete command was recognized"
            stop 1
        end if
        
        print *, "test_invalid_command_handling: PASSED"
    end subroutine
    
    subroutine test_mixed_text_and_commands()
        character(len=100) :: text
        integer :: start_pos, end_pos
        logical :: found
        
        ! Test command mixed with normal text
        text = "The angle \theta is important"
        call find_latex_command(text, 1, start_pos, end_pos, found)
        if (.not. found) then
            print *, "ERROR: Failed to find \\theta in mixed text"
            stop 1
        end if
        
        ! Verify the position is correct
        if (text(start_pos:end_pos) /= "\theta") then
            print *, "ERROR: Wrong command found in mixed text"
            stop 1
        end if
        
        print *, "test_mixed_text_and_commands: PASSED"
    end subroutine
    
    subroutine test_multiple_commands_in_sequence()
        character(len=100) :: text
        integer :: commands(10, 2), num_found
        
        ! Test multiple commands
        text = "\alpha \beta \gamma"
        call find_all_latex_commands(text, commands, num_found)
        if (num_found /= 3) then
            print *, "ERROR: Expected 3 commands, found", num_found
            stop 1
        end if
        
        ! Check positions
        if (text(commands(1,1):commands(1,2)) /= "\alpha") then
            print *, "ERROR: First command wrong"
            stop 1
        end if
        if (text(commands(2,1):commands(2,2)) /= "\beta") then
            print *, "ERROR: Second command wrong"
            stop 1
        end if
        if (text(commands(3,1):commands(3,2)) /= "\gamma") then
            print *, "ERROR: Third command wrong"
            stop 1
        end if
        
        print *, "test_multiple_commands_in_sequence: PASSED"
    end subroutine
    
    subroutine test_command_at_boundaries()
        character(len=100) :: text
        integer :: start_pos, end_pos
        logical :: found
        
        ! Command at start
        text = "\alpha is first"
        call find_latex_command(text, 1, start_pos, end_pos, found)
        if (.not. found .or. start_pos /= 1) then
            print *, "ERROR: Command at start not found properly"
            stop 1
        end if
        
        ! Command at end
        text = "Last is \omega"
        call find_latex_command(text, 1, start_pos, end_pos, found)
        if (.not. found) then
            print *, "ERROR: Command at end not found"
            stop 1
        end if
        if (text(start_pos:end_pos) /= "\omega") then
            print *, "ERROR: Wrong command at end"
            stop 1
        end if
        
        print *, "test_command_at_boundaries: PASSED"
    end subroutine
    

end program test_latex_command_parser