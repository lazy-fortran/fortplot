module fortplot_latex_parser
    !! LaTeX command parser for Greek letters and mathematical symbols
    implicit none
    
    private
    public :: find_latex_command, extract_latex_command, find_all_latex_commands
    public :: is_valid_greek_command, latex_to_unicode, process_latex_in_text
    
    ! Greek letter command mappings
    character(len=*), parameter :: LOWERCASE_GREEK(24) = [ &
        "alpha   ", "beta    ", "gamma   ", "delta   ", &
        "epsilon ", "zeta    ", "eta     ", "theta   ", &
        "iota    ", "kappa   ", "lambda  ", "mu      ", &
        "nu      ", "xi      ", "omicron ", "pi      ", &
        "rho     ", "sigma   ", "tau     ", "upsilon ", &
        "phi     ", "chi     ", "psi     ", "omega   " ]
    
    character(len=*), parameter :: UPPERCASE_GREEK(24) = [ &
        "Alpha   ", "Beta    ", "Gamma   ", "Delta   ", &
        "Epsilon ", "Zeta    ", "Eta     ", "Theta   ", &
        "Iota    ", "Kappa   ", "Lambda  ", "Mu      ", &
        "Nu      ", "Xi      ", "Omicron ", "Pi      ", &
        "Rho     ", "Sigma   ", "Tau     ", "Upsilon ", &
        "Phi     ", "Chi     ", "Psi     ", "Omega   " ]
    
    ! Unicode codepoints for lowercase Greek letters
    integer, parameter :: LOWERCASE_CODEPOINTS(24) = [ &
        945, 946, 947, 948, 949, 950, 951, 952, &
        953, 954, 955, 956, 957, 958, 959, 960, &
        961, 963, 964, 965, 966, 967, 968, 969 ]
    
    ! Unicode codepoints for uppercase Greek letters  
    integer, parameter :: UPPERCASE_CODEPOINTS(24) = [ &
        913, 914, 915, 916, 917, 918, 919, 920, &
        921, 922, 923, 924, 925, 926, 927, 928, &
        929, 931, 932, 933, 934, 935, 936, 937 ]

contains

    subroutine find_latex_command(text, start, start_pos, end_pos, found)
        character(len=*), intent(in) :: text
        integer, intent(in) :: start
        integer, intent(out) :: start_pos, end_pos
        logical, intent(out) :: found
        integer :: i, text_len, cmd_start, cmd_end
        
        found = .false.
        start_pos = 0
        end_pos = 0
        text_len = len_trim(text)
        
        ! Look for backslash
        do i = start, text_len
            if (text(i:i) == '\') then
                cmd_start = i
                
                ! Find end of command (next non-alphabetic character)
                cmd_end = cmd_start + 1  ! Start after backslash
                do while (cmd_end <= text_len)
                    if (.not. is_alpha(text(cmd_end:cmd_end))) then
                        cmd_end = cmd_end - 1
                        exit
                    end if
                    cmd_end = cmd_end + 1
                end do
                if (cmd_end > text_len) cmd_end = text_len
                
                ! Validate command
                if (cmd_end > cmd_start) then
                    if (is_valid_greek_command(text(cmd_start:cmd_end))) then
                        start_pos = cmd_start
                        end_pos = cmd_end
                        found = .true.
                        return
                    end if
                end if
            end if
        end do
    end subroutine find_latex_command
    
    subroutine extract_latex_command(text, start_pos, end_pos, command)
        character(len=*), intent(in) :: text
        integer, intent(in) :: start_pos, end_pos
        character(len=*), intent(out) :: command
        
        command = ""
        if (start_pos >= 1 .and. end_pos <= len(text) .and. start_pos <= end_pos) then
            if (text(start_pos:start_pos) == '\') then
                ! Extract command without the backslash
                command = text(start_pos+1:end_pos)
            end if
        end if
    end subroutine extract_latex_command
    
    subroutine find_all_latex_commands(text, commands, num_found)
        character(len=*), intent(in) :: text
        integer, intent(out) :: commands(:,:)
        integer, intent(out) :: num_found
        integer :: start, start_pos, end_pos
        logical :: found
        
        num_found = 0
        start = 1
        
        do while (start <= len_trim(text) .and. num_found < size(commands, 1))
            call find_latex_command(text, start, start_pos, end_pos, found)
            if (.not. found) exit
            
            num_found = num_found + 1
            commands(num_found, 1) = start_pos
            commands(num_found, 2) = end_pos
            
            start = end_pos + 1
        end do
    end subroutine find_all_latex_commands
    
    logical function is_valid_greek_command(command_text)
        character(len=*), intent(in) :: command_text
        character(len=20) :: command_name
        integer :: i
        
        is_valid_greek_command = .false.
        
        ! Extract command name (without backslash)
        if (len(command_text) < 2 .or. command_text(1:1) /= '\') return
        
        command_name = command_text(2:)
        
        ! Check lowercase Greek letters
        do i = 1, 24
            if (trim(command_name) == trim(LOWERCASE_GREEK(i))) then
                is_valid_greek_command = .true.
                return
            end if
        end do
        
        ! Check uppercase Greek letters
        do i = 1, 24
            if (trim(command_name) == trim(UPPERCASE_GREEK(i))) then
                is_valid_greek_command = .true.
                return
            end if
        end do
    end function is_valid_greek_command
    
    logical function is_alpha(ch)
        character(len=1), intent(in) :: ch
        integer :: ascii_val
        
        ascii_val = iachar(ch)
        is_alpha = (ascii_val >= iachar('A') .and. ascii_val <= iachar('Z')) .or. &
                   (ascii_val >= iachar('a') .and. ascii_val <= iachar('z'))
    end function is_alpha
    
    subroutine latex_to_unicode(latex_command, unicode_char, success)
        character(len=*), intent(in) :: latex_command
        character(len=*), intent(out) :: unicode_char
        logical, intent(out) :: success
        integer :: i, codepoint
        
        success = .false.
        unicode_char = ""
        
        if (len_trim(latex_command) == 0) return
        
        ! Check lowercase Greek letters
        do i = 1, 24
            if (trim(latex_command) == trim(LOWERCASE_GREEK(i))) then
                codepoint = LOWERCASE_CODEPOINTS(i)
                call codepoint_to_utf8(codepoint, unicode_char)
                success = .true.
                return
            end if
        end do
        
        ! Check uppercase Greek letters
        do i = 1, 24
            if (trim(latex_command) == trim(UPPERCASE_GREEK(i))) then
                codepoint = UPPERCASE_CODEPOINTS(i)
                call codepoint_to_utf8(codepoint, unicode_char)
                success = .true.
                return
            end if
        end do
    end subroutine latex_to_unicode
    
    subroutine codepoint_to_utf8(codepoint, utf8_char)
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: utf8_char
        
        utf8_char = ""
        
        if (codepoint <= 127) then
            ! 1-byte sequence
            utf8_char(1:1) = achar(codepoint)
        else if (codepoint <= 2047) then
            ! 2-byte sequence
            utf8_char(1:1) = achar(192 + (codepoint / 64))
            utf8_char(2:2) = achar(128 + mod(codepoint, 64))
        else if (codepoint <= 65535) then
            ! 3-byte sequence
            utf8_char(1:1) = achar(224 + (codepoint / 4096))
            utf8_char(2:2) = achar(128 + mod(codepoint / 64, 64))
            utf8_char(3:3) = achar(128 + mod(codepoint, 64))
        else
            ! 4-byte sequence (not needed for Greek letters)
            utf8_char(1:1) = achar(240 + (codepoint / 262144))
            utf8_char(2:2) = achar(128 + mod(codepoint / 4096, 64))
            utf8_char(3:3) = achar(128 + mod(codepoint / 64, 64))
            utf8_char(4:4) = achar(128 + mod(codepoint, 64))
        end if
    end subroutine codepoint_to_utf8
    
    subroutine process_latex_in_text(input_text, result_text, result_len)
        !! Convert LaTeX-style commands to Unicode while preserving math scopes
        character(len=*), intent(in) :: input_text
        character(len=*), intent(out) :: result_text
        integer, intent(out) :: result_len
        integer :: i, pos, n
        integer :: cmd_end
        character(len=20) :: command, unicode_char
        logical :: success

        result_text = ""
        result_len = 0
        pos = 1
        n = len_trim(input_text)
        i = 1

        do while (i <= n)
            if (input_text(i:i) == '$') then
                result_text(pos:pos) = '$'
                pos = pos + 1
                i = i + 1
                cycle
            end if

            if (input_text(i:i) == '\') then
                cmd_end = i + 1
                do while (cmd_end <= n)
                    if (.not. is_alpha(input_text(cmd_end:cmd_end))) exit
                    cmd_end = cmd_end + 1
                end do
                if (cmd_end - 1 > i) then
                    call extract_latex_command(input_text, i, cmd_end - 1, command)
                    call latex_to_unicode(trim(command), unicode_char, success)
                    if (success) then
                        result_text(pos:pos + len_trim(unicode_char) - 1) = trim(unicode_char)
                        pos = pos + len_trim(unicode_char)
                        i = cmd_end
                        cycle
                    end if
                end if
                ! If not a recognized command, fall through and copy the backslash
            end if

            result_text(pos:pos) = input_text(i:i)
            pos = pos + 1
            i = i + 1
        end do

        result_len = pos - 1
    end subroutine process_latex_in_text

end module fortplot_latex_parser
