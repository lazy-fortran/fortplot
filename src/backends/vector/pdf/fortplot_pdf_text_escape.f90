module fortplot_pdf_text_escape
    !! PDF text escaping and symbol mapping utilities

    implicit none
    private

    public :: escape_pdf_string
    public :: unicode_to_symbol_char
    public :: unicode_codepoint_to_pdf_escape

contains

    subroutine escape_pdf_string(input, output, output_len)
        !! Escape special characters in PDF strings
        character(len=*), intent(in) :: input
        character(len=*), intent(out) :: output
        integer, intent(out) :: output_len
        integer :: i, j, n
        character :: ch

        n = len(input)
        j = 0
        do i = 1, n
            ch = input(i:i)

            if (ch == '(' .or. ch == ')' .or. ch == '\\') then
                j = j + 1
                if (j <= len(output)) output(j:j) = '\\'
                j = j + 1
                if (j <= len(output)) output(j:j) = ch
            else
                j = j + 1
                if (j <= len(output)) output(j:j) = ch
            end if
        end do

        output_len = j
        if (j < len(output)) output(j + 1:) = ' '
    end subroutine escape_pdf_string

    subroutine unicode_to_symbol_char(unicode_codepoint, symbol_char)
        !! Convert Unicode codepoint to Symbol font character
        integer, intent(in) :: unicode_codepoint
        character(len=*), intent(out) :: symbol_char
        character(len=8) :: esc
        logical :: found

        symbol_char = ''
        esc = ''
        found = .false.

        ! Common math symbols supported by Symbol font
        ! U+221A (square root) maps to octal \214 in Symbol encoding
        if (unicode_codepoint == 8730) then
            symbol_char = achar(92)//'214'
            return
        end if

        ! U+221E (infinity) maps to octal \245 in Symbol encoding
        if (unicode_codepoint == 8734) then
            symbol_char = achar(92)//'245'
            return
        end if

        ! U+2202 (partial differential) maps to octal \266 in Symbol encoding
        if (unicode_codepoint == 8706) then
            symbol_char = achar(92)//'266'
            return
        end if

        ! Arrows in Symbol encoding
        select case (unicode_codepoint)
        case (8592)
            symbol_char = achar(92)//'254'
            return
        case (8593)
            symbol_char = achar(92)//'255'
            return
        case (8594)
            symbol_char = achar(92)//'256'
            return
        case (8595)
            symbol_char = achar(92)//'257'
            return
        case (8596)
            symbol_char = achar(92)//'253'
            return
        end select

        call lookup_lowercase_greek(unicode_codepoint, esc, found)
        if (.not. found) call lookup_uppercase_greek(unicode_codepoint, esc, found)

        if (found) symbol_char = trim(esc)
    end subroutine unicode_to_symbol_char

    subroutine unicode_codepoint_to_pdf_escape(codepoint, escape_seq)
        !! Convert Unicode codepoint to PDF escape sequence
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: escape_seq
        logical :: found

        escape_seq = ''
        found = .false.

        select case (codepoint)
        case (188)
            escape_seq = achar(92)//'274'
            found = .true.
        case (176)
            escape_seq = achar(92)//'260'
            found = .true.
        case (177)
            escape_seq = achar(92)//'261'
            found = .true.
        case (178)
            escape_seq = achar(92)//'262'
            found = .true.
        case (179)
            escape_seq = achar(92)//'263'
            found = .true.
        case (181)
            escape_seq = achar(92)//'265'
            found = .true.
        case (183)
            escape_seq = achar(92)//'267'
            found = .true.
        case (185)
            escape_seq = achar(92)//'271'
            found = .true.
        case (215)
            escape_seq = achar(92)//'327'
            found = .true.
        case (247)
            escape_seq = achar(92)//'367'
            found = .true.
        end select

        if (.not. found) then
            call lookup_lowercase_greek(codepoint, escape_seq, found)
        end if

        if (.not. found) then
            call lookup_uppercase_greek(codepoint, escape_seq, found)
        end if

        if (.not. found) escape_seq = ''
    end subroutine unicode_codepoint_to_pdf_escape

    subroutine lookup_lowercase_greek(codepoint, escape_seq, found)
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: escape_seq
        logical, intent(out) :: found

        found = .true.

        select case (codepoint)
        case (945)
            escape_seq = achar(92)//'141'
        case (946)
            escape_seq = achar(92)//'142'
        case (947)
            escape_seq = achar(92)//'147'
        case (948)
            escape_seq = achar(92)//'144'
        case (949)
            escape_seq = achar(92)//'145'
        case (950)
            escape_seq = achar(92)//'172'
        case (951)
            escape_seq = achar(92)//'150'
        case (952)
            escape_seq = achar(92)//'161'
        case (953)
            escape_seq = achar(92)//'151'
        case (954)
            escape_seq = achar(92)//'153'
        case (955)
            escape_seq = achar(92)//'154'
        case (956)
            escape_seq = achar(92)//'155'
        case (957)
            escape_seq = achar(92)//'156'
        case (958)
            escape_seq = achar(92)//'170'
        case (959)
            escape_seq = achar(92)//'157'
        case (960)
            escape_seq = achar(92)//'160'
        case (961)
            escape_seq = achar(92)//'162'
        case (963)
            escape_seq = achar(92)//'163'
        case (964)
            escape_seq = achar(92)//'164'
        case (965)
            escape_seq = achar(92)//'165'
        case (966)
            escape_seq = achar(92)//'146'
        case (967)
            escape_seq = achar(92)//'143'
        case (968)
            escape_seq = achar(92)//'171'
        case (969)
            escape_seq = achar(92)//'167'
        case default
            found = .false.
        end select
    end subroutine lookup_lowercase_greek

    subroutine lookup_uppercase_greek(codepoint, escape_seq, found)
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: escape_seq
        logical, intent(out) :: found

        found = .true.

        select case (codepoint)
        case (913)
            escape_seq = achar(92)//'101'
        case (914)
            escape_seq = achar(92)//'102'
        case (915)
            escape_seq = achar(92)//'107'
        case (916)
            escape_seq = achar(92)//'104'
        case (917)
            escape_seq = achar(92)//'105'
        case (918)
            escape_seq = achar(92)//'132'
        case (919)
            escape_seq = achar(92)//'110'
        case (920)
            escape_seq = achar(92)//'121'
        case (921)
            escape_seq = achar(92)//'111'
        case (922)
            escape_seq = achar(92)//'113'
        case (923)
            escape_seq = achar(92)//'114'
        case (924)
            escape_seq = achar(92)//'115'
        case (925)
            escape_seq = achar(92)//'116'
        case (926)
            escape_seq = achar(92)//'130'
        case (927)
            escape_seq = achar(92)//'117'
        case (928)
            escape_seq = achar(92)//'120'
        case (929)
            escape_seq = achar(92)//'122'
        case (931)
            escape_seq = achar(92)//'123'
        case (932)
            escape_seq = achar(92)//'124'
        case (933)
            escape_seq = achar(92)//'125'
        case (934)
            escape_seq = achar(92)//'106'
        case (935)
            escape_seq = achar(92)//'103'
        case (936)
            escape_seq = achar(92)//'131'
        case (937)
            escape_seq = achar(92)//'127'
        case default
            found = .false.
        end select
    end subroutine lookup_uppercase_greek

end module fortplot_pdf_text_escape
