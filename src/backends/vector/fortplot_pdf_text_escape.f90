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
        if (j < len(output)) output(j+1:) = ' '
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

        select case(codepoint)
        case(178)
            escape_seq = ''
            found = .false.
        case(179)
            escape_seq = ''
            found = .false.
        case(185)
            escape_seq = ''
            found = .false.
        case(215)
            escape_seq = 'x'
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

        select case(codepoint)
        case(945)
            escape_seq = '\\141'
        case(946)
            escape_seq = '\\142'
        case(947)
            escape_seq = '\\147'
        case(948)
            escape_seq = '\\144'
        case(949)
            escape_seq = '\\145'
        case(950)
            escape_seq = '\\172'
        case(951)
            escape_seq = '\\150'
        case(952)
            escape_seq = '\\161'
        case(953)
            escape_seq = '\\151'
        case(954)
            escape_seq = '\\153'
        case(955)
            escape_seq = '\\154'
        case(956)
            escape_seq = '\\155'
        case(957)
            escape_seq = '\\156'
        case(958)
            escape_seq = '\\170'
        case(959)
            escape_seq = '\\157'
        case(960)
            escape_seq = '\\160'
        case(961)
            escape_seq = '\\162'
        case(963)
            escape_seq = '\\163'
        case(964)
            escape_seq = '\\164'
        case(965)
            escape_seq = '\\165'
        case(966)
            escape_seq = '\\146'
        case(967)
            escape_seq = '\\143'
        case(968)
            escape_seq = '\\171'
        case(969)
            escape_seq = '\\167'
        case default
            found = .false.
        end select
    end subroutine lookup_lowercase_greek

    subroutine lookup_uppercase_greek(codepoint, escape_seq, found)
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: escape_seq
        logical, intent(out) :: found

        found = .true.

        select case(codepoint)
        case(913)
            escape_seq = '\\101'
        case(914)
            escape_seq = '\\102'
        case(915)
            escape_seq = '\\107'
        case(916)
            escape_seq = '\\104'
        case(917)
            escape_seq = '\\105'
        case(918)
            escape_seq = '\\132'
        case(919)
            escape_seq = '\\110'
        case(920)
            escape_seq = '\\121'
        case(921)
            escape_seq = '\\111'
        case(922)
            escape_seq = '\\113'
        case(923)
            escape_seq = '\\114'
        case(924)
            escape_seq = '\\115'
        case(925)
            escape_seq = '\\116'
        case(926)
            escape_seq = '\\130'
        case(927)
            escape_seq = '\\117'
        case(928)
            escape_seq = '\\120'
        case(929)
            escape_seq = '\\122'
        case(931)
            escape_seq = '\\123'
        case(932)
            escape_seq = '\\124'
        case(933)
            escape_seq = '\\125'
        case(934)
            escape_seq = '\\106'
        case(935)
            escape_seq = '\\103'
        case(936)
            escape_seq = '\\131'
        case(937)
            escape_seq = '\\127'
        case default
            found = .false.
        end select
    end subroutine lookup_uppercase_greek

end module fortplot_pdf_text_escape
