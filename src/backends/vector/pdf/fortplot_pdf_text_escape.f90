module fortplot_pdf_text_escape
    !! PDF text escaping and symbol mapping utilities
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    private

    public :: escape_pdf_string
    public :: unicode_to_symbol_char
    public :: unicode_codepoint_to_pdf_escape
    public :: lookup_script_fallback
    public :: SCRIPT_SCALE, SUPERSCRIPT_RISE, SUBSCRIPT_DROP

    ! Unicode super/subscript glyphs without a WinAnsi/Symbol code are drawn
    ! as reduced Helvetica glyphs raised/lowered by a text rise (em fractions).
    real(wp), parameter :: SCRIPT_SCALE = 0.7_wp
    real(wp), parameter :: SUPERSCRIPT_RISE = 0.35_wp
    real(wp), parameter :: SUBSCRIPT_DROP = 0.15_wp

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

            ! Fortran has no backslash escape: achar(92) is a literal '\'.
            if (ch == '(' .or. ch == ')' .or. ch == achar(92)) then
                j = j + 1
                if (j <= len(output)) output(j:j) = achar(92)
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
        ! U+221A (square root) maps to octal \326 in Symbol encoding
        if (unicode_codepoint == 8730) then
            symbol_char = achar(92)//'326'
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

        ! Math relations and operators in Adobe Symbol encoding
        select case (unicode_codepoint)
        case (8733) ! U+221D proportional
            symbol_char = achar(92)//'265'
            return
        case (8776) ! U+2248 approxequal
            symbol_char = achar(92)//'273'
            return
        case (8764) ! U+223C similar
            symbol_char = achar(92)//'176'
            return
        case (8747) ! U+222B integral
            symbol_char = achar(92)//'362'
            return
        case (8711) ! U+2207 nabla / gradient
            symbol_char = achar(92)//'321'
            return
        case (8804) ! U+2264 lessequal
            symbol_char = achar(92)//'243'
            return
        case (8805) ! U+2265 greaterequal
            symbol_char = achar(92)//'263'
            return
        case (8800) ! U+2260 notequal
            symbol_char = achar(92)//'271'
            return
        case (8801) ! U+2261 equivalence
            symbol_char = achar(92)//'272'
            return
        end select

        call lookup_symbol_operator(unicode_codepoint, esc, found)
        if (found) then
            symbol_char = trim(esc)
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
        case (8722)
            ! U+2212 MINUS SIGN: the Helvetica font object remaps code 31
            ! (octal \037) to the /minus glyph via a Differences array, so
            ! this renders and extracts as a true typographic minus, matching
            ! matplotlib's negative tick labels.
            escape_seq = achar(92)//'037'
            found = .true.
        case (8211)
            ! U+2013 EN DASH maps directly to the WinAnsi en dash.
            escape_seq = achar(92)//'226'
            found = .true.
        case (8212)
            ! U+2014 EM DASH maps to the WinAnsi em dash (octal \227).
            escape_seq = achar(92)//'227'
            found = .true.
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

    subroutine lookup_symbol_operator(codepoint, escape_seq, found)
        !! Further operators, relations and arrows of the Adobe Symbol encoding
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: escape_seq
        logical, intent(out) :: found

        found = .true.
        escape_seq = ''
        select case (codepoint)
        case (8658) ! U+21D2 double arrow right
            escape_seq = achar(92)//'336'
        case (8656) ! U+21D0 double arrow left
            escape_seq = achar(92)//'334'
        case (8660) ! U+21D4 double arrow both
            escape_seq = achar(92)//'333'
        case (8657) ! U+21D1 double arrow up
            escape_seq = achar(92)//'335'
        case (8659) ! U+21D3 double arrow down
            escape_seq = achar(92)//'337'
        case (8629) ! U+21B5 carriage return
            escape_seq = achar(92)//'277'
        case (8712) ! U+2208 element of
            escape_seq = achar(92)//'316'
        case (8713) ! U+2209 not element of
            escape_seq = achar(92)//'317'
        case (8715) ! U+220B contains as member
            escape_seq = achar(92)//'047'
        case (8704) ! U+2200 for all
            escape_seq = achar(92)//'042'
        case (8707) ! U+2203 there exists
            escape_seq = achar(92)//'044'
        case (8721) ! U+2211 summation
            escape_seq = achar(92)//'345'
        case (8719) ! U+220F product
            escape_seq = achar(92)//'325'
        case (8727) ! U+2217 asterisk operator
            escape_seq = achar(92)//'052'
        case (8901) ! U+22C5 dot operator
            escape_seq = achar(92)//'327'
        case (8869) ! U+22A5 up tack
            escape_seq = achar(92)//'136'
        case (8773) ! U+2245 approximately equal
            escape_seq = achar(92)//'100'
        case (8736) ! U+2220 angle
            escape_seq = achar(92)//'320'
        case (8743) ! U+2227 logical and
            escape_seq = achar(92)//'331'
        case (8744) ! U+2228 logical or
            escape_seq = achar(92)//'332'
        case (8745) ! U+2229 intersection
            escape_seq = achar(92)//'307'
        case (8746) ! U+222A union
            escape_seq = achar(92)//'310'
        case (8834) ! U+2282 subset
            escape_seq = achar(92)//'314'
        case (8835) ! U+2283 superset
            escape_seq = achar(92)//'311'
        case (8838) ! U+2286 subset or equal
            escape_seq = achar(92)//'315'
        case (8839) ! U+2287 superset or equal
            escape_seq = achar(92)//'312'
        case (8836) ! U+2284 not subset
            escape_seq = achar(92)//'313'
        case (8709) ! U+2205 empty set
            escape_seq = achar(92)//'306'
        case (8855) ! U+2297 circled times
            escape_seq = achar(92)//'304'
        case (8853) ! U+2295 circled plus
            escape_seq = achar(92)//'305'
        case (8501) ! U+2135 alef
            escape_seq = achar(92)//'300'
        case (8472) ! U+2118 Weierstrass p
            escape_seq = achar(92)//'303'
        case (8465) ! U+2111 imaginary part
            escape_seq = achar(92)//'301'
        case (8476) ! U+211C real part
            escape_seq = achar(92)//'302'
        case (8756) ! U+2234 therefore
            escape_seq = achar(92)//'134'
        case (9001) ! U+2329 left angle bracket
            escape_seq = achar(92)//'341'
        case (10216) ! U+27E8 mathematical left angle bracket
            escape_seq = achar(92)//'341'
        case (9002) ! U+232A right angle bracket
            escape_seq = achar(92)//'361'
        case (10217) ! U+27E9 mathematical right angle bracket
            escape_seq = achar(92)//'361'
        case (8242) ! U+2032 prime
            escape_seq = achar(92)//'242'
        case (8243) ! U+2033 double prime
            escape_seq = achar(92)//'262'
        case default
            found = .false.
        end select
    end subroutine lookup_symbol_operator

    subroutine lookup_script_fallback(codepoint, base, rise)
        !! Unicode superscript/subscript characters absent from WinAnsi and
        !! Symbol: the base character and its rise (+1 super, -1 sub, 0 none).
        integer, intent(in) :: codepoint
        character(len=1), intent(out) :: base
        integer, intent(out) :: rise

        base = ' '
        rise = 0
        select case (codepoint)
        case (8304) ! U+2070 superscript zero
            base = '0'; rise = 1
        case (8305) ! U+2071 superscript i
            base = 'i'; rise = 1
        case (8308:8313) ! U+2074..U+2079 superscript four..nine
            base = achar(iachar('4') + codepoint - 8308); rise = 1
        case (8314) ! U+207A superscript plus
            base = '+'; rise = 1
        case (8315) ! U+207B superscript minus
            base = '-'; rise = 1
        case (8316) ! U+207C superscript equals
            base = '='; rise = 1
        case (8317) ! U+207D superscript left parenthesis
            base = '('; rise = 1
        case (8318) ! U+207E superscript right parenthesis
            base = ')'; rise = 1
        case (8319) ! U+207F superscript n
            base = 'n'; rise = 1
        case (8320:8329) ! U+2080..U+2089 subscript zero..nine
            base = achar(iachar('0') + codepoint - 8320); rise = -1
        case (8330) ! U+208A subscript plus
            base = '+'; rise = -1
        case (8331) ! U+208B subscript minus
            base = '-'; rise = -1
        case (8332) ! U+208C subscript equals
            base = '='; rise = -1
        case (8333) ! U+208D subscript left parenthesis
            base = '('; rise = -1
        case (8334) ! U+208E subscript right parenthesis
            base = ')'; rise = -1
        case (8336) ! U+2090 subscript a
            base = 'a'; rise = -1
        case (8337) ! U+2091 subscript e
            base = 'e'; rise = -1
        case (8338) ! U+2092 subscript o
            base = 'o'; rise = -1
        case (8339) ! U+2093 subscript x
            base = 'x'; rise = -1
        end select
    end subroutine lookup_script_fallback

end module fortplot_pdf_text_escape
