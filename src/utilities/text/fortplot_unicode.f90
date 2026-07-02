!! Unicode text processing utilities for raster rendering
!!
!! This module provides Unicode-to-ASCII conversion functionality
!! for raster rendering backends that need text fallback.
module fortplot_unicode
    implicit none
    private
    
    public :: escape_unicode_for_raster
    public :: escape_unicode_for_ascii
    public :: unicode_codepoint_to_ascii
    public :: codepoint_to_lowercase_greek
    public :: codepoint_to_uppercase_greek
    public :: codepoint_to_default_placeholder
    public :: utf8_to_codepoint
    public :: utf8_char_length
    public :: contains_unicode, is_unicode_char, check_utf8_sequence, is_greek_letter_codepoint
    public :: ascii_minus_to_unicode

    ! UTF-8 byte sequence for U+2212 MINUS SIGN (E2 88 92)
    character(len=*), parameter :: UNICODE_MINUS = achar(226)//achar(136)//achar(146)

contains

    function ascii_minus_to_unicode(label) result(converted)
        !! Replace a leading ASCII hyphen-minus (U+002D) marking a negative
        !! number with the typographic minus sign U+2212, matching
        !! matplotlib's default tick/colorbar labels. Only the sign position
        !! is converted; interior hyphens (none occur in numeric labels) and
        !! non-negative labels pass through unchanged.
        character(len=*), intent(in) :: label
        character(len=len(label)+2) :: converted
        character(len=:), allocatable :: trimmed

        converted = label
        trimmed = adjustl(label)
        if (len_trim(trimmed) < 2) return
        if (trimmed(1:1) /= '-') return
        ! Require a digit or decimal point right after the sign so we only
        ! rewrite numeric labels, never words that begin with a hyphen.
        if (index('0123456789.', trimmed(2:2)) == 0) return

        converted = UNICODE_MINUS//trim(trimmed(2:))
    end function ascii_minus_to_unicode

    subroutine escape_unicode_for_raster(input_text, escaped_text)
        !! Pass through Unicode for raster rendering (STB TrueType supports Unicode)
        character(len=*), intent(in) :: input_text
        character(len=*), intent(out) :: escaped_text
        
        ! STB TrueType can handle Unicode directly, so just pass through
        escaped_text = input_text
    end subroutine escape_unicode_for_raster

    subroutine escape_unicode_for_ascii(input_text, escaped_text)
        !! Convert Unicode text to ASCII-compatible text for ASCII backend
        !! Processes UTF-8 input and converts Unicode characters to readable ASCII equivalents
        character(len=*), intent(in) :: input_text
        character(len=*), intent(out) :: escaped_text
        integer :: i, char_len, codepoint, out_pos
        character(len=50) :: ascii_equiv
        logical :: is_valid, is_word, prev_was_word

        escaped_text = ""
        i = 1
        out_pos = 1
        prev_was_word = .false.

        do while (i <= len_trim(input_text) .and. out_pos <= len(escaped_text))
            char_len = utf8_char_length(input_text(i:i))

            if (char_len == 1) then
                ! ASCII character - copy directly, keeping a boundary space after
                ! a transliterated word symbol so tokens do not collide.
                if (prev_was_word) then
                    call insert_word_boundary(escaped_text, out_pos, input_text(i:i))
                end if
                if (out_pos <= len(escaped_text)) then
                    escaped_text(out_pos:out_pos) = input_text(i:i)
                    out_pos = out_pos + 1
                end if
                prev_was_word = .false.
                i = i + 1
            else if (char_len > 1 .and. i + char_len - 1 <= len_trim(input_text)) then
                ! Unicode character - validate and convert
                call check_utf8_sequence(input_text, i, is_valid, char_len)

                if (is_valid) then
                    codepoint = utf8_to_codepoint(input_text, i)
                    if (codepoint > 0) then
                        call unicode_codepoint_to_ascii(codepoint, ascii_equiv)
                        is_word = is_word_symbol(codepoint, ascii_equiv)
                        if (is_word) call boundary_before_word(escaped_text, out_pos)
                        ! Append ASCII equivalent to output
                        call append_to_output(escaped_text, ascii_equiv, out_pos)
                        prev_was_word = is_word
                    end if
                end if

                i = i + char_len
            else
                ! Invalid or incomplete sequence - skip
                i = i + 1
            end if
        end do
    end subroutine escape_unicode_for_ascii

    logical function is_word_symbol(codepoint, ascii_equiv)
        !! A transliterated symbol is a "word" when it comes from a non-Latin
        !! codepoint (Greek letters, math symbols) and expands to an alphabetic
        !! token. Latin-1 substitutions such as umlauts stay glued to their host
        !! word and never introduce a boundary space.
        integer, intent(in) :: codepoint
        character(len=*), intent(in) :: ascii_equiv

        is_word_symbol = .false.
        if (codepoint < 880) return
        if (len_trim(ascii_equiv) < 1) return
        is_word_symbol = is_ascii_alpha(ascii_equiv(1:1))
    end function is_word_symbol

    subroutine boundary_before_word(output, out_pos)
        !! Insert a space before a word token when the previous output character
        !! is alphanumeric, keeping ``2 pi`` and ``pi sigma`` legible.
        character(len=*), intent(inout) :: output
        integer, intent(inout) :: out_pos

        if (out_pos <= 1) return
        if (out_pos > len(output)) return
        if (.not. is_ascii_alnum(output(out_pos - 1:out_pos - 1))) return
        output(out_pos:out_pos) = ' '
        out_pos = out_pos + 1
    end subroutine boundary_before_word

    subroutine insert_word_boundary(output, out_pos, next_char)
        !! Insert a space after a word token when the next copied character is
        !! alphanumeric, so ``sqrt`` never fuses with a following letter/digit.
        character(len=*), intent(inout) :: output
        integer, intent(inout) :: out_pos
        character(len=1), intent(in) :: next_char

        if (out_pos > len(output)) return
        if (.not. is_ascii_alnum(next_char)) return
        output(out_pos:out_pos) = ' '
        out_pos = out_pos + 1
    end subroutine insert_word_boundary

    logical function is_ascii_alpha(ch)
        character(len=1), intent(in) :: ch
        integer :: v

        v = iachar(ch)
        is_ascii_alpha = (v >= iachar('A') .and. v <= iachar('Z')) .or. &
                         (v >= iachar('a') .and. v <= iachar('z'))
    end function is_ascii_alpha

    logical function is_ascii_alnum(ch)
        character(len=1), intent(in) :: ch
        integer :: v

        v = iachar(ch)
        is_ascii_alnum = is_ascii_alpha(ch) .or. &
                         (v >= iachar('0') .and. v <= iachar('9'))
    end function is_ascii_alnum

    subroutine append_to_output(output, text_to_add, out_pos)
        !! Helper subroutine to append text to output buffer
        character(len=*), intent(inout) :: output
        character(len=*), intent(in) :: text_to_add
        integer, intent(inout) :: out_pos
        integer :: i, text_len
        
        text_len = len_trim(text_to_add)
        
        do i = 1, text_len
            if (out_pos <= len(output)) then
                output(out_pos:out_pos) = text_to_add(i:i)
                out_pos = out_pos + 1
            else
                exit  ! Output buffer full
            end if
        end do
    end subroutine append_to_output

    subroutine unicode_codepoint_to_ascii(codepoint, ascii_equiv)
        !! Convert Unicode codepoint to ASCII equivalent
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: ascii_equiv

        ! Try lowercase Greek first, then uppercase, then common math
        ! symbols with readable names, and finally fall back to the
        ! placeholder form so unexpected codepoints remain traceable.
        if (codepoint_to_umlaut_ascii(codepoint, ascii_equiv)) return
        if (codepoint_to_lowercase_greek(codepoint, ascii_equiv)) return
        if (codepoint_to_uppercase_greek(codepoint, ascii_equiv)) return
        if (codepoint_to_common_symbol(codepoint, ascii_equiv)) return
        call codepoint_to_default_placeholder(codepoint, ascii_equiv)
    end subroutine unicode_codepoint_to_ascii

    logical function codepoint_to_umlaut_ascii(codepoint, ascii_equiv)
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: ascii_equiv

        codepoint_to_umlaut_ascii = .true.
        select case (codepoint)
        case (196)  ! Ä
            ascii_equiv = 'Ae'
        case (214)  ! Ö
            ascii_equiv = 'Oe'
        case (220)  ! Ü
            ascii_equiv = 'Ue'
        case (228)  ! ä
            ascii_equiv = 'ae'
        case (246)  ! ö
            ascii_equiv = 'oe'
        case (252)  ! ü
            ascii_equiv = 'ue'
        case (223)  ! ß
            ascii_equiv = 'ss'
        case default
            codepoint_to_umlaut_ascii = .false.
        end select
    end function codepoint_to_umlaut_ascii

    logical function codepoint_to_common_symbol(codepoint, ascii_equiv)
        !! Map frequent math / punctuation codepoints to plain ASCII so the
        !! ASCII backend stops emitting raw U+XXXX escape fragments for
        !! routine symbols like ``^2``, ``x``, ``sqrt``, ``+/-``, etc.
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: ascii_equiv

        codepoint_to_common_symbol = .true.
        select case (codepoint)
        case (176)   ! U+00B0 degree sign
            ascii_equiv = 'deg'
        case (177)   ! U+00B1 plus-minus sign
            ascii_equiv = '+/-'
        case (181)   ! U+00B5 micro sign
            ascii_equiv = 'u'
        case (183)   ! U+00B7 middle dot
            ascii_equiv = '.'
        case (185)   ! U+00B9 superscript 1
            ascii_equiv = '1'
        case (178)   ! U+00B2 superscript 2
            ascii_equiv = '2'
        case (179)   ! U+00B3 superscript 3
            ascii_equiv = '3'
        case (188)   ! U+00BC vulgar fraction one quarter
            ascii_equiv = '1/4'
        case (215)   ! U+00D7 multiplication sign
            ascii_equiv = 'x'
        case (247)   ! U+00F7 division sign
            ascii_equiv = '/'
        case (8201)  ! U+2009 thin space
            ascii_equiv = ' '
        case (8211, 8212)  ! U+2013 en dash, U+2014 em dash
            ascii_equiv = '-'
        case (8216, 8217)  ! left/right single quotation marks
            ascii_equiv = "'"
        case (8220, 8221)  ! left/right double quotation marks
            ascii_equiv = '"'
        case (8226)  ! U+2022 bullet
            ascii_equiv = '*'
        case (8230)  ! U+2026 horizontal ellipsis
            ascii_equiv = '...'
        case (8242)  ! U+2032 prime
            ascii_equiv = "'"
        case (8243)  ! U+2033 double prime
            ascii_equiv = '"'
        case (8260)  ! U+2044 fraction slash
            ascii_equiv = '/'
        case (8592)  ! U+2190 leftwards arrow
            ascii_equiv = '<-'
        case (8594)  ! U+2192 rightwards arrow
            ascii_equiv = '->'
        case (8596)  ! U+2194 left-right arrow
            ascii_equiv = '<->'
        case (8710)  ! U+2206 increment
            ascii_equiv = 'Delta'
        case (8719)  ! U+220F n-ary product
            ascii_equiv = 'prod'
        case (8721)  ! U+2211 n-ary summation
            ascii_equiv = 'sum'
        case (8722)  ! U+2212 minus sign
            ascii_equiv = '-'
        case (8730)  ! U+221A square root
            ascii_equiv = 'sqrt'
        case (8734)  ! U+221E infinity
            ascii_equiv = 'inf'
        case (8743)  ! U+2227 logical and
            ascii_equiv = 'and'
        case (8744)  ! U+2228 logical or
            ascii_equiv = 'or'
        case (8733)  ! U+221D proportional to
            ascii_equiv = '~'
        case (8747)  ! U+222B integral
            ascii_equiv = 'int'
        case (8764)  ! U+223C tilde operator (similar)
            ascii_equiv = '~'
        case (8776)  ! U+2248 almost equal to
            ascii_equiv = '~='
        case (8800)  ! U+2260 not equal to
            ascii_equiv = '!='
        case (8801)  ! U+2261 identical to (equivalence)
            ascii_equiv = '='
        case (8804)  ! U+2264 less-than or equal to
            ascii_equiv = '<='
        case (8805)  ! U+2265 greater-than or equal to
            ascii_equiv = '>='
        case (8901)  ! U+22C5 dot operator
            ascii_equiv = '.'
        case (8706)  ! U+2202 partial differential
            ascii_equiv = 'd'
        case (8711)  ! U+2207 nabla
            ascii_equiv = 'grad'
        case default
            codepoint_to_common_symbol = .false.
        end select
    end function codepoint_to_common_symbol
    
    logical function codepoint_to_lowercase_greek(codepoint, ascii_equiv)
        !! Convert lowercase Greek codepoint to ASCII name
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: ascii_equiv
        
        codepoint_to_lowercase_greek = .true.
        select case (codepoint)
        case (945) ! α
            ascii_equiv = "alpha"
        case (946) ! β
            ascii_equiv = "beta"
        case (947) ! γ
            ascii_equiv = "gamma"
        case (948) ! δ
            ascii_equiv = "delta"
        case (949) ! ε
            ascii_equiv = "epsilon"
        case (950) ! ζ
            ascii_equiv = "zeta"
        case (951) ! η
            ascii_equiv = "eta"
        case (952) ! θ
            ascii_equiv = "theta"
        case (953) ! ι
            ascii_equiv = "iota"
        case (954) ! κ
            ascii_equiv = "kappa"
        case (955) ! λ
            ascii_equiv = "lambda"
        case (956) ! μ
            ascii_equiv = "mu"
        case (957) ! ν
            ascii_equiv = "nu"
        case (958) ! ξ
            ascii_equiv = "xi"
        case (959) ! ο
            ascii_equiv = "omicron"
        case (960) ! π
            ascii_equiv = "pi"
        case (961) ! ρ
            ascii_equiv = "rho"
        case (963) ! σ
            ascii_equiv = "sigma"
        case (964) ! τ
            ascii_equiv = "tau"
        case (965) ! υ
            ascii_equiv = "upsilon"
        case (966) ! φ
            ascii_equiv = "phi"
        case (967) ! χ
            ascii_equiv = "chi"
        case (968) ! ψ
            ascii_equiv = "psi"
        case (969) ! ω
            ascii_equiv = "omega"
        case default
            codepoint_to_lowercase_greek = .false.
        end select
    end function codepoint_to_lowercase_greek
    
    logical function codepoint_to_uppercase_greek(codepoint, ascii_equiv)
        !! Convert uppercase Greek codepoint to ASCII name
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: ascii_equiv
        
        codepoint_to_uppercase_greek = .true.
        select case (codepoint)
        case (913) ! Α
            ascii_equiv = "Alpha"
        case (914) ! Β
            ascii_equiv = "Beta"
        case (915) ! Γ
            ascii_equiv = "Gamma"
        case (916) ! Δ
            ascii_equiv = "Delta"
        case (917) ! Ε
            ascii_equiv = "Epsilon"
        case (918) ! Ζ
            ascii_equiv = "Zeta"
        case (919) ! Η
            ascii_equiv = "Eta"
        case (920) ! Θ
            ascii_equiv = "Theta"
        case (921) ! Ι
            ascii_equiv = "Iota"
        case (922) ! Κ
            ascii_equiv = "Kappa"
        case (923) ! Λ
            ascii_equiv = "Lambda"
        case (924) ! Μ
            ascii_equiv = "Mu"
        case (925) ! Ν
            ascii_equiv = "Nu"
        case (926) ! Ξ
            ascii_equiv = "Xi"
        case (927) ! Ο
            ascii_equiv = "Omicron"
        case (928) ! Π
            ascii_equiv = "Pi"
        case (929) ! Ρ
            ascii_equiv = "Rho"
        case (931) ! Σ
            ascii_equiv = "Sigma"
        case (932) ! Τ
            ascii_equiv = "Tau"
        case (933) ! Υ
            ascii_equiv = "Upsilon"
        case (934) ! Φ
            ascii_equiv = "Phi"
        case (935) ! Χ
            ascii_equiv = "Chi"
        case (936) ! Ψ
            ascii_equiv = "Psi"
        case (937) ! Ω
            ascii_equiv = "Omega"
        case default
            codepoint_to_uppercase_greek = .false.
        end select
    end function codepoint_to_uppercase_greek
    
    subroutine codepoint_to_default_placeholder(codepoint, ascii_equiv)
        !! Convert unknown codepoint to default placeholder format
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: ascii_equiv
        
        write(ascii_equiv, '("U+", Z4.4)') codepoint
    end subroutine codepoint_to_default_placeholder

    integer function utf8_char_length(char)
        !! Determine the number of bytes in a UTF-8 character
        character(len=1), intent(in) :: char
        integer :: byte_val
        
        byte_val = iachar(char)
        
        if (byte_val < 128) then
            ! ASCII (0xxxxxxx)
            utf8_char_length = 1
        else if (byte_val < 224) then
            ! 2-byte sequence (110xxxxx)
            utf8_char_length = 2
        else if (byte_val < 240) then
            ! 3-byte sequence (1110xxxx)
            utf8_char_length = 3
        else if (byte_val < 248) then
            ! 4-byte sequence (11110xxx)
            utf8_char_length = 4
        else
            ! Invalid UTF-8 start byte
            utf8_char_length = 0
        end if
    end function utf8_char_length

    integer function utf8_to_codepoint(text, start_pos)
        !! Convert UTF-8 sequence to Unicode codepoint
        character(len=*), intent(in) :: text
        integer, intent(in) :: start_pos
        integer :: char_len, byte_val, codepoint
        
        char_len = utf8_char_length(text(start_pos:start_pos))
        
        if (char_len == 0 .or. start_pos + char_len - 1 > len(text)) then
            ! Invalid sequence or out of bounds
            utf8_to_codepoint = 0
            return
        end if
        
        if (char_len == 1) then
            ! ASCII
            utf8_to_codepoint = iachar(text(start_pos:start_pos))
        else if (char_len == 2) then
            ! 2-byte sequence
            byte_val = iachar(text(start_pos:start_pos))
            codepoint = iand(byte_val, int(z'1F')) * 64
            byte_val = iachar(text(start_pos+1:start_pos+1))
            codepoint = codepoint + iand(byte_val, int(z'3F'))
            utf8_to_codepoint = codepoint
        else if (char_len == 3) then
            ! 3-byte sequence
            byte_val = iachar(text(start_pos:start_pos))
            codepoint = iand(byte_val, int(z'0F')) * 4096
            byte_val = iachar(text(start_pos+1:start_pos+1))
            codepoint = codepoint + iand(byte_val, int(z'3F')) * 64
            byte_val = iachar(text(start_pos+2:start_pos+2))
            codepoint = codepoint + iand(byte_val, int(z'3F'))
            utf8_to_codepoint = codepoint
        else if (char_len == 4) then
            ! 4-byte sequence
            byte_val = iachar(text(start_pos:start_pos))
            codepoint = iand(byte_val, int(z'07')) * 262144
            byte_val = iachar(text(start_pos+1:start_pos+1))
            codepoint = codepoint + iand(byte_val, int(z'3F')) * 4096
            byte_val = iachar(text(start_pos+2:start_pos+2))
            codepoint = codepoint + iand(byte_val, int(z'3F')) * 64
            byte_val = iachar(text(start_pos+3:start_pos+3))
            codepoint = codepoint + iand(byte_val, int(z'3F'))
            utf8_to_codepoint = codepoint
        else
            utf8_to_codepoint = 0
        end if
    end function utf8_to_codepoint

    logical function contains_unicode(text)
        !! Check if text contains Unicode characters
        character(len=*), intent(in) :: text
        integer :: i, char_len
        logical :: is_valid
        
        contains_unicode = .false.
        i = 1
        
        do while (i <= len_trim(text))
            char_len = utf8_char_length(text(i:i))
            if (char_len > 1) then
                ! Check if it's a valid UTF-8 sequence
                call check_utf8_sequence(text, i, is_valid, char_len)
                if (is_valid) then
                    contains_unicode = .true.
                    return
                end if
            end if
            i = i + char_len
        end do
    end function contains_unicode

    logical function is_unicode_char(char)
        !! Check if a character is Unicode (multi-byte)
        character(len=*), intent(in) :: char
        
        is_unicode_char = utf8_char_length(char(1:1)) > 1
    end function is_unicode_char

    subroutine check_utf8_sequence(text, start_pos, is_valid, seq_len)
        !! Check if UTF-8 sequence is valid and return its length
        character(len=*), intent(in) :: text
        integer, intent(in) :: start_pos
        logical, intent(out) :: is_valid
        integer, intent(out) :: seq_len
        integer :: i, byte_val
        
        is_valid = .false.
        seq_len = utf8_char_length(text(start_pos:start_pos))
        
        if (seq_len == 1) then
            is_valid = .true.
            return
        end if
        
        if (start_pos + seq_len - 1 > len(text)) then
            return  ! Not enough bytes
        end if
        
        ! Check continuation bytes
        do i = 1, seq_len - 1
            byte_val = iachar(text(start_pos + i:start_pos + i))
            if (iand(byte_val, int(z'C0')) /= int(z'80')) then
                return  ! Invalid continuation byte
            end if
        end do
        
        is_valid = .true.
    end subroutine check_utf8_sequence

    logical function is_greek_letter_codepoint(codepoint)
        !! Check if codepoint is a Greek letter
        integer, intent(in) :: codepoint
        
        ! Greek letters are in ranges U+0391-U+03A9 (uppercase) and U+03B1-U+03C9 (lowercase)
        is_greek_letter_codepoint = (codepoint >= 913 .and. codepoint <= 937) .or. &
                                   (codepoint >= 945 .and. codepoint <= 969)
    end function is_greek_letter_codepoint

end module fortplot_unicode
