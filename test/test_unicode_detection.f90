program test_unicode_detection
    use fortplot_unicode, only: is_unicode_char, check_utf8_sequence, utf8_char_length, &
                                utf8_to_codepoint, is_greek_letter_codepoint, contains_unicode
    implicit none
    
    call test_is_unicode_character()
    call test_utf8_byte_sequence_detection()
    call test_utf8_sequence_length()
    call test_unicode_codepoint_extraction()
    call test_is_greek_letter()
    call test_ascii_vs_unicode_detection()
    
    print *, "All Unicode detection tests passed!"
    
contains

    subroutine test_is_unicode_character()
        ! Test detection of Unicode vs ASCII characters
        logical :: is_unicode
        
        ! ASCII characters should return false
        is_unicode = is_unicode_char('A')
        if (is_unicode) then
            print *, "ERROR: 'A' detected as Unicode"
            stop 1
        end if
        
        is_unicode = is_unicode_char('0')
        if (is_unicode) then
            print *, "ERROR: '0' detected as Unicode"
            stop 1
        end if
        
        ! Unicode characters (Greek alpha: α = CE B1)
        is_unicode = is_unicode_char(achar(206) // achar(177))  ! CE=206, B1=177
        if (.not. is_unicode) then
            print *, "ERROR: Greek alpha not detected as Unicode"
            stop 1
        end if
        
        print *, "test_is_unicode_character: PASSED"
    end subroutine
    
    subroutine test_utf8_byte_sequence_detection()
        ! Test detection of valid UTF-8 sequences
        character(len=10) :: test_str
        integer :: seq_len
        logical :: is_valid
        
        ! Single-byte ASCII
        test_str = 'A'
        call check_utf8_sequence(test_str, 1, is_valid, seq_len)
        if (.not. is_valid .or. seq_len /= 1) then
            print *, "ERROR: ASCII 'A' not recognized as valid 1-byte sequence"
            stop 1
        end if
        
        ! Two-byte sequence (α = CE B1)
        test_str = achar(206) // achar(177)
        call check_utf8_sequence(test_str, 1, is_valid, seq_len)
        if (.not. is_valid .or. seq_len /= 2) then
            print *, "ERROR: Greek alpha not recognized as valid 2-byte sequence"
            stop 1
        end if
        
        ! Invalid sequence (incomplete UTF-8)
        test_str = achar(206) // 'A'  ! Invalid continuation
        call check_utf8_sequence(test_str, 1, is_valid, seq_len)
        if (is_valid) then
            print *, "ERROR: Invalid UTF-8 sequence not detected"
            stop 1
        end if
        
        print *, "test_utf8_byte_sequence_detection: PASSED"
    end subroutine
    
    subroutine test_utf8_sequence_length()
        ! Test determining length of UTF-8 sequences
        integer :: length
        
        ! 1-byte sequence (ASCII)
        length = utf8_char_length('A')
        if (length /= 1) then
            print *, "ERROR: ASCII char length should be 1, got", length
            stop 1
        end if
        
        ! 2-byte sequence
        length = utf8_char_length(achar(206))
        if (length /= 2) then
            print *, "ERROR: 2-byte UTF-8 start should return 2, got", length
            stop 1
        end if
        
        ! 3-byte sequence
        length = utf8_char_length(achar(224))
        if (length /= 3) then
            print *, "ERROR: 3-byte UTF-8 start should return 3, got", length
            stop 1
        end if
        
        ! 4-byte sequence
        length = utf8_char_length(achar(240))
        if (length /= 4) then
            print *, "ERROR: 4-byte UTF-8 start should return 4, got", length
            stop 1
        end if
        
        print *, "test_utf8_sequence_length: PASSED"
    end subroutine
    
    subroutine test_unicode_codepoint_extraction()
        ! Test extracting Unicode codepoints from UTF-8
        character(len=4) :: utf8_char
        integer :: codepoint
        
        ! ASCII A = U+0041
        utf8_char = 'A'
        codepoint = utf8_to_codepoint(utf8_char, 1)
        if (codepoint /= 65) then
            print *, "ERROR: ASCII 'A' codepoint wrong:", codepoint
            stop 1
        end if
        
        ! Greek α = U+03B1 (UTF-8: CE B1)
        utf8_char = achar(206) // achar(177)
        codepoint = utf8_to_codepoint(utf8_char, 1)
        if (codepoint /= 945) then
            print *, "ERROR: Greek alpha codepoint wrong:", codepoint
            stop 1
        end if
        
        ! Greek Ω = U+03A9 (UTF-8: CE A9)
        utf8_char = achar(206) // achar(169)
        codepoint = utf8_to_codepoint(utf8_char, 1)
        if (codepoint /= 937) then
            print *, "ERROR: Greek Omega codepoint wrong:", codepoint
            stop 1
        end if
        
        print *, "test_unicode_codepoint_extraction: PASSED"
    end subroutine
    
    subroutine test_is_greek_letter()
        ! Test detection of Greek letter codepoints
        logical :: is_greek
        
        ! Lowercase Greek letters (U+03B1 to U+03C9)
        is_greek = is_greek_letter_codepoint(945)  ! α
        if (.not. is_greek) then
            print *, "ERROR: greek alpha not detected as Greek letter"
            stop 1
        end if
        
        is_greek = is_greek_letter_codepoint(969)  ! ω
        if (.not. is_greek) then
            print *, "ERROR: greek omega not detected as Greek letter"
            stop 1
        end if
        
        ! Uppercase Greek letters (U+0391 to U+03A9)
        is_greek = is_greek_letter_codepoint(913)  ! Α
        if (.not. is_greek) then
            print *, "ERROR: greek Alpha not detected as Greek letter"
            stop 1
        end if
        
        ! Non-Greek should return false
        is_greek = is_greek_letter_codepoint(65)  ! Latin A
        if (is_greek) then
            print *, "ERROR: Latin A detected as Greek letter"
            stop 1
        end if
        
        print *, "test_is_greek_letter: PASSED"
    end subroutine
    
    subroutine test_ascii_vs_unicode_detection()
        ! Test distinguishing ASCII from Unicode in strings
        character(len=20) :: test_str
        logical :: has_unicode
        
        ! Pure ASCII string
        test_str = "Hello World"
        has_unicode = contains_unicode(test_str)
        if (has_unicode) then
            print *, "ERROR: Pure ASCII string detected as containing Unicode"
            stop 1
        end if
        
        ! String with Greek letters
        test_str = "α = 2π"  
        has_unicode = contains_unicode(test_str)
        if (.not. has_unicode) then
            print *, "ERROR: String with Greek letters not detected as Unicode"
            stop 1
        end if
        
        ! Mixed content
        test_str = "Time (μs)"
        has_unicode = contains_unicode(test_str)
        if (.not. has_unicode) then
            print *, "ERROR: Mixed ASCII/Unicode string not detected properly"
            stop 1
        end if
        
        print *, "test_ascii_vs_unicode_detection: PASSED"
    end subroutine
    

end program test_unicode_detection
