module fortplot_unicode
    !! Unicode utilities for detecting and handling UTF-8 encoded text
    implicit none
    
    private
    public :: is_unicode_char, check_utf8_sequence, utf8_char_length
    public :: utf8_to_codepoint, is_greek_letter_codepoint, contains_unicode
    
contains

    logical function is_unicode_char(str)
        character(len=*), intent(in) :: str
        integer :: first_byte
        
        if (len(str) == 0) then
            is_unicode_char = .false.
            return
        end if
        
        first_byte = iachar(str(1:1))
        
        ! UTF-8 multibyte sequences start with bytes >= 128 (0x80)
        is_unicode_char = (first_byte >= 128)
    end function is_unicode_char
    
    subroutine check_utf8_sequence(str, pos, is_valid, seq_len)
        character(len=*), intent(in) :: str
        integer, intent(in) :: pos
        logical, intent(out) :: is_valid
        integer, intent(out) :: seq_len
        integer :: first_byte, i, byte_val
        
        is_valid = .false.
        seq_len = 0
        
        if (pos < 1 .or. pos > len(str)) return
        
        first_byte = iachar(str(pos:pos))
        seq_len = utf8_char_length(str(pos:pos))
        
        if (seq_len == 0) return
        if (pos + seq_len - 1 > len(str)) return
        
        ! Check continuation bytes
        is_valid = .true.
        do i = 2, seq_len
            byte_val = iachar(str(pos + i - 1:pos + i - 1))
            ! Continuation bytes must be in range 0x80-0xBF (128-191)
            if (byte_val < 128 .or. byte_val > 191) then
                is_valid = .false.
                return
            end if
        end do
    end subroutine check_utf8_sequence
    
    integer function utf8_char_length(ch)
        character(len=1), intent(in) :: ch
        integer :: byte_val
        
        byte_val = iachar(ch)
        
        if (byte_val < 128) then
            ! ASCII (0xxxxxxx)
            utf8_char_length = 1
        else if (byte_val >= 192 .and. byte_val < 224) then
            ! 2-byte sequence (110xxxxx)
            utf8_char_length = 2
        else if (byte_val >= 224 .and. byte_val < 240) then
            ! 3-byte sequence (1110xxxx)
            utf8_char_length = 3
        else if (byte_val >= 240 .and. byte_val < 248) then
            ! 4-byte sequence (11110xxx)
            utf8_char_length = 4
        else
            ! Invalid UTF-8 start byte
            utf8_char_length = 0
        end if
    end function utf8_char_length
    
    integer function utf8_to_codepoint(str, pos)
        character(len=*), intent(in) :: str
        integer, intent(in) :: pos
        integer :: seq_len, i, byte_val
        
        utf8_to_codepoint = 0
        seq_len = utf8_char_length(str(pos:pos))
        
        if (seq_len == 0) return
        if (pos + seq_len - 1 > len(str)) return
        
        select case (seq_len)
        case (1)
            ! ASCII
            utf8_to_codepoint = iachar(str(pos:pos))
            
        case (2)
            ! 2-byte sequence
            utf8_to_codepoint = iand(iachar(str(pos:pos)), 31) * 64 + &
                               iand(iachar(str(pos+1:pos+1)), 63)
                               
        case (3)
            ! 3-byte sequence
            utf8_to_codepoint = iand(iachar(str(pos:pos)), 15) * 4096 + &
                               iand(iachar(str(pos+1:pos+1)), 63) * 64 + &
                               iand(iachar(str(pos+2:pos+2)), 63)
                               
        case (4)
            ! 4-byte sequence
            utf8_to_codepoint = iand(iachar(str(pos:pos)), 7) * 262144 + &
                               iand(iachar(str(pos+1:pos+1)), 63) * 4096 + &
                               iand(iachar(str(pos+2:pos+2)), 63) * 64 + &
                               iand(iachar(str(pos+3:pos+3)), 63)
        end select
    end function utf8_to_codepoint
    
    logical function is_greek_letter_codepoint(codepoint)
        integer, intent(in) :: codepoint
        
        ! Greek and Coptic block: U+0370-U+03FF
        ! Uppercase Greek: U+0391-U+03A9 (913-937)
        ! Lowercase Greek: U+03B1-U+03C9 (945-969)
        is_greek_letter_codepoint = &
            (codepoint >= 913 .and. codepoint <= 937) .or. &  ! Uppercase
            (codepoint >= 945 .and. codepoint <= 969)         ! Lowercase
    end function is_greek_letter_codepoint
    
    logical function contains_unicode(str)
        character(len=*), intent(in) :: str
        integer :: i, seq_len
        logical :: is_valid
        
        contains_unicode = .false.
        i = 1
        
        do while (i <= len_trim(str))
            if (iachar(str(i:i)) >= 128) then
                contains_unicode = .true.
                return
            end if
            
            ! Move to next character
            call check_utf8_sequence(str, i, is_valid, seq_len)
            if (seq_len > 0) then
                i = i + seq_len
            else
                i = i + 1
            end if
        end do
    end function contains_unicode

end module fortplot_unicode