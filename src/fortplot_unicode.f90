!! Unicode text processing utilities for raster rendering
!!
!! This module provides Unicode-to-ASCII conversion functionality
!! for raster rendering backends that need text fallback.
module fortplot_unicode
    implicit none
    private
    
    public :: escape_unicode_for_raster
    public :: unicode_codepoint_to_ascii
    public :: codepoint_to_lowercase_greek
    public :: codepoint_to_uppercase_greek
    public :: codepoint_to_default_placeholder
    public :: utf8_to_codepoint
    public :: utf8_char_length

contains

    subroutine escape_unicode_for_raster(input_text, escaped_text)
        !! Pass through Unicode for raster rendering (STB TrueType supports Unicode)
        character(len=*), intent(in) :: input_text
        character(len=*), intent(out) :: escaped_text
        
        ! STB TrueType can handle Unicode directly, so just pass through
        escaped_text = input_text
    end subroutine escape_unicode_for_raster

    subroutine unicode_codepoint_to_ascii(codepoint, ascii_equiv)
        !! Convert Unicode codepoint to ASCII equivalent
        integer, intent(in) :: codepoint
        character(len=*), intent(out) :: ascii_equiv
        
        ! Try lowercase Greek first, then uppercase, then default
        if (codepoint_to_lowercase_greek(codepoint, ascii_equiv)) return
        if (codepoint_to_uppercase_greek(codepoint, ascii_equiv)) return
        call codepoint_to_default_placeholder(codepoint, ascii_equiv)
    end subroutine unicode_codepoint_to_ascii
    
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
        
        byte_val = ichar(char)
        
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
        integer :: i
        
        char_len = utf8_char_length(text(start_pos:start_pos))
        
        if (char_len == 0 .or. start_pos + char_len - 1 > len(text)) then
            ! Invalid sequence or out of bounds
            utf8_to_codepoint = 0
            return
        end if
        
        if (char_len == 1) then
            ! ASCII
            utf8_to_codepoint = ichar(text(start_pos:start_pos))
        else if (char_len == 2) then
            ! 2-byte sequence
            byte_val = ichar(text(start_pos:start_pos))
            codepoint = iand(byte_val, int(z'1F')) * 64
            byte_val = ichar(text(start_pos+1:start_pos+1))
            codepoint = codepoint + iand(byte_val, int(z'3F'))
            utf8_to_codepoint = codepoint
        else if (char_len == 3) then
            ! 3-byte sequence
            byte_val = ichar(text(start_pos:start_pos))
            codepoint = iand(byte_val, int(z'0F')) * 4096
            byte_val = ichar(text(start_pos+1:start_pos+1))
            codepoint = codepoint + iand(byte_val, int(z'3F')) * 64
            byte_val = ichar(text(start_pos+2:start_pos+2))
            codepoint = codepoint + iand(byte_val, int(z'3F'))
            utf8_to_codepoint = codepoint
        else if (char_len == 4) then
            ! 4-byte sequence
            byte_val = ichar(text(start_pos:start_pos))
            codepoint = iand(byte_val, int(z'07')) * 262144
            byte_val = ichar(text(start_pos+1:start_pos+1))
            codepoint = codepoint + iand(byte_val, int(z'3F')) * 4096
            byte_val = ichar(text(start_pos+2:start_pos+2))
            codepoint = codepoint + iand(byte_val, int(z'3F')) * 64
            byte_val = ichar(text(start_pos+3:start_pos+3))
            codepoint = codepoint + iand(byte_val, int(z'3F'))
            utf8_to_codepoint = codepoint
        else
            utf8_to_codepoint = 0
        end if
    end function utf8_to_codepoint

end module fortplot_unicode