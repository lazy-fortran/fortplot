program debug_fillbits_state
    use fortplot_jpeg, only: get_jpeg_data
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    ! The strategy: instrument the bit writer to see the state just before fillBits
    ! Since we know our first 3 bytes (65 14 51) match STB, 
    ! the issue must be in the final fillBits or remaining bit handling
    
    print *, "=== Analysis of fillBits difference ==="
    print *, ""
    print *, "Known facts:"
    print *, "1. First 3 bytes match: 65 14 51"
    print *, "2. STB ends with: 40 1F"
    print *, "3. We end with: 45 15" 
    print *, "4. Both use fillBits(0x7F, 7)"
    print *, ""
    
    ! Let's examine the bit patterns
    print *, "Bit analysis of final bytes:"
    print *, "STB 40 1F = 01000000 00011111"
    print *, "Our 45 15 = 01000101 00010101"
    print *, ""
    print *, "Differences:"
    print *, "Byte 1: bit 0 and bit 2 differ"
    print *, "Byte 2: bit 1 and bit 3 differ"
    print *, ""
    
    ! This suggests either:
    ! 1. Different bit buffer state before fillBits
    ! 2. Different fillBits implementation
    ! 3. Different final bit extraction/padding
    
    print *, "Possible causes:"
    print *, "1. Different bit count before fillBits"
    print *, "2. Different bit buffer value before fillBits"
    print *, "3. Different final byte padding logic"
    print *, "4. Off-by-one in bit positions"
    print *, ""
    
    ! Since fillBits is supposed to pad to byte boundary with 1-bits,
    ! and both use 0x7F (01111111), the difference suggests
    ! the bit buffer has different content before fillBits is applied
    
    print *, "Next step: instrument stb_write_bits to show state before fillBits"
    
end program debug_fillbits_state