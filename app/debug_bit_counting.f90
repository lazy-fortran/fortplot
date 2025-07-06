program debug_bit_counting
    implicit none
    
    ! STB output: 65 14 51 40 1F = 5 bytes = 40 bits total
    ! With fillBits of 7 bits, actual data = 40 - 7 = 33 bits
    
    print *, "=== Bit counting analysis ==="
    print *, ""
    print *, "STB output: 65 14 51 40 1F"
    print *, "Total bits: 5 bytes * 8 = 40 bits"
    print *, "fillBits: 7 bits"
    print *, "Actual data bits: 40 - 7 = 33 bits"
    print *, ""
    
    ! Our expected data: 36 bits
    print *, "Our expected: 36 bits of data"
    print *, "Difference: 36 - 33 = 3 extra bits"
    print *, ""
    
    ! The 3 extra bits are the problem!
    print *, "We have 3 extra bits somewhere."
    print *, "Bit buffer before fillBits: 0x45140000, count=7"
    print *, ""
    
    ! Let's check: 0x45 = 01000101
    ! The '101' at the end (3 bits) are the extra bits
    print *, "0x45 = 01000101"
    print *, "         ^^^--- These 3 bits are extra!"
    print *, ""
    
    ! So we need to find where these 3 extra bits come from
    print *, "Possible sources of extra bits:"
    print *, "1. Extra AC coefficient encoded"
    print *, "2. Wrong Huffman code used"
    print *, "3. Extra EOB marker"
    print *, "4. Incorrect run-length encoding"
    
end program debug_bit_counting