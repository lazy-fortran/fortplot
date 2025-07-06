program trace_bit_encoding
    implicit none
    
    ! We know:
    ! - Byte 608 is 0xF3 (same in both)
    ! - Byte 609: STB=0xCD, OUR=0xCB
    ! - The difference is in bit 2 of byte 609
    
    integer :: bit_buffer, bit_count
    integer :: byte1, byte2, byte3
    integer :: code, bits
    
    print *, "Simulating bit encoding around the difference:"
    print *, ""
    
    ! Start with empty bit buffer
    bit_buffer = 0
    bit_count = 0
    
    ! Simulate encoding that produces 0xF3
    ! This could be a Category 7 DC coefficient (11110) + 3 bits (011)
    code = int(b'11110011')  ! Full byte value
    bits = 8
    
    print *, "First byte encoding:"
    print '(A,B8.8,A,I0)', "  Code: ", code, " (", code, ")"
    
    ! Now simulate the next bits that would produce either 0xCD or 0xCB
    print *, ""
    print *, "Next bits that differ:"
    print *, "  STB result: 0xCD = 11001101"
    print *, "  OUR result: 0xCB = 11001011"
    print *, "                          ^- bit 2 differs"
    print *, ""
    
    ! The key insight: the difference is a single bit
    ! This suggests either:
    ! 1. Different Huffman code was chosen
    ! 2. Different value was encoded
    ! 3. Rounding/quantization difference
    
    print *, "Possible causes:"
    print *, "1. AC coefficient rounding:"
    print *, "   - STB might round 0.5 up, we might round down"
    print *, "   - This would change the magnitude category"
    print *, ""
    print *, "2. Huffman table difference:"
    print *, "   - Different Huffman codes for same symbol"
    print *, "   - But we claim to use STB tables..."
    print *, ""
    print *, "3. Run-length encoding difference:"
    print *, "   - Different zero run counts"
    print *, "   - Would change the AC symbol entirely"
    
    ! Let's check what AC symbols would produce these patterns
    print *, ""
    print *, "Reverse engineering the AC encoding:"
    
    ! If the next Huffman code starts at bit 5 of byte 609:
    ! STB: ...1101... 
    ! OUR: ...1011...
    
    print *, "The 4-bit sequence that differs:"
    print *, "  STB: 1101"
    print *, "  OUR: 1011"
    print *, ""
    
    ! Check common AC Huffman codes
    print *, "Common 4-bit AC Huffman codes:"
    print *, "  1010 - Often RLE symbol 0/1 (1 AC coef of magnitude 1)"
    print *, "  1011 - Often RLE symbol 0/2 (1 AC coef of magnitude 2)" 
    print *, "  1100 - Often RLE symbol 0/3 (1 AC coef of magnitude 3)"
    print *, "  1101 - Often RLE symbol 0/4 (1 AC coef of magnitude 4)"
    print *, ""
    
    print *, "This suggests:"
    print *, "  STB encoded an AC coefficient with magnitude category 4"
    print *, "  OUR encoded an AC coefficient with magnitude category 2"
    print *, ""
    print *, "The most likely cause: different quantization or rounding"
    
end program trace_bit_encoding