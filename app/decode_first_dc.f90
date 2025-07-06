program decode_first_dc
    implicit none
    
    ! The scan data starts with 0xF3 in both files
    ! STB: F3 CD 2E D7 81 C5 45 69 67 F2 8E 29 9A 4D 9F 03 8A F4 2B 7B
    ! OUR: F3 CB 5C 55 15 14 48 6D 14 CB 4C 35 15 E8 51 E2 AA EF E2 35
    
    integer :: i
    integer(1) :: byte_val
    integer :: bit_pos
    character(len=8) :: bits
    
    print *, "Analyzing first DC coefficient encoding:"
    print *, ""
    print *, "Byte 608 (0xF3 = 11110011) is the same in both files"
    print *, ""
    
    ! Analyze 0xF3 as potential Huffman code
    print *, "Possible Huffman code interpretations of 0xF3:"
    print *, ""
    print *, "As a single byte: 11110011"
    print *, "  Could be Huffman code: 1111001 with extra bit 1"
    print *, "  Could be Huffman code: 111100 with 2 extra bits 11"
    print *, "  Could be Huffman code: 11110 with 3 extra bits 011"
    print *, ""
    
    ! Look at standard JPEG DC Huffman codes
    print *, "Standard JPEG DC Huffman codes (from ITU-T.81):"
    print *, "  Category 0: 00       (2 bits)"
    print *, "  Category 1: 010      (3 bits)"
    print *, "  Category 2: 011      (3 bits)"
    print *, "  Category 3: 100      (3 bits)"
    print *, "  Category 4: 101      (3 bits)"
    print *, "  Category 5: 110      (3 bits)"
    print *, "  Category 6: 1110     (4 bits)"
    print *, "  Category 7: 11110    (5 bits)"
    print *, "  Category 8: 111110   (6 bits)"
    print *, "  Category 9: 1111110  (7 bits)"
    print *, "  Category 10: 11111110 (8 bits)"
    print *, "  Category 11: 111111110 (9 bits)"
    print *, ""
    
    ! Check if 0xF3 starts with category 7
    print *, "0xF3 = 11110011 starts with 11110 (Category 7)"
    print *, "This means a 7-bit DC value follows: 011"
    print *, ""
    print *, "But we need the next bits to complete the 7-bit value!"
    print *, ""
    print *, "STB byte 609: 0xCD = 11001101"
    print *, "OUR byte 609: 0xCB = 11001011"
    print *, ""
    print *, "Complete 7-bit DC values:"
    print *, "  STB: 011 + 1100 = 0111100 = 60 (decimal)"
    print *, "  OUR: 011 + 1100 = 0111100 = 60 (decimal)"
    print *, ""
    print *, "Wait, the first 4 bits of byte 609 are the same (1100)!"
    print *, "The difference is in bit 2: STB has 1, OUR has 0"
    print *, ""
    print *, "This suggests the 7-bit value is actually:"
    print *, "  STB: 0111100 (using first 4 bits of 0xCD)"
    print *, "  OUR: 0111100 (using first 4 bits of 0xCB)"
    print *, "  Both = 60 in decimal"
    print *, ""
    print *, "The next Huffman code starts at bit 5 of byte 609:"
    print *, "  STB: ...1101... (from 0xCD)"
    print *, "  OUR: ...1011... (from 0xCB)"
    print *, "                ^-- This bit differs!"
    print *, ""
    print *, "This suggests the Huffman streams diverge in the middle of byte 609"
    
end program decode_first_dc