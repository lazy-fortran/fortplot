program analyze_huffman_difference
    implicit none
    
    integer :: i
    integer(1) :: stb_bytes(20), our_bytes(20)
    integer :: stb_unit, our_unit, ios
    
    ! The key bytes around position 609
    ! STB: F3 CD 2E D7 81 C5 45 69 67 F2 8E 29 9A 4D 9F 03 8A F4 2B 7B
    ! OUR: F3 CB 5C 55 15 14 48 6D 14 CB 4C 35 15 E8 51 E2 AA EF E2 35
    
    stb_bytes = [int(z'F3',1), int(z'CD',1), int(z'2E',1), int(z'D7',1), &
                 int(z'81',1), int(z'C5',1), int(z'45',1), int(z'69',1), &
                 int(z'67',1), int(z'F2',1), int(z'8E',1), int(z'29',1), &
                 int(z'9A',1), int(z'4D',1), int(z'9F',1), int(z'03',1), &
                 int(z'8A',1), int(z'F4',1), int(z'2B',1), int(z'7B',1)]
    
    our_bytes = [int(z'F3',1), int(z'CB',1), int(z'5C',1), int(z'55',1), &
                 int(z'15',1), int(z'14',1), int(z'48',1), int(z'6D',1), &
                 int(z'14',1), int(z'CB',1), int(z'4C',1), int(z'35',1), &
                 int(z'15',1), int(z'E8',1), int(z'51',1), int(z'E2',1), &
                 int(z'AA',1), int(z'EF',1), int(z'E2',1), int(z'35',1)]
    
    print *, "Analyzing Huffman-encoded scan data differences:"
    print *, ""
    print *, "Byte 608 (0xF3 = 11110011) - Same in both files"
    print *, "This is the first DC coefficient encoded value"
    print *, ""
    
    print *, "Byte 609 - THE DIFFERENCE:"
    print '(A,Z2.2,A,B8.8)', "  STB: 0x", stb_bytes(2), " = ", stb_bytes(2)
    print '(A,Z2.2,A,B8.8)', "  OUR: 0x", our_bytes(2), " = ", our_bytes(2)
    print *, ""
    
    ! Analyze bit patterns
    print *, "Bit difference analysis:"
    print *, "  STB: 11001101 (0xCD)"
    print *, "  OUR: 11001011 (0xCB)"
    print *, "       ------^- Bit 2 differs (STB=1, OUR=0)"
    print *, ""
    
    ! Look at the bitstream context
    print *, "Extended bitstream (combining bytes 608-610):"
    call show_bitstream("STB", stb_bytes(1:3))
    call show_bitstream("OUR", our_bytes(1:3))
    print *, ""
    
    ! Analyze possible Huffman code boundaries
    print *, "Possible Huffman code interpretations:"
    print *, ""
    print *, "If we assume the difference starts at a Huffman code boundary:"
    print *, "  STB stream: ...11110011 11001101 00101110..."
    print *, "  OUR stream: ...11110011 11001011 01011100..."
    print *, ""
    print *, "The streams diverge completely after the first difference."
    print *, ""
    
    ! Check for DC value encoding
    print *, "DC coefficient encoding analysis:"
    print *, "First byte 0xF3 could be:"
    print *, "  - A Huffman code followed by magnitude bits"
    print *, "  - Part of a larger Huffman code"
    print *, ""
    
    ! Analyze the pattern
    print *, "Pattern analysis:"
    print *, "STB appears to have more varied bytes (0x2E, 0xD7, 0x81, etc.)"
    print *, "OUR has many repeated values (0x15 appears 3 times, 0x14 twice)"
    print *, "This suggests different Huffman table usage or different quantization"
    
contains
    
    subroutine show_bitstream(label, bytes)
        character(len=*), intent(in) :: label
        integer(1), intent(in) :: bytes(:)
        integer :: j
        
        write(*, '(A,A)', advance='no') label, ": "
        do j = 1, size(bytes)
            write(*, '(B8.8,A)', advance='no') bytes(j), " "
        end do
        write(*, *)
    end subroutine show_bitstream
    
end program analyze_huffman_difference