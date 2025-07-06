program debug_exact_bits
    implicit none
    
    ! We know the exact bit sequence that should be written:
    ! 1001001010 001010 001010 001010 001010 001010 = 40 bits total
    
    ! Let's pack this into bytes to see what we should get
    integer :: bit_buffer, bit_count
    integer :: byte1, byte2, byte3, byte4, byte5
    
    print *, "=== Expected bit sequence analysis ==="
    print *, ""
    print *, "Bit sequence: 1001001010 001010 001010 001010 001010 001010"
    print *, ""
    
    ! Pack bits manually
    ! Byte 1: 10010010 = 0x92
    ! Byte 2: 10001010 = 0x8A  
    ! Byte 3: 00101000 = 0x28
    ! Byte 4: 10100010 = 0xA2
    ! Byte 5: 10001010 = 0x8A
    
    print *, "Expected bytes: 92 8A 28 A2 8A"
    print *, ""
    
    ! Now let's see what happens with fillBits(0x7F, 7)
    ! We have 40 bits, which is exactly 5 bytes
    ! So bit_count = 0 before fillBits
    
    ! But wait - STB output is: 65 14 51 40 1F
    ! That's different from our expected sequence!
    
    print *, "STB actual output: 65 14 51 40 1F"
    print *, ""
    
    ! Let's work backwards from STB output
    ! 65 = 01100101
    ! 14 = 00010100
    ! 51 = 01010001
    ! 40 = 01000000
    ! 1F = 00011111
    
    print *, "STB bit sequence:"
    print *, "01100101 00010100 01010001 01000000 00011111"
    print *, ""
    
    ! The last 7 bits (0011111) are the fillBits
    ! So the actual data is:
    ! 01100101 00010100 01010001 01000000 0
    ! = 33 bits of actual data
    
    print *, "Actual data bits: 33"
    print *, "This doesn't match our expected 40 bits!"
    print *, ""
    print *, "This suggests we're not encoding the blocks correctly."
    
end program debug_exact_bits