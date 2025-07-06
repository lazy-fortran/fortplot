program debug_reverse_engineer
    implicit none
    
    ! Reverse engineer what bit sequence produces STB's 40 1F
    integer :: target_bytes(2) = [int(z'40'), int(z'1f')]
    integer :: bit_buffer, bit_count
    integer :: i, bit_val
    
    print *, "=== Reverse engineering STB's 40 1F pattern ==="
    print *, ""
    
    ! STB: 40 1F = 01000000 00011111
    print *, "STB target: 40 1F"
    print *, "Binary: 01000000 00011111"
    print *, ""
    
    ! If we assume this comes from fillBits(0x7F, 7) applied to some initial state,
    ! let's work backwards to find what initial bit buffer state would produce this
    
    ! The pattern 40 1F suggests:
    ! - First byte: 01000000 (bit 6 set)
    ! - Second byte: 00011111 (bits 0-4 set)
    
    ! If fillBits adds 1111111 (7 ones), and we get this pattern,
    ! then the initial buffer before fillBits must have been different
    
    print *, "Hypothesis: Our bit buffer state before fillBits differs from STB"
    print *, ""
    print *, "Our analysis showed:"
    print *, "Before fillBits: buffer=1158938624, count=7"
    print *, "This gives us 45 15, but STB produces 40 1F"
    print *, ""
    
    ! Let's see what bit buffer state would produce 40 1F
    ! If fillBits(0x7F, 7) produces different output, then either:
    ! 1. The initial buffer state is different
    ! 2. The fillBits implementation is different
    ! 3. The bit extraction is different
    
    print *, "Possible causes:"
    print *, "1. Different DC coefficient encoding"
    print *, "2. Different AC coefficient handling"  
    print *, "3. Different bit buffer management"
    print *, "4. Different fillBits algorithm"
    print *, ""
    
    print *, "Next step: Compare our DC/AC encoding with STB bit-by-bit"
    print *, "to find where the bit streams diverge."
    
end program debug_reverse_engineer