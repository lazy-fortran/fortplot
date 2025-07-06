program debug_bit_sequence
    use fortplot_jpeg, only: jpeg_context, create_jpeg_canvas, get_jpeg_data, initialize_huffman_tables
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer :: our_buffer, i, bit_pos, bit_val
    
    ! We know:
    ! - Our bit_buffer = 1158938624 (0x45140000), bit_count = 7
    ! - This produces 0x45 0x15 after fillBits
    ! - STB produces 0x40 0x1F instead
    
    print *, "=== Analyzing bit sequences ==="
    print *, ""
    
    ! Our state before fillBits:
    print *, "Our state before fillBits:"
    print *, "bit_buffer = 0x45140000"
    print *, "bit_count = 7"
    print *, ""
    
    ! Let's understand what bits are in the buffer
    ! With bit_count=7, we have 7 bits starting from position 24
    ! The bits are in positions 24,23,22,21,20,19,18
    
    our_buffer = int(Z'45140000')
    
    print *, "Our 7 bits in buffer (from left):"
    do i = 0, 6
        bit_pos = 24 - i
        bit_val = iand(ishft(our_buffer, -(bit_pos-1)), 1)
        write(*, '("Bit ", I0, ": ", I0)', advance='no') i, bit_val
        if (i < 6) write(*, '(", ")', advance='no')
    end do
    print *, ""
    
    ! Convert to binary for clarity
    print *, ""
    print *, "Our buffer top byte: 0x45 = 01000101"
    print *, "Bits in buffer:      0100010 (7 bits)"
    print *, ""
    
    ! Now let's see what STB must have to produce 0x40 0x1F
    print *, "STB produces: 0x40 0x1F"
    print *, "STB top byte: 0x40 = 01000000"
    print *, ""
    
    ! The difference is:
    ! Our bits:  0100010
    ! STB bits:  0100000
    !               ^^^-- difference here
    
    print *, "Bit difference analysis:"
    print *, "Position: 6543210"
    print *, "Our bits: 0100010"
    print *, "STB bits: 0100000"
    print *, "          -----^-- Bit 1 differs!"
    print *, ""
    
    print *, "This means our encoding produces an extra '1' bit"
    print *, "in position 1 (second-to-last bit) that STB doesn't."
    print *, ""
    
    print *, "This extra bit likely comes from:"
    print *, "1. Different DC coefficient value"
    print *, "2. Different AC coefficient encoding"
    print *, "3. Different Huffman code selection"
    
end program debug_bit_sequence