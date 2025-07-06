program debug_bit_analysis
    implicit none
    
    integer :: buffer_value = 1158938624
    integer :: bit_count = 7
    integer :: fillbits_value = 127  ! 0x7F
    integer :: fillbits_count = 7
    
    integer :: final_buffer, final_count
    integer :: byte1, byte2
    integer :: shifted_fillbits
    
    print *, "=== Bit Buffer Analysis ==="
    print *, "Initial buffer value:", buffer_value
    print *, "Initial bit count:", bit_count
    print *, ""
    
    ! Convert to binary representation
    print *, "Buffer in hex:", buffer_value
    write(*, '("Buffer in hex format: 0x", Z8.8)') buffer_value
    print *, "Buffer in binary (top 16 bits):"
    call print_binary(ishft(buffer_value, -16), 16)
    print *, ""
    
    ! Apply fillBits step by step
    print *, "fillBits calculation:"
    print *, "fillbits_value = 0x7F =", fillbits_value
    print *, "fillbits_count =", fillbits_count
    
    final_count = bit_count + fillbits_count
    print *, "new bit count =", bit_count, "+", fillbits_count, "=", final_count
    
    print *, "shift amount = 24 -", final_count, "=", 24 - final_count
    print *, "shifted fillbits = 0x7F <<", 24 - final_count
    
    ! Calculate the shifted value manually
    shifted_fillbits = ishft(fillbits_value, 24 - final_count)
    write(*, '("shifted fillbits = 0x", Z8.8)') shifted_fillbits
    
    final_buffer = ior(buffer_value, shifted_fillbits)
    write(*, '("final buffer = 0x", Z8.8, " | 0x", Z8.8, " = 0x", Z8.8)') &
           buffer_value, shifted_fillbits, final_buffer
    
    print *, ""
    print *, "Final buffer binary (top 16 bits):"
    call print_binary(ishft(final_buffer, -16), 16)
    print *, ""
    
    ! Simulate STB bit extraction process exactly
    print *, "Simulating STB writeBits process:"
    
    ! First iteration: bitCnt = 14 >= 8
    write(*, '("Extracting from buffer: 0x", Z8.8)') final_buffer
    write(*, '("Top 16 bits: 0x", Z4.4)') ishft(final_buffer, -16)
    byte1 = iand(ishft(final_buffer, -16), 255)
    write(*, '("First byte extracted: ", I0, " hex: 0x", Z2.2)') byte1, byte1
    write(*, '("Expected: 0x45, Got: 0x", Z2.2)') byte1
    
    ! Buffer shifts left by 8, count decreases by 8
    final_buffer = ishft(final_buffer, 8)
    final_count = final_count - 8
    
    print *, "After first byte:"
    write(*, '("Buffer: 0x", Z8.8)') final_buffer
    print *, "Bit count:", final_count
    
    ! Second iteration: bitCnt = 6 < 8, so no more bytes extracted
    if (final_count >= 8) then
        byte2 = iand(ishft(final_buffer, -16), 255)
        write(*, '("Second byte extracted: ", I0, " hex: 0x", Z2.2)') byte2, byte2
    else
        print *, "No second byte extracted (bit count < 8)"
        byte2 = 0
    end if
    print *, ""
    
    ! Now let's see what STB's exact pattern would be
    print *, "STB produces: 40 1F"
    print *, "We produce: 45 15"
    print *, "Difference analysis:"
    print *, "40 = 01000000"
    print *, "45 = 01000101"
    print *, "1F = 00011111"  
    print *, "15 = 00010101"
    
contains
    
    subroutine print_binary(value, bits)
        integer, intent(in) :: value, bits
        integer :: i, bit
        
        do i = bits-1, 0, -1
            bit = iand(ishft(value, -i), 1)
            write(*, '(I0)', advance='no') bit
        end do
        print *, ""
    end subroutine print_binary
    
end program debug_bit_analysis