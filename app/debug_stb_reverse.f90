program debug_stb_reverse
    implicit none
    
    ! We know STB produces 0x40 0x1F
    ! Let's reverse engineer what bit buffer state produces this
    
    integer :: stb_byte1, stb_byte2, combined
    integer :: test_bit_count, test_buffer, result_buffer
    integer :: extracted_byte1, extracted_byte2
    integer :: i, shift_pos, fillbits_contribution
    
    stb_byte1 = int(Z'40')  ! 64
    stb_byte2 = int(Z'1F')  ! 31
    
    ! Work backwards from the output
    ! If STB calls fillBits(0x7F, 7) and gets 0x40 0x1F,
    ! what was the initial bit buffer state?
    
    print *, "=== Reverse Engineering STB Pattern ==="
    print *, "STB produces: 0x40 0x1F"
    print *, ""
    
    ! The output 0x40 0x1F means:
    ! First byte: 0x40 = 01000000
    ! Second byte: 0x1F = 00011111
    
    ! Combine these into a 16-bit value
    combined = ior(ishft(stb_byte1, 8), stb_byte2)
    write(*, '("Combined 16-bit: 0x", Z4.4)') combined
    print *, "Binary: "
    call print_binary(combined, 16)
    print *, ""
    
    ! Now, we know fillBits adds 0x7F (1111111) for 7 bits
    ! The question is: what initial state + fillBits = this pattern?
    
    ! Let's assume the pattern came from some initial_buffer + fillBits(0x7F, 7)
    ! We need to find initial_buffer and initial_count such that:
    ! final_buffer = initial_buffer | (0x7F << (24 - (initial_count + 7)))
    ! And (final_buffer >> 16) & 0xFFFF gives us 0x401F
    
    ! This is complex to reverse, so let's try a different approach:
    ! Test different initial bit counts to see what produces 0x40 0x1F
    
    print *, "Testing different initial bit states:"
    print *, ""
    
    do i = 1, 15
        test_bit_count = i
        
        ! Calculate what buffer value we'd need
        ! If we have i bits, and add 7 fillBits = i+7 total
        ! The 0x7F goes to position (24 - (i+7)) = (17-i)
        shift_pos = 17 - i
        fillbits_contribution = ishft(int(Z'7F'), shift_pos)
        
        ! The total buffer after fillBits should put 0x401F in the top 16 bits
        ! So buffer >> 16 should be 0x401F
        ! This means buffer = 0x401F0000 + lower_bits
        
        ! Back-calculate what the initial buffer should be
        test_buffer = int(Z'401F0000') - fillbits_contribution
        
        ! Now test this combination
        result_buffer = ior(test_buffer, fillbits_contribution)
        extracted_byte1 = iand(ishft(result_buffer, -16), 255)
        extracted_byte2 = iand(ishft(ishft(result_buffer, 8), -16), 255)
        
        if (extracted_byte1 == stb_byte1) then
            write(*, '("MATCH! Bit count: ", I0, " Initial buffer: 0x", Z8.8)') &
                   test_bit_count, test_buffer
            write(*, '("Final buffer: 0x", Z8.8)') result_buffer
            write(*, '("Extracted: 0x", Z2.2, " 0x", Z2.2)') extracted_byte1, extracted_byte2
            print *, ""
        end if
    end do
    
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
    
end program debug_stb_reverse