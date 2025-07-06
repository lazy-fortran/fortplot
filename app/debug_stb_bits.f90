program debug_stb_bits
    implicit none
    
    ! Manual bit-by-bit reconstruction of what STB should produce
    ! for 8x8 uniform gray (127,127,127) with quality 90
    
    integer :: bit_buffer, bit_count, i
    integer(1) :: output_bytes(10)
    integer :: output_pos
    
    print *, "=== STB bit-by-bit reconstruction ==="
    print *, ""
    
    ! Initialize bit buffer
    bit_buffer = 0
    bit_count = 0
    output_pos = 1
    
    ! For uniform gray, we expect:
    ! Y DC = -21 (category 5, 5 bits: 01010)
    ! U DC = 0 (category 0, 0 bits: none)
    ! V DC = 0 (category 0, 0 bits: none)
    ! Y AC = EOB (4 bits: 1010)
    ! U AC = EOB (2 bits: 00)
    ! V AC = EOB (2 bits: 00)
    ! fillBits = 0x7F (7 bits: 1111111)
    
    print *, "Expected encoding sequence:"
    print *, "1. Y DC category 5: code=6, bits=3 -> 110"
    print *, "2. Y DC value -21: 5 bits -> 01010"
    print *, "3. U DC category 0: code=0, bits=2 -> 00"
    print *, "4. V DC category 0: code=0, bits=2 -> 00"
    print *, "5. Y AC EOB: code=10, bits=4 -> 1010"
    print *, "6. U AC EOB: code=0, bits=2 -> 00"
    print *, "7. V AC EOB: code=0, bits=2 -> 00"
    print *, "8. fillBits: code=0x7F, bits=7 -> 1111111"
    print *, ""
    
    ! Manually construct the bit stream with correct Huffman codes
    call add_bits(bit_buffer, bit_count, 6, 3)      ! Y DC category 5: code=6, bits=3
    call add_bits(bit_buffer, bit_count, 10, 5)     ! Y DC value -21: 01010 (5 bits)
    call add_bits(bit_buffer, bit_count, 0, 2)      ! U DC category 0: code=0, bits=2
    call add_bits(bit_buffer, bit_count, 0, 2)      ! V DC category 0: code=0, bits=2
    call add_bits(bit_buffer, bit_count, 10, 4)     ! Y AC EOB: code=10, bits=4
    call add_bits(bit_buffer, bit_count, 0, 2)      ! U AC EOB: code=0, bits=2
    call add_bits(bit_buffer, bit_count, 0, 2)      ! V AC EOB: code=0, bits=2
    call add_bits(bit_buffer, bit_count, 127, 7)    ! fillBits: 0x7F, 7 bits
    
    print *, "Total bits:", bit_count
    print *, "Bit buffer (hex):", bit_buffer
    print *, "Remaining bits:", bit_count
    
    ! Extract final bytes
    call extract_bytes(bit_buffer, bit_count, output_bytes, output_pos)
    
    print *, ""
    print *, "Final bytes:"
    do i = 1, output_pos-1
        write(*,'(1X,Z2.2)',advance='no') output_bytes(i)
    end do
    print *, ""
    print *, ""
    print *, "Compare with STB: 65 14 51 40 1F"
    print *, "Compare with Ours: 65 14 51 45 15"
    
contains
    
    subroutine add_bits(bit_buffer, bit_count, code, bits)
        integer, intent(inout) :: bit_buffer, bit_count
        integer, intent(in) :: code, bits
        
        bit_count = bit_count + bits
        bit_buffer = ior(bit_buffer, ishft(code, 24 - bit_count))
        
        write(*,'(A,I0,A,I0,A,B0.32)') "Added code=", code, " bits=", bits, " -> ", bit_buffer
    end subroutine add_bits
    
    subroutine extract_bytes(bit_buffer, bit_count, output_bytes, output_pos)
        integer, intent(inout) :: bit_buffer, bit_count
        integer(1), intent(out) :: output_bytes(:)
        integer, intent(inout) :: output_pos
        
        integer :: byte_val
        
        do while (bit_count >= 8)
            byte_val = iand(ishft(bit_buffer, -16), 255)
            output_bytes(output_pos) = int(byte_val, 1)
            output_pos = output_pos + 1
            bit_buffer = ishft(bit_buffer, 8)
            bit_count = bit_count - 8
        end do
        
        ! Handle remaining bits
        if (bit_count > 0) then
            byte_val = iand(ishft(bit_buffer, -16), 255)
            output_bytes(output_pos) = int(byte_val, 1)
            output_pos = output_pos + 1
        end if
    end subroutine extract_bytes
    
end program debug_stb_bits