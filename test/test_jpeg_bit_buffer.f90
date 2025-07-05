program test_jpeg_bit_buffer
    use iso_fortran_env, only: int32
    implicit none
    
    call test_bit_accumulation()
    call test_byte_extraction()
    call test_buffer_shifting()
    call test_bit_stuffing()
    
    print *, "All bit buffer tests passed!"
    
contains
    
    subroutine test_bit_accumulation()
        integer(int32) :: bit_buf, bit_count
        
        ! Initialize buffer
        bit_buf = 0
        bit_count = 0
        
        ! Add some bits: 101 (3 bits)
        call add_bits(bit_buf, bit_count, 5, 3)  ! 101 in binary
        
        if (bit_buf /= 5 .or. bit_count /= 3) then
            print *, "FAIL: After adding 101, expected buf=5, count=3, got", bit_buf, bit_count
            error stop
        end if
        
        ! Add more bits: 1100 (4 bits)
        call add_bits(bit_buf, bit_count, 12, 4)  ! 1100 in binary
        
        ! Should now have: 1011100 (7 bits total)
        if (bit_buf /= 92 .or. bit_count /= 7) then
            print *, "FAIL: After adding 1100, expected buf=92, count=7, got", bit_buf, bit_count
            error stop
        end if
        
        print *, "Bit accumulation test passed"
    end subroutine
    
    subroutine test_byte_extraction()
        integer(int32) :: bit_buf, bit_count
        integer(int32) :: extracted_byte
        
        ! Set up buffer with 24 bits: 110110011010101111001000
        bit_buf = 14249416  ! This is the decimal value of the binary above
        bit_count = 24
        
        ! Extract top byte: should be 11011001 = 217
        extracted_byte = ishft(bit_buf, -16)
        extracted_byte = iand(extracted_byte, 255)
        
        if (extracted_byte /= 217) then
            print *, "FAIL: Expected extracted byte 217, got", extracted_byte
            error stop
        end if
        
        ! Shift buffer left by 8 bits
        bit_buf = ishft(bit_buf, 8)
        bit_buf = iand(bit_buf, 16777215)  ! Keep only 24 bits
        bit_count = bit_count - 8
        
        if (bit_count /= 16) then
            print *, "FAIL: Expected bit count 16 after extraction, got", bit_count
            error stop
        end if
        
        print *, "Byte extraction test passed"
    end subroutine
    
    subroutine test_buffer_shifting()
        integer(int32) :: bit_buf, bit_count
        
        ! Test shifting with various bit counts
        bit_buf = 0
        bit_count = 0
        
        ! Add 32 bits worth of data (should handle overflow)
        call add_bits(bit_buf, bit_count, 255, 8)   ! 8 bits
        call add_bits(bit_buf, bit_count, 170, 8)   ! 8 more bits (16 total)
        call add_bits(bit_buf, bit_count, 85, 8)    ! 8 more bits (24 total)
        call add_bits(bit_buf, bit_count, 51, 8)    ! 8 more bits (32 total)
        
        ! Should have triggered byte output and reset
        if (bit_count > 24) then
            print *, "FAIL: Bit count should not exceed 24, got", bit_count
            error stop
        end if
        
        print *, "Buffer shifting test passed"
    end subroutine
    
    subroutine test_bit_stuffing()
        integer(int32) :: bit_buf, bit_count
        integer(int32) :: output_bytes(10), output_count, i
        
        ! Create a pattern that will generate 0xFF
        bit_buf = 0
        bit_count = 0
        output_count = 0
        
        ! Add bits that will create 0xFF when extracted
        call add_bits(bit_buf, bit_count, 255, 8)   ! 11111111
        call add_bits(bit_buf, bit_count, 128, 8)   ! 10000000
        
        ! Extract bytes and check for stuffing
        call extract_bytes_with_stuffing(bit_buf, bit_count, output_bytes, output_count)
        
        
        ! Should have 0xFF followed by 0x00 (stuffing)
        if (output_count < 2) then
            print *, "FAIL: Expected at least 2 output bytes, got", output_count
            error stop
        end if
        
        if (output_bytes(1) /= 255) then
            print *, "FAIL: Expected first byte to be 255, got", output_bytes(1)
            error stop
        end if
        
        if (output_bytes(2) /= 0) then
            print *, "FAIL: Expected second byte to be 0 (stuffing), got", output_bytes(2)
            error stop
        end if
        
        print *, "Bit stuffing test passed"
    end subroutine
    
    subroutine add_bits(bit_buf, bit_count, value, num_bits)
        integer(int32), intent(inout) :: bit_buf, bit_count
        integer(int32), intent(in) :: value, num_bits
        
        ! Shift existing bits left
        bit_buf = ishft(bit_buf, num_bits)
        
        ! Add new bits
        bit_buf = ior(bit_buf, value)
        
        ! Update bit count
        bit_count = bit_count + num_bits
        
        ! Keep only 24 bits (simulate real buffer limit)
        if (bit_count > 24) then
            bit_buf = iand(bit_buf, 16777215)  ! Keep only 24 bits
            bit_count = 24
        end if
    end subroutine
    
    subroutine extract_bytes_with_stuffing(bit_buf, bit_count, output_bytes, output_count)
        integer(int32), intent(inout) :: bit_buf, bit_count
        integer(int32), intent(out) :: output_bytes(10)
        integer(int32), intent(out) :: output_count
        
        integer(int32) :: byte_val
        
        output_count = 0
        
        do while (bit_count >= 8)
            ! Extract top byte from 24-bit buffer
            byte_val = ishft(bit_buf, -(bit_count - 8))
            byte_val = iand(byte_val, 255)
            
            ! Store byte
            output_count = output_count + 1
            output_bytes(output_count) = byte_val
            
            ! Check for 0xFF and add stuffing
            if (byte_val == 255) then
                output_count = output_count + 1
                output_bytes(output_count) = 0  ! Stuff 0x00 after 0xFF
            end if
            
            ! Shift buffer
            bit_buf = ishft(bit_buf, 8)
            bit_buf = iand(bit_buf, 16777215)  ! Keep only 24 bits
            bit_count = bit_count - 8
        end do
    end subroutine
    
end program