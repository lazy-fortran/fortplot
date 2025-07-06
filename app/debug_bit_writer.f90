program debug_bit_writer
    implicit none
    
    ! Simulate our exact bit writer with debug output
    integer :: bit_buffer, bit_count
    integer(1) :: output(20)
    integer :: output_pos
    
    bit_buffer = 0
    bit_count = 0
    output_pos = 1
    
    print *, "=== Simulating our bit writer ==="
    print *, ""
    
    ! Reproduce the exact sequence that leads to scan data
    ! Based on our scan data: 65 14 51 xx xx
    
    ! First, let's work backwards from known good bytes
    ! 65 14 51 = 01100101 00010100 01010001
    ! This represents the first part of our encoding
    
    ! Let's trace what gives us these exact bytes:
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 6, 3)      ! Y DC cat 5
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 10, 5)     ! Y DC val -21
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! U DC cat 0
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! V DC cat 0
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 10, 4)     ! Y AC EOB
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! U AC EOB
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! V AC EOB
    
    print *, ""
    print *, "Before fillBits:"
    print *, "Bit buffer:", bit_buffer
    print *, "Bit count:", bit_count
    print *, "Output so far:"
    call show_output(output, output_pos)
    
    ! Now apply fillBits
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 127, 7)    ! fillBits
    
    print *, ""
    print *, "After fillBits:"
    print *, "Bit buffer:", bit_buffer
    print *, "Bit count:", bit_count
    print *, "Final output:"
    call show_output(output, output_pos)
    
    ! Flush any remaining bits
    if (bit_count > 0) then
        print *, ""
        print *, "Flushing remaining", bit_count, "bits"
        call flush_remaining_bits(bit_buffer, bit_count, output, output_pos)
    end if
    
contains

    subroutine debug_write_bits(bit_buffer, bit_count, output, output_pos, code, bits)
        integer, intent(inout) :: bit_buffer, bit_count
        integer(1), intent(inout) :: output(:)
        integer, intent(inout) :: output_pos
        integer, intent(in) :: code, bits
        
        integer :: c
        
        write(*,'(A,I0,A,I0,A)',advance='no') "Writing code=", code, " bits=", bits, " -> "
        
        ! STB algorithm
        bit_count = bit_count + bits
        bit_buffer = ior(bit_buffer, ishft(code, 24 - bit_count))
        
        write(*,'(A,I0,A,B0.32)') "buffer=", bit_buffer, " (", bit_buffer, ")"
        
        ! Extract bytes
        do while (bit_count >= 8)
            c = iand(ishft(bit_buffer, -16), 255)
            output(output_pos) = int(c, 1)
            write(*,'(A,I0,A,Z2.2)') "  Extracted byte ", output_pos, ": ", c
            output_pos = output_pos + 1
            
            if (c == 255) then
                output(output_pos) = 0_1
                write(*,'(A,I0,A)') "  Stuffed byte ", output_pos, ": 00"
                output_pos = output_pos + 1
            end if
            
            bit_buffer = ishft(bit_buffer, 8)
            bit_count = bit_count - 8
        end do
    end subroutine debug_write_bits
    
    subroutine flush_remaining_bits(bit_buffer, bit_count, output, output_pos)
        integer, intent(inout) :: bit_buffer, bit_count
        integer(1), intent(inout) :: output(:)
        integer, intent(inout) :: output_pos
        
        integer :: c
        
        if (bit_count > 0) then
            c = iand(ishft(bit_buffer, -16), 255)
            output(output_pos) = int(c, 1)
            write(*,'(A,I0,A,Z2.2)') "Final flush byte ", output_pos, ": ", c
            output_pos = output_pos + 1
        end if
    end subroutine flush_remaining_bits
    
    subroutine show_output(output, output_pos)
        integer(1), intent(in) :: output(:)
        integer, intent(in) :: output_pos
        integer :: i
        
        do i = 1, output_pos-1
            write(*,'(1X,Z2.2)',advance='no') output(i)
        end do
        print *
    end subroutine show_output
    
end program debug_bit_writer