program debug_correct_bits
    implicit none
    
    ! Correct simulation based on actual DC values: -3, 0, 0, 0, 0, 0
    integer :: bit_buffer, bit_count
    integer(1) :: output(20)
    integer :: output_pos
    
    bit_buffer = 0
    bit_count = 0
    output_pos = 1
    
    print *, "=== Correct bit simulation based on actual DC values ==="
    print *, ""
    print *, "Actual encoding sequence:"
    print *, "Y1: DC=-3 (cat 2, code=3, 3 bits) + value 00 (2 bits)"
    print *, "Y2: DC=0 (cat 0, code=0, 2 bits)"
    print *, "Y3: DC=0 (cat 0, code=0, 2 bits)"
    print *, "Y4: DC=0 (cat 0, code=0, 2 bits)"
    print *, "U:  DC=0 (cat 0, code=0, 2 bits)"
    print *, "V:  DC=0 (cat 0, code=0, 2 bits)"
    print *, "4x Y AC EOB (code=10, 4 bits each)"
    print *, "2x UV AC EOB (code=0, 2 bits each)"
    print *, "fillBits (code=0x7F, 7 bits)"
    print *, ""
    
    ! Encode exactly what our encoder does
    ! Y1 DC: category 2, value -3 -> 00 in category 2
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 3, 3)      ! Y DC cat 2
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! Y DC val -3 -> 00
    
    ! Y2, Y3, Y4 DC: category 0
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! Y DC cat 0
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! Y DC cat 0  
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! Y DC cat 0
    
    ! U and V DC: category 0
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! U DC cat 0
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! V DC cat 0
    
    ! AC coefficients: all EOB
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 10, 4)     ! Y1 AC EOB
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 10, 4)     ! Y2 AC EOB
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 10, 4)     ! Y3 AC EOB
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 10, 4)     ! Y4 AC EOB
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! U AC EOB
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 0, 2)      ! V AC EOB
    
    ! fillBits
    call debug_write_bits(bit_buffer, bit_count, output, output_pos, 127, 7)    ! fillBits
    
    print *, ""
    print *, "Final simulation output:"
    call show_output(output, output_pos)
    
    print *, ""
    print *, "Compare with:"
    print *, "STB: 65 14 51 40 1F"
    print *, "Our: 65 14 51 45 15"
    
contains

    subroutine debug_write_bits(bit_buffer, bit_count, output, output_pos, code, bits)
        integer, intent(inout) :: bit_buffer, bit_count
        integer(1), intent(inout) :: output(:)
        integer, intent(inout) :: output_pos
        integer, intent(in) :: code, bits
        
        integer :: c
        
        ! STB algorithm
        bit_count = bit_count + bits
        bit_buffer = ior(bit_buffer, ishft(code, 24 - bit_count))
        
        ! Extract bytes
        do while (bit_count >= 8)
            c = iand(ishft(bit_buffer, -16), 255)
            output(output_pos) = int(c, 1)
            output_pos = output_pos + 1
            
            if (c == 255) then
                output(output_pos) = 0_1
                output_pos = output_pos + 1
            end if
            
            bit_buffer = ishft(bit_buffer, 8)
            bit_count = bit_count - 8
        end do
    end subroutine debug_write_bits
    
    subroutine show_output(output, output_pos)
        integer(1), intent(in) :: output(:)
        integer, intent(in) :: output_pos
        integer :: i
        
        do i = 1, output_pos-1
            write(*,'(1X,Z2.2)',advance='no') output(i)
        end do
        print *
    end subroutine show_output
    
end program debug_correct_bits