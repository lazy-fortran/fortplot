program debug_buffer_analysis
    implicit none
    
    integer :: before_buffer, after_buffer
    integer :: before_count, after_count
    integer :: extracted_byte, remaining_bits
    
    ! Values from debug output
    before_buffer = 1158938624
    before_count = 7
    after_buffer = 368836608  
    after_count = 6
    
    print *, "=== Bit buffer analysis ==="
    print *, ""
    print *, "Before fillBits:"
    print *, "Buffer =", before_buffer
    print *, "Count  =", before_count
    call print_buffer_bits(before_buffer, before_count)
    print *, ""
    
    print *, "After fillBits:"
    print *, "Buffer =", after_buffer
    print *, "Count  =", after_count
    call print_buffer_bits(after_buffer, after_count)
    print *, ""
    
    ! What byte was extracted during fillBits?
    extracted_byte = iand(ishft(before_buffer, -16), 255)
    print *, "Byte extracted during fillBits:", extracted_byte
    write(*,'(A,Z2.2,A)') "Hex: ", extracted_byte, ""
    call print_byte_bits(extracted_byte)
    print *, ""
    
    ! What are the remaining 6 bits?
    remaining_bits = iand(ishft(after_buffer, -16), 255)
    print *, "Remaining bits for final flush:", remaining_bits
    write(*,'(A,Z2.2,A)') "Hex: ", remaining_bits, ""
    call print_byte_bits(remaining_bits)
    print *, ""
    
    print *, "This explains our final 2 bytes: 45 15"
    print *, "Expected STB final 2 bytes: 40 1F"
    print *, ""
    print *, "The difference is in how the bit buffer state"
    print *, "differs before fillBits between STB and our impl."
    
contains
    
    subroutine print_buffer_bits(buffer, count)
        integer, intent(in) :: buffer, count
        integer :: i, bit_val
        
        write(*,'(A)',advance='no') "Bits: "
        do i = 23, 24-count, -1
            bit_val = iand(ishft(buffer, -i), 1)
            write(*,'(I1)',advance='no') bit_val
        end do
        print *, ""
        write(*,'(A,B0.32)') "Full: ", buffer
    end subroutine print_buffer_bits
    
    subroutine print_byte_bits(byte_val)
        integer, intent(in) :: byte_val
        integer :: i, bit_val
        
        write(*,'(A)',advance='no') "Bits: "
        do i = 7, 0, -1
            bit_val = iand(ishft(byte_val, -i), 1)
            write(*,'(I1)',advance='no') bit_val
        end do
        print *
    end subroutine print_byte_bits
    
end program debug_buffer_analysis