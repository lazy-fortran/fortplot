program debug_exact_calculation
    implicit none
    
    ! We want to find the exact buffer value that produces 0x40 0x1F
    ! when fillBits(0x7F, 7) is called with initial bit_count = 7
    
    integer :: target_byte1, target_byte2, target_final_top16
    integer :: target_final_buffer, fillbits_contribution, required_initial_buffer
    integer :: test_buffer, test_count, extracted1, extracted2
    
    target_byte1 = int(Z'40')  ! 64
    target_byte2 = int(Z'1F')  ! 31
    
    ! Reconstruct what the final 32-bit buffer should look like
    ! After fillBits, we'll have 14 bits total
    ! First extraction: (buffer >> 16) & 255 should give 0x40
    ! After left shift by 8: ((buffer << 8) >> 16) & 255 should give 0x1F
    
    ! This means the top 16 bits of the final buffer should be 0x401F
    target_final_top16 = ior(ishft(target_byte1, 8), target_byte2)
    write(*, '("Target final top 16 bits: 0x", Z4.4)') target_final_top16
    
    ! The final buffer should have 0x401F in positions 31:16
    ! Let's assume the bottom 16 bits can be anything, set them to 0 for simplicity
    target_final_buffer = ishft(target_final_top16, 16)
    write(*, '("Target final buffer: 0x", Z8.8)') target_final_buffer
    
    ! Now work backwards: initial_buffer + fillBits_contribution = target_final_buffer
    ! fillBits_contribution = 0x7F << (24 - 14) = 0x7F << 10 = 0x1FC00
    fillbits_contribution = ishft(int(Z'7F'), 10)
    write(*, '("fillBits contribution: 0x", Z8.8)') fillbits_contribution
    
    ! So: initial_buffer = target_final_buffer - fillbits_contribution
    required_initial_buffer = target_final_buffer - fillbits_contribution
    write(*, '("Required initial buffer: 0x", Z8.8)') required_initial_buffer
    
    ! Test this calculation
    print *, ""
    print *, "Testing the calculation:"
    
    test_buffer = required_initial_buffer
    test_count = 7
    
    ! Apply fillBits
    test_count = test_count + 7
    test_buffer = ior(test_buffer, ishft(int(Z'7F'), 24 - test_count))
    
    write(*, '("After fillBits: buffer=0x", Z8.8, " count=", I0)') test_buffer, test_count
    
    ! Extract bytes
    extracted1 = iand(ishft(test_buffer, -16), 255)
    test_buffer = ishft(test_buffer, 8)
    test_count = test_count - 8
    extracted2 = iand(ishft(test_buffer, -16), 255)
    
    write(*, '("Extracted byte 1: 0x", Z2.2)') extracted1
    write(*, '("Extracted byte 2: 0x", Z2.2)') extracted2
    
    if (extracted1 == target_byte1 .and. extracted2 == target_byte2) then
        print *, "SUCCESS! Calculation is correct."
    else
        print *, "FAILED! Need to adjust calculation."
    end if
    
end program debug_exact_calculation