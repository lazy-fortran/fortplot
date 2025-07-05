program verify_dc_encoding
    implicit none
    
    integer :: i, dc_value, size, additional_bits, expected
    
    print *, "Verifying DC encoding matches one's complement:"
    print *, ""
    
    ! Test case 1: DC = -1, size = 1
    dc_value = -1
    size = 1
    additional_bits = dc_value + (2**size - 1)
    expected = 0  ! One's complement of 1 in 1 bit
    print '("DC = ", I4, ", size = ", I1, ": additional_bits = ", I3, " (binary: ", B0.1, "), expected = ", I3)', &
        dc_value, size, additional_bits, additional_bits, expected
    if (additional_bits /= expected) print *, "ERROR!"
    
    ! Test case 2: DC = 1, size = 1
    dc_value = 1
    size = 1
    additional_bits = dc_value
    expected = 1
    print '("DC = ", I4, ", size = ", I1, ": additional_bits = ", I3, " (binary: ", B0.1, "), expected = ", I3)', &
        dc_value, size, additional_bits, additional_bits, expected
    if (additional_bits /= expected) print *, "ERROR!"
    
    print *, ""
    
    ! Test case 3: DC = -3, size = 2
    dc_value = -3
    size = 2
    additional_bits = dc_value + (2**size - 1)
    expected = 0  ! One's complement of 3 (11b) in 2 bits = 00b
    print '("DC = ", I4, ", size = ", I1, ": additional_bits = ", I3, " (binary: ", B0.2, "), expected = ", I3)', &
        dc_value, size, additional_bits, additional_bits, expected
    if (additional_bits /= expected) print *, "ERROR!"
    
    ! Test case 4: DC = -2, size = 2  
    dc_value = -2
    size = 2
    additional_bits = dc_value + (2**size - 1)
    expected = 1  ! One's complement of 2 (10b) in 2 bits = 01b
    print '("DC = ", I4, ", size = ", I1, ": additional_bits = ", I3, " (binary: ", B0.2, "), expected = ", I3)', &
        dc_value, size, additional_bits, additional_bits, expected
    if (additional_bits /= expected) print *, "ERROR!"
    
    print *, ""
    
    ! Test more cases
    print *, "More examples showing one's complement pattern:"
    print *, "Size 3 (3 bits):"
    do i = -7, 7
        if (i == 0) cycle
        if (abs(i) <= 7 .and. abs(i) > 3) then
            size = 3
            if (i > 0) then
                additional_bits = i
            else
                additional_bits = i + (2**size - 1)
            end if
            print '("  DC = ", I3, ": bits = ", I3, " (binary: ", B0.3, ")")', &
                i, additional_bits, additional_bits
        end if
    end do
    
    print *, ""
    print *, "Notice the pattern:"
    print *, "  Positive:  7 -> 111b"
    print *, "  Negative: -7 -> 000b (one's complement of 111b)"
    print *, "  Positive:  6 -> 110b"  
    print *, "  Negative: -6 -> 001b (one's complement of 110b)"
    print *, "  Positive:  5 -> 101b"
    print *, "  Negative: -5 -> 010b (one's complement of 101b)"
    print *, "  Positive:  4 -> 100b"
    print *, "  Negative: -4 -> 011b (one's complement of 100b)"
    
end program verify_dc_encoding