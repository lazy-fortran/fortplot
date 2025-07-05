program debug_dc_bits
    implicit none
    
    integer :: dc_val, category, value_bits
    
    dc_val = -3
    category = 2  ! abs(-3) = 3, which is category 2
    
    print *, "DC value encoding for -3:"
    print *, "Category:", category
    
    ! JPEG uses one's complement for negative values
    ! For category 2, we have 2 bits
    ! Range is -3 to -2 and 2 to 3
    
    ! One's complement of -3 in 2 bits:
    ! -3 in binary would be 11 (two's complement)
    ! One's complement is NOT(-3) = NOT(11111101) = 00000010 = 2
    ! But that's for 8-bit. For 2-bit:
    
    ! Actually, JPEG formula is: negative_value + (2^bits - 1)
    value_bits = dc_val + (2**category - 1)
    print *, "Formula: -3 + (2^2 - 1) = -3 + 3 = 0"
    print *, "Value bits:", value_bits
    print '(A,B2.2)', "In binary: ", value_bits
    
    print *, ""
    print *, "Wait, this gives 00 for -3"
    print *, "Let me check if the formula is correct..."
    
    ! Actually, for -3 in category 2:
    ! The pattern should be the bitwise NOT of 3
    ! 3 = 11, so NOT(11) = 00
    ! Yes, 00 is correct for -3!
    
end program debug_dc_bits