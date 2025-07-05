program test_dc_encoding_fix
    implicit none
    
    integer :: dc_value, size, additional_bits
    integer :: test_values(15) = [-127, -63, -31, -15, -7, -3, -1, 0, 1, 3, 7, 15, 31, 63, 127]
    integer :: i
    
    print *, "Testing MPEG-1 DC encoding (one's complement for negative values)..."
    print *, ""
    print *, "DC Value | Size | Additional Bits | Binary representation"
    print *, "---------|------|-----------------|----------------------"
    
    do i = 1, 15
        dc_value = test_values(i)
        
        ! Calculate size category
        if (dc_value == 0) then
            size = 0
        else if (abs(dc_value) <= 1) then
            size = 1
        else if (abs(dc_value) <= 3) then
            size = 2
        else if (abs(dc_value) <= 7) then
            size = 3
        else if (abs(dc_value) <= 15) then
            size = 4
        else if (abs(dc_value) <= 31) then
            size = 5
        else if (abs(dc_value) <= 63) then
            size = 6
        else if (abs(dc_value) <= 127) then
            size = 7
        else
            size = 8
        end if
        
        ! Calculate additional bits
        if (size > 0) then
            if (dc_value >= 0) then
                ! Positive values: use value directly
                additional_bits = dc_value
            else
                ! Negative values: MPEG-1 uses one's complement
                ! For negative DC, we need to represent it as (value + 2^size - 1)
                ! This is equivalent to the one's complement of |value|
                additional_bits = dc_value + (2**size - 1)
            end if
        else
            additional_bits = 0
        end if
        
        write(*, '(I9, " | ", I4, " | ", I15, " | ", B0.8)') dc_value, size, additional_bits, additional_bits
    end do
    
    print *, ""
    print *, "Examples:"
    print *, "  -1 (size 1): -1 + 2^1 - 1 = -1 + 2 - 1 = 0 = 0b"
    print *, "  -3 (size 2): -3 + 2^2 - 1 = -3 + 4 - 1 = 0 = 00b"  
    print *, "  -7 (size 3): -7 + 2^3 - 1 = -7 + 8 - 1 = 0 = 000b"
    print *, ""
    print *, "This is the one's complement representation where:"
    print *, "  - For positive values: use the value itself"
    print *, "  - For negative values: use (value + 2^size - 1)"
    print *, "  - This gives the bitwise NOT of the positive value"
    
end program test_dc_encoding_fix