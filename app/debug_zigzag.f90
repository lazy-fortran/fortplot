program debug_zigzag
    implicit none
    
    ! Our current zigzag table
    integer, parameter :: our_zigzag(64) = [ &
        1,  2,  9, 17, 10,  3,  4, 11, &
       18, 25, 33, 26, 19, 12,  5,  6, &
       13, 20, 27, 34, 41, 49, 42, 35, &
       28, 21, 14,  7,  8, 15, 22, 29, &
       36, 43, 50, 57, 58, 51, 44, 37, &
       30, 23, 16, 24, 31, 38, 45, 52, &
       59, 60, 53, 46, 39, 32, 40, 47, &
       54, 61, 62, 55, 48, 56, 63, 64]
       
    ! STB zigzag table (0-based) converted to 1-based
    integer, parameter :: stb_zigzag(64) = [ &
        1,  2,  6,  7, 15, 16, 28, 29, &
        3,  5,  8, 14, 17, 27, 30, 43, &
        4,  9, 13, 18, 26, 31, 42, 44, &
       10, 12, 19, 25, 32, 41, 45, 54, &
       11, 20, 24, 33, 40, 46, 53, 55, &
       21, 23, 34, 39, 47, 52, 56, 61, &
       22, 35, 38, 48, 51, 57, 60, 62, &
       36, 37, 49, 50, 58, 59, 63, 64]
       
    integer :: i
    logical :: tables_match
    
    print *, "Zigzag Table Comparison"
    print *, "======================="
    
    tables_match = .true.
    do i = 1, 64
        if (our_zigzag(i) /= stb_zigzag(i)) then
            print '("Position ", I2, ": Our=", I2, " STB=", I2, " MISMATCH")', &
                  i, our_zigzag(i), stb_zigzag(i)
            tables_match = .false.
        end if
    end do
    
    if (tables_match) then
        print *, "Tables match!"
    else
        print *, ""
        print *, "TABLES DO NOT MATCH!"
        print *, ""
        print *, "Our zigzag order:"
        do i = 1, 8
            print '(8I3)', our_zigzag((i-1)*8+1:i*8)
        end do
        print *, ""
        print *, "STB zigzag order (1-based):"
        do i = 1, 8
            print '(8I3)', stb_zigzag((i-1)*8+1:i*8)
        end do
    end if
    
end program