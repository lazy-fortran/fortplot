program debug_block_encoding
    use fortplot_jpeg
    implicit none
    
    ! For an 8x8 image, we should encode:
    ! 1. One Y block at (1,1)
    ! 2. One U block at (1,1) 
    ! 3. One V block at (1,1)
    
    print *, "Block encoding for 8x8 image:"
    print *, "x=1, y=1:"
    print *, "  - Process Y block"
    print *, "  - Condition (x==1 .and. y==1) = ", (1==1 .and. 1==1)
    print *, "  - Should process U and V blocks"
    
    print *, ""
    print *, "Expected bit stream segments:"
    print *, "1. Y DC code + Y DC value + Y EOB"
    print *, "2. U DC code + U DC value + U EOB"
    print *, "3. V DC code + V DC value + V EOB"
    print *, "4. Flush padding bits"
    
end program debug_block_encoding