program debug_bit_count
    implicit none
    
    print *, "Bit count for 8x8 gray (127,127,127) image:"
    print *, ""
    print *, "Y block (Y=-1 after DC shift):"
    print *, "  DC category 1: 3 bits (code=2)"
    print *, "  DC value -1:   1 bit (0 in ones complement)"  
    print *, "  EOB:           4 bits (code=10)"
    print *, "  Total:         8 bits"
    print *, ""
    print *, "U block (U=0):"
    print *, "  DC category 0: 2 bits (code=0)"
    print *, "  EOB:           2 bits (code=0)"
    print *, "  Total:         4 bits"
    print *, ""
    print *, "V block (V=0):"
    print *, "  DC category 0: 2 bits (code=0)"
    print *, "  EOB:           2 bits (code=0)" 
    print *, "  Total:         4 bits"
    print *, ""
    print *, "Grand total: 8 + 4 + 4 = 16 bits = 2 bytes"
    print *, "Plus padding to byte boundary"
    print *, ""
    print *, "But we're outputting 3 bytes, STB outputs 5 bytes"
    print *, "Something is wrong with our encoding!"
    
end program debug_bit_count