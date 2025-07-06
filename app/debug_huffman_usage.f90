program debug_huffman_usage
    use fortplot_jpeg, only: initialize_huffman_tables, YDC_HT, YAC_HT, UVDC_HT, UVAC_HT
    implicit none
    
    call initialize_huffman_tables()
    
    print *, "=== Checking Huffman table usage ==="
    print *, ""
    
    ! For U and V blocks with DC=0:
    print *, "U/V DC encoding (category 0):"
    print *, "UVDC_HT(1,:) =", UVDC_HT(1,1), UVDC_HT(1,2)
    print *, ""
    
    ! For U and V AC (EOB):
    print *, "U/V AC EOB encoding:"
    print *, "UVAC_HT(1,:) =", UVAC_HT(1,1), UVAC_HT(1,2)
    print *, ""
    
    ! Let's check if we're using the wrong table
    print *, "Our code uses YAC_HT for all AC, but should use:"
    print *, "- YAC_HT for Y blocks"
    print *, "- UVAC_HT for U/V blocks"
    print *, ""
    
    ! Recalculate with correct tables:
    print *, "Correct bit sequence:"
    print *, "Y1: 100 (DC) + 100 (val) + 1010 (YAC EOB) = 1001001010"
    print *, "Y2: 00 (DC) + 1010 (YAC EOB) = 001010"  
    print *, "Y3: 00 (DC) + 1010 (YAC EOB) = 001010"
    print *, "Y4: 00 (DC) + 1010 (YAC EOB) = 001010"
    print *, "U:  00 (UVDC) + 00 (UVAC EOB) = 0000"
    print *, "V:  00 (UVDC) + 00 (UVAC EOB) = 0000"
    print *, ""
    print *, "New total: 10 + 6 + 6 + 6 + 4 + 4 = 36 bits"
    
end program debug_huffman_usage