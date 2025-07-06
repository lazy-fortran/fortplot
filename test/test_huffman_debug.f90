program test_huffman_debug
    use fortplot_jpeg, only: YDC_HT, UVDC_HT, YAC_HT, UVAC_HT, initialize_huffman_tables
    implicit none
    
    integer :: i, category
    
    call initialize_huffman_tables()
    
    print *, "=== Huffman table verification ==="
    print *, ""
    
    ! Check Y DC table (categories 0-11)
    print *, "Y DC Huffman codes:"
    do i = 1, 12
        print *, "Category", i-1, ": code=", YDC_HT(i,1), "bits=", YDC_HT(i,2)
    end do
    print *, ""
    
    ! Check UV DC table  
    print *, "UV DC Huffman codes:"
    do i = 1, 12
        print *, "Category", i-1, ": code=", UVDC_HT(i,1), "bits=", UVDC_HT(i,2)
    end do
    print *, ""
    
    ! Check AC tables
    print *, "Y AC key codes:"
    print *, "EOB (0x00):", YAC_HT(1,1), YAC_HT(1,2)
    print *, ""
    print *, "UV AC key codes:"
    print *, "EOB (0x00):", UVAC_HT(1,1), UVAC_HT(1,2)
    print *, ""
    
    ! For value -21, determine category
    ! Category is the number of bits needed to represent the value
    ! -21 in binary (5 bits): 11101 -> category 5
    print *, "Value -21 analysis:"
    print *, "Category: 5 (needs 5 bits)"
    print *, "Binary representation: 01010 (two's complement form)"
    print *, "Y DC category 5 code:", YDC_HT(6,1), "bits:", YDC_HT(6,2)
    
end program test_huffman_debug