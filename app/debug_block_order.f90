program debug_block_order
    use fortplot_jpeg, only: initialize_huffman_tables, YDC_HT, YAC_HT
    implicit none
    
    integer :: total_bits, dc_y, dc_u, dc_v, i
    
    call initialize_huffman_tables()
    
    print *, "=== Analyzing block encoding order for 8x8 image ==="
    print *, ""
    
    ! For an 8x8 image with 2x2 subsampling (quality <= 90), STB processes:
    ! - 16x16 Y data (4 8x8 blocks)
    ! - 8x8 U data (1 block)
    ! - 8x8 V data (1 block)
    
    print *, "For 8x8 image with quality 90:"
    print *, "STB processes 16x16 MCU with 2x2 subsampling"
    print *, ""
    
    ! Let's calculate the bit stream for all-gray image
    ! All blocks have DC=-3, no AC coefficients
    
    total_bits = 0
    dc_y = -3
    dc_u = 0
    dc_v = 0
    
    ! First Y block: DC=-3
    print *, "Y block 1: DC=-3"
    print *, "  Category 3: Huffman=", YDC_HT(4,1), "bits=", YDC_HT(4,2)
    print *, "  Value bits: 4 (3 bits)"
    print *, "  EOB: Huffman=", YAC_HT(1,1), "bits=", YAC_HT(1,2)
    total_bits = total_bits + YDC_HT(4,2) + 3 + YAC_HT(1,2)
    
    ! Remaining 3 Y blocks: DC=0 (diff from previous)
    do i = 2, 4
        print *, "Y block", i, ": DC=0 (diff)"
        print *, "  Category 0: Huffman=", YDC_HT(1,1), "bits=", YDC_HT(1,2)
        print *, "  EOB: Huffman=", YAC_HT(1,1), "bits=", YAC_HT(1,2)
        total_bits = total_bits + YDC_HT(1,2) + YAC_HT(1,2)
    end do
    
    ! U block: DC=0
    print *, "U block: DC=0"
    print *, "  Category 0: Huffman=", YDC_HT(1,1), "bits=", YDC_HT(1,2)
    print *, "  EOB: Huffman=", YAC_HT(1,1), "bits=", YAC_HT(1,2)
    total_bits = total_bits + YDC_HT(1,2) + YAC_HT(1,2)
    
    ! V block: DC=0
    print *, "V block: DC=0"
    print *, "  Category 0: Huffman=", YDC_HT(1,1), "bits=", YDC_HT(1,2)
    print *, "  EOB: Huffman=", YAC_HT(1,1), "bits=", YAC_HT(1,2)
    total_bits = total_bits + YDC_HT(1,2) + YAC_HT(1,2)
    
    print *, ""
    print *, "Total bits before fillBits:", total_bits
    print *, ""
    
    ! Now let's trace the exact bit sequence
    print *, "Bit sequence:"
    print *, "Y1: 100 (DC cat3) + 100 (value -3) + 1010 (EOB) = 1001001010"
    print *, "Y2: 00 (DC cat0) + 1010 (EOB) = 001010"
    print *, "Y3: 00 (DC cat0) + 1010 (EOB) = 001010"
    print *, "Y4: 00 (DC cat0) + 1010 (EOB) = 001010"
    print *, "U:  00 (DC cat0) + 1010 (EOB) = 001010"
    print *, "V:  00 (DC cat0) + 1010 (EOB) = 001010"
    print *, ""
    print *, "Combined: 1001001010 001010 001010 001010 001010 001010"
    print *, "Total: 10 + 6 + 6 + 6 + 6 + 6 = 40 bits"
    
end program debug_block_order