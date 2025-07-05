program debug_dct_output
    implicit none
    
    real :: block(8,8)
    real :: temp_block(8,8)
    integer :: i, j
    
    ! Create a uniform block with value -1 (Y=127 shifted by 128)
    block = -1.0
    temp_block = block
    
    print *, "Input block (all -1.0):"
    print '(8F8.2)', block(1,:)
    
    ! Apply simple DCT to understand the output
    ! For a uniform block, only the DC coefficient should be non-zero
    ! DC coefficient = sum of all values * normalization
    ! = 64 * (-1.0) * sqrt(1/64) = -8.0
    
    print *, ""
    print *, "Expected DCT output:"
    print *, "DC coefficient (top-left): -8.0"
    print *, "All AC coefficients: 0.0"
    
    ! The issue might be in the STB DCT implementation
    ! or in the quantization step
    
    print *, ""
    print *, "After quantization with quality 90:"
    print *, "Y quant table first value is typically 3"
    print *, "So DC becomes: -8.0 / 3 = -2.67, rounds to -3"
    
end program debug_dct_output