program debug_dct_normalization
    implicit none
    
    real :: block(8,8), result
    integer :: i, j
    
    ! Create uniform block
    block = 1.0
    
    print *, "DCT normalization check:"
    print *, "Input: 8x8 block of all 1.0"
    
    ! Expected DC coefficient after 2D DCT:
    ! 2D DCT formula for DC: (2/N) * sum(all values)
    ! But different implementations use different scaling
    
    ! Method 1: Standard DCT
    result = 64.0 * (1.0/8.0)  ! sum * normalization
    print *, "Standard DCT DC:", result
    
    ! Method 2: STB style (no normalization in forward DCT)
    result = 64.0  ! just the sum
    print *, "STB style DC:", result
    
    print *, ""
    print *, "If STB doesn't normalize in DCT but in quantization,"
    print *, "our DC values will be 8x too small!"
    
end program debug_dct_normalization