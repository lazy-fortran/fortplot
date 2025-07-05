program debug_dct_dc
    use fortplot_jpeg
    implicit none
    
    real :: block(8,8)
    real :: y_val, shifted_y
    integer :: i, j
    
    ! Create uniform block with Y=127 (gray)
    y_val = 127.0
    shifted_y = y_val - 128.0  ! JPEG shifts by 128
    
    ! Fill 8x8 block with shifted value
    block = shifted_y
    
    print *, "Input block (all values):", shifted_y
    print *, "Expected DC coefficient after DCT:", shifted_y * 8.0
    
    ! The issue might be that for an 8x8 image with 2x2 subsampling,
    ! we're creating 4 Y blocks but only have data for 1
    
    print *, ""
    print *, "For 8x8 image with 2x2 subsampling:"
    print *, "- We encode 4 Y blocks (top-left, top-right, bottom-left, bottom-right)"
    print *, "- But we only have 8x8 pixels total"
    print *, "- The extra blocks will contain edge-replicated data"
    
    print *, ""
    print *, "This might cause the black appearance if the blocks are not properly filled"
    
end program debug_dct_dc