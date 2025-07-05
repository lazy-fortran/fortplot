program debug_jpeg_trace
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: gray_data(8*8*3)
    integer :: i, j
    real :: y_val, cb_val, cr_val
    
    ! Create 8x8 solid gray (127,127,127) image
    do i = 1, 8*8*3, 3
        gray_data(i) = int(127, 1)      ! R
        gray_data(i+1) = int(127, 1)    ! G
        gray_data(i+2) = int(127, 1)    ! B
    end do
    
    ! Calculate expected YCbCr values
    y_val = 0.299*127 + 0.587*127 + 0.114*127
    cb_val = 128.0 - 0.168736*127 - 0.331264*127 + 0.5*127
    cr_val = 128.0 + 0.5*127 - 0.418688*127 - 0.081312*127
    
    print *, "Expected YCbCr values for RGB(127,127,127):"
    print '(A,F6.1)', "Y  = ", y_val
    print '(A,F6.1)', "Cb = ", cb_val 
    print '(A,F6.1)', "Cr = ", cr_val
    
    ! After DC subtraction (Y-128):
    print *, ""
    print *, "After DC subtraction:"
    print '(A,F6.1)', "Y-128  = ", y_val - 128.0
    print '(A,F6.1)', "Cb-128 = ", cb_val - 128.0
    print '(A,F6.1)', "Cr-128 = ", cr_val - 128.0
    
    ! For a solid block, all DCT coefficients except DC should be 0
    print *, ""
    print *, "For solid color block:"
    print *, "- DC coefficient = (Y-128) * 8 (due to DCT scaling)"
    print '(A,F6.1)', "- Y DC = ", (y_val - 128.0) * 8
    print *, "- All AC coefficients = 0"
    print *, ""
    print *, "Encoded data should be:"
    print *, "1. Y block: DC coefficient, then EOB"
    print *, "2. Cb block: DC coefficient, then EOB"  
    print *, "3. Cr block: DC coefficient, then EOB"
    
end program debug_jpeg_trace