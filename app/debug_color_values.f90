program debug_color_values
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: rgb_data(8*8*3)
    integer :: i
    real :: y, cb, cr
    
    ! Create 8x8 gray (127,127,127) image
    do i = 1, 8*8*3, 3
        rgb_data(i) = int(127, 1)      ! R
        rgb_data(i+1) = int(127, 1)    ! G
        rgb_data(i+2) = int(127, 1)    ! B
    end do
    
    ! Calculate YCbCr values
    y = 0.299*127 + 0.587*127 + 0.114*127
    cb = 128.0 - 0.168736*127 - 0.331264*127 + 0.5*127
    cr = 128.0 + 0.5*127 - 0.418688*127 - 0.081312*127
    
    print *, "RGB(127,127,127) -> YCbCr:"
    print '(A,F6.1)', "Y  = ", y
    print '(A,F6.1)', "Cb = ", cb
    print '(A,F6.1)', "Cr = ", cr
    
    ! After shifting for JPEG (subtract 128)
    print *, ""
    print *, "After JPEG shift (subtract 128):"
    print '(A,F6.1)', "Y-128  = ", y - 128.0
    print '(A,F6.1)', "Cb-128 = ", cb - 128.0
    print '(A,F6.1)', "Cr-128 = ", cr - 128.0
    
    ! DCT DC coefficient (multiply by 8)
    print *, ""
    print *, "DCT DC coefficients:"
    print '(A,F6.1)', "Y DC  = ", (y - 128.0) * 8
    print '(A,F6.1)', "Cb DC = ", (cb - 128.0) * 8
    print '(A,F6.1)', "Cr DC = ", (cr - 128.0) * 8
    
    ! Create test images
    call write_jpeg_file("debug_gray127.jpg", 8, 8, rgb_data, 90)
    
    ! Create pure white
    rgb_data = int(-1, 1)  ! 255 in unsigned
    call write_jpeg_file("debug_white.jpg", 8, 8, rgb_data, 90)
    
    ! Create pure black
    rgb_data = 0
    call write_jpeg_file("debug_black.jpg", 8, 8, rgb_data, 90)
    
    print *, ""
    print *, "Created test images:"
    print *, "- debug_gray127.jpg (should be medium gray)"
    print *, "- debug_white.jpg (should be white)"
    print *, "- debug_black.jpg (should be black)"
    
end program debug_color_values