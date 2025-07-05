program debug_ycbcr_conversion
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: test_pixels(3*4)  ! 4 test pixels
    real :: r, g, b, y, cb, cr
    integer :: i
    
    ! Test pixel 1: White (255, 255, 255)
    test_pixels(1:3) = -1_int8
    
    ! Test pixel 2: Black (0, 0, 0)
    test_pixels(4:6) = 0_int8
    
    ! Test pixel 3: Red (255, 0, 0)
    test_pixels(7) = -1_int8
    test_pixels(8:9) = 0_int8
    
    ! Test pixel 4: Gray (128, 128, 128)
    test_pixels(10:12) = int(-128, int8)  ! This is 128 in unsigned
    
    print *, "Testing RGB to YCbCr conversion:"
    print *, "================================"
    
    do i = 1, 4
        ! Extract RGB
        r = real(iand(int(test_pixels((i-1)*3 + 1)), 255))
        g = real(iand(int(test_pixels((i-1)*3 + 2)), 255))
        b = real(iand(int(test_pixels((i-1)*3 + 3)), 255))
        
        ! Convert to YCbCr using JPEG formula
        y  =  0.299 * r + 0.587 * g + 0.114 * b
        cb = -0.168736 * r - 0.331264 * g + 0.5 * b + 128.0
        cr =  0.5 * r - 0.418688 * g - 0.081312 * b + 128.0
        
        print '(A,I0,A)', "Pixel ", i, ":"
        print '(A,3(F6.1))', "  RGB: ", r, g, b
        print '(A,3(F6.1))', "  YCbCr: ", y, cb, cr
        print '(A,3(I4))', "  YCbCr (int): ", nint(y), nint(cb), nint(cr)
        print *
    end do
    
    ! Now test with a small image
    call test_small_jpeg()
    
contains

    subroutine test_small_jpeg()
        integer(1) :: pixels(3*4*4)  ! 4x4 RGB image
        integer :: i
        
        ! Fill with solid gray
        do i = 1, size(pixels)
            pixels(i) = 127_int8  ! Mid gray (127 unsigned)
        end do
        
        print *, "Creating 4x4 gray test image..."
        call write_jpeg_file("test_4x4_gray.jpg", 4, 4, pixels, 90)
        
        ! Fill with white
        pixels = -1_int8  ! 255
        print *, "Creating 4x4 white test image..."
        call write_jpeg_file("test_4x4_white.jpg", 4, 4, pixels, 90)
        
        ! Fill with red
        do i = 1, size(pixels), 3
            pixels(i) = -1_int8     ! R=255
            pixels(i+1) = 0_int8    ! G=0
            pixels(i+2) = 0_int8    ! B=0
        end do
        print *, "Creating 4x4 red test image..."
        call write_jpeg_file("test_4x4_red.jpg", 4, 4, pixels, 90)
        
    end subroutine

end program debug_ycbcr_conversion