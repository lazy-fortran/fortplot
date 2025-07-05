program debug_jpeg_entropy
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer(1) :: pixels(3*8*8)  ! 8x8 test block
    integer :: i
    
    ! Create a solid color block (all white)
    pixels = -1_int8  ! 255
    
    print *, "Creating 8x8 white block..."
    call write_jpeg_file("debug_white_8x8.jpg", 8, 8, pixels, 90)
    
    ! Create a black block
    pixels = 0_int8
    print *, "Creating 8x8 black block..."
    call write_jpeg_file("debug_black_8x8.jpg", 8, 8, pixels, 90)
    
    ! Create a mid-gray block
    do i = 1, size(pixels)
        pixels(i) = int(127, int8)
    end do
    print *, "Creating 8x8 gray block..."
    call write_jpeg_file("debug_gray_8x8.jpg", 8, 8, pixels, 90)
    
    ! Create alternating pattern
    do i = 1, size(pixels), 6
        pixels(i:i+2) = -1_int8    ! White pixel
        if (i+3 <= size(pixels)) pixels(i+3:i+5) = 0_int8  ! Black pixel
    end do
    print *, "Creating 8x8 pattern block..."  
    call write_jpeg_file("debug_pattern_8x8.jpg", 8, 8, pixels, 90)
    
    print *, "Minimal test images created!"
    
end program debug_jpeg_entropy