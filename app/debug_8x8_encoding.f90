program debug_8x8_encoding
    use fortplot_jpeg
    implicit none
    
    integer(1) :: gray_data(8*8*3)
    integer :: i
    
    ! Create 8x8 solid gray (127,127,127) image
    do i = 1, 8*8*3, 3
        gray_data(i) = int(127, 1)      ! R
        gray_data(i+1) = int(127, 1)    ! G
        gray_data(i+2) = int(127, 1)    ! B
    end do
    
    print *, "8x8 gray image created"
    print *, "First pixel RGB:", gray_data(1:3)
    print *, "Last pixel RGB:", gray_data(190:192)
    
    ! Create JPEG
    call write_jpeg_file("debug_8x8.jpg", 8, 8, gray_data, 90)
    print *, "JPEG written to debug_8x8.jpg"
    
end program debug_8x8_encoding