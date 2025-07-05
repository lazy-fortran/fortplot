program debug_16x16_gray
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    integer(1), allocatable :: gray_data(:)
    integer :: i
    
    ! Create 16x16 solid gray (127,127,127) image
    allocate(gray_data(16*16*3))
    do i = 1, 16*16*3, 3
        gray_data(i) = int(127, 1)      ! R
        gray_data(i+1) = int(127, 1)    ! G
        gray_data(i+2) = int(127, 1)    ! B
    end do
    
    call write_jpeg_file("test_16x16_gray.jpg", 16, 16, gray_data, 90)
    print *, "Created test_16x16_gray.jpg (should be uniform gray)"
    
    ! Also create a larger test
    deallocate(gray_data)
    allocate(gray_data(32*32*3))
    
    do i = 1, 32*32*3, 3
        gray_data(i) = int(127, 1)      ! R
        gray_data(i+1) = int(127, 1)    ! G
        gray_data(i+2) = int(127, 1)    ! B
    end do
    
    call write_jpeg_file("test_32x32_gray.jpg", 32, 32, gray_data, 90)
    print *, "Created test_32x32_gray.jpg (should be uniform gray)"
    
end program debug_16x16_gray