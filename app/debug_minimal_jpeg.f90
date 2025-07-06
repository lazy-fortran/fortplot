program debug_minimal_jpeg
    use fortplot_jpeg
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    ! Create a minimal 16x16 image to test
    integer(1), allocatable :: image_data(:,:,:)
    integer(1) :: flat_data(16*16*3)
    integer :: x, y, idx
    real :: r, g, b
    
    allocate(image_data(16, 16, 3))
    
    ! Simple gradient pattern that STB can handle
    do y = 1, 16
        do x = 1, 16
            r = real(x-1) * 15.0  ! 0 to 225
            g = real(y-1) * 15.0  ! 0 to 225
            b = 128.0             ! Constant blue
            
            image_data(x, y, 1) = int(r, int8)
            image_data(x, y, 2) = int(g, int8)
            image_data(x, y, 3) = int(b, int8)
        end do
    end do
    
    ! Encode using our implementation
    ! Flatten the image data
    
    idx = 1
    do y = 1, 16
        do x = 1, 16
            flat_data(idx) = image_data(x, y, 1)
            flat_data(idx+1) = image_data(x, y, 2)
            flat_data(idx+2) = image_data(x, y, 3)
            idx = idx + 3
        end do
    end do
    
    call write_jpeg_file('our_minimal.jpg', 16, 16, flat_data, 90)
    print *, "Created our_minimal.jpg"
    
    ! Try to create the same image with STB using an external call
    call system('echo "Creating STB reference..." > /dev/null')
    
    ! Compare files
    call system('ls -la our_minimal.jpg; wc -c our_minimal.jpg')
    
end program debug_minimal_jpeg