program debug_content_validation
    use fortplot_jpeg, only: create_jpeg_canvas, jpeg_context, get_jpeg_data
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    type(jpeg_context) :: ctx
    integer(int8), allocatable :: our_data(:)
    integer :: i, data_size
    
    ! Create minimal test JPEG
    ctx = create_jpeg_canvas(8, 8, 90)
    
    ! Fill with uniform gray  
    do i = 1, 192  ! 8*8*3 = 192 bytes
        ctx%raster%image_data(i) = int(127, int8)
    end do
    
    ! Get JPEG data
    call get_jpeg_data(8, 8, ctx%raster%image_data, 90, our_data)
    data_size = size(our_data)
    
    ! Save our output
    open(10, file='our_validation.jpg', form='unformatted', access='stream')
    write(10) our_data
    close(10)
    
    ! Compare against STB reference
    print *, "=== Content validation ==="
    print *, "Our file size:", data_size
    print *, "File saved as: our_validation.jpg"
    print *, ""
    print *, "To validate:"
    print *, "1. hexdump -C our_validation.jpg | tail -5"
    print *, "2. Compare last scan bytes with STB reference"
    print *, ""
    print *, "Expected STB pattern (last 10 bytes):"
    print *, "00 3F 00 65 14 51 40 1F FF D9"
    print *, ""
    print *, "Our pattern (last 10 bytes):"
    do i = data_size-9, data_size
        write(*,'(1X,Z2.2)',advance='no') our_data(i)
    end do
    print *
    
    ! Check if it's a grayscale image (all pixels should be 127)
    print *, ""
    print *, "First 10 pixel values (should be all 127):"
    do i = 1, min(30, 192), 3
        print *, "RGB:", int(ctx%raster%image_data(i)), int(ctx%raster%image_data(i+1)), int(ctx%raster%image_data(i+2))
    end do
    
end program debug_content_validation