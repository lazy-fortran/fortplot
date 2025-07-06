program debug_fillbits_exact
    use fortplot_jpeg, only: jpeg_context, create_jpeg_canvas, get_jpeg_data, initialize_huffman_tables
    use, intrinsic :: iso_fortran_env, only: int8
    implicit none
    
    integer, parameter :: width = 8, height = 8, quality = 90
    integer(int8) :: rgb_data(width*height*3)
    integer(int8), allocatable :: jpeg_data(:)
    type(jpeg_context) :: ctx
    integer :: i
    
    ! Initialize everything
    call initialize_huffman_tables()
    
    ! Create 8x8 uniform gray image (all pixels = 127)
    do i = 1, width*height*3
        rgb_data(i) = 127_int8
    end do
    
    print *, "=== Debug bit buffer state before fillBits ==="
    print *, "Image: 8x8 gray (127,127,127)"
    print *, "Quality:", quality
    print *, ""
    
    ! Create JPEG context
    ctx = create_jpeg_canvas(width, height, quality)
    
    ! Get JPEG data with debug output
    call get_jpeg_data(width, height, rgb_data, quality, jpeg_data)
    
    print *, "JPEG encoded successfully"
    print *, "Total size:", size(jpeg_data), "bytes"
    
    ! Write to file for comparison
    open(unit=10, file='debug_fillbits_exact.jpg', access='stream', form='unformatted')
    write(10) jpeg_data
    close(10)
    
    print *, "Output written to debug_fillbits_exact.jpg"
    
end program debug_fillbits_exact