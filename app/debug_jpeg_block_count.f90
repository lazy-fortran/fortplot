program debug_jpeg_block_count
    use fortplot_jpeg
    implicit none
    
    integer, parameter :: width = 64, height = 64
    integer(1) :: image_data(width * height * 3)
    integer :: x, y, idx
    real :: r, g, b, t
    
    ! Create same test pattern as in test
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            t = real(x + y) / real(width + height - 2)
            r = 128.0 + 127.0 * sin(real(x) * 6.28 / real(width))
            g = real(y) * 255.0 / real(height - 1)
            b = t * 255.0
            
            if (mod(x/8, 2) == mod(y/8, 2)) then
                b = 255.0 - b
            end if
            
            image_data(idx) = int(min(255.0, max(0.0, r)), 1)
            image_data(idx + 1) = int(min(255.0, max(0.0, g)), 1) 
            image_data(idx + 2) = int(min(255.0, max(0.0, b)), 1)
        end do
    end do
    
    print *, "Image size:", width, "x", height
    print *, "Expected 8x8 blocks:"
    print *, "  Blocks per row:", width / 8
    print *, "  Blocks per column:", height / 8
    print *, "  Total Y blocks:", (width / 8) * (height / 8)
    print *, "  Total U blocks:", (width / 8) * (height / 8)
    print *, "  Total V blocks:", (width / 8) * (height / 8)
    print *, "  Total blocks:", 3 * (width / 8) * (height / 8)
    
    ! Generate JPEG
    call write_jpeg_file("debug_block_count.jpg", width, height, image_data, 85)
    
end program