program debug_encode_trace
    use fortplot_jpeg
    implicit none
    
    integer, parameter :: width = 64, height = 64
    integer(1) :: image_data(width * height * 3)
    integer :: x, y, idx
    real :: r, g, b, t
    
    ! Create simple gradient pattern
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            ! Simple gradient
            image_data(idx) = int(x * 4, 1)      ! R
            image_data(idx + 1) = int(y * 4, 1)  ! G
            image_data(idx + 2) = int(-128, 1)    ! B = 128
        end do
    end do
    
    print *, "Creating 64x64 test image with quality 85 (subsampled)"
    print *, "Expected MCUs: 4x4 = 16"
    print *, "Expected blocks: 16 * 6 = 96 (4Y + 1U + 1V per MCU)"
    
    call write_jpeg_file("debug_trace_q85.jpg", width, height, image_data, 85)
    
    print *, ""
    print *, "Creating 64x64 test image with quality 95 (not subsampled)"
    print *, "Expected blocks: 64 * 3 = 192 (1Y + 1U + 1V per 8x8)"
    
    call write_jpeg_file("debug_trace_q95.jpg", width, height, image_data, 95)
    
end program