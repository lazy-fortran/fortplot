program debug_simple_gradient
    use fortplot_jpeg
    implicit none
    
    integer, parameter :: width = 32, height = 32
    integer(1) :: image_data(width * height * 3)
    integer :: x, y, idx
    
    ! Create a simple horizontal gradient
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            ! Red gradient from 0 to 255
            image_data(idx) = int(x * 255 / (width - 1), 1)
            ! Green fixed at 128
            image_data(idx + 1) = int(-128, 1)
            ! Blue fixed at 64
            image_data(idx + 2) = int(64, 1)
        end do
    end do
    
    print *, "Creating simple gradient test"
    call write_jpeg_file("debug_gradient.jpg", width, height, image_data, 85)
    
    ! Also create with quality > 90 to test non-subsampled
    call write_jpeg_file("debug_gradient_q95.jpg", width, height, image_data, 95)
    
end program