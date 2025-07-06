program debug_pixel_values
    use fortplot_jpeg
    implicit none
    
    integer, parameter :: width = 64, height = 64
    integer(1) :: image_data(width * height * 3)
    integer :: x, y, idx
    real :: r, g, b, t
    integer :: sample_points(5, 2)
    integer :: i
    
    ! Create same test pattern
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
    
    ! Sample some points
    sample_points(1,:) = [1, 1]     ! Top-left
    sample_points(2,:) = [32, 1]    ! Top-middle
    sample_points(3,:) = [64, 1]    ! Top-right
    sample_points(4,:) = [32, 32]   ! Center
    sample_points(5,:) = [64, 64]   ! Bottom-right
    
    print *, "Sample pixel values from test pattern:"
    print *, "Position    R    G    B"
    
    do i = 1, 5
        x = sample_points(i, 1)
        y = sample_points(i, 2)
        idx = ((y-1) * width + (x-1)) * 3 + 1
        
        print '("(", I2, ",", I2, ")   ", I3, "  ", I3, "  ", I3)', &
              x, y, &
              iand(int(image_data(idx)), 255), &
              iand(int(image_data(idx+1)), 255), &
              iand(int(image_data(idx+2)), 255)
    end do
    
    ! Check byte values
    print *, ""
    print *, "First 10 bytes of image data:"
    do i = 1, 10
        print '("byte ", I2, ": ", I3, " (0x", Z2.2, ")")', &
              i, iand(int(image_data(i)), 255), iand(int(image_data(i)), 255)
    end do
    
end program