program debug_complex_pattern
    implicit none
    
    integer, parameter :: width = 64, height = 64
    integer(1) :: image_data(width * height * 3)
    integer :: x, y, idx, i, j
    real :: r, g, b, t
    
    ! Recreate the exact pattern from test_jpeg_large_complex
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            t = real(x + y) / real(width + height - 2)
            
            ! Red channel: horizontal gradient with sine wave
            r = 128.0 + 127.0 * sin(real(x) * 6.28 / real(width))
            
            ! Green channel: vertical gradient  
            g = real(y) * 255.0 / real(height - 1)
            
            ! Blue channel: diagonal gradient with blocks
            b = t * 255.0
            
            ! Add some solid color blocks
            if (mod(x/8, 2) == mod(y/8, 2)) then
                b = 255.0 - b
            end if
            
            image_data(idx) = int(min(255.0, max(0.0, r)), 1)
            image_data(idx + 1) = int(min(255.0, max(0.0, g)), 1) 
            image_data(idx + 2) = int(min(255.0, max(0.0, b)), 1)
        end do
    end do
    
    ! Print some sample values
    print *, "Sample RGB values from complex pattern:"
    print *, "Position  R    G    B"
    
    do i = 0, 3
        do j = 0, 3
            x = i * 16
            y = j * 16
            idx = (y * width + x) * 3 + 1
            print '("(", I2, ",", I2, ") ", I3, " ", I3, " ", I3)', &
                  x, y, &
                  iand(int(image_data(idx)), 255), &
                  iand(int(image_data(idx+1)), 255), &
                  iand(int(image_data(idx+2)), 255)
        end do
    end do
    
    ! Check for any zeros or unusual values
    print *, ""
    print *, "Checking for unusual values..."
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            if (image_data(idx) == 0 .and. image_data(idx+1) == 0 .and. image_data(idx+2) == 0) then
                print *, "Found black pixel at", x, y
            end if
        end do
    end do
    
end program