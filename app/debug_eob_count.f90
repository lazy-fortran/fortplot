program debug_eob_count
    use fortplot_jpeg
    implicit none
    
    integer, parameter :: width = 64, height = 64
    integer(1) :: image_data(width * height * 3)
    integer :: x, y, idx
    real :: r, g, b, t
    
    ! Create the exact same complex pattern as in test
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            ! Create complex gradients and patterns
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
    
    ! First check RGB values at key points
    print *, "RGB values at key positions:"
    print *, "Position  R    G    B"
    
    idx = 1
    print '("( 0, 0) ", I3, " ", I3, " ", I3)', &
          iand(int(image_data(idx)), 255), &
          iand(int(image_data(idx+1)), 255), &
          iand(int(image_data(idx+2)), 255)
          
    idx = 16 * 3 + 1  
    print '("(16, 0) ", I3, " ", I3, " ", I3)', &
          iand(int(image_data(idx)), 255), &
          iand(int(image_data(idx+1)), 255), &
          iand(int(image_data(idx+2)), 255)
          
    idx = 16 * width * 3 + 1
    print '("( 0,16) ", I3, " ", I3, " ", I3)', &
          iand(int(image_data(idx)), 255), &
          iand(int(image_data(idx+1)), 255), &
          iand(int(image_data(idx+2)), 255)
          
    print *, ""
    print *, "Writing JPEG with quality 85..."
    call write_jpeg_file("debug_eob.jpg", width, height, image_data, 85)
    
end program