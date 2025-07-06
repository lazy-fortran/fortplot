program debug_first_mcu_bits
    use fortplot_jpeg
    implicit none
    
    ! Create the same 64x64 complex pattern as in the test
    integer, parameter :: WIDTH = 64, HEIGHT = 64
    integer(1) :: image_3d(3, WIDTH, HEIGHT)
    integer(1) :: image_data(3 * WIDTH * HEIGHT)
    integer :: x, y, pos
    real :: t, r
    
    ! Create complex pattern (same as test)
    do y = 1, HEIGHT
        do x = 1, WIDTH
            t = real(x-1) / real(WIDTH-1) * 6.28  ! 0 to 2π
            r = real(y-1) / real(HEIGHT-1)
            
            image_3d(1, x, y) = int(127.5 * (1.0 + sin(t * 3) * cos(r * 4)), kind=1)  ! Red
            image_3d(2, x, y) = int(127.5 * (1.0 + cos(t * 2) * sin(r * 3)), kind=1)  ! Green  
            image_3d(3, x, y) = int(127.5 * (1.0 + sin(t * 4) * cos(r * 2)), kind=1)  ! Blue
        end do
    end do
    
    ! Flatten to 1D array: RGBRGBRGB...
    pos = 1
    do y = 1, HEIGHT
        do x = 1, WIDTH
            image_data(pos) = image_3d(1, x, y)     ! R
            image_data(pos+1) = image_3d(2, x, y)   ! G  
            image_data(pos+2) = image_3d(3, x, y)   ! B
            pos = pos + 3
        end do
    end do
    
    print *, 'JPEG First MCU Bit-Level Debug'
    print *, '=============================='
    print *, 'Creating complex 64x64 pattern...'
    
    ! Write our JPEG file
    call write_jpeg_file('debug_first_mcu.jpg', WIDTH, HEIGHT, image_data, 95)
    print *, 'Our JPEG written to debug_first_mcu.jpg'
    
end program debug_first_mcu_bits