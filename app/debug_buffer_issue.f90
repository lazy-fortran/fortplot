program debug_buffer_issue
    use fortplot_jpeg
    use iso_fortran_env, only: int8
    implicit none
    
    integer, parameter :: width = 64, height = 64
    integer(int8) :: image_data(width * height * 3)
    integer :: x, y, idx
    real :: t
    
    ! Create same pattern as test
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            t = real(x + y) / real(width + height - 2)
            
            ! Red: sine wave
            image_data(idx) = int(128.0 + 127.0 * sin(real(x) * 6.28 / real(width)), int8)
            
            ! Green: vertical gradient
            image_data(idx + 1) = int(real(y) * 255.0 / real(height - 1), int8)
            
            ! Blue: diagonal with blocks
            image_data(idx + 2) = int(t * 255.0, int8)
            if (mod(x/8, 2) == mod(y/8, 2)) then
                image_data(idx + 2) = int(255.0 - real(image_data(idx + 2)), int8)
            end if
        end do
    end do
    
    print *, "Creating 64x64 test pattern"
    print *, "Expected MCUs: 4x4 = 16"
    print *, "Expected blocks: 16 * 6 = 96"
    
    call write_jpeg_file('debug_buffer_64x64.jpg', width, height, image_data, 85)
    
    ! Check file size
    block
        integer :: unit, file_size
        open(newunit=unit, file='debug_buffer_64x64.jpg', access='stream', form='unformatted', status='old')
        inquire(unit=unit, size=file_size)
        close(unit)
        print *, "File size:", file_size, "bytes"
    end block
    
end program debug_buffer_issue