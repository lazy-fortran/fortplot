program debug_mcu_encoding_trace
    use fortplot_jpeg
    use iso_fortran_env, only: int8, int32
    implicit none
    
    integer, parameter :: width = 64, height = 64
    integer(1) :: image_data(width * height * 3)
    integer :: x, y, idx, mcu_idx
    
    ! Create a pattern where each MCU has a distinct color
    mcu_idx = 0
    do y = 0, height - 1, 16
        do x = 0, width - 1, 16
            mcu_idx = mcu_idx + 1
            ! Fill this MCU with a unique color
            call fill_mcu_with_color(image_data, width, height, x, y, mcu_idx)
        end do
    end do
    
    print *, "Created test pattern with 16 MCUs, each with unique color"
    print *, "MCU 1: Red=32, MCU 2: Red=64, etc."
    
    ! Write the file
    call write_jpeg_file("debug_mcu_pattern.jpg", width, height, image_data, 85)
    print *, "Written debug_mcu_pattern.jpg"
    
contains

    subroutine fill_mcu_with_color(img, w, h, start_x, start_y, mcu_num)
        integer(1), intent(inout) :: img(:)
        integer, intent(in) :: w, h, start_x, start_y, mcu_num
        integer :: x, y, idx
        integer(1) :: r, g, b
        
        ! Create unique color for this MCU
        r = int(mod(mcu_num * 32, 256), 1)
        g = int(mod(mcu_num * 48, 256), 1)
        b = int(mod(mcu_num * 64, 256), 1)
        
        ! Fill 16x16 MCU area
        do y = start_y, min(start_y + 15, h - 1)
            do x = start_x, min(start_x + 15, w - 1)
                idx = (y * w + x) * 3 + 1
                img(idx) = r
                img(idx + 1) = g
                img(idx + 2) = b
            end do
        end do
        
        print '("MCU ", I2, " at (", I2, ",", I2, ") color: R=", I3, " G=", I3, " B=", I3)', &
              mcu_num, start_x, start_y, r, g, b
    end subroutine

end program debug_mcu_encoding_trace