program debug_mcu_extraction
    use fortplot_jpeg
    implicit none
    
    integer, parameter :: width = 32, height = 32
    integer(1) :: rgb_data(width * height * 3)
    real, allocatable :: ycbcr_data(:,:,:)
    real :: y_16x16(256)
    integer :: x, y, idx, mcu_x, mcu_y
    
    ! Create a distinctive pattern - different color in each quadrant
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            if (x < 16 .and. y < 16) then
                ! Top-left: Red
                rgb_data(idx) = int(-1, 1)      ! 255
                rgb_data(idx+1) = int(0, 1)
                rgb_data(idx+2) = int(0, 1)
            else if (x >= 16 .and. y < 16) then
                ! Top-right: Green
                rgb_data(idx) = int(0, 1)
                rgb_data(idx+1) = int(-1, 1)    ! 255
                rgb_data(idx+2) = int(0, 1)
            else if (x < 16 .and. y >= 16) then
                ! Bottom-left: Blue
                rgb_data(idx) = int(0, 1)
                rgb_data(idx+1) = int(0, 1)
                rgb_data(idx+2) = int(-1, 1)    ! 255
            else
                ! Bottom-right: Yellow
                rgb_data(idx) = int(-1, 1)      ! 255
                rgb_data(idx+1) = int(-1, 1)    ! 255
                rgb_data(idx+2) = int(0, 1)
            end if
        end do
    end do
    
    ! Convert to YCbCr
    call rgb_to_ycbcr_test(rgb_data, ycbcr_data)
    
    ! Test extracting first MCU (top-left)
    mcu_x = 1
    mcu_y = 1
    call extract_16x16_y_block_test(ycbcr_data(:,:,1), width, height, mcu_x, mcu_y, y_16x16)
    
    print *, "MCU at (1,1) - should be red (Y = -51.76)"
    print *, "First few Y values:", y_16x16(1:5)
    print *, "Values at positions 1, 16, 17:", y_16x16(1), y_16x16(16), y_16x16(17)
    
    ! Test extracting second MCU (top-right)
    mcu_x = 17
    mcu_y = 1
    call extract_16x16_y_block_test(ycbcr_data(:,:,1), width, height, mcu_x, mcu_y, y_16x16)
    
    print *, ""
    print *, "MCU at (17,1) - should be green (Y = 21.68)"
    print *, "First few Y values:", y_16x16(1:5)
    
    ! Write test image
    call write_jpeg_file("debug_quadrants.jpg", width, height, rgb_data, 85)
    print *, ""
    print *, "Created debug_quadrants.jpg"
    
contains

    subroutine rgb_to_ycbcr_test(rgb_data, ycbcr_data)
        integer(1), intent(in) :: rgb_data(:)
        real, allocatable, intent(out) :: ycbcr_data(:,:,:)
        
        integer :: x, y, idx
        real :: r, g, b
        
        allocate(ycbcr_data(width, height, 3))
        
        do y = 1, height
            do x = 1, width
                idx = ((y - 1) * width + (x - 1)) * 3 + 1
                r = real(iand(int(rgb_data(idx)), 255))
                g = real(iand(int(rgb_data(idx + 1)), 255))  
                b = real(iand(int(rgb_data(idx + 2)), 255))
                
                ycbcr_data(x, y, 1) = 0.299*r + 0.587*g + 0.114*b - 128.0
                ycbcr_data(x, y, 2) = -0.16874*r - 0.33126*g + 0.5*b
                ycbcr_data(x, y, 3) = 0.5*r - 0.41869*g - 0.08131*b
            end do
        end do
    end subroutine
    
    subroutine extract_16x16_y_block_test(y_data, width, height, start_x, start_y, y_block)
        real, intent(in) :: y_data(:,:)
        integer, intent(in) :: width, height, start_x, start_y
        real, intent(out) :: y_block(256)
        
        integer :: x, y, pos, src_x, src_y
        
        pos = 1
        do y = 0, 15
            do x = 0, 15
                src_y = min(start_y + y, height)
                src_x = min(start_x + x, width)
                y_block(pos) = y_data(src_x, src_y)
                pos = pos + 1
            end do
        end do
    end subroutine

end program