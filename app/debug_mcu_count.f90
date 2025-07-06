program debug_mcu_count
    implicit none
    
    integer :: width, height
    integer :: mcu_width, mcu_height
    integer :: x, y
    integer :: mcu_count, y_blocks, total_blocks
    
    width = 64
    height = 64
    
    print *, "Image size:", width, "x", height
    print *, "With 2x2 subsampling (4:2:0):"
    
    mcu_count = 0
    y_blocks = 0
    
    ! Count MCUs like our code does
    do y = 1, height, 16
        do x = 1, width, 16
            mcu_count = mcu_count + 1
            y_blocks = y_blocks + 4
            print '("  MCU at (", I2, ",", I2, ") covers pixels (", I2, "-", I2, ",", I2, "-", I2, ")")', &
                  x, y, x, min(x+15, width), y, min(y+15, height)
        end do
    end do
    
    print *, ""
    print *, "Our encoding:"
    print *, "  MCUs:", mcu_count
    print *, "  Y blocks:", y_blocks
    print *, "  U blocks:", mcu_count
    print *, "  V blocks:", mcu_count
    print *, "  Total blocks:", y_blocks + 2*mcu_count
    
    ! Calculate what STB should encode
    mcu_width = (width + 15) / 16
    mcu_height = (height + 15) / 16
    
    print *, ""
    print *, "STB calculation:"
    print *, "  MCU width:", mcu_width
    print *, "  MCU height:", mcu_height
    print *, "  Total MCUs:", mcu_width * mcu_height
    print *, "  Total blocks:", mcu_width * mcu_height * 6
    
end program