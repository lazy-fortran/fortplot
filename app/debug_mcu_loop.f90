program debug_mcu_loop
    implicit none
    
    integer :: width, height
    integer :: x, y, mcu_count
    
    width = 64
    height = 64
    
    print *, "MCU loop test for 64x64 image with 16x16 MCUs:"
    
    mcu_count = 0
    do y = 1, height, 16
        do x = 1, width, 16
            mcu_count = mcu_count + 1
            print '("MCU ", I2, ": x=", I2, "-", I2, " y=", I2, "-", I2)', &
                  mcu_count, x, min(x+15, width), y, min(y+15, height)
        end do
    end do
    
    print *, ""
    print *, "Total MCUs:", mcu_count
    print *, "Expected MCUs:", (width/16) * (height/16)
    
end program