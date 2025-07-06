program debug_restart_interval
    implicit none
    
    integer :: width, height
    integer :: mcu_width, mcu_height
    integer :: total_mcus
    
    width = 64
    height = 64
    
    ! For 4:2:0 subsampling, MCU is 16x16
    mcu_width = (width + 15) / 16
    mcu_height = (height + 15) / 16
    total_mcus = mcu_width * mcu_height
    
    print *, "Image dimensions:", width, "x", height
    print *, "MCU dimensions with 4:2:0 subsampling: 16x16"
    print *, "MCUs per row:", mcu_width
    print *, "MCU rows:", mcu_height
    print *, "Total MCUs:", total_mcus
    print *, ""
    print *, "Blocks per MCU: 6 (4Y + 1U + 1V)"
    print *, "Total blocks:", total_mcus * 6
    
end program