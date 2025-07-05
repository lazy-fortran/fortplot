program debug_stb_quant_order
    implicit none
    
    ! Natural order table
    integer :: YQT(64) = [ &
        16, 11, 10, 16, 24, 40, 51, 61, &
        12, 12, 14, 19, 26, 58, 60, 55, &
        14, 13, 16, 24, 40, 57, 69, 56, &
        14, 17, 22, 29, 51, 87, 80, 62, &
        18, 22, 37, 56, 68,109,103, 77, &
        24, 35, 55, 64, 81,104,113, 92, &
        49, 64, 78, 87,103,121,120,101, &
        72, 92, 95, 98,112,100,103, 99]
        
    ! Zigzag indices (1-based)
    integer :: zigzag(64) = [ &
         1,  2,  9, 17, 10,  3,  4, 11, &
        18, 25, 33, 26, 19, 12,  5,  6, &
        13, 20, 27, 34, 41, 49, 42, 35, &
        28, 21, 14,  7,  8, 15, 22, 29, &
        36, 43, 50, 57, 58, 51, 44, 37, &
        30, 23, 16, 24, 31, 38, 45, 52, &
        59, 60, 53, 46, 39, 32, 40, 47, &
        54, 61, 62, 55, 48, 56, 63, 64]
        
    integer :: i, pos, scaled
    integer :: quality_scale = 20  ! q=90
    
    print *, "STB writes quantization table in zigzag order:"
    print *, "First 16 values in zigzag order:"
    
    do i = 1, 16
        pos = zigzag(i)
        scaled = (YQT(pos) * quality_scale + 50) / 100
        print '(A,I2,A,I2,A,I3,A,I2)', "Zigzag pos ", i, " -> natural pos ", pos, &
            ", value ", YQT(pos), " -> ", scaled
    end do
    
    print *
    print *, "This gives the pattern:"
    do i = 1, 16
        pos = zigzag(i)
        scaled = (YQT(pos) * quality_scale + 50) / 100
        write(*, '(Z2.2,A)', advance='no') scaled, " "
    end do
    print *
    
end program debug_stb_quant_order