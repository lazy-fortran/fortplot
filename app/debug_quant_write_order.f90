program debug_quant_write_order
    implicit none
    
    integer :: YQT(64) = [ &
        16, 11, 10, 16, 24, 40, 51, 61, &
        12, 12, 14, 19, 26, 58, 60, 55, &
        14, 13, 16, 24, 40, 57, 69, 56, &
        14, 17, 22, 29, 51, 87, 80, 62, &
        18, 22, 37, 56, 68,109,103, 77, &
        24, 35, 55, 64, 81,104,113, 92, &
        49, 64, 78, 87,103,121,120,101, &
        72, 92, 95, 98,112,100,103, 99]
        
    integer :: i, quality_scale, scaled
    
    quality_scale = 20  ! For quality 90
    
    print *, "First 16 quantization values (natural order):"
    do i = 1, 16
        scaled = (YQT(i) * quality_scale + 50) / 100
        print '(I2,A,I3,A,I3)', i, ": ", YQT(i), " -> ", scaled
    end do
    
    print *
    print *, "Expected STB pattern: 03 02 02 03 02 02 03 03 03 03 04 03 03 04 05 08"
    print *, "Our pattern would be:"
    do i = 1, 16
        scaled = (YQT(i) * quality_scale + 50) / 100
        write(*, '(Z2.2,A)', advance='no') scaled, " "
    end do
    print *
    
end program debug_quant_write_order