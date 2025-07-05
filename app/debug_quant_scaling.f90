program debug_quant_scaling
    implicit none
    
    integer :: YQT(8) = [16, 11, 10, 16, 24, 40, 51, 61]
    integer :: quality_scale, i, scaled
    integer :: quality = 90
    
    ! Calculate quality scale like STB
    quality_scale = max(1, min(100, quality))
    if (quality_scale < 50) then
        quality_scale = 5000 / quality_scale
    else
        quality_scale = 200 - quality_scale * 2
    end if
    
    print *, "Quality:", quality
    print *, "Quality scale:", quality_scale
    print *
    print *, "First 8 Y quantization values:"
    print *, "Original -> Scaled"
    
    do i = 1, 8
        scaled = (YQT(i) * quality_scale + 50) / 100
        scaled = max(1, min(255, scaled))
        print '(I3,A,I3)', YQT(i), " -> ", scaled
    end do
    
end program debug_quant_scaling