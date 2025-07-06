program debug_stb_dc_analysis
    use, intrinsic :: iso_fortran_env, only: real64
    implicit none
    
    ! Let's check STB's exact calculation
    real(real64) :: rgb_val = 127.0_real64
    real(real64) :: y_stb, y_ours
    real(real64) :: dc_stb, dc_ours
    integer :: y_int
    integer :: quant_dc_ours, quant_dc_stb
    
    print *, "=== Comparing DC calculations with STB ==="
    print *, ""
    
    ! Our calculation
    y_ours = 0.299_real64*rgb_val + 0.587_real64*rgb_val + 0.114_real64*rgb_val - 128.0_real64
    dc_ours = y_ours * 8.0_real64
    
    print *, "Our calculation:"
    print *, "Y = 0.299*127 + 0.587*127 + 0.114*127 - 128"
    print *, "Y =", y_ours
    print *, "DC =", dc_ours
    print *, ""
    
    ! STB uses integer arithmetic for RGB to Y conversion
    ! From stb_image_write.h: Y = (R*77 + G*150 + B*29) >> 8
    y_int = (127*77 + 127*150 + 127*29) / 256
    print *, "STB calculation (integer):"
    print *, "Y = (127*77 + 127*150 + 127*29) / 256 =", y_int
    
    ! Then STB subtracts 128 in float
    y_stb = real(y_int, real64) - 128.0_real64
    dc_stb = y_stb * 8.0_real64
    
    print *, "Y after centering =", y_stb
    print *, "DC =", dc_stb
    print *, ""
    
    ! After quantization with quant=3
    quant_dc_ours = nint(dc_ours / 3.0_real64)
    quant_dc_stb = nint(dc_stb / 3.0_real64)
    
    print *, "After quantization (quant=3):"
    print *, "Our quantized DC:", quant_dc_ours
    print *, "STB quantized DC:", quant_dc_stb
    print *, ""
    
    if (quant_dc_ours /= quant_dc_stb) then
        print *, "DIFFERENCE FOUND! This explains the bit difference."
        print *, "Our DC encodes as", quant_dc_ours
        print *, "STB DC encodes as", quant_dc_stb
    else
        print *, "DC values match. Need to check AC encoding."
    end if
    
end program debug_stb_dc_analysis