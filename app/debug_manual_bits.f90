program debug_manual_bits
    implicit none
    
    ! Manual calculation of what the scan data should be
    ! for 8x8 uniform gray (127,127,127) with quality 90
    
    integer :: dc_y, dc_u, dc_v
    integer :: quantized_dc_y, quantized_dc_u, quantized_dc_v
    integer :: diff_y, diff_u, diff_v
    real :: fdtbl_y_dc, fdtbl_uv_dc
    real :: y_value, u_value, v_value
    
    print *, "=== Manual scan data calculation ==="
    print *, ""
    
    ! RGB to YCbCr conversion for (127,127,127)
    y_value = 0.299*127 + 0.587*127 + 0.114*127 - 128.0
    u_value = -0.16874*127 - 0.33126*127 + 0.5*127
    v_value = 0.5*127 - 0.41869*127 - 0.08131*127
    
    print *, "YCbCr values:"
    print *, "Y =", y_value
    print *, "U =", u_value  
    print *, "V =", v_value
    print *, ""
    
    ! DCT of uniform block - DC coefficient is sum/8
    ! For 8x8 uniform Y block with value y_value:
    ! DC coefficient = y_value * 64 (sum of all pixels)
    print *, "DCT DC coefficients (before quantization):"
    print *, "Y DC =", y_value * 64
    print *, "U DC =", u_value * 64
    print *, "V DC =", v_value * 64
    print *, ""
    
    ! Quantization for quality 90
    ! Y quantization table [0,0] = 3 (for quality 90)
    ! UV quantization table [0,0] = 3 (for quality 90)
    fdtbl_y_dc = 1.0 / (3.0 * 1.0 * 1.0)    ! 1/(qtable * aasf * aasf)
    fdtbl_uv_dc = 1.0 / (3.0 * 1.0 * 1.0)   ! 1/(qtable * aasf * aasf)
    
    quantized_dc_y = nint(y_value * 64 * fdtbl_y_dc)
    quantized_dc_u = nint(u_value * 64 * fdtbl_uv_dc)
    quantized_dc_v = nint(v_value * 64 * fdtbl_uv_dc)
    
    print *, "Quantized DC coefficients:"
    print *, "Y DC =", quantized_dc_y
    print *, "U DC =", quantized_dc_u
    print *, "V DC =", quantized_dc_v
    print *, ""
    
    ! DC encoding (first block, so previous DC = 0)
    diff_y = quantized_dc_y - 0
    diff_u = quantized_dc_u - 0
    diff_v = quantized_dc_v - 0
    
    print *, "DC differences (first block):"
    print *, "Y diff =", diff_y
    print *, "U diff =", diff_u
    print *, "V diff =", diff_v
    print *, ""
    
    ! The scan data should encode:
    ! 1. Y DC difference
    ! 2. U DC difference  
    ! 3. V DC difference
    ! 4. EOB for Y AC (all zeros)
    ! 5. EOB for U AC (all zeros)
    ! 6. EOB for V AC (all zeros)
    ! 7. fillBits (0x7F, 7 bits)
    
    print *, "Expected scan data structure:"
    print *, "1. Y DC category + Y diff bits"
    print *, "2. U DC category + U diff bits"
    print *, "3. V DC category + V diff bits"
    print *, "4. Y AC EOB (10, 4 bits)"
    print *, "5. U AC EOB (0, 2 bits)"
    print *, "6. V AC EOB (0, 2 bits)"
    print *, "7. fillBits (0x7F, 7 bits)"
    
end program debug_manual_bits