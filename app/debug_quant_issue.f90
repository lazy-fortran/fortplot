program debug_quant_issue
    implicit none
    
    real :: block_value, dct_coef, quant_scale, result
    integer :: quant_table_value
    
    ! For a gray image (127,127,127):
    ! Y = 127, after shift: -1
    ! Uniform block of -1
    
    block_value = -1.0
    
    ! After DCT, DC coefficient should be:
    ! sum * normalization = 64 * (-1) * 0.125 = -8
    dct_coef = -8.0
    
    ! Quantization table value for DC (quality 90)
    quant_table_value = 3  ! typical value
    
    ! STB uses fdtbl which is 1/(qtable * aasf)
    ! For DC position, aasf = 2.828427125
    quant_scale = 1.0 / (quant_table_value * 2.828427125)
    
    ! Quantized value
    result = dct_coef * quant_scale
    
    print *, "Debug quantization:"
    print *, "Block value (Y-128):", block_value
    print *, "DCT DC coefficient:", dct_coef
    print *, "Quant table value:", quant_table_value
    print *, "Quant scale (fdtbl):", quant_scale
    print *, "Quantized result:", result
    print *, "Rounded:", nint(result)
    
    print *, ""
    print *, "If result is very small (near 0), image will be black!"
    
end program debug_quant_issue