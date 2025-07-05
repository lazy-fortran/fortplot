program test_trace_encoding
    use fortplot_jpeg
    implicit none
    
    real :: y_value, u_value, v_value
    real :: block_y(8,8), block_u(8,8), block_v(8,8)
    real :: dct_y, dct_u, dct_v
    integer :: quant_y, quant_u, quant_v
    
    print *, "Trace JPEG Encoding for Gray (127,127,127)"
    print *, "=========================================="
    
    ! Step 1: RGB to YCbCr (STB style)
    y_value = 0.299*127 + 0.587*127 + 0.114*127 - 128.0
    u_value = -0.16874*127 - 0.33126*127 + 0.5*127
    v_value = 0.5*127 - 0.41869*127 - 0.08131*127
    
    print *, ""
    print *, "Step 1: RGB to YCbCr"
    print '(A,3F8.3)', "RGB(127,127,127) -> Y,U,V: ", y_value, u_value, v_value
    
    ! Step 2: Fill 8x8 blocks
    block_y = y_value
    block_u = u_value
    block_v = v_value
    
    print *, ""
    print *, "Step 2: 8x8 blocks (all values same)"
    print '(A,F8.3)', "Y block: all ", y_value
    print '(A,F8.3)', "U block: all ", u_value
    print '(A,F8.3)', "V block: all ", v_value
    
    ! Step 3: DCT (for uniform block, DC = sum)
    dct_y = 64 * y_value
    dct_u = 64 * u_value
    dct_v = 64 * v_value
    
    print *, ""
    print *, "Step 3: DCT DC coefficients"
    print '(A,F8.3)', "Y DC: ", dct_y
    print '(A,F8.3)', "U DC: ", dct_u
    print '(A,F8.3)', "V DC: ", dct_v
    
    ! Step 4: Quantization (quality 90)
    ! For DC, fdtbl ≈ 1/(3 * 2.828^2) ≈ 0.0416
    quant_y = nint(dct_y * 0.0416)
    quant_u = nint(dct_u * 0.0416)
    quant_v = nint(dct_v * 0.0416)
    
    print *, ""
    print *, "Step 4: Quantized DC values"
    print '(A,I5)', "Y DC quant: ", quant_y
    print '(A,I5)', "U DC quant: ", quant_u
    print '(A,I5)', "V DC quant: ", quant_v
    
    print *, ""
    print *, "Expected encoding:"
    print *, "- First Y block: DC=-3, EOB"
    print *, "- Three more Y blocks: DC=0 (diff), EOB"
    print *, "- U block: DC=0, EOB"
    print *, "- V block: DC=0, EOB"
    
    print *, ""
    print *, "If Y DC is wrong, image will appear black!"
    
end program test_trace_encoding