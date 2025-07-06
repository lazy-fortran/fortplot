program debug_bit_trace_detailed
    use fortplot_jpeg, only: jpeg_context, create_jpeg_canvas, get_jpeg_data, initialize_huffman_tables
    use fortplot_jpeg, only: YDC_HT, UVDC_HT, YAC_HT, UVAC_HT
    use, intrinsic :: iso_fortran_env, only: int8, real64
    implicit none
    
    integer, parameter :: width = 8, height = 8, quality = 90
    integer(int8) :: rgb_data(width*height*3)
    integer :: i, j
    real(real64) :: y_val, cb_val, cr_val
    real(real64) :: r, g, b
    real(real64) :: dc_coeff
    integer :: quant_dc, quantized_dc, dc_diff, category, dc_bits
    
    ! Initialize Huffman tables
    call initialize_huffman_tables()
    
    print *, "=== Tracing bit writes for 8x8 gray image ==="
    print *, ""
    
    ! Create 8x8 uniform gray image (all pixels = 127)
    do i = 1, width*height*3
        rgb_data(i) = 127_int8
    end do
    
    ! Let's manually trace what happens with this image
    ! Step 1: RGB to YCbCr conversion
    
    r = 127.0_real64
    g = 127.0_real64
    b = 127.0_real64
    
    y_val = 0.299_real64*r + 0.587_real64*g + 0.114_real64*b - 128.0_real64
    cb_val = -0.169_real64*r - 0.331_real64*g + 0.5_real64*b
    cr_val = 0.5_real64*r - 0.419_real64*g - 0.081_real64*b
    
    print *, "YCbCr values for gray pixel (127,127,127):"
    print *, "Y =", y_val
    print *, "Cb =", cb_val
    print *, "Cr =", cr_val
    print *, ""
    
    ! Step 2: After DCT, all AC coefficients should be 0 for uniform block
    ! DC coefficient = sum of all pixels / 8 = 8 * y_val
    dc_coeff = y_val * 8.0_real64
    print *, "DC coefficient before quantization:", dc_coeff
    
    ! Step 3: Quantization
    ! For quality 90, scale factor = 0.2
    ! Base quant for DC luma = 16
    ! Scaled quant = 16 * 0.2 = 3.2, rounded to 3
    quant_dc = 3
    quantized_dc = nint(dc_coeff / real(quant_dc, real64))
    print *, "Quantized DC:", quantized_dc
    print *, ""
    
    ! Step 4: DC encoding
    ! For the first block, DC diff = DC value
    dc_diff = quantized_dc
    print *, "DC diff to encode:", dc_diff
    
    ! Get category
    if (abs(dc_diff) == 0) then
        category = 0
    else
        category = 1
        do while (ishft(1, category-1) <= abs(dc_diff))
            category = category + 1
        end do
    end if
    print *, "DC category:", category
    
    ! Get Huffman code
    if (category >= 0 .and. category <= 11) then
        print *, "DC Huffman code: value=", YDC_HT(category+1,1), "bits=", YDC_HT(category+1,2)
    end if
    
    ! Calculate DC value bits
    if (category > 0) then
        if (dc_diff >= 0) then
            dc_bits = dc_diff
        else
            dc_bits = dc_diff + ishft(1, category) - 1
        end if
        print *, "DC value bits:", dc_bits, "num_bits=", category
    end if
    
    print *, ""
    print *, "For uniform 8x8 block, all AC coefficients are 0"
    print *, "So we only write EOB marker"
    print *, "EOB code: value=", YAC_HT(1,1), "bits=", YAC_HT(1,2)
    
end program debug_bit_trace_detailed