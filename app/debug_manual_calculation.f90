program debug_manual_calculation
    use fortplot_jpeg
    implicit none
    
    real :: r, g, b, y_val, expected_y
    real :: dct_in(64), dct_out(64), quantized(64)
    real :: fdtbl_y(64)
    integer :: dc_coeff, expected_dc
    
    ! Test values: RGB(127, 0, 0)
    r = 127.0
    g = 0.0
    b = 0.0
    
    ! Manual Y calculation
    y_val = 0.299*r + 0.587*g + 0.114*b - 128.0
    expected_y = 0.299*127 + 0.587*0 + 0.114*0 - 128.0
    
    print *, "Manual Y calculation check:"
    print *, "RGB input: (", int(r), ",", int(g), ",", int(b), ")"
    print *, "Our Y formula result:", y_val
    print *, "Expected Y result:", expected_y
    print *, "Difference:", abs(y_val - expected_y)
    
    ! Create solid color 8x8 block
    dct_in = y_val  ! All pixels same value
    
    ! Get quantization table
    call get_luminance_quantization_table(85, fdtbl_y)
    
    print *, ""
    print *, "Y quantization table [1]:", fdtbl_y(1)
    print *, "Y quantization table [2]:", fdtbl_y(2)
    
    ! Perform DCT (simplified - just for DC component)
    ! For solid color, DC component = average * sqrt(64) = value * 8
    dct_out(1) = y_val * 8.0  ! DC component for solid color
    dct_out(2:64) = 0.0       ! AC components = 0 for solid color
    
    ! Quantize
    quantized(1) = dct_out(1) * fdtbl_y(1)
    dc_coeff = nint(quantized(1))
    
    print *, ""
    print *, "DCT DC component (before quantization):", dct_out(1)
    print *, "Quantized DC component:", quantized(1)
    print *, "Final DC coefficient:", dc_coeff
    
    ! Expected calculation for verification
    expected_dc = nint(expected_y * 8.0 * fdtbl_y(1))
    print *, "Expected DC coefficient:", expected_dc
    print *, ""
    
    ! Check what STB should produce
    print *, "STB Y conversion: +0.29900*r + 0.58700*g + 0.11400*b - 128"
    print *, "STB Y value:", +0.29900*r + 0.58700*g + 0.11400*b - 128
    
contains
    
    subroutine get_luminance_quantization_table(quality, fdtbl)
        integer, intent(in) :: quality
        real, intent(out) :: fdtbl(64)
        
        ! STB luminance quantization table (Q50)
        integer, parameter :: std_lum_qt(64) = [ &
            16,  11,  10,  16,  24,  40,  51,  61, &
            12,  12,  14,  19,  26,  58,  60,  55, &
            14,  13,  16,  24,  40,  57,  69,  56, &
            14,  17,  22,  29,  51,  87,  80,  62, &
            18,  22,  37,  56,  68, 109, 103,  77, &
            24,  35,  55,  64,  81, 104, 113,  92, &
            49,  64,  78,  87, 103, 121, 120, 101, &
            72,  92,  95,  98, 112, 100, 103,  99]
        
        real :: scale_factor
        integer :: i, temp
        
        ! Calculate quality scale factor like STB
        if (quality < 50) then
            scale_factor = 5000.0 / real(quality)
        else
            scale_factor = 200.0 - 2.0 * real(quality)
        end if
        
        ! Create quantization table
        do i = 1, 64
            temp = int((real(std_lum_qt(i)) * scale_factor + 50.0) / 100.0)
            temp = max(1, min(temp, 255))
            fdtbl(i) = 1.0 / real(temp)
        end do
    end subroutine get_luminance_quantization_table
    
end program debug_manual_calculation