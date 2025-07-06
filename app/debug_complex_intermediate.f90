program debug_complex_intermediate
    use iso_c_binding
    use fortplot_jpeg
    implicit none
    
    interface
        function stbi_write_jpg(filename, w, h, comp, data, quality) &
                bind(C, name="stbi_write_jpg")
            import :: c_char, c_int, c_ptr
            character(kind=c_char), intent(in) :: filename(*)
            integer(c_int), value :: w, h, comp, quality
            type(c_ptr), value :: data
            integer(c_int) :: stbi_write_jpg
        end function
    end interface
    
    integer, parameter :: width = 64, height = 64
    integer(1), target :: image_data(width * height * 3)
    integer :: x, y, idx, result
    real :: r, g, b, t
    
    print *, "Debugging Complex Pattern - Intermediate Values"
    print *, "==============================================="
    
    ! Create the same complex test pattern as test_jpeg_large_complex
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            ! Create complex gradients and patterns
            t = real(x + y) / real(width + height - 2)
            
            ! Red channel: horizontal gradient with sine wave
            r = 128.0 + 127.0 * sin(real(x) * 6.28 / real(width))
            
            ! Green channel: vertical gradient  
            g = real(y) * 255.0 / real(height - 1)
            
            ! Blue channel: diagonal gradient with blocks
            b = t * 255.0
            
            ! Add some solid color blocks
            if (mod(x/8, 2) == mod(y/8, 2)) then
                b = 255.0 - b
            end if
            
            image_data(idx) = int(min(255.0, max(0.0, r)), 1)
            image_data(idx + 1) = int(min(255.0, max(0.0, g)), 1) 
            image_data(idx + 2) = int(min(255.0, max(0.0, b)), 1)
        end do
    end do
    
    ! Show sample pixels that will likely cause issues
    print *, "Sample pixel values from complex pattern:"
    print *, ""
    
    ! Top-left corner (0,0)
    idx = 1
    print *, "Pixel (0,0): RGB(", int(image_data(idx)), ",", int(image_data(idx+1)), ",", int(image_data(idx+2)), ")"
    
    ! Middle (32,32) 
    idx = (32 * width + 32) * 3 + 1
    print *, "Pixel (32,32): RGB(", int(image_data(idx)), ",", int(image_data(idx+1)), ",", int(image_data(idx+2)), ")"
    
    ! High frequency area (8,8) - block edge
    idx = (8 * width + 8) * 3 + 1
    print *, "Pixel (8,8): RGB(", int(image_data(idx)), ",", int(image_data(idx+1)), ",", int(image_data(idx+2)), ")"
    
    ! Bottom-right (63,63)
    idx = (63 * width + 63) * 3 + 1
    print *, "Pixel (63,63): RGB(", int(image_data(idx)), ",", int(image_data(idx+1)), ",", int(image_data(idx+2)), ")"
    
    print *, ""
    print *, "Writing both versions..."
    
    ! Write STB version for comparison
    result = stbi_write_jpg("debug_stb_complex.jpg"//c_null_char, width, height, 3, c_loc(image_data), 85)
    if (result == 0) then
        print *, "STB write failed"
        stop 1
    end if
    
    ! Write our version
    call write_jpeg_file("debug_our_complex.jpg", width, height, image_data, 85)
    
    ! Now let's analyze the first MCU where the difference occurs
    print *, ""
    print *, "Complex pattern analysis complete."
    print *, "For 64x64 with 4:2:0 subsampling: 4x4 MCUs = 16 total MCUs"
    print *, "Each MCU covers 16x16 pixels (4 Y blocks + 1 U + 1 V)"
    print *, ""
    print *, "Byte 611 occurs ~17 bytes into scan data."
    print *, "This suggests the difference is in the first or second MCU,"
    print *, "possibly in the AC coefficients of a high-frequency area."
    
end program debug_complex_intermediate