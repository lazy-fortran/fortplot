program debug_large_complex_trace
    use iso_c_binding
    use fortplot_jpeg
    use iso_fortran_env, only: int8
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
    character(len=100) :: filename
    
    print *, "Creating large complex pattern for debugging"
    
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
    
    ! Show some sample pixels
    print *, "Sample pixels (R,G,B):"
    print '("Pixel (0,0): ", 3(I4))', image_data(1:3)
    print '("Pixel (31,0): ", 3(I4))', image_data(31*3+1:31*3+3)
    print '("Pixel (0,31): ", 3(I4))', image_data(31*width*3+1:31*width*3+3)
    print '("Pixel (63,63): ", 3(I4))', image_data((63*width+63)*3+1:(63*width+63)*3+3)
    
    ! Write with STB for reference
    filename = "debug_stb_complex.jpg" // c_null_char
    result = stbi_write_jpg(filename, width, height, 3, c_loc(image_data), 85)
    if (result == 0) error stop "STB write failed"
    print *, "STB file written: debug_stb_complex.jpg"
    
    ! Write with our implementation
    print *, "Writing with our implementation..."
    call write_jpeg_file("debug_our_complex.jpg", width, height, image_data, 85)
    print *, "Our file written: debug_our_complex.jpg"
    
    print *, ""
    print *, "Check the generated files to see the difference"
    
end program debug_large_complex_trace