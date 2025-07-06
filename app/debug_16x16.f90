program debug_16x16
    use fortplot_jpeg
    use iso_c_binding
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
    
    integer, parameter :: width = 16, height = 16
    integer(1), target :: image_data(width * height * 3)
    integer :: x, y, idx, result
    real :: r, g, b
    character(len=100) :: filename
    
    ! Create simple gradient
    do y = 0, height - 1
        do x = 0, width - 1
            idx = (y * width + x) * 3 + 1
            
            r = real(x) * 255.0 / real(width - 1)
            g = real(y) * 255.0 / real(height - 1)
            b = 128.0
            
            image_data(idx) = int(r, 1)
            image_data(idx + 1) = int(g, 1) 
            image_data(idx + 2) = int(b, 1)
        end do
    end do
    
    print *, "Creating 16x16 gradient (1 MCU for subsampled mode)..."
    
    ! Write with STB
    filename = "stb_16x16.jpg" // c_null_char
    result = stbi_write_jpg(filename, width, height, 3, c_loc(image_data), 85)
    if (result == 0) error stop "STB write failed"
    
    ! Write with our implementation
    call write_jpeg_file("our_16x16.jpg", width, height, image_data, 85)
    
    print *, "Files created. Check visual output."
    
end program