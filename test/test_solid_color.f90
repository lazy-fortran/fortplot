program test_solid_color
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
    
    integer(1), target :: image_data(32 * 32 * 3)
    character(len=100) :: filename
    integer :: result
    
    ! Fill with solid gray (128, 128, 128)
    image_data = int(-128, 1)  ! This is 128 when interpreted as unsigned
    
    ! Write with STB
    filename = "stb_solid_gray_32.jpg" // c_null_char
    result = stbi_write_jpg(filename, 32, 32, 3, c_loc(image_data), 85)
    
    ! Write with our implementation
    call write_jpeg_file("our_solid_gray_32.jpg", 32, 32, image_data, 85)
    
    print *, "Created solid gray 32x32 test images"
    
end program