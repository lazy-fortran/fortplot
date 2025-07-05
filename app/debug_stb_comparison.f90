program debug_stb_comparison
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
    
    integer(1), target :: gray_data(16*16*3)
    integer :: i, result
    character(len=100) :: filename
    
    ! Create 16x16 solid gray (127,127,127) image
    do i = 1, 16*16*3, 3
        gray_data(i) = int(127, 1)      ! R
        gray_data(i+1) = int(127, 1)    ! G
        gray_data(i+2) = int(127, 1)    ! B
    end do
    
    ! Write with STB
    filename = "stb_16x16_gray.jpg" // c_null_char
    result = stbi_write_jpg(filename, 16, 16, 3, c_loc(gray_data), 90)
    print *, "STB JPEG written"
    
    ! Write with our implementation
    call write_jpeg_file("our_16x16_gray.jpg", 16, 16, gray_data, 90)
    print *, "Our JPEG written"
    
end program debug_stb_comparison