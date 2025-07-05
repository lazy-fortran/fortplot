program debug_stb_8x8
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
    
    integer(1), target :: gray_data(8*8*3)
    integer :: i
    character(len=100) :: filename
    integer :: result
    
    ! Create 8x8 solid gray (127,127,127) image
    do i = 1, 8*8*3, 3
        gray_data(i) = int(127, 1)      ! R
        gray_data(i+1) = int(127, 1)    ! G
        gray_data(i+2) = int(127, 1)    ! B
    end do
    
    ! Write with STB
    filename = "stb_8x8.jpg" // c_null_char
    result = stbi_write_jpg(filename, 8, 8, 3, c_loc(gray_data), 90)
    
    if (result == 1) then
        print *, "STB JPEG written to stb_8x8.jpg"
    else
        print *, "Failed to write STB JPEG"
    end if
    
end program debug_stb_8x8