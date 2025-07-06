program debug_stb_solid_red
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
    
    integer, parameter :: width = 64, height = 64
    integer(1), target :: image_data(width * height * 3)
    integer :: i, result
    character(len=100) :: filename
    
    ! Fill with solid red
    do i = 1, width * height * 3, 3
        image_data(i) = int(-1, 1)     ! R = 255
        image_data(i + 1) = int(0, 1)  ! G = 0
        image_data(i + 2) = int(0, 1)  ! B = 0
    end do
    
    ! Write with STB
    filename = "stb_solid_red.jpg" // c_null_char
    result = stbi_write_jpg(filename, width, height, 3, c_loc(image_data), 85)
    if (result == 0) error stop "STB write failed"
    
    print *, "STB solid red image created"
    
end program