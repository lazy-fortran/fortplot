program debug_gradient_test
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
    
    ! Try the simplest possible gradient that will have AC coefficients
    integer, parameter :: width = 8, height = 8
    integer(1), target :: image_data(width * height * 3)
    integer :: i, j, idx, result, gradient_val
    
    print *, "Creating simple horizontal gradient"
    print *, "=================================="
    
    ! Create horizontal gradient: 0 to 127 across 8 pixels
    do i = 0, height-1
        do j = 0, width-1
            idx = (i * width + j) * 3 + 1
            gradient_val = (j * 127) / 7  ! 0, 18, 36, 54, 72, 90, 108, 127
            
            image_data(idx) = int(gradient_val, 1)     ! R
            image_data(idx+1) = int(gradient_val, 1)   ! G  
            image_data(idx+2) = int(gradient_val, 1)   ! B
        end do
    end do
    
    print *, "Pattern: horizontal gradient 0->127"
    print *, "This should produce minimal AC coefficients"
    print *, ""
    
    ! Write with both
    result = stbi_write_jpg("debug_stb_gradient.jpg"//c_null_char, width, height, 3, c_loc(image_data), 95)
    call write_jpeg_file("debug_our_gradient.jpg", width, height, image_data, 95)
    
    if (result == 0) then
        print *, "STB write failed"
        stop 1  
    end if
    
    call compare_file_sizes()
    
contains

    subroutine compare_file_sizes()
        integer :: unit1, unit2, size1, size2
        
        open(newunit=unit1, file="debug_stb_gradient.jpg", access='stream', status='old')
        inquire(unit=unit1, size=size1)
        close(unit1)
        
        open(newunit=unit2, file="debug_our_gradient.jpg", access='stream', status='old')  
        inquire(unit=unit2, size=size2)
        close(unit2)
        
        print *, "File sizes:"
        print *, "STB:", size1, "bytes"
        print *, "Our:", size2, "bytes"
        print *, "Difference:", abs(size1 - size2), "bytes"
        
        if (size1 == size2) then
            print *, "SUCCESS: Same file size!"
        else
            print *, "ISSUE: Different file sizes"
        end if
    end subroutine compare_file_sizes

end program debug_gradient_test