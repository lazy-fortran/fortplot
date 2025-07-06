program debug_ac_coefficients
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
    
    ! Create a simple test pattern that will have AC coefficients
    integer, parameter :: width = 8, height = 8
    integer(1), target :: image_data(width * height * 3)
    integer :: i, j, idx, result
    
    print *, "Creating simple pattern with AC coefficients"
    print *, "============================================"
    
    ! Create a simple checker pattern that will definitely have AC coefficients
    do i = 0, height-1
        do j = 0, width-1
            idx = (i * width + j) * 3 + 1
            if (mod(i + j, 2) == 0) then
                ! White squares
                image_data(idx) = int(127, 1)     ! R (use 127 to avoid overflow)
                image_data(idx+1) = int(127, 1)   ! G  
                image_data(idx+2) = int(127, 1)   ! B
            else
                ! Black squares  
                image_data(idx) = int(0, 1)       ! R
                image_data(idx+1) = int(0, 1)     ! G
                image_data(idx+2) = int(0, 1)     ! B
            end if
        end do
    end do
    
    print *, "Pattern created: 8x8 checkerboard (white=255, black=0)"
    print *, "This will definitely produce non-zero AC coefficients"
    print *, ""
    
    ! Write with both implementations
    result = stbi_write_jpg("debug_stb_checker.jpg"//c_null_char, width, height, 3, c_loc(image_data), 95)
    call write_jpeg_file("debug_our_checker.jpg", width, height, image_data, 95)
    
    if (result == 0) then
        print *, "STB write failed"
        stop 1
    end if
    
    print *, "Both files written. Check the difference!"
    
    ! Compare file sizes
    call compare_file_sizes()
    
contains

    subroutine compare_file_sizes()
        integer :: unit1, unit2, size1, size2
        
        open(newunit=unit1, file="debug_stb_checker.jpg", access='stream', status='old')
        inquire(unit=unit1, size=size1)
        close(unit1)
        
        open(newunit=unit2, file="debug_our_checker.jpg", access='stream', status='old')
        inquire(unit=unit2, size=size2)
        close(unit2)
        
        print *, "File sizes:"
        print *, "STB:", size1, "bytes"
        print *, "Our:", size2, "bytes"
        print *, "Difference:", abs(size1 - size2), "bytes"
        
        if (size1 == size2) then
            print *, "SUCCESS: Same file size!"
        else
            print *, "ISSUE: Different file sizes indicate encoding differences"
        end if
    end subroutine compare_file_sizes

end program debug_ac_coefficients