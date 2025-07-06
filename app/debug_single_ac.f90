program debug_single_ac
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
    
    ! Create simplest possible AC coefficient case: 2x2 image with minimal pattern
    integer, parameter :: width = 8, height = 8
    integer(1), target :: image_data(width * height * 3)
    integer :: i, j, idx, result
    
    print *, "Creating minimal AC coefficient test"
    print *, "==================================="
    
    ! Create simple 2-tone pattern: half bright, half dark
    do i = 0, height-1
        do j = 0, width-1
            idx = (i * width + j) * 3 + 1
            
            if (j < 4) then
                ! Left half: dark (32)
                image_data(idx:idx+2) = int(32, 1)
            else
                ! Right half: bright (96)  
                image_data(idx:idx+2) = int(96, 1)
            end if
        end do
    end do
    
    print *, "Pattern: left half=32, right half=96"
    print *, "This should create minimal AC coefficients"
    print *, ""
    
    ! Write with both
    result = stbi_write_jpg("debug_stb_single_ac.jpg"//c_null_char, width, height, 3, c_loc(image_data), 85)
    call write_jpeg_file("debug_our_single_ac.jpg", width, height, image_data, 85)
    
    if (result == 0) then
        print *, "STB write failed"
        stop 1  
    end if
    
    call compare_file_sizes()
    
contains

    subroutine compare_file_sizes()
        integer :: unit1, unit2, size1, size2
        
        open(newunit=unit1, file="debug_stb_single_ac.jpg", access='stream', status='old')
        inquire(unit=unit1, size=size1)
        close(unit1)
        
        open(newunit=unit2, file="debug_our_single_ac.jpg", access='stream', status='old')
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

end program debug_single_ac