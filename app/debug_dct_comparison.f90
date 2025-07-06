program debug_dct_comparison
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
    
    ! Test with a simple pattern that should produce predictable DCT coefficients
    integer, parameter :: width = 8, height = 8
    integer(1), target :: image_data(width * height * 3)
    integer :: x, y, idx, result
    real :: val
    
    print *, "DCT Comparison Test"
    print *, "==================="
    
    ! Create a simple gradient that will produce AC coefficients
    do y = 0, height-1
        do x = 0, width-1
            idx = (y * width + x) * 3 + 1
            
            ! Simple horizontal gradient 0-127
            val = real(x) * 127.0 / 7.0
            
            image_data(idx) = int(val, 1)     ! R
            image_data(idx+1) = int(val, 1)   ! G  
            image_data(idx+2) = int(val, 1)   ! B
        end do
    end do
    
    print *, "Test pattern: Horizontal gradient 0-127"
    print *, ""
    
    ! Trace what happens to first row
    print *, "First row RGB values:"
    do x = 0, 7
        idx = x * 3 + 1
        print '("  x=", I1, ": RGB(", I3, ",", I3, ",", I3, ")")', &
            x, image_data(idx), image_data(idx+1), image_data(idx+2)
    end do
    
    ! Write with both implementations
    result = stbi_write_jpg("debug_stb_dct.jpg"//c_null_char, width, height, 3, c_loc(image_data), 95)
    call write_jpeg_file("debug_our_dct.jpg", width, height, image_data, 95)
    
    if (result == 0) then
        print *, "STB write failed"
        stop 1  
    end if
    
    call compare_files()
    
contains

    subroutine compare_files()
        integer :: unit1, unit2, size1, size2
        integer(1), allocatable :: data1(:), data2(:)
        integer :: i, diffs
        
        open(newunit=unit1, file="debug_stb_dct.jpg", access='stream', status='old')
        inquire(unit=unit1, size=size1)
        allocate(data1(size1))
        read(unit1) data1
        close(unit1)
        
        open(newunit=unit2, file="debug_our_dct.jpg", access='stream', status='old')  
        inquire(unit=unit2, size=size2)
        allocate(data2(size2))
        read(unit2) data2
        close(unit2)
        
        print *, ""
        print *, "File sizes:"
        print *, "STB:", size1, "bytes"
        print *, "Our:", size2, "bytes"
        
        ! Count differences
        diffs = 0
        do i = 1, min(size1, size2)
            if (data1(i) /= data2(i)) diffs = diffs + 1
        end do
        
        if (size1 /= size2) then
            diffs = diffs + abs(size1 - size2)
        end if
        
        print *, "Total differences:", diffs, "bytes"
        
        if (diffs == 0) then
            print *, "SUCCESS: Files match exactly!"
        else
            print *, "Files differ"
            
            ! Find scan data start
            do i = 1, min(size1, size2) - 1
                if (data1(i) == int(Z'FF', 1) .and. data1(i+1) == int(Z'DA', 1)) then
                    print *, "SOS marker at byte", i
                    exit
                end if
            end do
        end if
    end subroutine

end program debug_dct_comparison