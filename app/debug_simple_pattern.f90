program debug_simple_pattern
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
    
    ! Try patterns of increasing complexity
    call test_pattern("solid_color", create_solid_pattern)
    call test_pattern("two_colors", create_two_color_pattern)
    call test_pattern("simple_checker", create_simple_checker)
    call test_pattern("gradient", create_gradient_pattern)
    
contains

    subroutine test_pattern(name, pattern_creator)
        character(len=*), intent(in) :: name
        interface
            subroutine pattern_creator(data)
                integer(1), intent(out) :: data(:)
            end subroutine
        end interface
        
        integer, parameter :: width = 16, height = 16
        integer(1), target :: image_data(width * height * 3)
        integer :: stb_result
        character(len=100) :: stb_file, our_file
        
        call pattern_creator(image_data)
        
        stb_file = "debug_stb_" // trim(name) // ".jpg"
        our_file = "debug_our_" // trim(name) // ".jpg"
        
        stb_result = stbi_write_jpg(trim(stb_file)//c_null_char, width, height, 3, c_loc(image_data), 85)
        call write_jpeg_file(trim(our_file), width, height, image_data, 85)
        
        if (stb_result == 0) then
            print *, "STB write failed for ", name
            return
        end if
        
        call compare_files(trim(stb_file), trim(our_file), name)
    end subroutine test_pattern
    
    subroutine create_solid_pattern(data)
        integer(1), intent(out) :: data(:)
        data = int(64, 1)  ! Solid gray
    end subroutine
    
    subroutine create_two_color_pattern(data)
        integer(1), intent(out) :: data(:)
        integer :: i
        do i = 1, size(data), 3
            if (mod((i-1)/3, 2) == 0) then
                data(i:i+2) = int(64, 1)   ! Dark
            else
                data(i:i+2) = int(127, 1)  ! Light
            end if
        end do
    end subroutine
    
    subroutine create_simple_checker(data)
        integer(1), intent(out) :: data(:)
        integer :: x, y, idx
        
        do y = 0, 15
            do x = 0, 15
                idx = (y * 16 + x) * 3 + 1
                if (mod(x + y, 2) == 0) then
                    data(idx:idx+2) = int(0, 1)     ! Black
                else
                    data(idx:idx+2) = int(127, 1)   ! White
                end if
            end do
        end do
    end subroutine
    
    subroutine create_gradient_pattern(data)
        integer(1), intent(out) :: data(:)
        integer :: x, y, idx, val
        
        do y = 0, 15
            do x = 0, 15
                idx = (y * 16 + x) * 3 + 1
                val = (x * 127) / 15
                data(idx:idx+2) = int(val, 1)
            end do
        end do
    end subroutine
    
    subroutine compare_files(stb_file, our_file, name)
        character(len=*), intent(in) :: stb_file, our_file, name
        integer :: unit1, unit2, size1, size2
        
        open(newunit=unit1, file=stb_file, access='stream', status='old')
        inquire(unit=unit1, size=size1)
        close(unit1)
        
        open(newunit=unit2, file=our_file, access='stream', status='old')
        inquire(unit=unit2, size=size2)
        close(unit2)
        
        print *, trim(name), ": STB=", size1, "Our=", size2, "Diff=", abs(size1-size2)
        
        if (size1 == size2) then
            print *, "  SUCCESS: ", trim(name), " matches exactly!"
        else
            print *, "  ISSUE: ", trim(name), " differs by", abs(size1-size2), "bytes"
        end if
    end subroutine compare_files

end program debug_simple_pattern