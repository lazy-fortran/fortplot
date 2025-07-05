program test_jpeg_exact_match_stb
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
    
    integer(1), target :: test_data(8*8*3)
    integer :: i, result
    character(len=100) :: filename
    logical :: all_tests_pass = .true.
    
    print *, "JPEG Exact Match Test - Bit-for-bit comparison with STB"
    print *, "======================================================"
    
    ! Test 1: Solid gray (127,127,127)
    do i = 1, 8*8*3, 3
        test_data(i) = int(127, 1)
        test_data(i+1) = int(127, 1)
        test_data(i+2) = int(127, 1)
    end do
    
    filename = "stb_exact_gray.jpg" // c_null_char
    result = stbi_write_jpg(filename, 8, 8, 3, c_loc(test_data), 90)
    call write_jpeg_file("our_exact_gray.jpg", 8, 8, test_data, 90)
    
    if (.not. compare_files_exact("stb_exact_gray.jpg", "our_exact_gray.jpg")) then
        all_tests_pass = .false.
    end if
    
    ! Test 2: Black (0,0,0)
    test_data = 0
    
    filename = "stb_exact_black.jpg" // c_null_char
    result = stbi_write_jpg(filename, 8, 8, 3, c_loc(test_data), 90)
    call write_jpeg_file("our_exact_black.jpg", 8, 8, test_data, 90)
    
    if (.not. compare_files_exact("stb_exact_black.jpg", "our_exact_black.jpg")) then
        all_tests_pass = .false.
    end if
    
    ! Test 3: White (255,255,255)
    test_data = -1_int8  ! 255 in unsigned
    
    filename = "stb_exact_white.jpg" // c_null_char
    result = stbi_write_jpg(filename, 8, 8, 3, c_loc(test_data), 90)
    call write_jpeg_file("our_exact_white.jpg", 8, 8, test_data, 90)
    
    if (.not. compare_files_exact("stb_exact_white.jpg", "our_exact_white.jpg")) then
        all_tests_pass = .false.
    end if
    
    if (all_tests_pass) then
        print *, ""
        print *, "ALL TESTS PASSED - Exact match with STB!"
    else
        print *, ""
        print *, "TESTS FAILED - Not bit-exact with STB"
        stop 1
    end if
    
contains

    function compare_files_exact(file1, file2) result(match)
        character(*), intent(in) :: file1, file2
        logical :: match
        integer :: unit1, unit2, ios1, ios2
        integer(1) :: byte1, byte2
        integer :: pos, diff_count
        
        match = .true.
        diff_count = 0
        pos = 0
        
        open(newunit=unit1, file=file1, access='stream', status='old', iostat=ios1)
        open(newunit=unit2, file=file2, access='stream', status='old', iostat=ios2)
        
        if (ios1 /= 0 .or. ios2 /= 0) then
            match = .false.
            return
        end if
        
        do
            read(unit1, iostat=ios1) byte1
            read(unit2, iostat=ios2) byte2
            pos = pos + 1
            
            if (ios1 /= 0 .or. ios2 /= 0) exit
            
            if (byte1 /= byte2) then
                if (diff_count < 10) then  ! Show first 10 differences
                    print '(A,I6,A,Z2.2,A,Z2.2)', &
                        "Diff at byte ", pos, ": STB=", byte1, " Our=", byte2
                end if
                diff_count = diff_count + 1
                match = .false.
            end if
        end do
        
        close(unit1)
        close(unit2)
        
        if (diff_count > 0) then
            print '(A,I0,A)', "Total differences: ", diff_count, " bytes"
        else
            print '(A)', "Files match exactly!"
        end if
        
    end function compare_files_exact

end program test_jpeg_exact_match_stb