program test_windows_path_handling
    !! Test Windows path handling functions
    !! Tests the path manipulation logic independently
    use iso_c_binding
    implicit none
    
    interface
        ! Test the path mapping function
        function map_unix_to_windows_path_c(path) bind(C, name="map_unix_to_windows_path")
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: path(*)
            type(c_ptr) :: map_unix_to_windows_path_c
        end function
        
        ! Test the separator finding function
        function find_last_separator_c(path) bind(C, name="find_last_separator")
            import :: c_int, c_char
            character(kind=c_char), intent(in) :: path(*)
            integer(c_int) :: find_last_separator_c
        end function
        
        ! Test parent directory extraction
        function get_parent_directory_c(path) bind(C, name="get_parent_directory")
            import :: c_ptr, c_char
            character(kind=c_char), intent(in) :: path(*)
            type(c_ptr) :: get_parent_directory_c
        end function
        
        ! Check if compiled for Windows
        function is_windows_build_c() bind(C, name="is_windows_build")
            import :: c_int
            integer(c_int) :: is_windows_build_c
        end function
        
        ! C string functions
        function strlen(str) bind(C, name="strlen")
            import :: c_ptr, c_size_t
            type(c_ptr), value :: str
            integer(c_size_t) :: strlen
        end function
        
        subroutine free(ptr) bind(C, name="free")
            import :: c_ptr
            type(c_ptr), value :: ptr
        end subroutine
    end interface
    
    print *, "=== Windows Path Handling Tests ==="
    print *, ""
    
    ! First check if we're compiled for Windows
    if (is_windows_build_c() == 1) then
        print *, "Compiled with _WIN32 defined"
    else
        print *, "Compiled without _WIN32 (Unix/Linux)"
    end if
    print *, ""
    
    call test_path_mapping()
    call test_separator_finding()
    call test_parent_directory()
    
    print *, ""
    print *, "All Windows path handling tests completed!"
    
contains
    
    subroutine test_path_mapping()
        character(len=256) :: test_path
        character(len=256) :: result
        type(c_ptr) :: c_result
        integer :: i
        
        print *, "Test: Path Mapping"
        print *, "------------------"
        
        ! Test 1: /tmp -> tmp
        test_path = "/tmp" // c_null_char
        c_result = map_unix_to_windows_path_c(test_path)
        if (c_associated(c_result)) then
            call c_ptr_to_string(c_result, result)
            print *, "  /tmp -> '", trim(result), "'"
            if (trim(result) /= "tmp") then
                print *, "    ERROR: Expected 'tmp'"
                stop 1
            end if
            call free(c_result)
        else
            print *, "    ERROR: NULL result"
            stop 1
        end if
        
        ! Test 2: /tmp/file.txt -> tmp/file.txt
        test_path = "/tmp/file.txt" // c_null_char
        c_result = map_unix_to_windows_path_c(test_path)
        if (c_associated(c_result)) then
            call c_ptr_to_string(c_result, result)
            print *, "  /tmp/file.txt -> '", trim(result), "'"
            if (trim(result) /= "tmp/file.txt") then
                print *, "    ERROR: Expected 'tmp/file.txt'"
                stop 1
            end if
            call free(c_result)
        end if
        
        ! Test 3: /tmp/subdir/file.txt -> tmp/subdir/file.txt
        test_path = "/tmp/subdir/file.txt" // c_null_char
        c_result = map_unix_to_windows_path_c(test_path)
        if (c_associated(c_result)) then
            call c_ptr_to_string(c_result, result)
            print *, "  /tmp/subdir/file.txt -> '", trim(result), "'"
            if (trim(result) /= "tmp/subdir/file.txt") then
                print *, "    ERROR: Expected 'tmp/subdir/file.txt'"
                stop 1
            end if
            call free(c_result)
        end if
        
        ! Test 4: Regular path unchanged
        test_path = "output/test/file.txt" // c_null_char
        c_result = map_unix_to_windows_path_c(test_path)
        if (c_associated(c_result)) then
            call c_ptr_to_string(c_result, result)
            print *, "  output/test/file.txt -> '", trim(result), "'"
            if (trim(result) /= "output/test/file.txt") then
                print *, "    ERROR: Expected 'output/test/file.txt'"
                stop 1
            end if
            call free(c_result)
        end if
        
        print *, "  PASS: All path mappings correct"
    end subroutine
    
    subroutine test_separator_finding()
        character(len=256) :: test_path
        integer(c_int) :: last_sep
        
        print *, ""
        print *, "Test: Separator Finding"
        print *, "-----------------------"
        
        ! Test with forward slashes
        test_path = "path/to/file.txt" // c_null_char
        last_sep = find_last_separator_c(test_path)
        print *, "  path/to/file.txt -> last separator at position", last_sep
        if (last_sep /= 7) then  ! position of last /
            print *, "    ERROR: Expected 7"
            stop 1
        end if
        
        ! Test with backslashes
        test_path = "path\to\file.txt" // c_null_char
        last_sep = find_last_separator_c(test_path)
        print *, "  path\to\file.txt -> last separator at position", last_sep
        if (last_sep /= 7) then  ! position of last \
            print *, "    ERROR: Expected 7"
            stop 1
        end if
        
        ! Test with mixed separators
        test_path = "path/to\file.txt" // c_null_char
        last_sep = find_last_separator_c(test_path)
        print *, "  path/to\file.txt -> last separator at position", last_sep
        if (last_sep /= 7) then  ! position of last \
            print *, "    ERROR: Expected 7"
            stop 1
        end if
        
        ! Test with no separators
        test_path = "file.txt" // c_null_char
        last_sep = find_last_separator_c(test_path)
        print *, "  file.txt -> last separator at position", last_sep
        if (last_sep /= -1) then
            print *, "    ERROR: Expected -1"
            stop 1
        end if
        
        print *, "  PASS: All separator findings correct"
    end subroutine
    
    subroutine test_parent_directory()
        character(len=256) :: test_path
        character(len=256) :: result
        type(c_ptr) :: c_result
        
        print *, ""
        print *, "Test: Parent Directory Extraction"
        print *, "---------------------------------"
        
        ! Test with forward slash
        test_path = "path/to/file.txt" // c_null_char
        c_result = get_parent_directory_c(test_path)
        if (c_associated(c_result)) then
            call c_ptr_to_string(c_result, result)
            print *, "  path/to/file.txt -> '", trim(result), "'"
            if (trim(result) /= "path/to") then
                print *, "    ERROR: Expected 'path/to'"
                stop 1
            end if
            call free(c_result)
        end if
        
        ! Test with backslash
        test_path = "path\to\file.txt" // c_null_char
        c_result = get_parent_directory_c(test_path)
        if (c_associated(c_result)) then
            call c_ptr_to_string(c_result, result)
            print *, "  path\to\file.txt -> '", trim(result), "'"
            if (trim(result) /= "path\to") then
                print *, "    ERROR: Expected 'path\to'"
                stop 1
            end if
            call free(c_result)
        end if
        
        ! Test with no parent
        test_path = "file.txt" // c_null_char
        c_result = get_parent_directory_c(test_path)
        if (c_associated(c_result)) then
            print *, "    ERROR: Expected NULL for file without parent"
            stop 1
        else
            print *, "  file.txt -> NULL (correct)"
        end if
        
        print *, "  PASS: All parent directory extractions correct"
    end subroutine
    
    subroutine c_ptr_to_string(ptr, f_string)
        use iso_c_binding
        type(c_ptr), intent(in) :: ptr
        character(len=*), intent(out) :: f_string
        character(kind=c_char), pointer :: c_string(:)
        integer :: i
        
        f_string = ''
        if (.not. c_associated(ptr)) return
        
        call c_f_pointer(ptr, c_string, [256])
        
        do i = 1, len(f_string)
            if (c_string(i) == c_null_char) exit
            f_string(i:i) = c_string(i)
        end do
    end subroutine
    
end program test_windows_path_handling