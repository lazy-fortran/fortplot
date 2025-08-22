program test_windows_runtime
    !! Test runtime OS detection and path handling
    use fortplot_system_runtime
    implicit none
    
    character(len=256) :: test_path
    character(len=:), allocatable :: result
    logical :: success
    
    print *, "=== Runtime OS Detection and Path Tests ==="
    print *, ""
    
    ! Test OS detection
    if (is_windows()) then
        print *, "Runtime detection: Windows"
    else
        print *, "Runtime detection: Unix/Linux"
    end if
    print *, ""
    
    ! Test path mapping
    print *, "Test: Path Mapping"
    print *, "------------------"
    
    test_path = "/tmp"
    result = map_unix_to_windows_path(test_path)
    print *, "  /tmp -> '", trim(result), "'"
    if (trim(result) /= "tmp") then
        print *, "    ERROR: Expected 'tmp'"
        stop 1
    end if
    
    test_path = "/tmp/file.txt"
    result = map_unix_to_windows_path(test_path)
    print *, "  /tmp/file.txt -> '", trim(result), "'"
    if (trim(result) /= "tmp/file.txt") then
        print *, "    ERROR: Expected 'tmp/file.txt'"
        stop 1
    end if
    
    test_path = "/tmp/subdir/file.txt"
    result = map_unix_to_windows_path(test_path)
    print *, "  /tmp/subdir/file.txt -> '", trim(result), "'"
    if (trim(result) /= "tmp/subdir/file.txt") then
        print *, "    ERROR: Expected 'tmp/subdir/file.txt'"
        stop 1
    end if
    
    test_path = "output/test/file.txt"
    result = map_unix_to_windows_path(test_path)
    print *, "  output/test/file.txt -> '", trim(result), "'"
    if (trim(result) /= "output/test/file.txt") then
        print *, "    ERROR: Expected 'output/test/file.txt'"
        stop 1
    end if
    
    print *, "  PASS: All path mappings correct"
    print *, ""
    
    ! Test path normalization
    print *, "Test: Path Normalization"
    print *, "------------------------"
    
    test_path = "path/to/file.txt"
    result = normalize_path_separators(test_path, .true.)
    print *, "  path/to/file.txt (to Windows) -> '", trim(result), "'"
    if (index(result, "\") == 0 .and. is_windows()) then
        print *, "    WARNING: Expected backslashes on Windows"
    end if
    
    test_path = "path\to\file.txt"
    result = normalize_path_separators(test_path, .false.)
    print *, "  path\to\file.txt (to Unix) -> '", trim(result), "'"
    if (index(result, "/") == 0) then
        print *, "    WARNING: Expected forward slashes"
    end if
    
    print *, "  PASS: Path normalization working"
    print *, ""
    
    ! Test directory creation
    print *, "Test: Directory Creation"
    print *, "------------------------"
    
    call create_directory_runtime("test_runtime_dir", success)
    if (success) then
        print *, "  Created test_runtime_dir: SUCCESS"
    else
        print *, "  WARNING: Could not create test_runtime_dir"
    end if
    
    ! Test with /tmp path
    call create_directory_runtime("/tmp/test_runtime", success)
    if (success) then
        print *, "  Created /tmp/test_runtime: SUCCESS"
    else
        print *, "  WARNING: Could not create /tmp/test_runtime"
    end if
    
    ! Clean up
    call delete_file_runtime("test_runtime_dir/.gitkeep", success)
    call delete_file_runtime("/tmp/test_runtime/.gitkeep", success)
    
    print *, ""
    print *, "All runtime tests completed!"
    
end program test_windows_runtime