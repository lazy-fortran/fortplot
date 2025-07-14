program test_system_cmake_example
    implicit none
    integer :: exit_code
    character(len=256) :: cwd
    
    ! Get current working directory
    call getcwd(cwd)
    
    ! Change to cmake example directory
    call chdir("doc/cmake_example")
    
    ! Clean any previous build
    call execute_command_line("rm -rf build", exitstat=exit_code)
    
    ! Create build directory
    call execute_command_line("mkdir -p build", exitstat=exit_code)
    if (exit_code /= 0) then
        print *, "Failed to create build directory"
        stop 1
    end if
    
    ! Change to build directory
    call chdir("build")
    
    ! Configure with CMake
    call execute_command_line("cmake ..", exitstat=exit_code)
    if (exit_code /= 0) then
        print *, "Failed to configure cmake example"
        stop 1
    end if
    
    ! Build with CMake
    call execute_command_line("cmake --build .", exitstat=exit_code)
    if (exit_code /= 0) then
        print *, "Failed to build cmake example"
        stop 1
    end if
    
    ! Change back to original directory
    call chdir(trim(cwd))
    
    print *, "CMake example build test passed"
    
end program test_system_cmake_example