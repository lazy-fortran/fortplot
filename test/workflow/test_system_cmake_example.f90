program test_system_cmake_example
    implicit none
    logical :: cmake_exists
    
    ! Check if CMakeLists.txt exists
    inquire(file="doc/cmake_example/CMakeLists.txt", exist=cmake_exists)
    if (.not. cmake_exists) then
        print *, "CMake example files not found - CMake support has been removed"
        print *, "CMake example build test skipped"
        return
    end if
    
end program test_system_cmake_example