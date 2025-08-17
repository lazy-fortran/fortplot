cmake_minimum_required(VERSION 3.20)
project(fortplot_cmake_example Fortran)

# Enable Fortran
enable_language(Fortran)

# Include FetchContent module
include(FetchContent)

# Fetch and build fortplotlib using CMake
FetchContent_Declare(
    fortplotlib
    GIT_REPOSITORY https://github.com/lazy-fortran/fortplot
    GIT_TAG main
)
FetchContent_MakeAvailable(fortplotlib)

# Create a simple test program
add_executable(fortplot_test main.f90)

# Link against fortplotlib (using old name until rename is merged to main)
target_link_libraries(fortplot_test fortplotlib::fortplotlib)

# Set Fortran compiler flags
set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fimplicit-none")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

# Default to Release build if not specified
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "Fortran flags: ${CMAKE_Fortran_FLAGS}")