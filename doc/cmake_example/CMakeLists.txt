cmake_minimum_required(VERSION 3.20)
project(fortplot_example LANGUAGES Fortran)

# Set build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Compiler options
set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fimplicit-none")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

# Find or add fortplot as subdirectory
if(NOT TARGET fortplot::fortplot)
    # Add fortplot from parent directory
    add_subdirectory(../../ fortplot)
endif()

# Create the example executable
add_executable(fortplot_test main.f90)

# Link against fortplot
target_link_libraries(fortplot_test PRIVATE fortplot::fortplot)