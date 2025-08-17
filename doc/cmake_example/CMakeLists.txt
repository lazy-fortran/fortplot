cmake_minimum_required(VERSION 3.20)
project(fortplot_cmake_example Fortran)

# Enable Fortran
enable_language(Fortran)

# Include FetchContent module
include(FetchContent)

# For testing with local repository, add fortplot as subdirectory
# In production, users would use FetchContent as shown in comments below
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/../../ fortplot_lib)

# Production example using FetchContent (commented out for local testing):
# FetchContent_Declare(
#     fortplot
#     GIT_REPOSITORY https://github.com/lazy-fortran/fortplot
#     GIT_TAG main
# )
# FetchContent_MakeAvailable(fortplot)

# Create a simple test program
add_executable(fortplot_test main.f90)

# Ensure fortplot is built before our test program
add_dependencies(fortplot_test fortplot)

# Link against fortplot library
# Note: FetchContent makes the library target available directly
target_link_libraries(fortplot_test fortplot)

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