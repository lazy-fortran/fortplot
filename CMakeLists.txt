cmake_minimum_required(VERSION 3.20)
project(fortplotlib VERSION 2025.06.25 LANGUAGES Fortran C)

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Compiler flags
set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fimplicit-none")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

# Find required dependencies
find_package(PkgConfig REQUIRED)
pkg_check_modules(FREETYPE2 REQUIRED freetype2)
pkg_check_modules(ZLIB REQUIRED zlib)

# Automatically collect all source files from src directory
file(GLOB_RECURSE FORTPLOTLIB_SOURCES "src/*.f90" "src/*.c")

# Create static library
add_library(fortplotlib STATIC ${FORTPLOTLIB_SOURCES})

# Set module directory
set_target_properties(fortplotlib PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
)

# Make modules available to consumers
target_include_directories(fortplotlib 
    PUBLIC ${CMAKE_CURRENT_BINARY_DIR}/include
    PRIVATE ${FREETYPE2_INCLUDE_DIRS} ${ZLIB_INCLUDE_DIRS}
)

# Link dependencies
target_link_libraries(fortplotlib PRIVATE ${FREETYPE2_LIBRARIES} ${ZLIB_LIBRARIES})

# Create alias for FetchContent compatibility
add_library(fortplotlib::fortplotlib ALIAS fortplotlib)