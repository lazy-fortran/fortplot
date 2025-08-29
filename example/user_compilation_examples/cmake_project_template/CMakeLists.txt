cmake_minimum_required(VERSION 3.20)

# Project configuration
project(FortplotUserTemplate VERSION 0.1.0 LANGUAGES Fortran)

# Set build type if not specified
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

# Fortran compiler requirements
set(CMAKE_Fortran_STANDARD 2008)
set(CMAKE_Fortran_STANDARD_REQUIRED ON)

# Enable modern Fortran features
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fimplicit-none")

# Add debug information in debug builds
if(CMAKE_BUILD_TYPE STREQUAL "Debug")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -g -Wall -Wextra -fcheck=all")
else()
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3")
endif()

# Fetch fortplot library
include(FetchContent)

message(STATUS "Fetching fortplot library...")
FetchContent_Declare(
    fortplot
    GIT_REPOSITORY https://github.com/lazy-fortran/fortplot
    GIT_TAG        main
    GIT_PROGRESS   TRUE
)

# Make fortplot available
FetchContent_MakeAvailable(fortplot)

# Create the executable
add_executable(fortplot_demo
    src/cmake_template_demo.f90
)

# Link fortplot library
target_link_libraries(fortplot_demo 
    PRIVATE 
    fortplot::fortplot
)

# Set output directory
set_target_properties(fortplot_demo
    PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

# Print configuration summary
message(STATUS "")
message(STATUS "=== Fortplot User Template Configuration ===")
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER_ID} ${CMAKE_Fortran_COMPILER_VERSION}")
message(STATUS "Fortran flags: ${CMAKE_Fortran_FLAGS}")
message(STATUS "Output directory: ${CMAKE_BINARY_DIR}/bin")
message(STATUS "")
message(STATUS "To build:")
message(STATUS "  cmake --build .")
message(STATUS "To run:")
message(STATUS "  ./bin/fortplot_demo")
message(STATUS "==========================================")
message(STATUS "")