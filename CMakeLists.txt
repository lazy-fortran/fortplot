cmake_minimum_required(VERSION 3.20)
project(fortplot VERSION 2025.06.25 LANGUAGES Fortran C)

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Compiler flags
set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fimplicit-none")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

# Automatically collect all source files from src directory
file(GLOB_RECURSE FORTPLOT_SOURCES "src/*.f90" "src/*.c")

# Create static library
add_library(fortplot STATIC ${FORTPLOT_SOURCES})

# Set module directory and enable position-independent code for shared library usage
set_target_properties(fortplot PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/include
    POSITION_INDEPENDENT_CODE ON
)

# Make modules available to consumers
target_include_directories(fortplot
    PUBLIC 
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
        $<INSTALL_INTERFACE:include>
)

# Create alias for FetchContent compatibility
add_library(fortplot::fortplot ALIAS fortplot)

# Export targets for FetchContent
export(TARGETS fortplot
    NAMESPACE fortplot::
    FILE ${CMAKE_CURRENT_BINARY_DIR}/fortplotTargets.cmake
)

# Check if Python interface should be built
if (ENABLE_PYTHON)
    include(cmake/PythonHelpers.cmake)
    add_f90wrap_wrapper(
        fortplot_wrapper
        fortplot
        src/fortplot.f90
    )
    # After f90wrap generates files, fix the imports
    add_custom_command(TARGET _fortplot_wrapper POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E echo "Fixing f90wrap imports..."
        COMMAND sed -i.bak 's/import _fortplot_wrapper/from . import _fortplot_wrapper/g'
            ${CMAKE_CURRENT_BINARY_DIR}/fortplot_wrapper.py
        COMMENT "Fixing f90wrap relative imports"
    )
endif()