cmake_minimum_required(VERSION 3.20)
project(fortplot VERSION 0.1.0 LANGUAGES Fortran)

# Set default build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Set Fortran compiler flags
set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fimplicit-none")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all")  
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

# Find all Fortran source files in src directory
file(GLOB_RECURSE FORTPLOT_SOURCES "src/*.f90")

# Remove C files from Fortran sources
list(FILTER FORTPLOT_SOURCES EXCLUDE REGEX ".*\\.c$")

# Create fortplot library
add_library(fortplot ${FORTPLOT_SOURCES})

# Set library properties
set_target_properties(fortplot PROPERTIES
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules"
    POSITION_INDEPENDENT_CODE ON
)

# Include module directory for building
target_include_directories(fortplot PUBLIC
    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/modules>
    $<INSTALL_INTERFACE:include>
)

# Handle C source files if any exist
file(GLOB_RECURSE C_SOURCES "src/*.c")
if(C_SOURCES)
    enable_language(C)
    target_sources(fortplot PRIVATE ${C_SOURCES})
endif()

# Installation rules
include(GNUInstallDirs)

install(TARGETS fortplot
    EXPORT fortplotTargets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)

install(DIRECTORY "${CMAKE_BINARY_DIR}/modules/"
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    FILES_MATCHING PATTERN "*.mod"
)

# Export targets
install(EXPORT fortplotTargets
    FILE fortplotTargets.cmake
    NAMESPACE fortplot::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortplot
)

# Configure package config file
include(CMakePackageConfigHelpers)
configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/fortplotConfig.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/fortplotConfig.cmake"
    INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortplot
)

# Install package config files
install(FILES
    "${CMAKE_CURRENT_BINARY_DIR}/fortplotConfig.cmake"
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortplot
)

message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "Fortran flags: ${CMAKE_Fortran_FLAGS}")