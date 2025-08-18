cmake_minimum_required(VERSION 3.20)
project(fortplot VERSION 2025.08.17 LANGUAGES Fortran C)

# Set build type
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Compiler options
set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fimplicit-none")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

# Set position independent code for shared library support
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

# Find all source files automatically
file(GLOB_RECURSE FORTRAN_SOURCES "src/*.f90")
file(GLOB_RECURSE C_SOURCES "src/*.c")

# Debug output
list(LENGTH FORTRAN_SOURCES fortran_count)
list(LENGTH C_SOURCES c_count)
message(STATUS "Found ${fortran_count} Fortran source files")
message(STATUS "Found ${c_count} C source files")

# Create the fortplot library
add_library(fortplot ${FORTRAN_SOURCES} ${C_SOURCES})

# Set Fortran module directory
set_target_properties(fortplot PROPERTIES
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules"
    POSITION_INDEPENDENT_CODE ON
)

# Include module directory for dependent projects
target_include_directories(fortplot
    PUBLIC 
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>
        $<INSTALL_INTERFACE:include>
)

# Create namespaced alias for use in parent projects
add_library(fortplot::fortplot ALIAS fortplot)

# Export configuration for find_package() support
if(NOT CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    # This is a subproject - don't install
    message(STATUS "fortplot configured as subproject")
else()
    # This is the main project - configure install
    include(GNUInstallDirs)
    
    install(TARGETS fortplot
        EXPORT fortplotTargets
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
        RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    )
    
    install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules/
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
        FILES_MATCHING PATTERN "*.mod"
    )
    
    install(EXPORT fortplotTargets
        FILE fortplotTargets.cmake
        NAMESPACE fortplot::
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortplot
    )
    
    include(CMakePackageConfigHelpers)
    configure_package_config_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/cmake/fortplotConfig.cmake.in
        ${CMAKE_CURRENT_BINARY_DIR}/fortplotConfig.cmake
        INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortplot
    )
    
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/fortplotConfig.cmake
        DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/fortplot
    )
endif()
