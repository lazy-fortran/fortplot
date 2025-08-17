cmake_minimum_required(VERSION 3.20)
project(fortplot VERSION 2025.06.25 LANGUAGES Fortran C)

# Set Fortran compiler flags
set(CMAKE_Fortran_FLAGS "-Wall -Wextra -fimplicit-none")
set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0 -fcheck=all")
set(CMAKE_Fortran_FLAGS_RELEASE "-O3")

# Default to Release build if not specified
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
endif()

# Collect all source files
file(GLOB_RECURSE FORTPLOT_SOURCES 
    "src/*.f90"
    "src/*.c"
)

# Create the library target
add_library(fortplot STATIC ${FORTPLOT_SOURCES})

# Set library properties
target_include_directories(fortplot PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/src>
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
    $<INSTALL_INTERFACE:include>
)

# Set Fortran module directory
set_target_properties(fortplot PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules
)

# Make modules available to dependent targets
target_include_directories(fortplot PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/modules>
)

# Create namespaced alias target for FetchContent compatibility
add_library(fortplot::fortplot ALIAS fortplot)

# Enable export of targets
export(TARGETS fortplot 
       NAMESPACE fortplot::
       FILE "${CMAKE_CURRENT_BINARY_DIR}/fortplotTargets.cmake")

# Configure package config file (only when not used as subproject)
if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    include(CMakePackageConfigHelpers)
    configure_package_config_file(
        "${CMAKE_CURRENT_SOURCE_DIR}/cmake/fortplotConfig.cmake.in"
        "${CMAKE_CURRENT_BINARY_DIR}/fortplotConfig.cmake"
        INSTALL_DESTINATION lib/cmake/fortplot
    )

    # Generate the version file
    write_basic_package_version_file(
        "${CMAKE_CURRENT_BINARY_DIR}/fortplotConfigVersion.cmake"
        VERSION ${PROJECT_VERSION}
        COMPATIBILITY SameMajorVersion
    )
endif()

# Installation rules (only when not used as subproject)
if(CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    install(TARGETS fortplot
        EXPORT fortplotTargets
        LIBRARY DESTINATION lib
        ARCHIVE DESTINATION lib
        RUNTIME DESTINATION bin
        INCLUDES DESTINATION include
    )

    install(DIRECTORY src/ DESTINATION include
        FILES_MATCHING PATTERN "*.mod")

    install(EXPORT fortplotTargets
        FILE fortplotTargets.cmake
        NAMESPACE fortplot::
        DESTINATION lib/cmake/fortplot
    )

    install(FILES
        "${CMAKE_CURRENT_BINARY_DIR}/fortplotConfig.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/fortplotConfigVersion.cmake"
        DESTINATION lib/cmake/fortplot
    )
endif()

# Debug information
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
message(STATUS "Fortran compiler: ${CMAKE_Fortran_COMPILER}")
message(STATUS "Fortran flags: ${CMAKE_Fortran_FLAGS}")
message(STATUS "Found ${list_length} source files in fortplot library")