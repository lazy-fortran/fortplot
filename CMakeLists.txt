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

option(FORTPLOT_BUILD_FULL "Build the monolithic fortplot target" ON)
option(FORTPLOT_BUILD_COMPONENTS "Build component targets for package-style consumption" OFF)
option(FORTPLOT_BUILD_CORE_COMPONENT "Build fortplot::core" ON)
option(FORTPLOT_BUILD_TEXT_COMPONENT "Build fortplot::text" ON)
option(FORTPLOT_BUILD_ZLIB_COMPONENT "Build fortplot::zlib" ON)
option(FORTPLOT_BUILD_RASTER_COMPONENT "Build fortplot::raster" ON)
option(FORTPLOT_BUILD_VECTOR_COMPONENT "Build fortplot::vector" ON)
option(FORTPLOT_BUILD_ASCII_COMPONENT "Build fortplot::ascii" ON)

# Find all source files automatically for the full target
file(GLOB_RECURSE FORTRAN_SOURCES "src/*.f90")
file(GLOB_RECURSE C_SOURCES "src/*.c")

set(FORTPLOT_CORE_SOURCES
    src/core/fortplot_constants.f90
    src/core/fortplot_context.f90
    src/core/fortplot_errors.f90
    src/core/fortplot_layout.f90
    src/core/fortplot_margins.f90
    src/core/fortplot_projection.f90
    src/external/fortplot_logging.f90
    src/plotting/fortplot_3d_axes.f90
    src/plotting/fortplot_interpolation.f90
    src/plotting/fortplot_pcolormesh.f90
    src/system/fortplot_system_viewer.f90
    src/testing/fortplot_plot_data.f90
    src/utilities/core/fortplot_string_utils.f90
    src/utilities/colors/fortplot_color_conversions.f90
    src/utilities/colors/fortplot_color_definitions.f90
    src/utilities/colors/fortplot_color_parsing.f90
    src/utilities/colors/fortplot_colormap.f90
    src/utilities/colors/fortplot_colors.f90
    src/utilities/fortplot_datetime.f90
    src/utilities/fortplot_line_styles.f90
    src/utilities/fortplot_markers.f90
    src/utilities/fortplot_polar.f90
    src/utilities/fortplot_scales.f90
    src/utilities/fortplot_utils_sort.f90
    src/utilities/fortplot_vector.f90
    src/utilities/text/fortplot_format_parser.f90
    src/utilities/text/fortplot_latex_parser.f90
    src/utilities/text/fortplot_unicode.f90
    src/utilities/ticks/fortplot_contour_level_calculation.f90
    src/utilities/ticks/fortplot_tick_calculation.f90
    src/utilities/ticks/fortplot_tick_formatting.f90
    src/utilities/ticks/fortplot_tick_scales.f90
    src/utilities/ticks/fortplot_ticks.f90
    src/ui/fortplot_axes.f90
)

set(FORTPLOT_TEXT_SOURCES
    src/text/fortplot_annotation_coordinates.f90
    src/text/fortplot_annotation_layout.f90
    src/text/fortplot_annotation_types.f90
    src/text/fortplot_annotations.f90
    src/text/fortplot_label_positioning.f90
    src/text/fortplot_mathtext.f90
    src/text/fortplot_truetype.f90
    src/text/fortplot_tt_binary.f90
    src/text/fortplot_tt_cmap.f90
    src/text/fortplot_tt_curves.f90
    src/text/fortplot_tt_metrics.f90
    src/text/fortplot_tt_outlines.f90
    src/text/fortplot_tt_rasterizer.f90
    src/text/fortplot_tt_tables.f90
    src/text/fortplot_text.f90
    src/text/fortplot_text_fonts.f90
    src/text/fortplot_text_helpers.f90
    src/text/fortplot_text_layout.f90
    src/ui/fortplot_legend.f90
    src/ui/fortplot_legend_layout.f90
)

set(FORTPLOT_ZLIB_SOURCES
    src/external/fortplot_zlib_core.f90
)

file(GLOB FORTPLOT_RASTER_DIR_SOURCES CONFIGURE_DEPENDS "src/backends/raster/*.f90")
set(FORTPLOT_RASTER_SOURCES
    ${FORTPLOT_RASTER_DIR_SOURCES}
    src/text/fortplot_text_rendering.f90
)

file(GLOB FORTPLOT_VECTOR_SOURCES CONFIGURE_DEPENDS "src/backends/vector/*.f90")
file(GLOB FORTPLOT_ASCII_BACKEND_SOURCES CONFIGURE_DEPENDS "src/backends/ascii/*.f90")
set(FORTPLOT_ASCII_SOURCES
    ${FORTPLOT_ASCII_BACKEND_SOURCES}
)

# Debug output
list(LENGTH FORTRAN_SOURCES fortran_count)
list(LENGTH C_SOURCES c_count)
message(STATUS "Found ${fortran_count} Fortran source files")
message(STATUS "Found ${c_count} C source files")

function(fortplot_apply_common_settings target module_subdir)
    set(mod_dir "${CMAKE_BINARY_DIR}/modules/${module_subdir}")
    set_target_properties(${target} PROPERTIES
        Fortran_MODULE_DIRECTORY "${mod_dir}"
        POSITION_INDEPENDENT_CODE ON
    )
    target_include_directories(${target}
        PUBLIC
            $<BUILD_INTERFACE:${mod_dir}>
            $<INSTALL_INTERFACE:include/${module_subdir}>
    )
    if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        target_compile_options(${target} PRIVATE
            $<$<COMPILE_LANGUAGE:Fortran>:-Wtrampolines>
            $<$<COMPILE_LANGUAGE:Fortran>:-Werror=trampolines>
        )
        if(NOT APPLE)
            target_link_options(${target} PRIVATE -Wl,-z,noexecstack)
        endif()
    endif()
endfunction()

if(FORTPLOT_BUILD_FULL)
    add_library(fortplot ${FORTRAN_SOURCES} ${C_SOURCES})
    fortplot_apply_common_settings(fortplot fortplot)
    add_library(fortplot::fortplot ALIAS fortplot)
endif()

if(FORTPLOT_BUILD_COMPONENTS)
    if(FORTPLOT_BUILD_CORE_COMPONENT)
        add_library(fortplot_core ${FORTPLOT_CORE_SOURCES})
        fortplot_apply_common_settings(fortplot_core core)
        add_library(fortplot::core ALIAS fortplot_core)
    endif()

    if(FORTPLOT_BUILD_TEXT_COMPONENT)
        add_library(fortplot_text_component ${FORTPLOT_TEXT_SOURCES})
        fortplot_apply_common_settings(fortplot_text_component text)
        target_link_libraries(fortplot_text_component PUBLIC fortplot_core)
        add_library(fortplot::text ALIAS fortplot_text_component)
    endif()

    if(FORTPLOT_BUILD_ZLIB_COMPONENT)
        add_library(fortplot_zlib ${FORTPLOT_ZLIB_SOURCES})
        fortplot_apply_common_settings(fortplot_zlib zlib)
        target_link_libraries(fortplot_zlib PUBLIC fortplot_core)
        add_library(fortplot::zlib ALIAS fortplot_zlib)
    endif()

    if(FORTPLOT_BUILD_RASTER_COMPONENT)
        add_library(fortplot_raster_component ${FORTPLOT_RASTER_SOURCES})
        fortplot_apply_common_settings(fortplot_raster_component raster)
        target_link_libraries(fortplot_raster_component PUBLIC fortplot_core fortplot_text_component fortplot_zlib)
        add_library(fortplot::raster ALIAS fortplot_raster_component)
    endif()

    if(FORTPLOT_BUILD_VECTOR_COMPONENT)
        add_library(fortplot_vector_component ${FORTPLOT_VECTOR_SOURCES})
        fortplot_apply_common_settings(fortplot_vector_component vector)
        target_link_libraries(fortplot_vector_component PUBLIC fortplot_core fortplot_text_component fortplot_zlib)
        add_library(fortplot::vector ALIAS fortplot_vector_component)
    endif()

    if(FORTPLOT_BUILD_ASCII_COMPONENT)
        add_library(fortplot_ascii_component ${FORTPLOT_ASCII_SOURCES})
        fortplot_apply_common_settings(fortplot_ascii_component ascii)
        target_link_libraries(fortplot_ascii_component PUBLIC fortplot_core fortplot_text_component)
        add_library(fortplot::ascii ALIAS fortplot_ascii_component)
    endif()
endif()

# Export configuration for find_package() support
if(NOT CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    # This is a subproject - don't install
    message(STATUS "fortplot configured as subproject")
else()
    # This is the main project - configure install
    include(GNUInstallDirs)

    set(FORTPLOT_INSTALL_TARGETS)
    if(FORTPLOT_BUILD_FULL)
        list(APPEND FORTPLOT_INSTALL_TARGETS fortplot)
    endif()
    if(FORTPLOT_BUILD_COMPONENTS)
        if(FORTPLOT_BUILD_CORE_COMPONENT)
            list(APPEND FORTPLOT_INSTALL_TARGETS fortplot_core)
        endif()
        if(FORTPLOT_BUILD_TEXT_COMPONENT)
            list(APPEND FORTPLOT_INSTALL_TARGETS fortplot_text_component)
        endif()
        if(FORTPLOT_BUILD_ZLIB_COMPONENT)
            list(APPEND FORTPLOT_INSTALL_TARGETS fortplot_zlib)
        endif()
        if(FORTPLOT_BUILD_RASTER_COMPONENT)
            list(APPEND FORTPLOT_INSTALL_TARGETS fortplot_raster_component)
        endif()
        if(FORTPLOT_BUILD_VECTOR_COMPONENT)
            list(APPEND FORTPLOT_INSTALL_TARGETS fortplot_vector_component)
        endif()
        if(FORTPLOT_BUILD_ASCII_COMPONENT)
            list(APPEND FORTPLOT_INSTALL_TARGETS fortplot_ascii_component)
        endif()
    endif()

    if(FORTPLOT_INSTALL_TARGETS)
        install(TARGETS ${FORTPLOT_INSTALL_TARGETS}
            EXPORT fortplotTargets
            LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
            ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
            RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
        )
    endif()

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
