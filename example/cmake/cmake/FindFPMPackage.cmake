# FindFPMPackage.cmake
# Generic CMake module to fetch and build FPM packages
#
# Usage:
#   find_fpm_package(
#     NAME <package_name>
#     GIT_REPOSITORY <git_url>
#     GIT_TAG <tag_or_branch>
#     [C_FLAGS <c_flags>]
#     [LINK_FLAGS <link_flags>]
#     [PKG_CONFIG_PACKAGES <pkg1> <pkg2> ...]
#   )
#
# This creates an IMPORTED target named <package_name>::<package_name>

include(FindPackageHandleStandardArgs)
include(ExternalProject)

function(find_fpm_package)
    set(options "")
    set(oneValueArgs NAME GIT_REPOSITORY GIT_TAG)
    set(multiValueArgs C_FLAGS LINK_FLAGS PKG_CONFIG_PACKAGES)
    
    cmake_parse_arguments(FPM "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
    
    if(NOT FPM_NAME)
        message(FATAL_ERROR "find_fmp_package: NAME is required")
    endif()
    
    if(NOT FPM_GIT_REPOSITORY)
        message(FATAL_ERROR "find_fpm_package: GIT_REPOSITORY is required")
    endif()
    
    if(NOT FPM_GIT_TAG)
        set(FPM_GIT_TAG "main")
    endif()
    
    # Find FPM executable
    find_program(FPM_EXECUTABLE fpm)
    if(NOT FPM_EXECUTABLE)
        message(FATAL_ERROR "FPM executable not found. Please install fpm.")
    endif()
    
    # Handle pkg-config dependencies
    set(PKG_CONFIG_CFLAGS "")
    set(PKG_CONFIG_LIBS "")
    
    if(FPM_PKG_CONFIG_PACKAGES)
        find_package(PkgConfig REQUIRED)
        
        foreach(pkg ${FPM_PKG_CONFIG_PACKAGES})
            pkg_check_modules(${pkg}_PC REQUIRED ${pkg})
            list(APPEND PKG_CONFIG_CFLAGS ${${pkg}_PC_CFLAGS})
            list(APPEND PKG_CONFIG_LIBS ${${pkg}_PC_LDFLAGS})
        endforeach()
        
        # Convert lists to strings
        string(REPLACE ";" " " PKG_CONFIG_CFLAGS "${PKG_CONFIG_CFLAGS}")
        string(REPLACE ";" " " PKG_CONFIG_LIBS "${PKG_CONFIG_LIBS}")
    endif()
    
    # Combine user-provided flags with pkg-config flags
    set(ALL_C_FLAGS "${FPM_C_FLAGS} ${PKG_CONFIG_CFLAGS}")
    set(ALL_LINK_FLAGS "${FPM_LINK_FLAGS} ${PKG_CONFIG_LIBS}")
    
    # Clean up extra spaces
    string(STRIP "${ALL_C_FLAGS}" ALL_C_FLAGS)
    string(STRIP "${ALL_LINK_FLAGS}" ALL_LINK_FLAGS)
    
    # Build FPM command line arguments
    set(FPM_BUILD_ARGS "")
    
    if(ALL_C_FLAGS)
        list(APPEND FPM_BUILD_ARGS --c-flag "${ALL_C_FLAGS}")
    endif()
    
    if(ALL_LINK_FLAGS)
        list(APPEND FPM_BUILD_ARGS --link-flag "${ALL_LINK_FLAGS}")
    endif()
    
    # Set up external project
    set(EXTERNAL_PROJECT_NAME ${FPM_NAME}_external)
    set(INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/${FPM_NAME}_install)
    
    ExternalProject_Add(${EXTERNAL_PROJECT_NAME}
        GIT_REPOSITORY ${FPM_GIT_REPOSITORY}
        GIT_TAG ${FPM_GIT_TAG}
        PREFIX ${CMAKE_CURRENT_BINARY_DIR}/${FPM_NAME}
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ${FPM_EXECUTABLE} build ${FPM_BUILD_ARGS} --verbose
        INSTALL_COMMAND ""
        BUILD_IN_SOURCE ON
        LOG_DOWNLOAD ON
        LOG_BUILD ON
    )
    
    # Get source directory for paths
    ExternalProject_Get_Property(${EXTERNAL_PROJECT_NAME} source_dir)
    
    # Create imported target
    add_library(${FPM_NAME}::${FPM_NAME} STATIC IMPORTED GLOBAL)
    add_dependencies(${FPM_NAME}::${FPM_NAME} ${EXTERNAL_PROJECT_NAME})
    
    # Create a build-time script to find FPM build directory and create a wrapper library
    set(WRAPPER_LIB_DIR ${CMAKE_CURRENT_BINARY_DIR}/${FPM_NAME}_wrapper)
    set(WRAPPER_SCRIPT ${WRAPPER_LIB_DIR}/create_wrapper.cmake)
    
    file(MAKE_DIRECTORY ${WRAPPER_LIB_DIR})
    
    # Script to find the actual FPM build directory and create symlinks
    file(WRITE ${WRAPPER_SCRIPT} "
        # Find all FPM build directories
        file(GLOB BUILD_DIRS \"${source_dir}/build/gfortran_*\")
        if(NOT BUILD_DIRS)
            message(FATAL_ERROR \"No FPM build directory found in ${source_dir}/build/\")
        endif()
        
        # Find the directory with the library (check multiple possible locations)
        set(LIB_PATH \"\")
        foreach(BUILD_DIR \${BUILD_DIRS})
            # Check standard lib directory
            if(EXISTS \"\${BUILD_DIR}/lib/lib${FPM_NAME}.a\")
                set(LIB_PATH \"\${BUILD_DIR}/lib/lib${FPM_NAME}.a\")
                break()
            endif()
            # Check project-named subdirectory (common FPM pattern)
            if(EXISTS \"\${BUILD_DIR}/${FPM_NAME}/lib${FPM_NAME}.a\")
                set(LIB_PATH \"\${BUILD_DIR}/${FPM_NAME}/lib${FPM_NAME}.a\")
                break()
            endif()
        endforeach()
        
        if(NOT LIB_PATH)
            message(FATAL_ERROR \"Library lib${FPM_NAME}.a not found in any build directory\")
        endif()
        
        # Find the directory with module files
        set(MOD_DIR \"\")
        foreach(BUILD_DIR \${BUILD_DIRS})
            file(GLOB MOD_FILES \"\${BUILD_DIR}/*.mod\")
            if(MOD_FILES)
                set(MOD_DIR \${BUILD_DIR})
                break()
            endif()
        endforeach()
        
        if(NOT MOD_DIR)
            message(FATAL_ERROR \"Module files not found in any build directory\")
        endif()
        
        # Create symlink to the library
        execute_process(
            COMMAND \${CMAKE_COMMAND} -E remove -f \"${WRAPPER_LIB_DIR}/lib${FPM_NAME}.a\"
            COMMAND \${CMAKE_COMMAND} -E create_symlink 
                    \"\${LIB_PATH}\" 
                    \"${WRAPPER_LIB_DIR}/lib${FPM_NAME}.a\"
        )
        
        # Create symlinks to module files
        file(GLOB MOD_FILES \"\${MOD_DIR}/*.mod\")
        foreach(MOD_FILE \${MOD_FILES})
            get_filename_component(MOD_NAME \${MOD_FILE} NAME)
            execute_process(
                COMMAND \${CMAKE_COMMAND} -E remove -f \"${WRAPPER_LIB_DIR}/\${MOD_NAME}\"
                COMMAND \${CMAKE_COMMAND} -E create_symlink 
                        \"\${MOD_FILE}\" 
                        \"${WRAPPER_LIB_DIR}/\${MOD_NAME}\"
            )
        endforeach()
        
        list(LENGTH MOD_FILES MOD_COUNT)
        message(STATUS \"FPM wrapper created:\")
        message(STATUS \"  Library: \${LIB_PATH}\")
        message(STATUS \"  Modules: \${MOD_DIR} (\${MOD_COUNT} files)\")
    ")
    
    # Add custom command to run the wrapper script after FPM build
    add_custom_command(TARGET ${EXTERNAL_PROJECT_NAME} POST_BUILD
        COMMAND ${CMAKE_COMMAND} -P ${WRAPPER_SCRIPT}
        COMMENT "Creating FPM wrapper for ${FPM_NAME}"
        BYPRODUCTS ${WRAPPER_LIB_DIR}/lib${FPM_NAME}.a
    )
    
    # Set target properties to use the wrapper directory
    set_target_properties(${FPM_NAME}::${FPM_NAME} PROPERTIES
        IMPORTED_LOCATION ${WRAPPER_LIB_DIR}/lib${FPM_NAME}.a
        INTERFACE_INCLUDE_DIRECTORIES ${WRAPPER_LIB_DIR}
    )
    
    # Add system libraries if needed
    if(ALL_LINK_FLAGS)
        set_target_properties(${FPM_NAME}::${FPM_NAME} PROPERTIES
            INTERFACE_LINK_LIBRARIES "${ALL_LINK_FLAGS}"
        )
    endif()
    
    message(STATUS "FMP package ${FPM_NAME} configured:")
    message(STATUS "  Repository: ${FPM_GIT_REPOSITORY}")
    message(STATUS "  Tag: ${FPM_GIT_TAG}")
    message(STATUS "  C Flags: ${ALL_C_FLAGS}")
    message(STATUS "  Link Flags: ${ALL_LINK_FLAGS}")
    
endfunction()