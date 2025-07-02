# ----------------------------------------------------------------------------
# PythonHelpers.cmake
# ----------------------------------------------------------------------------
# This module provides functions to generate Python extension modules from
# Fortran sources using f90wrap and f2py.


# ----------------------------------------------------------------------------
# add_python_wrapper
# ----------------------------------------------------------------------------
# Inputs:
#   PYTHON_MODULE - The name of the Python module to generate.
#   LIBRARY_TO_WRAP - The library target to wrap.
#   FILES_TO_WRAP - The source files to wrap. Needs not to include all library sources.
#
function(add_python_wrapper PYTHON_MODULE LIBRARY_TO_WRAP FILES_TO_WRAP)
    list(TRANSFORM FILES_TO_WRAP PREPEND "${CMAKE_SOURCE_DIR}/")

    message(STATUS "Python module name: ${PYTHON_MODULE}")
    message(STATUS "Library to wrap: ${LIBRARY_TO_WRAP}")
    message(STATUS "Files to wrap: ${FILES_TO_WRAP}")
    message(STATUS "Python binary output dir: ${CMAKE_CURRENT_BINARY_DIR}")

    find_python_with_numpy(Python_FOUND)
    if(NOT Python_FOUND)
        message(WARNING "Python with NumPy not found, skipping interface build.")
        return()
    endif()

    find_f90wrap(F90WRAP_FOUND)
    if (NOT F90WRAP_FOUND)
        message(WARNING "f90wrap not found, skipping interface build.")
        return()
    endif()

    get_f2py_include_dir(F2PY_INCLUDE_DIR)
    set(F2PY_F2CMAP "${CMAKE_CURRENT_SOURCE_DIR}/.f2py_f2cmap")

    get_f90wrap_wrappers(${PYTHON_MODULE} ${F2PY_F2CMAP} "${FILES_TO_WRAP}" F90WRAP_WRAPPERS)
    get_f2py_wrappers(${PYTHON_MODULE} ${F2PY_F2CMAP} "${F90WRAP_WRAPPERS}" F2PY_WRAPPERS)

    add_fortranobject_target(${F2PY_INCLUDE_DIR})
    add_extension_module_target(${PYTHON_MODULE} ${LIBRARY_TO_WRAP}
        ${F2PY_INCLUDE_DIR} "${F2PY_WRAPPERS}" "${F90WRAP_WRAPPERS}")
endfunction()


# ----------------------------------------------------------------------------
# find_python_with_numpy
# ----------------------------------------------------------------------------
# Inputs: None
# Ouputs:
#   Python_FOUND - True if Python and NumPy were found, False otherwise.
# Globals set:
#   Python_EXECUTABLE
#   Python_SOABI - The Python ABI tag, e.g. cpython-312-aarch64-linux-gnu
#   Python_INCLUDE_DIRS
#   Python_NumPy_INCLUDE_DIRS
#
function(find_python_with_numpy Python_FOUND)
    find_package(Python COMPONENTS Interpreter Development NumPy)

    set(Python_FOUND "${Python_FOUND}" PARENT_SCOPE)
    set(Python_EXECUTABLE "${Python_EXECUTABLE}" PARENT_SCOPE)
    set(Python_INCLUDE_DIRS "${Python_INCLUDE_DIRS}" PARENT_SCOPE)
    set(Python_NumPy_INCLUDE_DIRS "${Python_NumPy_INCLUDE_DIRS}" PARENT_SCOPE)
    set(Python_SOABI "${Python_SOABI}" PARENT_SCOPE)

    if (Python_FOUND)
        message(STATUS "Python libraries: ${Python_LIBRARIES}")
        message(STATUS "Python include dirs: ${Python_INCLUDE_DIRS}")
        message(STATUS "Python numpy include dirs: ${Python_NumPy_INCLUDE_DIRS}")
    endif()
endfunction()


# ----------------------------------------------------------------------------
# get_f2py_include_dir
# ----------------------------------------------------------------------------
# Globals used:
#   Python_EXECUTABLE
# Outputs:
#   F2PY_INCLUDE_DIR
#
function(get_f2py_include_dir F2PY_INCLUDE_DIR)
    execute_process(
        COMMAND "${Python_EXECUTABLE}"
        -c "import numpy.f2py; print(numpy.f2py.get_include())"
        OUTPUT_VARIABLE F2PY_INCLUDE_DIR
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    set(F2PY_INCLUDE_DIR "${F2PY_INCLUDE_DIR}" PARENT_SCOPE)
    message(STATUS "Python f2py include dir: ${F2PY_INCLUDE_DIR}")
endfunction()


# ----------------------------------------------------------------------------
# find_f90wrap
# ----------------------------------------------------------------------------
# Globals used:
#   Python_EXECUTABLE
# Outputs:
#   F90WRAP_FOUND - True if f90wrap was found, False otherwise.
#
function(find_f90wrap F90WRAP_FOUND)
    execute_process(
        COMMAND "${Python_EXECUTABLE}" -m "f90wrap"
        RESULT_VARIABLE F90WRAP_RESULT
        OUTPUT_QUIET
        ERROR_QUIET
    )
    if (F90WRAP_RESULT EQUAL 0)
        set(F90WRAP_FOUND TRUE PARENT_SCOPE)
    else()
        set(F90WRAP_FOUND FALSE PARENT_SCOPE)
    endif()
endfunction()


# ----------------------------------------------------------------------------
# get_f90wrap_wrappers
# ----------------------------------------------------------------------------
# Inputs:
#   FILES_TO_WRAP - The source files to wrap.
# Outputs:
#   F90WRAP_WRAPPERS - The wrapper source files.
#
function(get_f90wrap_wrappers PYTHON_MODULE F2PY_F2CMAP FILES_TO_WRAP F90WRAP_WRAPPERS)
    execute_process(
        COMMAND "${Python_EXECUTABLE}" -m f90wrap --f90wrap
            -m ${PYTHON_MODULE}
            -k ${F2PY_F2CMAP}
            ${FILES_TO_WRAP}
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            OUTPUT_FILE f90wrap.log
            ERROR_FILE f90wrap.err
    )
    file(GLOB F90WRAP_WRAPPERS "${CMAKE_CURRENT_BINARY_DIR}/f90wrap*.f*")
    set(F90WRAP_WRAPPERS ${F90WRAP_WRAPPERS} PARENT_SCOPE)
endfunction()


# ----------------------------------------------------------------------------
# get_f2py_wrappers
# ----------------------------------------------------------------------------
# Inputs:
#   PYTHON_MODULE
#   F2PY_F2CMAP.
#   F90WRAP_WRAPPERS - The f90wrappers wrapper source files.
# Outputs:
#   F2PY_WRAPPERS - The f2py wrapper source files.
#
function(get_f2py_wrappers PYTHON_MODULE F2PY_F2CMAP F90WRAP_WRAPPERS F2PY_WRAPPERS)
    set(F2PY_MODULE_NAME "_${PYTHON_MODULE}")
    set(F2PY_WRAPPERS "${CMAKE_CURRENT_BINARY_DIR}/_${PYTHON_MODULE}module.c")
    add_custom_command(OUTPUT ${F2PY_WRAPPERS}
        COMMAND ${Python_EXECUTABLE}
            -m f90wrap
            --f2py-f90wrap
            ${F90WRAP_WRAPPERS}
            -m ${F2PY_MODULE_NAME}
            --f2cmap ${F2PY_F2CMAP}
            --lower
            > ${CMAKE_CURRENT_BINARY_DIR}/f2py.log 2>&1
        DEPENDS ${F90WRAP_WRAPPERS}
        COMMENT "Processing wrapped sources with f2py"
    )
    set(F2PY_WRAPPERS "${F2PY_WRAPPERS}" PARENT_SCOPE)
endfunction()


# ----------------------------------------------------------------------------
# add_fortranobject_target
# ----------------------------------------------------------------------------
# Inputs:
#   F2PY_INCLUDE_DIR
# Globals used:
#   Python_INCLUDE_DIRS
#
function(add_fortranobject_target F2PY_INCLUDE_DIR)
    add_library(fortranobject OBJECT "${F2PY_INCLUDE_DIR}/fortranobject.c")
    target_link_libraries(fortranobject PUBLIC Python::NumPy)
    target_include_directories(fortranobject PUBLIC
        ${Python_INCLUDE_DIRS} ${F2PY_INCLUDE_DIR})
    set_property(TARGET fortranobject PROPERTY POSITION_INDEPENDENT_CODE ON)
endfunction()


# ----------------------------------------------------------------------------
# add_extension_module_target
# ----------------------------------------------------------------------------
# Inputs:
#   PYTHON_MODULE
#   LIBRARY_TO_WRAP
#   F2PY_INCLUDE_DIR
#   F2PY_WRAPPERS
#   F90WRAP_WRAPPERS
# Globals used:
#   Python_SOABI - The Python ABI tag, e.g. cpython-312-aarch64-linux-gnu
#   Python_INCLUDE_DIRS
#   Python_NumPy_INCLUDE_DIRS
#
function(add_extension_module_target PYTHON_MODULE LIBRARY_TO_WRAP
    F2PY_INCLUDE_DIR F2PY_WRAPPERS F90WRAP_WRAPPERS
)
    set(F2PY_MODULE_NAME "_${PYTHON_MODULE}")

    Python_add_library(${F2PY_MODULE_NAME} WITH_SOABI
        ${F2PY_WRAPPERS}
        ${F2PY_INCLUDE_DIR}/fortranobject.c
        ${F90WRAP_WRAPPERS}
    )
    add_dependencies(${F2PY_MODULE_NAME} ${LIBRARY_TO_WRAP})
    target_include_directories(${F2PY_MODULE_NAME} PUBLIC
        ${Python_INCLUDE_DIRS} ${Python_NumPy_INCLUDE_DIRS} ${F2PY_INCLUDE_DIR})
    target_link_libraries(${F2PY_MODULE_NAME} PUBLIC ${LIBRARY_TO_WRAP})

    set(GENERATED_MODULE_FILE "${F2PY_MODULE_NAME}.${Python_SOABI}")

    set_target_properties(
        ${F2PY_MODULE_NAME}
        PROPERTIES
        PREFIX ""
        OUTPUT_NAME "${GENERATED_MODULE_FILE}"
        SUFFIX "${CMAKE_SHARED_LIBRARY_SUFFIX}"
        INSTALL_RPATH "$ORIGIN/lib"
        BUILD_WITH_INSTALL_RPATH TRUE
        LINKER_LANGUAGE C
    )

    install(TARGETS ${F2PY_MODULE_NAME} DESTINATION .)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PYTHON_MODULE}.py DESTINATION .)
endfunction()
