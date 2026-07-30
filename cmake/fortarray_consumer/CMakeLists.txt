cmake_minimum_required(VERSION 3.20)
project(fortplot_fortarray_consumer LANGUAGES Fortran)

find_package(fortplot CONFIG REQUIRED)
add_executable(fortplot_fortarray_consumer main.f90)
target_link_libraries(fortplot_fortarray_consumer PRIVATE fortplot::fortarray)
