project: fortplotlib
summary: Modern Fortran plotting library with multiple backends
author: Plasma Theory Group, TU Graz
author_description: Computational plasma physics research group
github: https://github.com/krystophny/fortplotlib
project_github: https://github.com/krystophny/fortplotlib
project_download: https://github.com/krystophny/fortplotlib/releases
output_dir: ./build/doc
media_dir: ./doc/media
page_dir: ./doc
          ./build/example
src_dir: ./src
         ./example
exclude_dir: ./thirdparty
             ./build
             ./test
             ./app
preprocessor: cpp -E
macro: USE_FORD
display: public
         protected
         private
source: true
graph: true
search: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html

Welcome to the fortplotlib documentation. This library provides a modern, matplotlib-inspired plotting interface for Fortran with multiple backends including PNG, PDF, and ASCII.

## Features

- Multiple backends: PNG (via STB), PDF (native), ASCII (terminal)
- Matplotlib-compatible API design
- No external dependencies
- Pure Fortran implementation with C interop for image writing
- Support for various plot types: line plots, scatter plots, contours, heatmaps
- Unicode support for mathematical symbols
- Animation support

## Navigation

- [API Documentation](./page/index.html) - Detailed module and procedure documentation
- [Examples Gallery](./page/examples/index.html) - Visual examples with code
- [Getting Started](./page/fpm_example/index.html) - Quick start guide