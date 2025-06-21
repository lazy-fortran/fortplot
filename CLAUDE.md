# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a Fortran project that generates high-quality PNG images from scratch using the PNG file format specification. The project implements a pure Fortran PNG generator with advanced text rendering capabilities, creating binary PNG files with proper chunk structure, CRC32 checksums, and zlib compression.

## Development Commands

### Build and Run (Recommended)
- `make build` - Compile the project with automatic library detection
- `make run` - Build and execute the main program (generates 640x480 output.png)
- `make test` - Run comprehensive test suite including FreeType tests
- `make clean` - Clean build artifacts
- `make check-deps` - Show detected library flags and paths
- `make help` - Show all available targets

### Manual FMP Commands (Alternative)
- `fpm build --c-flag "-I/path/to/freetype" --link-flag "-L/path/to/libs -lfreetype -lz"`
- `fpm run --c-flag "-I/path/to/freetype" --link-flag "-L/path/to/libs -lfreetype -lz"`
- `fpm test --c-flag "-I/path/to/freetype" --link-flag "-L/path/to/libs -lfreetype -lz"`

## Architecture

### Core Components
- **app/main.f90**: Main PNG generator program (640x480 resolution)
- **src/png_module.f90**: PNG file format implementation with chunk structure
- **src/text_module.f90**: Clean text rendering interface using C wrapper
- **src/freetype_wrapper.c**: Safe C wrapper for FreeType operations
- **src/freetype_wrapper.h**: C interface definitions
- **src/png_context_module.f90**: High-level plotting and text interface
- **src/plotting_module.f90**: Coordinate system and plotting utilities

### Text Rendering System
- **Safe C Wrapper**: Uses `freetype_wrapper.c` to isolate FreeType complexity from Fortran
- **Proper Alpha Blending**: Textbook-perfect antialiasing using `background * (1-alpha)` formula
- **System Font Detection**: Automatically finds fonts on macOS, Linux, and Windows
- **High Quality**: Smooth, professional text rendering at any size

### Key Features
- **C Interoperability**: Uses `iso_c_binding` for zlib and FreeType integration
- **Binary File I/O**: Uses stream access for writing binary PNG data
- **Chunk-based Architecture**: Implements PNG chunk structure with proper length, type, data, and CRC fields
- **Memory Management**: Proper allocation/deallocation with C wrapper safety
- **Cross-Platform**: Works on macOS, Linux, and Windows with pkg-config

### Dependencies
- **zlib**: System zlib library for PNG compression
- **FreeType**: System FreeType library for text rendering
- **pkg-config**: For automatic library detection (recommended)
- **Fortran 2008**: Uses modern Fortran features including C interoperability

### Data Handling
- Uses `integer(1)` for byte-level data manipulation
- Handles signed/unsigned byte conversion for PNG format compatibility
- Implements big-endian byte order conversion for PNG compliance
- Safe alpha blending for smooth text antialiasing

### Build Configuration
- **Makefile**: Portable build system using pkg-config for dynamic library detection
- **fpm.toml**: Fortran Package Manager configuration with C source compilation
- **Auto-detection**: Automatically detects and compiles C wrapper alongside Fortran code
- **Library linking**: Links against system zlib and FreeType libraries