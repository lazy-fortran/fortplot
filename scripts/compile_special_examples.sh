#!/bin/bash
# Build special examples that require manual compilation

# Exit on error
set -e

echo "Building special examples..."

# Find the build directory with module files
BUILD_DIR=$(find build -name "fortplot.mod" -type f 2>/dev/null | head -1 | xargs dirname)
LIB_DIR=$(find build -name "libfortplot.a" -type f 2>/dev/null | head -1 | xargs dirname)

if [ -z "$BUILD_DIR" ] || [ -z "$LIB_DIR" ]; then
    echo "Error: fortplot library not built. Run 'make build' first."
    exit 1
fi

# Build animation example
echo "Building animation example..."
mkdir -p output/example/fortran/animation
gfortran -I "$BUILD_DIR" -o save_animation_demo_temp \
    example/fortran/animation/save_animation_demo.f90 \
    "$LIB_DIR/libfortplot.a" -lm

# Run animation example to generate MP4
echo "Generating animation..."
./save_animation_demo_temp
rm -f save_animation_demo_temp

echo "Special examples built successfully!"