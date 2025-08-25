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

# Function to compile and run an example
compile_and_run_example() {
    local source_file="$1"
    local example_name="$2"
    local output_dir="$3"
    
    echo "Building $example_name..."
    mkdir -p "$output_dir"
    
    # Compile
    gfortran -I "$BUILD_DIR" -o "${example_name}_temp" \
        "$source_file" \
        "$LIB_DIR/libfortplot.a" -lm
    
    # Run
    echo "Running $example_name..."
    ./"${example_name}_temp"
    
    # Clean up
    rm -f "${example_name}_temp"
}

# Build and run all examples with program statements
echo "Searching for example programs..."

# Find all Fortran example files with program statements
for example_file in $(find example/fortran -name "*.f90" -type f -exec grep -l "^program " {} \; | sort); do
    # Get the directory and name
    example_dir=$(dirname "$example_file")
    example_name=$(basename "$example_file" .f90)
    
    # Determine output directory
    if [ -f "$example_dir/$example_name.f90" ]; then
        # Example is in its own directory
        output_dir="output/example/fortran/$example_name"
    else
        # Example is in parent directory
        parent_name=$(basename "$example_dir")
        output_dir="output/example/fortran/$parent_name"
    fi
    
    # Compile and run
    compile_and_run_example "$example_file" "$example_name" "$output_dir"
done

echo "Special examples built successfully!"