#!/bin/bash
# Ensure output directories exist and run examples via fpm.
#
# Earlier versions compiled each example manually with gfortran, but that
# caused segfaults on GCC 13 due to ABI mismatches with fpm-built modules
# (see #1598, docs workflow run 24139459214). Since every example is now
# fpm-discoverable, running them through fpm avoids the issue entirely.

set -e

echo "Running examples via fpm..."

for example_file in $(find example/fortran -name "*.f90" -type f -exec grep -l "^program " {} \; | sort); do
    example_dir=$(dirname "$example_file")
    example_name=$(basename "$example_file" .f90)

    if [ -f "$example_dir/$example_name.f90" ]; then
        output_dir="output/example/fortran/$example_name"
    else
        parent_name=$(basename "$example_dir")
        output_dir="output/example/fortran/$parent_name"
    fi

    mkdir -p "$output_dir"
    echo "  $example_name"
    fpm run --example "$example_name"
done

echo "Special examples built successfully!"
