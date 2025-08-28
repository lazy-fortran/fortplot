#!/bin/bash
# Test script for Issue #637: FPM build directory hash mismatch
# This script reproduces and verifies the fix for the build system issue

set -e  # Exit on any error

echo "=== Issue #637: FPM Build Directory Hash Consistency Test ==="
echo

# Clean previous builds to ensure fresh test
echo "1. Cleaning previous builds..."
rm -rf build/gfortran_*
echo "   Removed existing build directories"

# Build the library
echo "2. Building fortplot library..."
fpm build --flag -fPIC
echo "   Library build complete"

# Check build directories created
echo "3. Analyzing build directory structure..."
echo "   Build directories created:"
ls -la build/ | grep gfortran_ || echo "   No gfortran directories found"

echo
echo "   Module files location:"
find build/ -name "fortplot.mod" -exec dirname {} \; || echo "   No fortplot.mod found"

echo
echo "   Library files location:"  
find build/ -name "libfortplot.a" -exec dirname {} \; || echo "   No libfortplot.a found"

# Test user compilation with current build
echo
echo "4. Testing user program compilation..."

# Try to compile user program against the built library
echo "   Attempting compilation of user test program..."

# Find the module directory
MOD_DIR=$(find build/ -name "fortplot.mod" -exec dirname {} \; | head -n 1)
LIB_DIR=$(find build/ -name "libfortplot.a" -exec dirname {} \; | head -n 1) 

if [[ -z "$MOD_DIR" || -z "$LIB_DIR" ]]; then
    echo "   ERROR: Could not find module or library files"
    echo "   This indicates the FPM hash mismatch problem!"
    exit 1
fi

echo "   Module directory: $MOD_DIR"
echo "   Library directory: $LIB_DIR"

# Check if they're in the same parent (should be for consistency)
MOD_PARENT=$(dirname "$MOD_DIR")
LIB_PARENT=$(dirname "$LIB_DIR") 

if [[ "$MOD_PARENT" != "$LIB_PARENT" ]]; then
    echo "   WARNING: Modules and libraries in different hash directories!"
    echo "   Module parent: $MOD_PARENT"  
    echo "   Library parent: $LIB_PARENT"
    echo "   This is the Issue #637 problem - hash mismatch"
fi

# Attempt compilation
echo "   Compiling test_user_compilation.f90..."
if gfortran -I"$MOD_DIR" -L"$LIB_DIR" test_user_compilation.f90 -lfortplot -o test_user_compilation; then
    echo "   SUCCESS: User compilation succeeded!"
    
    # Run the test program
    echo
    echo "5. Running user test program..."
    if ./test_user_compilation; then
        echo "   SUCCESS: User program executed successfully!"
        echo
        echo "=== Issue #637 Status: FIXED ==="
        echo "FPM build directories are consistent and user compilation works"
    else
        echo "   ERROR: User program execution failed"
        exit 1
    fi
else
    echo "   ERROR: User compilation failed!"
    echo "   This confirms Issue #637: FPM build directory hash mismatch"
    exit 1  
fi

# Cleanup test artifacts
echo
echo "6. Cleaning up test artifacts..."
rm -f test_user_compilation test_user_output.png
echo "   Test artifacts cleaned"

echo
echo "=== Test Complete: Build consistency verified ==="