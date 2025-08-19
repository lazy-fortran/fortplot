#!/bin/bash
# Given: Documentation build process creates artifacts
# When: Documentation is built with FORD and media files
# Then: All required structure and files must be present for GitHub Pages

set -e

echo "=== Documentation Structure Verification ==="
echo "Checking local documentation build structure for GitHub Pages deployment"

TEST_FAILURES=0

# Test 1: Verify example outputs exist
echo ""
echo "1. Testing example output generation..."

EXPECTED_OUTPUTS=(
    "output/example/fortran/basic_plots/simple_plot.png"
    "output/example/fortran/basic_plots/multi_line.png"
    "output/example/fortran/line_styles/line_styles.png"
    "output/example/fortran/marker_demo/all_marker_types.png"
    "output/example/fortran/contour_demo/contour_gaussian.png"
    "output/example/fortran/colored_contours/gaussian_default.png"
    "output/example/fortran/pcolormesh_demo/pcolormesh_basic.png"
    "output/example/fortran/streamplot_demo/streamplot_demo.png"
    "output/example/fortran/legend_demo/basic_legend.png"
    "output/example/fortran/unicode_demo/unicode_demo.png"
)

for output_file in "${EXPECTED_OUTPUTS[@]}"; do
    if [ -f "$output_file" ]; then
        echo "  ✓ $output_file exists"
    else
        echo "  ✗ $output_file missing - run 'make example' first"
        ((TEST_FAILURES++))
    fi
done

# Test 2: Verify documentation media directory structure
echo ""
echo "2. Testing documentation media directory structure..."

REQUIRED_DIRS=(
    "doc/media"
    "doc/media/examples"
)

for dir in "${REQUIRED_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        echo "  ✓ $dir exists"
    else
        echo "  ✗ $dir missing"
        ((TEST_FAILURES++))
    fi
done

# Test 3: Verify documentation markdown files exist
echo ""
echo "3. Testing documentation markdown files..."

REQUIRED_DOCS=(
    "doc/example/basic_plots.md"
    "doc/example/line_styles.md"
    "doc/example/marker_demo.md"
    "doc/example/contour_demo.md"
    "doc/example/colored_contours.md"
    "doc/example/pcolormesh_demo.md"
    "doc/example/streamplot_demo.md"
    "doc/example/legend_demo.md"
    "doc/example/unicode_demo.md"
)

for doc_file in "${REQUIRED_DOCS[@]}"; do
    if [ -f "$doc_file" ]; then
        echo "  ✓ $doc_file exists"
    else
        echo "  ✗ $doc_file missing"
        ((TEST_FAILURES++))
    fi
done

# Test 4: Verify FORD build directory structure
echo ""
echo "4. Testing FORD build directory structure..."

if [ -d "build/doc" ]; then
    echo "  ✓ build/doc exists"
    
    REQUIRED_BUILD_DIRS=(
        "build/doc/page"
        "build/doc/media"
        "build/doc/media/examples"
    )
    
    for build_dir in "${REQUIRED_BUILD_DIRS[@]}"; do
        if [ -d "$build_dir" ]; then
            echo "  ✓ $build_dir exists"
        else
            echo "  ✗ $build_dir missing - run 'make doc' first"
            ((TEST_FAILURES++))
        fi
    done
else
    echo "  ✗ build/doc missing - run 'make doc' first"
    ((TEST_FAILURES++))
fi

# Test 5: Verify CI workflow configuration
echo ""
echo "5. Testing CI workflow configuration..."

if [ -f ".github/workflows/docs.yml" ]; then
    echo "  ✓ GitHub Actions docs workflow exists"
    
    # Check for required workflow steps
    if grep -q "make example" .github/workflows/docs.yml; then
        echo "  ✓ Workflow includes example generation step"
    else
        echo "  ✗ Workflow missing example generation step"
        ((TEST_FAILURES++))
    fi
    
    if grep -q "make doc" .github/workflows/docs.yml; then
        echo "  ✓ Workflow includes documentation build step"
    else
        echo "  ✗ Workflow missing documentation build step"  
        ((TEST_FAILURES++))
    fi
    
    if grep -q "find.*\\.png.*cp" .github/workflows/docs.yml; then
        echo "  ✓ Workflow includes PNG file copying"
    else
        echo "  ✗ Workflow missing PNG file copying step"
        ((TEST_FAILURES++))
    fi
    
else
    echo "  ✗ GitHub Actions docs workflow missing"
    ((TEST_FAILURES++))
fi

# Test 6: Check for image references in documentation
echo ""
echo "6. Testing image references in documentation..."

# This is a critical test that MUST FAIL until documentation integration is fixed
echo "  ✗ Image reference validation not implemented"
((TEST_FAILURES++))

# Test 7: Verify GitHub Pages artifact structure
echo ""
echo "7. Testing GitHub Pages artifact structure..."

# This test MUST FAIL until GitHub Pages deployment is verified to work
echo "  ✗ GitHub Pages artifact structure validation not implemented"
((TEST_FAILURES++))

# Summary
echo ""
echo "=== Documentation Structure Verification Summary ==="
echo "Total failures: $TEST_FAILURES"

if [ $TEST_FAILURES -eq 0 ]; then
    echo "✓ All documentation structure tests PASSED"
    echo "Documentation structure is ready for GitHub Pages deployment"
    exit 0
else
    echo "✗ Documentation structure tests FAILED"
    echo "Issues found that prevent proper GitHub Pages deployment"
    echo "This confirms the regression reported in Issue #117"
    exit 1
fi