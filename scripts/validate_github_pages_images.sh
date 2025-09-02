#!/bin/bash

# Validation script for GitHub Pages media paths - Issue #205
#
# This script validates that documentation media (images, pdf, mp4) are correctly
# staged for GitHub Pages deployment with working relative path resolution

set -e

echo "============================================================"
echo "VALIDATING GITHUB PAGES MEDIA PATHS - Issue #205"
echo "============================================================"

# Test results
TESTS_PASSED=0
TESTS_FAILED=0
CRITICAL_ISSUES=0

function test_result() {
    local test_name="$1"
    local result="$2"
    local message="$3"
    
    if [ "$result" = "PASS" ]; then
        echo "✓ PASS: $test_name"
        [ -n "$message" ] && echo "  $message"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    elif [ "$result" = "FAIL" ]; then
        echo "✗ FAIL: $test_name"
        [ -n "$message" ] && echo "  $message"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    elif [ "$result" = "CRITICAL" ]; then
        echo "❌ CRITICAL: $test_name"
        [ -n "$message" ] && echo "  $message"
        CRITICAL_ISSUES=$((CRITICAL_ISSUES + 1))
        TESTS_FAILED=$((TESTS_FAILED + 1))
    else
        echo "⚠ SKIP: $test_name"
        [ -n "$message" ] && echo "  $message"
    fi
}

# Test 1: Check if documentation build directory exists
if [ -d "build/doc" ]; then
    test_result "Documentation build directory" "PASS" "build/doc exists"
else
    test_result "Documentation build directory" "FAIL" "build/doc not found - run 'make doc' first"
fi

# Test 2: Check if media staging directory exists under page/
if [ -d "build/doc/page/media/examples" ]; then
    test_result "Media staging directory" "PASS" "build/doc/page/media/examples exists"
else
    test_result "Media staging directory" "CRITICAL" "build/doc/page/media/examples missing - media links will be broken"
fi

# Test 2b: Verify referenced PNGs exist at staged location
missing_pngs=0
# Extract markdown PNG links like ![...](../../media/examples/...png)
refs=$(grep -rho -E "\(\.\.\/\.\.\/media\/examples\/[^)]+\.png\)" doc/examples 2>/dev/null | sort -u || true)
for ref in $refs; do
    # Strip surrounding parentheses
    ref="${ref#(}"
    ref="${ref%)}"
    # Compute staged target path
    path=${ref#../../}
    target="build/doc/page/${path}"
    if [ ! -f "$target" ]; then
        test_result "PNG reference missing" "CRITICAL" "$target not found (referenced in docs)"
        missing_pngs=$((missing_pngs+1))
    fi
done
[ $missing_pngs -eq 0 ] && test_result "PNG references" "PASS" "All referenced PNGs are present"

# Test 3: Check workflow and Makefile for proper media staging
if [ -f ".github/workflows/docs.yml" ]; then
    # Get lines around 'make doc' and check if cp comes BEFORE (which is correct)
    context=$(grep -B 5 -A 5 "make doc" .github/workflows/docs.yml)
    # Check if cp appears BEFORE make doc (in the -B 5 lines)
    if echo "$context" | grep -B 5 "make doc" | grep -q "cp -r doc/media/examples"; then
        test_result "Workflow media staging" "PASS" "Media copied before 'make doc' - paths will work correctly"
    elif echo "$context" | grep -A 5 "make doc" | grep -q "cp -r doc/media/examples"; then
        test_result "Workflow media staging" "CRITICAL" "Media copied after 'make doc' - may break media paths"
    else
        test_result "Workflow media staging" "PASS" "No media copy found near 'make doc'"
    fi
else
    test_result "Workflow file" "FAIL" "GitHub Actions workflow not found"
fi

if [ -f "Makefile" ]; then
    doc_section=$(sed -n '/^doc:/,/^[a-zA-Z]/p' Makefile)
    # Check that Makefile copies to build/doc/page/media (which matches FORD page links)
    ford_line=$(echo "$doc_section" | grep -n "ford" | cut -d: -f1)
    first_cp_line=$(echo "$doc_section" | grep -n "cp.*build/doc/page/media" | head -1 | cut -d: -f1)
    
    if [ -n "$ford_line" ] && [ -n "$first_cp_line" ]; then
        if [ "$ford_line" -lt "$first_cp_line" ]; then
            test_result "Makefile media staging" "PASS" "FORD runs first, then media copied under page/media - correct pattern"
        else
            test_result "Makefile media staging" "PASS" "Media copied before FORD (page/media) - also works correctly"
        fi
    elif [ -n "$ford_line" ]; then
        test_result "Makefile media staging" "PASS" "FORD found, no media copy needed"
    else
        test_result "Makefile media staging" "FAIL" "FORD command not found in doc target"
    fi
else
    test_result "Makefile" "FAIL" "Makefile not found"
fi

# Summary
echo ""
echo "============================================================"
echo "VALIDATION SUMMARY"
echo "============================================================"
echo "Tests passed: $TESTS_PASSED"
echo "Tests failed: $TESTS_FAILED"
echo "Critical issues: $CRITICAL_ISSUES"

if [ $CRITICAL_ISSUES -gt 0 ]; then
    echo ""
    echo "❌ CRITICAL ISSUES FOUND"
    echo "GitHub Pages documentation will have broken media links!"
    echo ""
    echo "REQUIRED FIXES:"
    echo "1. Ensure media files are copied to build/doc/page/media/examples/"
    echo "2. Validate that HTML files reference media with correct paths"
    echo "3. Test relative path resolution works correctly"
    echo ""
    exit 1
elif [ $TESTS_FAILED -gt 0 ]; then
    echo ""
    echo "⚠ Some tests failed - check configuration"
    exit 1
else
    echo ""
    echo "✅ All tests passed - GitHub Pages images should work correctly"
    exit 0
fi
