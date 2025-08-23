#!/bin/bash

# Validation script for GitHub Pages image paths - Issue #205
# 
# This script validates that documentation images are correctly staged
# for GitHub Pages deployment with working relative path resolution

set -e

echo "============================================================"
echo "VALIDATING GITHUB PAGES IMAGE PATHS - Issue #205"
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

# Test 2: Check if media staging directory exists
if [ -d "build/doc/media/examples" ]; then
    test_result "Media staging directory" "PASS" "build/doc/media/examples exists"
else
    test_result "Media staging directory" "CRITICAL" "build/doc/media/examples missing - images will be broken"
fi

# Test 3: Check workflow and Makefile for proper media staging
if [ -f ".github/workflows/docs.yml" ]; then
    if grep -A 5 -B 5 "make doc" .github/workflows/docs.yml | grep -q "cp -r doc/media/examples"; then
        test_result "Workflow media staging" "CRITICAL" "Media copied after 'make doc' - will break image paths"
    else
        test_result "Workflow media staging" "PASS" "Media staging appears correct in workflow"
    fi
else
    test_result "Workflow file" "FAIL" "GitHub Actions workflow not found"
fi

if [ -f "Makefile" ]; then
    doc_section=$(sed -n '/^doc:/,/^[a-zA-Z]/p' Makefile)
    if echo "$doc_section" | grep -n -E "(ford|cp.*doc/media)" | head -2 | tail -1 | grep -q "cp"; then
        test_result "Makefile media staging" "CRITICAL" "Media copied after FORD - will break image paths"
    else
        test_result "Makefile media staging" "PASS" "Media staging appears correct in Makefile"
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
    echo "GitHub Pages documentation will have broken images!"
    echo ""
    echo "REQUIRED FIXES:"
    echo "1. Copy media files BEFORE running FORD in both Makefile and workflow"
    echo "2. Ensure media staging happens early in build process"
    echo "3. Validate relative path resolution works correctly"
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