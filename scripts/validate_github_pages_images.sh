#!/bin/bash

# Validation script for GitHub Pages media paths - Issues #205, #1649, #1760
#
# This script validates that documentation media (images, pdf, mp4) are correctly
# staged for GitHub Pages deployment with working relative path resolution.

set -euo pipefail

DOC_BUILD_ROOT=${DOC_BUILD_ROOT:-build/doc}
DOC_EXAMPLES_ROOT=${DOC_EXAMPLES_ROOT:-doc/examples}
EXAMPLE_SOURCE_ROOT=${EXAMPLE_SOURCE_ROOT:-example/fortran}
EXAMPLE_OUTPUT_ROOT=${EXAMPLE_OUTPUT_ROOT:-output/example/fortran}
SKIP_MEDIA_EXAMPLES=${SKIP_MEDIA_EXAMPLES:-display_demo}
MEDIA_OUTPUT_ALIASES=${MEDIA_OUTPUT_ALIASES:-}

echo "============================================================"
echo "VALIDATING GITHUB PAGES MEDIA PATHS - Issues #205, #1649, #1760"
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
        echo "PASS: $test_name"
        [ -n "$message" ] && echo "  $message"
        TESTS_PASSED=$((TESTS_PASSED + 1))
    elif [ "$result" = "FAIL" ]; then
        echo "FAIL: $test_name"
        [ -n "$message" ] && echo "  $message"
        TESTS_FAILED=$((TESTS_FAILED + 1))
    elif [ "$result" = "CRITICAL" ]; then
        echo "FAIL: CRITICAL: $test_name"
        [ -n "$message" ] && echo "  $message"
        CRITICAL_ISSUES=$((CRITICAL_ISSUES + 1))
        TESTS_FAILED=$((TESTS_FAILED + 1))
    else
        echo "WARNING: SKIP: $test_name"
        [ -n "$message" ] && echo "  $message"
    fi
}

# Test 1: Check if documentation build directory exists
if [ -d "$DOC_BUILD_ROOT" ]; then
    test_result "Documentation build directory" "PASS" "$DOC_BUILD_ROOT exists"
else
    test_result "Documentation build directory" "FAIL" "$DOC_BUILD_ROOT not found - run 'make doc' first"
fi

# Test 2: Check if media staging directories exist (both link roots)
if [ -d "$DOC_BUILD_ROOT/page/media/examples" ]; then
    test_result "Media staging directory (page)" "PASS" "$DOC_BUILD_ROOT/page/media/examples exists"
else
    test_result "Media staging directory (page)" "CRITICAL" "$DOC_BUILD_ROOT/page/media/examples missing - some links may break"
fi
if [ -d "$DOC_BUILD_ROOT/media/examples" ]; then
    test_result "Media staging directory (root)" "PASS" "$DOC_BUILD_ROOT/media/examples exists"
else
    test_result "Media staging directory (root)" "CRITICAL" "$DOC_BUILD_ROOT/media/examples missing - some links may break"
fi

# Test 2b: Verify referenced PNGs exist at staged location
missing_pngs=0
# Extract markdown PNG links like ![...](../../media/examples/...png)
refs=$(grep -rho -E "\((\.\.\/)?\.\.\/media\/examples\/[^)]+\.(png|pdf|txt|mp4)\)" "$DOC_EXAMPLES_ROOT" 2>/dev/null | sort -u || true)
for ref in $refs; do
    # Strip surrounding parentheses
    ref="${ref#(}"
    ref="${ref%)}"
    # Compute staged target path
    if [[ "$ref" == ../../* ]]; then
        base="$DOC_BUILD_ROOT"
        path=${ref#../../}
    else
        base="$DOC_BUILD_ROOT/page"
        path=${ref#../}
    fi
    target="$base/${path}"
    if [ ! -f "$target" ]; then
        test_result "Media reference missing" "CRITICAL" "$target not found (referenced in docs)"
        missing_pngs=$((missing_pngs+1))
    fi
done
[ $missing_pngs -eq 0 ] && test_result "Media references" "PASS" "All referenced media files are present"

should_skip_media_example() {
    local example="$1"
    local skip

    for skip in $SKIP_MEDIA_EXAMPLES; do
        if [ "$example" = "$skip" ]; then
            return 0
        fi
    done
    return 1
}

media_output_name() {
    local example="$1"
    local alias

    for alias in $MEDIA_OUTPUT_ALIASES; do
        if [ "${alias%%:*}" = "$example" ]; then
            echo "${alias#*:}"
            return
        fi
    done
    echo "$example"
}

example_declares_media() {
    local example_dir="$1"

    rg -q "savefig|save_animation|output/example|\\.png|\\.pdf|\\.txt|\\.mp4" \
        "$example_dir" -g '*.f90' -g 'README.md' 2>/dev/null
}

count_media_files() {
    local dir="$1"
    local ext="$2"

    if [ ! -d "$dir" ]; then
        echo 0
        return
    fi
    find "$dir" -maxdepth 1 -type f -name "*.${ext}" | wc -l | tr -d ' '
}

validate_expected_examples() {
    local example_dir example output_dir page_dir root_dir html_file doc_file
    local expected page_count root_count ext
    local examples_checked=0
    local media_examples_checked=0

    if [ ! -d "$EXAMPLE_SOURCE_ROOT" ]; then
        test_result "Example source directory" "FAIL" "$EXAMPLE_SOURCE_ROOT missing"
        return
    fi

    while IFS= read -r example_dir; do
        example=$(basename "$example_dir")
        examples_checked=$((examples_checked + 1))

        html_file="$DOC_BUILD_ROOT/page/examples/${example}.html"
        if [ -f "$html_file" ]; then
            test_result "Example page: $example" "PASS" "$html_file exists"
        else
            test_result "Example page: $example" "CRITICAL" "$html_file missing"
        fi

        if should_skip_media_example "$example"; then
            continue
        fi
        if ! example_declares_media "$example_dir"; then
            continue
        fi

        media_examples_checked=$((media_examples_checked + 1))
        output_dir="$EXAMPLE_OUTPUT_ROOT/$(media_output_name "$example")"
        page_dir="$DOC_BUILD_ROOT/page/media/examples/$example"
        root_dir="$DOC_BUILD_ROOT/media/examples/$example"

        if [ ! -d "$output_dir" ]; then
            test_result "Generated media: $example" "CRITICAL" "$output_dir missing"
            continue
        fi

        expected=0
        for ext in png pdf txt mp4; do
            expected=$((expected + $(count_media_files "$output_dir" "$ext")))
        done
        if [ "$expected" -eq 0 ]; then
            test_result "Generated media: $example" "CRITICAL" "$output_dir has no media files"
            continue
        fi

        for ext in png pdf txt mp4; do
            expected=$(count_media_files "$output_dir" "$ext")
            if [ "$expected" -eq 0 ]; then
                continue
            fi
            page_count=$(count_media_files "$page_dir" "$ext")
            root_count=$(count_media_files "$root_dir" "$ext")
            if [ "$page_count" -lt "$expected" ]; then
                test_result "Page media count: $example .$ext" "CRITICAL" \
                    "$page_dir has $page_count, expected at least $expected"
            fi
            if [ "$root_count" -lt "$expected" ]; then
                test_result "Root media count: $example .$ext" "CRITICAL" \
                    "$root_dir has $root_count, expected at least $expected"
            fi
        done

        doc_file="$DOC_EXAMPLES_ROOT/${example}.md"
        expected=$(count_media_files "$output_dir" "png")
        if [ "$expected" -gt 0 ] && [ -f "$doc_file" ]; then
            if ! rg -q "!\[[^]]*\]\(\.\./\.\./media/examples/${example}/[^)]*\.png\)" \
                    "$doc_file"; then
                test_result "PNG embed: $example" "CRITICAL" \
                    "$doc_file has no embedded PNG output"
            fi
        fi
    done < <(find "$EXAMPLE_SOURCE_ROOT" -mindepth 1 -maxdepth 1 -type d | sort)

    test_result "Expected examples" "PASS" "$examples_checked example pages checked"
    test_result "Expected media examples" "PASS" "$media_examples_checked media-producing examples checked"
}

validate_expected_examples

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
    echo "FAIL: CRITICAL ISSUES FOUND"
    echo "GitHub Pages documentation will have broken media links!"
    echo ""
    echo "REQUIRED FIXES:"
    echo "1. Ensure media files are copied to $DOC_BUILD_ROOT/page/media/examples/"
    echo "2. Validate that HTML files reference media with correct paths"
    echo "3. Test relative path resolution works correctly"
    echo ""
    exit 1
elif [ $TESTS_FAILED -gt 0 ]; then
    echo ""
    echo "WARNING: Some tests failed - check configuration"
    exit 1
else
    echo ""
    echo "PASS: All tests passed - GitHub Pages images should work correctly"
    exit 0
fi
