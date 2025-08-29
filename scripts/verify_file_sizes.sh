#!/bin/bash
# File Size Verification Script - Fraud Prevention Tool
# Reports actual file sizes vs. documented limits to prevent size fraud

echo "=== FILE SIZE FRAUD PREVENTION REPORT ==="
echo "Hard Limit: 1000 lines | Target: 500 lines"
echo "========================================="

# Check if src directory exists
if [ ! -d "src/" ]; then
    echo "ERROR: src/ directory not found. Run from project root."
    exit 1
fi

# Find all Fortran files and check sizes
VIOLATIONS=0
WARNINGS=0
TOTAL_FILES=0

echo "CRITICAL VIOLATIONS (>1000 lines):"
while IFS= read -r file_info; do
    lines=$(echo "$file_info" | awk '{print $1}')
    file=$(echo "$file_info" | awk '{print $2}')
    
    if [ "$lines" -gt 1000 ]; then
        echo "  CRITICAL: $file has $lines lines ($(( (lines - 1000) * 100 / 1000 ))% over hard limit)"
        VIOLATIONS=$((VIOLATIONS + 1))
    fi
    TOTAL_FILES=$((TOTAL_FILES + 1))
done < <(find src/ -name "*.f90" -exec wc -l {} \; | sort -nr)

echo ""
echo "SIZE LIMIT WARNINGS (>500 lines):"
while IFS= read -r file_info; do
    lines=$(echo "$file_info" | awk '{print $1}')
    file=$(echo "$file_info" | awk '{print $2}')
    
    if [ "$lines" -gt 500 ] && [ "$lines" -le 1000 ]; then
        echo "  WARNING: $file has $lines lines ($(( (lines - 500) * 100 / 500 ))% over target)"
        WARNINGS=$((WARNINGS + 1))
    fi
done < <(find src/ -name "*.f90" -exec wc -l {} \; | sort -nr)

echo ""
echo "========================================="
echo "FRAUD PREVENTION SUMMARY:"
echo "  Total files: $TOTAL_FILES"
echo "  Critical violations: $VIOLATIONS"
echo "  Size warnings: $WARNINGS"

if [ "$VIOLATIONS" -gt 0 ]; then
    echo "  STATUS: CRITICAL - Hard limits exceeded"
    echo "  ACTION REQUIRED: Immediate refactoring needed"
    exit 1
elif [ "$WARNINGS" -gt 0 ]; then
    echo "  STATUS: WARNING - Target limits exceeded"
    echo "  ACTION RECOMMENDED: Consider refactoring large files"
    exit 0
else
    echo "  STATUS: COMPLIANT - All files within limits"
    exit 0
fi