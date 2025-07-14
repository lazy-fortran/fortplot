# Failing Tests Documentation

This document tracks tests that are currently failing in the fortplotlib codebase as of 2025-07-14.

## Summary

Total failing tests: 1

## Detailed Failing Tests

### test_scatter_behavior.f90

**Test**: `test_marker_only_no_line`

**Error**:
```
DEBUG: marker="o"
DEBUG: linestyle="None"
ASSERTION FAILED: Expected NO line style for scatter plot
Expected: ""
Actual: "None"
```

**Description**: 
- The test expects an empty string for linestyle when using marker-only scatter plot
- The actual implementation returns "None" as the linestyle
- This appears to be a design decision where "None" explicitly indicates no line

**Status**: Failing on main branch (not caused by warnings fixes)

**Suggested Fix**: 
Either:
1. Update the test to expect "None" instead of empty string
2. Update the implementation to return empty string instead of "None"

**Impact**: Low - this is a test expectation issue, not a functional bug