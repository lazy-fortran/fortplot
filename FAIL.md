# Failing Tests Documentation

This document tracks tests that are currently failing in the fortplotlib codebase as of 2025-07-14.

## Summary

Total failing tests: 5

## Detailed Failing Tests

### test_debug_corner_cases.f90 - ACTIVE FAILURE

**Test**: `test_case_05_to_25()` - Range 0.5 to 2.5 decimal consistency

**Error**:
```
FAIL: Inconsistent decimal places in 0.5-2.5 range
```

**Description**: 
- Test expects consistent decimal places for all tick labels in range 0.5 to 2.5
- Current implementation produces inconsistent formatting:
  - Labels with 1 decimal: "0.5", "1.5", "2.5"
  - Labels with 0 decimals: "1", "2"
- This inconsistency violates the test's requirement for uniform formatting

**Root Cause**: Tick formatting logic produces inconsistent decimal places for integer vs non-integer values

**Status**: Needs fix in tick formatting consistency

**Impact**: Medium - affects tick label appearance and user experience

### Other Failing Tests

**Also failing with exit code 1:**
- test_consistent_tick_digits.f90
- test_log_symlog_ticks.f90  
- test_fine_tuned_positioning.f90
- test_antialiased_markers.f90

**Status**: All related to tick formatting consistency issues introduced during warning cleanup

## CI/CD Status

✅ **CI/CD now correctly fails on test failures** - Mission accomplished!
- Removed `continue-on-error: true` from test workflows
- CI properly detects and reports test failures
- Multiple compiler testing (gfortran-11, gfortran-12) working
- CMake build testing working