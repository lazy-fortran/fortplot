title: Testing Guide
---

# Testing Guide

## MPEG Validation Testing

### Running MPEG Tests

```bash
# Run comprehensive validation framework tests
make test ARGS="--target test_mpeg_comprehensive_validation_framework"

# Run false positive detection tests  
make test ARGS="--target test_mpeg_false_positive_detection_comprehensive"

# Run all MPEG validation tests
find test/ -name "*mpeg*.f90" | wc -l  # Shows 21 test files
make test | grep mpeg
```

### Test Coverage

**21 Test Files** covering:
- Comprehensive validation framework
- False positive detection (Issue #32)
- Size validation with frame/resolution correlation
- Header validation for MP4 format signatures
- FFprobe external tool integration
- Edge cases and stress testing
- Performance validation
- Media player compatibility

### Test Interpretation

**Successful Output**:
```
=== COMPREHENSIVE VALIDATION FRAMEWORK TESTS ===
Layer 1 - Basic validation: T
Layer 2 - Size validation: T  
Layer 3 - Header validation: T
Layer 4 - Semantic validation: T
Layer 5 - External tool validation: T
Overall framework validation: T
```

**Failure Analysis**:
- **Layer 1 Failure**: File missing or corrupted
- **Layer 2 Failure**: File size inadequate for content
- **Layer 3 Failure**: Invalid MP4 format headers
- **Layer 5 Failure**: External tools detect format errors

## Warning Control

### Environment Variables

Control warning output during testing and development:

```bash
# Suppress all warnings (useful for CI and automated testing)
export FORTPLOT_SUPPRESS_WARNINGS=1
make test

# Force warnings even in CI environments
export FORTPLOT_FORCE_WARNINGS=1
make test

# Default behavior (warnings visible during development)
unset FORTPLOT_SUPPRESS_WARNINGS
unset FORTPLOT_FORCE_WARNINGS
make test
```

**Supported Values**:
- `FORTPLOT_SUPPRESS_WARNINGS`: `1`, `true`, `yes`, `on` (case-insensitive)
- `FORTPLOT_FORCE_WARNINGS`: `1`, `true`, `yes`, `on` (case-insensitive)

**CI Auto-Detection**: Warnings automatically suppressed in CI environments (GitHub Actions, Jenkins, Travis CI, CircleCI).

## General Testing

### All Tests
```bash
make test
```

### Clean Test Output
```bash
# Run tests without warning noise
FORTPLOT_SUPPRESS_WARNINGS=1 make test
```

### Specific Test Categories
```bash
# Figure and basic plotting
make test ARGS="--target test_figure_basics"

# Animation functionality  
make test ARGS="--target test_animation_save"

# Unicode support
make test ARGS="--target test_unicode_detection"

# Warning suppression system
make test ARGS="--target test_warning_suppression_control"
```

### Test Requirements
- Modern Fortran compiler (gfortran-11+)
- Optional: FFmpeg for MPEG validation tests
- Test files automatically cleaned up after execution