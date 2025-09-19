title: Warning Suppression System
---

# Warning Suppression System

## Overview

The warning suppression system eliminates test output noise while preserving developer warnings during development. The system uses environment variables and automatic CI detection to control warning output behavior.

## Environment Variables

### Manual Control

```bash
# Suppress all warnings
export FORTPLOT_SUPPRESS_WARNINGS=1

# Enable all warnings (default)
export FORTPLOT_SUPPRESS_WARNINGS=0
```

**Supported Values**: `1`, `true`, `yes`, `on` (case-insensitive) enable suppression.

### CI Override

```bash
# Force warnings even in CI environments
export FORTPLOT_FORCE_WARNINGS=1
```

Forces warning output even when CI environment is detected. Useful for debugging CI issues.

## Automatic CI Detection

The system automatically detects CI environments and suppresses warnings:

- **GitHub Actions**: `GITHUB_ACTIONS=true`
- **Generic CI**: `CI=true`
- **Jenkins**: `CONTINUOUS_INTEGRATION=true` or `BUILD_ID` present
- **Travis CI**: `TRAVIS=true`
- **CircleCI**: `CIRCLECI=true`

## Priority Order

1. **Manual Suppression**: `FORTPLOT_SUPPRESS_WARNINGS` takes highest priority
2. **Force Override**: `FORTPLOT_FORCE_WARNINGS` overrides CI auto-detection
3. **CI Auto-Detection**: Automatic suppression in detected CI environments
4. **Default**: Warnings visible in development environments

## Usage Examples

### Development Workflow
```bash
# Normal development (warnings visible)
make test

# Clean output for focus
FORTPLOT_SUPPRESS_WARNINGS=1 make test
```

### CI Integration
```bash
# CI automatically suppresses warnings
make test  # Clean output in CI

# Debug CI issues with warnings
FORTPLOT_FORCE_WARNINGS=1 make test
```

### Local Testing
```bash
# Test with CI-like clean output
export FORTPLOT_SUPPRESS_WARNINGS=1
make test
unset FORTPLOT_SUPPRESS_WARNINGS

# Test warning generation
export FORTPLOT_FORCE_WARNINGS=1
make test
unset FORTPLOT_FORCE_WARNINGS
```

## Warning Types

### Suppressed Warnings
- Subplot limit warnings (>25 subplots)
- Plot count limit warnings (performance degradation)
- Performance threshold warnings

### Never Suppressed
- **ERROR** level messages (always shown)
- **CRITICAL** level messages (always shown)
- Fatal errors that prevent execution

## Implementation Details

### Core Module
- `fortplot_logging.f90`: Warning suppression infrastructure
- Lazy initialization ensures environment checked when needed
- Thread-safe state management

### Integration Points
- `fortplot_figure_base.f90`: Subplot limit warnings
- `fortplot_plotting.f90`: Plot count warnings
- All warning generation uses `log_warning()` for consistent behavior

### Architecture
```fortran
! Check if warnings should be output
if (.not. is_warnings_suppressed()) then
    call log_warning("Performance warning: many subplots")
end if
```

## Testing

### Test Suite
```bash
# Test warning suppression functionality
make test ARGS="--target test_warning_suppression_control"

# Test CI environment detection
make test ARGS="--target test_ci_environment_warning_detection"

# Test warning generation
make test ARGS="--target test_subplot_warning_management"
```

### Validation
```bash
# Verify warnings appear in development
make test 2>&1 | grep WARNING

# Verify warnings suppressed in CI mode
FORTPLOT_SUPPRESS_WARNINGS=1 make test 2>&1 | grep WARNING
# Should return empty
```

## Troubleshooting

### Warnings Not Suppressed
1. Check environment variable value: `echo $FORTPLOT_SUPPRESS_WARNINGS`
2. Verify CI detection: Run with `FORTPLOT_FORCE_WARNINGS=1`
3. Check log level: Ensure not set to `LOG_LEVEL_SILENT`

### Warnings Never Shown
1. Check force override: `echo $FORTPLOT_FORCE_WARNINGS`
2. Verify CI detection: May be auto-suppressing
3. Set explicit control: `export FORTPLOT_SUPPRESS_WARNINGS=0`

## Querying And Restoring Log Level

When temporarily increasing verbosity (for example during debugging), capture the
current level with `get_log_level()` and restore it afterwards to avoid leaking
state into other tests or applications:

```fortran
use fortplot, only: set_log_level, get_log_level, &
                    LOG_LEVEL_DEBUG

integer :: prev
prev = get_log_level()
call set_log_level(LOG_LEVEL_DEBUG)

! ... perform debug operations ...

call set_log_level(prev)
```

### Performance Impact
The warning suppression system has minimal performance impact:
- Environment variables checked once during initialization
- Boolean flag checks for each warning (negligible overhead)
- No impact when warnings are suppressed
