title: Enhanced Animation Validation
---

# Enhanced Animation Validation Framework

Comprehensive validation system with Issue #186 pipe reliability improvements.

## Quick Start

```fortran
use fortplot
use fortplot_animation

type(animation_t) :: anim
integer :: status

! Create and save animation with enhanced validation
anim = FuncAnimation(update_func, frames=20, interval=50, fig=fig)
call anim%save("output.mp4", fps=24, status=status)

! Status code now includes comprehensive validation
if (status == 0) print *, "Animation created and validated successfully"
```

## Enhanced Validation (Issue #186)

**Problem**: Animation saves failing with status -6 and file validation issues
**Solution**: Integrated validation with pipe reliability improvements

**Key Improvements**:
- Real-time pipe health monitoring during frame transmission
- Enhanced cross-platform file validation
- Better error recovery with detailed diagnostics
- Automatic fallback to PNG sequences when video encoding fails

## Integrated Validation System

### Real-Time Validation During Save

Validation now occurs throughout the animation save process:

```fortran
! Enhanced save with integrated validation
call anim%save(filename, fps=24, status=status)

! Status codes reflect comprehensive validation results
select case (status)
case (0)
    ! File created, validated, and confirmed playable
case (-6)
    ! Pipe write failed - exponential backoff retry exhausted
case (-7)
    ! Video created but failed final validation
end select
```

### Validation Stages

**Stage 1: Pre-Flight Validation**
- Format compatibility check
- Output path validation
- FFmpeg availability verification

**Stage 2: Pipeline Monitoring**
- Real-time pipe health during frame transmission
- Frame data integrity checking
- Memory usage and error detection

**Stage 3: Post-Creation Validation**
- File existence and size verification
- Header format validation (MP4 signatures)
- Optional external tool verification with FFprobe

**Stage 4: Playback Verification**
- Basic format compliance checking
- Container structure validation
- Graceful degradation when external tools unavailable

## Enhanced Status Codes

Animation save now returns comprehensive status reflecting all validation stages:

```fortran
subroutine enhanced_animation_save_example()
    type(animation_t) :: anim
    integer :: status

    call anim%save("animation.mp4", fps=24, status=status)

    select case (status)
    case (0)
        print *, "✓ Animation created and fully validated"
    case (-1)
        print *, "✗ FFmpeg not available"
    case (-3)
        print *, "✗ Invalid file format"
    case (-4)
        print *, "✗ Pipe connection failed"
    case (-5)
        print *, "✗ Frame generation failed"
    case (-6)
        print *, "✗ Pipe write failed (Issue #186 - exponential backoff exhausted)"
    case (-7)
        print *, "✗ Video validation failed"
    end select
end subroutine
```

## Issue #186 Pipe Reliability Integration

**Enhanced Error Recovery**:
- **Automatic retry**: Failed pipe writes trigger retry with exponential backoff
- **Fallback mechanisms**: PNG sequence generation when video encoding fails
- **Cross-platform consistency**: Unified behavior across Windows, Linux, macOS
- **Better diagnostics**: Detailed error reporting for troubleshooting

**Validation Improvements**:
- **Real-time monitoring**: Pipe health checked during frame transmission
- **File consistency**: Better detection of "File exists: F" and "File size: -1" issues
- **Memory management**: Enhanced frame data validation and cleanup
- **Format robustness**: Improved MP4 container validation

## Test Procedure

### Running MPEG Validation Tests
```bash
# Run comprehensive validation framework tests
make test ARGS="--target test_mpeg_comprehensive_validation_framework"

# Run false positive detection tests
make test ARGS="--target test_mpeg_false_positive_detection_comprehensive"

# Run all MPEG validation tests
make test | grep mpeg
```

### Expected Test Outputs
```
=== COMPREHENSIVE VALIDATION FRAMEWORK TESTS ===
Layer 1 - Basic validation: T
Layer 2 - Size validation: T  
Layer 3 - Header validation: T
Layer 4 - Semantic validation: T
Layer 5 - External tool validation: T
Overall framework validation: T
```

### Enhanced Diagnostic Output

**Status -6 Enhanced Recovery** (Issue #186):
```
Pipe write failed - attempting recovery...
✓ Retry 1/3: Frame transmission restored
✓ Animation completed with enhanced reliability
```

**File Validation Issues**:
```
File validation failed:
- File exists: T
- File size: 2.5KB (expected >1KB for 10 frames)
- Format: Valid MP4 container
- Recommendation: Animation completed successfully
```

**Advanced Diagnostics**:
```fortran
! Enable detailed validation logging
use fortplot_logging
call set_log_level(LOG_LEVEL_DEBUG)
call anim%save("debug.mp4", fps=24, status=status)
! Outputs detailed pipe and validation diagnostics
```

## Integration with Animation Save

### Enhanced save() Method
```fortran
type(animation_t) :: anim
integer :: status
logical :: validation_passed

call anim%save("output.mp4", fps=24, status=status)

if (status == 0) then
    validation_passed = validate_mpeg_comprehensive("output.mp4")
    if (.not. validation_passed) then
        print *, "WARNING: Animation saved but failed validation"
    end if
end if
```

### Validation Automation
Future implementation will integrate validation directly into save process:
```fortran
call anim%save("output.mp4", fps=24, validate=.true., status=status)
! status indicates both save success and validation results
```

## External Tool Requirements

### FFmpeg Installation
```bash
# Ubuntu/Debian
sudo apt install ffmpeg

# Arch Linux  
sudo pacman -S ffmpeg

# macOS
brew install ffmpeg

# Windows
# Download from https://ffmpeg.org/download.html
```

### Validation Without External Tools
Framework gracefully handles missing external tools:
- Layers 1-4 provide substantial validation coverage
- Layer 5 automatically passes if tools unavailable
- Manual verification possible using media players

### Manual Verification Steps
1. **File existence**: Check file created with expected name
2. **File size**: Verify size > 2KB for multi-frame content
3. **Media player test**: Open file in VLC, QuickTime, or similar
4. **Format verification**: Use `file` command to check format

```bash
file output.mp4
# Expected: output.mp4: ISO Media, MP4 v2
```

## Troubleshooting Enhanced System

### Status -6 Pipe Write Issues (Resolved)
**Problem**: "Failed to write frame to pipe" errors  
**Solution**: Enhanced pipe reliability with automatic recovery
- Retry mechanisms with exponential backoff
- Improved cross-platform pipe handling  
- Better error state management
- Fallback to PNG sequence when needed

### File Validation Issues
**Problem**: "File exists: F" or "File size: -1"  
**Solution**: Enhanced file system consistency checking
- Multi-stage validation during save process
- Better cross-platform file handling
- Improved error reporting with actionable feedback

### Performance and Memory
**Problem**: Large animations fail or hang  
**Solution**: Enhanced resource management
- Better memory allocation for frame data
- Improved timeout handling for slow systems
- Configurable retry and fallback thresholds

### Advanced Debugging

```fortran
! Comprehensive animation debugging
use fortplot_animation
use fortplot_logging

type(animation_t) :: anim
integer :: status

! Enable detailed diagnostics
call set_log_level(LOG_LEVEL_DEBUG)

! Save with enhanced error reporting
call anim%save("debug.mp4", fps=24, status=status)

! Check fallback PNG sequence if video fails
if (status /= 0) then
    call anim%save_png_sequence("fallback_frame_")
end if
```

**Debug Output Includes**:
- Pipe connection status and health monitoring
- Frame transmission progress and validation
- File system operations and validation results
- FFmpeg command execution and output parsing
- Memory usage and performance metrics
