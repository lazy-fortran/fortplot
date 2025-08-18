# MPEG Animation Validation Framework

## Quick Start

```fortran
use fortplot
type(animation_t) :: anim
logical :: is_valid

! Create animation
anim = FuncAnimation(update_func, frames=20, interval=50, fig=fig)
call anim%save("output.mp4", fps=24)

! Validate result with comprehensive framework
is_valid = validate_mpeg_comprehensive("output.mp4")
```

## Validation Problem

**Issue #32**: Previous MPEG validation tests gave false positives - files existed but contained invalid MPEG content (624-byte dummy files).

**Solution**: 5-layer validation framework prevents false positives through comprehensive checks.

## 5-Layer Validation Framework

### Layer 1: Basic File Validation
```fortran
logical :: basic_valid
basic_valid = validate_basic_requirements(filename)
```

**Checks**:
- File exists on filesystem
- File size > 0 bytes
- File readable

**Purpose**: Eliminates completely missing or empty files.

### Layer 2: Size Validation
```fortran
logical :: size_valid
size_valid = validate_size_requirements(filename)
```

**Checks**:
- Minimum 2KB for any valid video
- Frame count vs file size correlation
- Resolution vs file size correlation

**Critical Thresholds**:
- Multi-frame content: 300 bytes per frame minimum
- High resolution (1024x768): Additional 500+ bytes expected
- Complex animations: 400+ bytes per frame

### Layer 3: Header Validation
```fortran
logical :: header_valid
header_valid = validate_header_requirements(filename)
```

**Checks**:
- MP4 format signatures: `ftyp`, `mdat`, `moov`
- Valid binary header structure
- Container format integrity

**Implementation**:
```fortran
! Read first 16 bytes for format detection
character(len=16) :: header
is_valid = (index(header, 'ftyp') > 0 .or. &
           index(header, 'mdat') > 0 .or. &
           index(header, 'moov') > 0)
```

### Layer 4: Semantic Validation
```fortran
logical :: semantic_valid
semantic_valid = validate_semantic_requirements(filename)
```

**Checks**:
- Frame count consistency
- Resolution metadata validation
- Duration calculation accuracy
- Content complexity assessment

### Layer 5: External Tool Validation
```fortran
logical :: tool_valid
tool_valid = validate_external_tool_requirements(filename)
```

**Tools**:
- **FFprobe**: Media format validation
- **Media players**: Playback compatibility testing

**FFprobe Command**:
```bash
ffprobe -v error -show_format "filename.mp4"
```

**Graceful Degradation**: If external tools unavailable, layer passes automatically.

## Comprehensive Validation Function

```fortran
function validate_mpeg_comprehensive(filename) result(is_valid)
    character(len=*), intent(in) :: filename
    logical :: is_valid
    logical :: basic_valid, size_valid, header_valid, semantic_valid, tool_valid
    
    ! Apply all 5 layers sequentially
    basic_valid = validate_basic_requirements(filename)
    if (.not. basic_valid) then
        is_valid = .false.
        return
    end if
    
    size_valid = validate_size_requirements(filename)
    header_valid = validate_header_requirements(filename)
    semantic_valid = validate_semantic_requirements(filename)
    tool_valid = validate_external_tool_requirements(filename)
    
    is_valid = basic_valid .and. size_valid .and. header_valid .and. &
              semantic_valid .and. tool_valid
end function
```

## False Positive Prevention

**Problem Scenarios**:
1. **624-byte dummy files**: Caught by Layer 2 (size validation)
2. **Wrong format files**: Caught by Layer 3 (header validation)
3. **Corrupted content**: Caught by Layer 5 (external tool validation)
4. **Inadequate compression**: Caught by Layer 4 (semantic validation)

**Prevention Strategy**:
- **Multiple validation criteria**: No single check determines validity
- **Context-aware sizing**: File size validated against frame count, resolution, complexity
- **External verification**: Third-party tools confirm format validity
- **Graduated failure**: Specific layer failures indicate specific problem types

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

### Test Failure Interpretation

**Layer 1 Failure**: File missing or unreadable
```
Layer 1 - Basic validation: F
>>> File does not exist or has zero size
```

**Layer 2 Failure**: File too small for content
```
Layer 2 - Size validation: F
>>> File size 624 bytes inadequate for 20 frames at 640x480
```

**Layer 3 Failure**: Invalid format headers
```
Layer 3 - Header validation: F
>>> No MP4 format signatures found in file header
```

**Layer 5 Failure**: External tool rejects file
```
Layer 5 - External tool validation: F
>>> FFprobe reports format errors or corruption
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

## Troubleshooting

### Common Validation Failures

**Problem**: All layers pass but animation quality poor
**Solution**: Increase fps, resolution, or frame count

**Problem**: Layer 5 fails consistently  
**Solution**: Check FFmpeg installation and PATH configuration

**Problem**: Layer 2 fails for valid content
**Solution**: Review minimum size thresholds for specific use case

**Problem**: Layer 3 fails for generated files
**Solution**: Verify MPEG encoder implementation produces valid headers

### Debug Mode Validation
```fortran
! Enable detailed validation logging
call validate_mpeg_comprehensive_debug("output.mp4", debug=.true.)
```

Outputs detailed information for each validation layer to help diagnose failures.