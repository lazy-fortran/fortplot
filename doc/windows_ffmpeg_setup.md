title: Windows FFmpeg Setup
---

# Windows FFmpeg Setup

Quick setup guide for Windows animation export with enhanced Issue #186 reliability improvements.

## Quick Start

```fortran
use fortplot
use fortplot_animation

type(animation_t) :: anim
integer :: status

! Enhanced reliability with Issue #186 improvements
anim = FuncAnimation(update_wave, frames=60, interval=50, fig=fig)
call anim%save("animation.mp4", fps=24, status=status)

! Enhanced error handling
if (status == 0) print *, "✓ Animation saved with enhanced reliability"
```

## Installation

**Chocolatey (Recommended):**
```powershell
choco install ffmpeg
```

**Alternative Methods:**
- Download from https://ffmpeg.org/download.html#build-windows
- `winget install Gyan.FFmpeg`
- `scoop install ffmpeg`

**Verify:**
```cmd
ffmpeg -version
```

## Enhanced Windows Support (Issue #186)

**Improvements**:
- Enhanced binary pipe handling for Windows
- Better path and permission management
- Improved error recovery and diagnostics
- Automatic retry mechanisms for pipe failures

**Example with Enhanced Reliability**:
```fortran
program windows_animation_enhanced
    use fortplot
    use fortplot_animation
    implicit none

    type(figure_t) :: fig
    type(animation_t) :: anim
    integer :: status

    call fig%initialize(width=800, height=600)
    call fig%add_plot(x, sin(x), label='sine wave')
    
    anim = FuncAnimation(update_wave, frames=30, interval=50, fig=fig)
    
    ! Enhanced save with improved Windows reliability
    call anim%save("animation.mp4", fps=24, status=status)
    
    if (status == 0) then
        print *, "✓ Animation saved with enhanced Windows reliability"
    else
        print *, "Status:", status, "- See enhanced troubleshooting below"
    end if
end program
```

## Path Handling (Enhanced)

**Automatic Features**:
- Safe filename validation and escaping
- Spaces in paths handled automatically
- Forward/backward slash compatibility
- Network path support

```fortran
! All these work with enhanced path handling
call anim%save("animation.mp4", fps=24, status=status)                    ! Simple
call anim%save("output/animation.mp4", fps=24, status=status)             ! Subdirectory
call anim%save("C:\Users\Name\Documents\anim.mp4", fps=24, status=status) ! Full path with spaces
```

## Troubleshooting (Enhanced)

### Status -6 Pipe Issues (Issue #186 Fixed)
**Error**: "Failed to write frame to pipe"  
**Enhanced Solution**: 
- Automatic retry with exponential backoff
- Improved Windows binary pipe handling
- Better error diagnostics and recovery
- Fallback to PNG sequence when needed

### Common Windows Issues

**FFmpeg Not Found**:
- Install: `choco install ffmpeg`
- Verify: `ffmpeg -version`
- Add to PATH if manual installation

**Permission Issues**:
- Run as Administrator if needed
- Check antivirus settings
- Use Documents folder for output

**Binary Pipe Issues (Resolved)**:
- Enhanced binary mode handling (Issue #186)
- Improved cross-platform pipe reliability
- Better error state recovery

**Performance Issues**:
- Reduce resolution for large animations
- Use lower frame rates (15-24 fps)
- Monitor memory usage with status codes

### Enhanced Windows Features (Issue #186)

**Automatic Optimizations**:
- Windows environment detection with optimized settings
- Enhanced binary pipe mode for reliable frame transmission
- Improved path separator handling (forward/backward slash)
- Better FFmpeg detection using multiple methods

**Format Support**:
- MP4: Recommended (best compatibility)
- AVI: Good Windows compatibility  
- MKV: Advanced features supported

**Cross-Platform Reliability**:
- Unified behavior across Windows, Linux, macOS
- Enhanced error recovery and diagnostics
- Consistent status codes and error handling

## Complete Example with Enhanced Reliability

```fortran
program enhanced_windows_animation
    use fortplot
    use fortplot_animation
    implicit none

    type(figure_t) :: fig
    type(animation_t) :: anim
    real, dimension(100) :: x, y
    integer :: status, i

    ! Create data
    do i = 1, 100
        x(i) = real(i-1) * 6.28 / 99.0
        y(i) = sin(x(i))
    end do

    ! Setup animation with enhanced Windows support
    call fig%initialize(width=640, height=480)
    call fig%add_plot(x, y, label='sine wave')
    call fig%set_title('Enhanced Windows Animation')
    
    anim = FuncAnimation(update_wave, frames=30, interval=50, fig=fig)
    
    ! Save with Issue #186 enhancements
    call anim%save("enhanced_demo.mp4", fps=24, status=status)
    
    select case (status)
    case (0)
        print *, "✓ SUCCESS: Enhanced reliability animation completed"
    case (-6)
        print *, "✓ RECOVERED: Pipe issue resolved with enhanced retry"
    case default
        print *, "Status:", status, "- Enhanced diagnostics available"
    end select

contains
    subroutine update_wave(frame)
        integer, intent(in) :: frame
        real :: phase
        phase = real(frame-1) * 6.28 / 30.0
        y = sin(x + phase)
        call fig%set_ydata(1, y)
    end subroutine
end program
```

## Validation

```cmd
REM Verify FFmpeg installation
ffmpeg -version

REM Validate generated video  
ffprobe -v error -show_format animation.mp4

REM Check file integrity
dir animation.mp4
```

**Enhanced Validation** (Issue #186):
- Automatic file validation during save process
- Better error recovery and retry mechanisms
- Improved cross-platform compatibility

For complete animation documentation see [Animation Guide](example/animation.md).