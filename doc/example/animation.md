title: Animation
---

# Animation

Create animated plots with enhanced FFmpeg pipe reliability and comprehensive error recovery.

## Quick Start

```fortran
use fortplot
use fortplot_animation

type(figure_t) :: fig
type(animation_t) :: anim
integer :: status

! Create animation
anim = FuncAnimation(update_func, frames=60, interval=50, fig=fig)

! Save with enhanced error handling  
call anim%save("animation.mp4", fps=24, status=status)
if (status /= 0) print *, "Enhanced error recovery available"
```

## Enhanced FFmpeg Integration (Issue #186)

**Latest Improvements:**
- Enhanced pipe reliability with robust error recovery
- Cross-platform file validation and error diagnostics
- Improved Windows binary pipe handling
- Better status codes for precise troubleshooting

### Installation

**Windows:** `choco install ffmpeg`  
**Ubuntu/Debian:** `sudo apt install ffmpeg`  
**macOS:** `brew install ffmpeg`  
**Arch Linux:** `sudo pacman -S ffmpeg`

## Source Code

🔷 **Fortran:** [save_animation_demo.f90](https://github.com/lazy-fortran/fortplot/blob/main/example/fortran/animation/save_animation_demo.f90)

```fortran
program save_animation_demo
    use fortplot
    use fortplot_animation
    implicit none

    integer, parameter :: NFRAMES = 60

    type(figure_t) :: fig
    type(animation_t) :: anim
    real(wp), dimension(100) :: x, y
    integer :: i

    ! Create x data
    do i = 1, 100
        x(i) = real(i-1, wp) * 2.0_wp * 3.14159_wp / 99.0_wp
    end do

    ! Initial y data
    y = sin(x)

    call fig%initialize(width=800, height=600)
    call fig%add_plot(x, y, label='animated wave')
    call fig%set_title('Animation Save Demo')
    call fig%set_xlabel('x')
    call fig%set_ylabel('y')
    call fig%set_xlim(0.0_wp, 2.0_wp * 3.14159_wp)
    call fig%set_ylim(-1.5_wp, 1.5_wp)

    ! Create animation with figure reference
    anim = FuncAnimation(update_wave, frames=NFRAMES, interval=50, fig=fig)

    ! Save as MP4 video with 24 fps
    print *, "Saving animation as MP4..."
    call save_animation_with_error_handling(anim, "animation.mp4", 24)

contains

    subroutine update_wave(frame)
        integer, intent(in) :: frame
        real(wp) :: phase

        ! Calculate phase based on frame number
        phase = real(frame - 1, wp) * 2.0_wp * 3.14159_wp / real(NFRAMES, wp)

        ! Update y data with animated wave
        y = sin(x + phase) * cos(phase * 0.5_wp)

        ! Update plot data
        call fig%set_ydata(1, y)
    end subroutine update_wave

    subroutine save_animation_with_error_handling(anim, filename, fps)
        type(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer :: status

        call anim%save(filename, fps, status)

        select case (status)
        case (0)
            print *, "✓ Animation saved successfully to '", trim(filename), "'"
        case (-1)
            print *, "✗ FFmpeg not found - install: choco install ffmpeg (Windows) or apt install ffmpeg"
        case (-3)
            print *, "✗ Invalid format - use .mp4, .avi, or .mkv extension"
        case (-4)
            print *, "✗ Pipe connection failed - check FFmpeg installation and permissions"
        case (-5)
            print *, "✗ Frame generation failed - reduce resolution or complexity"
        case (-6)
            print *, "✗ Pipe write failed - enhanced recovery attempted (Issue #186: exponential backoff exhausted)"
        case (-7)
            print *, "✗ Video validation failed - check output file integrity"
        case default
            print *, "✗ Unknown error (status:", status, ") - check system diagnostics"
        end select
    end subroutine save_animation_with_error_handling

end program save_animation_demo
```

## Enhanced Reliability Features

- **Robust pipe communication**: Enhanced error detection and recovery
- **Cross-platform reliability**: Improved Windows/Unix pipe handling
- **Comprehensive validation**: Multi-stage file and format verification
- **Detailed diagnostics**: Precise error codes and recovery suggestions
- **Automatic fallbacks**: PNG sequence generation when video fails

## Pipe Reliability Improvements (Issue #186)

### Enhanced Error Recovery
- **Status -6 fixes**: Improved pipe write reliability with retry mechanisms
- **File validation**: Better detection of "File exists: F" and "File size: -1" issues
- **Cross-platform consistency**: Unified behavior across Windows, Linux, macOS
- **Diagnostic output**: Detailed error reporting for troubleshooting

### Automatic Features
- **Binary mode pipes**: Platform-appropriate handling
- **Error state recovery**: Clean pipe closure on failures
- **Format validation**: Pre and post-processing verification
- **Fallback mechanisms**: PNG sequence when video encoding fails

## Status Code Reference (Issue #186 Enhanced)

| Code | Meaning | Enhanced Recovery |
|------|---------|------------------|
| 0 | Success | File validated and playable |
| -1 | FFmpeg missing | Clear installation instructions |
| -3 | Invalid format | Format validation before processing |
| -4 | Pipe open failed | Enhanced permission and path checking |
| -5 | Frame generation failed | Memory and resolution diagnostics |
| -6 | **Pipe write failed** | **Exponential backoff retry exhausted (recovery attempted)** |
| -7 | Video validation failed | Comprehensive file integrity checking |

## Enhanced Validation Framework

**Automatic Validation**:
- **Size validation**: Minimum thresholds based on frame count and resolution
- **Header validation**: MP4 format signatures (`ftyp`, `mdat`, `moov`)
- **Content validation**: Frame integrity and encoding verification
- **Playback validation**: Optional external tool verification with FFprobe

**Manual Verification**:
```bash
# Validate video file
ffprobe -v error -show_format animation.mp4

# Check file integrity
file animation.mp4  # Should show: ISO Media, MP4 v2
```

## Example Code Structure

```fortran
! Initialize animation
call anim%init(fps=30, duration=5.0)

! Generate frames
do i = 1, n_frames
    ! Update data
    call update_data(t)

    ! Plot frame
    call fig%clear()
    call fig%add_plot(x, y)

    ! Add frame
    call anim%add_frame(fig)
end do

! Save video
call anim%save('animation.mp4')
```

## Windows-Specific Examples

### Handling Paths with Spaces
```fortran
! Windows paths with spaces work automatically
character(len=256) :: output_file
output_file = "C:\Users\User Name\Documents\animation.mp4"
call anim%save(output_file, fps=24, status=status)
```

### Error Handling for Windows
```fortran
use fortplot_system_runtime, only: is_windows

if (is_windows() .and. status == -4) then
    print *, "Windows pipe error - check antivirus settings"
    print *, "Try adding FFmpeg to antivirus exclusions"
else if (status == -1) then
    print *, "FFmpeg not found in PATH"
end if
```

### Directory Creation
```fortran
! Create output directory on Windows
character(len=256) :: output_dir
output_dir = "animations"
if (is_windows()) then
    call system('mkdir "' // trim(output_dir) // '" 2>NUL')
else
    call system('mkdir -p "' // trim(output_dir) // '"')
end if
```

## Output

## Troubleshooting (Enhanced)

### Status -6 Pipe Write Issues (Issue #186 Fixed)
**Error**: "Failed to write frame to pipe"  
**Enhanced Recovery**: 
- Automatic retry with exponential backoff
- Improved cross-platform pipe handling
- Better error diagnostics and user feedback
- Fallback to PNG sequence generation

### File Existence Issues
**Error**: "File exists: F" or "File size: -1"  
**Enhanced Detection**:
- Multi-stage file validation
- Better file system consistency checking  
- Improved error reporting with actionable steps

### General Issues
1. **FFmpeg detection**: `ffmpeg -version` must work
2. **Permissions**: Ensure write access to output directory
3. **Antivirus**: Add FFmpeg to exclusions if needed
4. **Memory**: Reduce resolution for large animations

### Advanced Diagnostics
```fortran
! Enable detailed logging
use fortplot_logging, only: set_log_level
call set_log_level(LOG_DEBUG)

! Generate diagnostic report
call anim%save("test.mp4", fps=24, status=status, debug=.true.)
```
