title: Windows FFmpeg Setup
---

# Windows FFmpeg Setup and Animation Export

Complete guide for Windows users to enable video animation export in fortplot.

## Quick Start

```fortran
use fortplot
use fortplot_animation

type(figure_t) :: fig
type(animation_t) :: anim
integer :: status

! Create animation
anim = FuncAnimation(update_wave, frames=60, interval=50, fig=fig)

! Save with error handling
call anim%save("animation.mp4", fps=24, status=status)
call handle_save_status(status)
```

## FFmpeg Installation

### Option 1: Chocolatey (Recommended)
```powershell
# Install Chocolatey if not present
Set-ExecutionPolicy Bypass -Scope Process -Force
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072
iex ((New-Object System.Net.WebClient).DownloadString('https://community.chocolatey.org/install.ps1'))

# Install FFmpeg
choco install ffmpeg
```

### Option 2: Official Download
1. Download from https://ffmpeg.org/download.html#build-windows
2. Extract to `C:\ffmpeg\bin\`
3. Add to PATH environment variable:
   ```
   C:\ffmpeg\bin
   ```

### Option 3: Package Manager
```powershell
# Using winget
winget install Gyan.FFmpeg

# Using scoop
scoop install ffmpeg
```

### Verify Installation
```cmd
ffmpeg -version
```
Should display version information without errors.

## Windows-Specific Usage

### Basic Animation Export
```fortran
program windows_animation_demo
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

    ! Initialize figure with Windows-compatible settings
    call fig%initialize(width=800, height=600)
    call fig%add_plot(x, y, label='sine wave')
    call fig%set_title('Windows Animation Demo')

    ! Create animation
    anim = FuncAnimation(update_wave, frames=60, interval=50, fig=fig)

    ! Save to current directory (Windows paths)
    call anim%save("output\animation.mp4", fps=24, status=status)
    
    ! Handle errors
    select case (status)
    case (0)
        print *, "Animation saved successfully!"
        print *, "Location: ", trim(get_current_directory()) // "\output\animation.mp4"
    case (-1)
        print *, "ERROR: FFmpeg not found in PATH"
        print *, "Install FFmpeg and add to PATH environment variable"
    case (-4)
        print *, "ERROR: Failed to open pipe to FFmpeg"
        print *, "Check Windows permissions and antivirus settings"
    case default
        print *, "ERROR: Save failed with status:", status
    end select

contains

    subroutine update_wave(frame)
        integer, intent(in) :: frame
        real :: phase
        
        phase = real(frame-1) * 6.28 / 60.0
        y = sin(x + phase)
        call fig%set_ydata(1, y)
    end subroutine

end program
```

### Filename Handling on Windows
```fortran
! Safe Windows filename handling
program windows_filenames
    use fortplot_animation
    implicit none
    
    type(animation_t) :: anim
    character(len=256) :: safe_filename
    integer :: status
    
    ! Avoid Windows reserved characters: \ / : * ? " < > |
    safe_filename = "animation_output.mp4"  ! Good
    ! safe_filename = "output:*.mp4"        ! Bad - contains reserved chars
    
    ! Use forward slashes for cross-platform compatibility
    safe_filename = "output/my_animation.mp4"
    
    ! Or use Windows backslashes (will be escaped automatically)
    safe_filename = "output\my_animation.mp4"
    
    call anim%save(safe_filename, fps=30, status=status)
    
    if (status == -2) then
        print *, "ERROR: Invalid filename for Windows"
        print *, "Avoid characters: \ / : * ? \" < > |"
    end if
end program
```

### Working with Spaces in Paths
```fortran
! Handling Windows paths with spaces
character(len=512) :: output_path

! Paths with spaces work automatically
output_path = "C:\Users\User Name\Documents\animations\wave.mp4"
call anim%save(output_path, fps=24, status=status)

! Network paths also supported
output_path = "\\server\share\animations\output.mp4"
call anim%save(output_path, fps=24, status=status)
```

## Windows-Specific Troubleshooting

### Common Issues and Solutions

#### 1. "The system cannot find the path specified"
```fortran
! Check FFmpeg availability
if (.not. check_ffmpeg_available()) then
    print *, "FFmpeg not found. Install from:"
    print *, "https://ffmpeg.org/download.html#build-windows"
    print *, "Then add C:\ffmpeg\bin to PATH"
    stop 1
end if
```

**Solutions:**
- Install FFmpeg using Chocolatey: `choco install ffmpeg`
- Download from official site and add to PATH
- Verify with `ffmpeg -version` in Command Prompt

#### 2. Binary Data Corruption
Issue: Video files created but corrupted or unplayable.
```fortran
! Windows requires binary mode for pipes
! This is handled automatically in fortplot, but verify:
logical :: is_binary_mode
is_binary_mode = verify_binary_mode()  ! Internal check
```

**Solutions:**
- Update to latest fortplot version (Issue #189 fixed)
- Use absolute paths instead of relative paths
- Check antivirus is not interfering with temporary files

#### 3. Permission Denied Errors
```fortran
! Check write permissions
character(len=256) :: test_file
logical :: can_write

test_file = "test_write.tmp"
can_write = test_file_write_permission(test_file)

if (.not. can_write) then
    print *, "ERROR: No write permission in current directory"
    print *, "Try saving to Documents folder or check permissions"
end if
```

**Solutions:**
- Run as Administrator if needed
- Use Documents folder: `%USERPROFILE%\Documents\animations\`
- Check Windows folder permissions

#### 4. Antivirus Interference
```fortran
! Test if antivirus blocks FFmpeg
if (status == -4) then
    print *, "Pipe creation failed - possible antivirus interference"
    print *, "Add FFmpeg and fortplot to antivirus exclusions"
    print *, "Or try Windows Defender exclusions"
end if
```

**Solutions:**
- Add FFmpeg executable to antivirus exclusions
- Add fortplot working directory to exclusions
- Temporarily disable real-time protection for testing

#### 5. Large Animation Performance
```fortran
! For large animations on Windows
program large_animation_windows
    use fortplot_animation
    implicit none
    
    type(animation_t) :: anim
    integer :: status
    
    ! Use lower resolution for better performance
    call fig%initialize(width=640, height=480)  ! Instead of 1920x1080
    
    ! Use efficient frame rates
    call anim%save("large.mp4", fps=15, status=status)  ! Instead of fps=60
    
    ! Monitor memory usage
    if (status == -5) then
        print *, "Frame generation failed - reduce resolution or frame count"
    end if
end program
```

### Windows Environment Detection
```fortran
use fortplot_system_runtime, only: is_windows

if (is_windows()) then
    print *, "Running on Windows - using Windows-optimized settings"
    ! Windows-specific optimizations are applied automatically
else
    print *, "Running on Unix-like system"
end if
```

## Cross-Platform Considerations

### Path Separators
```fortran
! fortplot handles path separators automatically
character(len=256) :: output_file

! These all work on Windows:
output_file = "animations/wave.mp4"        ! Forward slash (recommended)
output_file = "animations\wave.mp4"        ! Backslash (Windows native)
output_file = "animations//wave.mp4"       ! Double slash (handled)
```

### Performance Differences
| Feature | Windows | Linux/macOS | Notes |
|---------|---------|-------------|--------|
| Pipe Creation | `_popen` | `popen` | Binary mode crucial on Windows |
| Path Handling | Backslash escaping | Direct paths | Automatic in fortplot |
| FFmpeg Detection | Multiple methods | Single method | Windows tries `where` command |
| Error Reporting | Windows API codes | POSIX codes | Platform-specific details |

### Format Compatibility
```fortran
! Video formats supported on Windows
call anim%save("output.mp4", fps=24, status=status)  ! Recommended
call anim%save("output.avi", fps=24, status=status)  ! Good compatibility
call anim%save("output.mkv", fps=24, status=status)  ! Advanced features

! Avoid on Windows:
! call anim%save("output.webm", fps=24, status=status)  ! Limited support
```

## Complete Windows Example

```fortran
program complete_windows_example
    use fortplot
    use fortplot_animation
    use fortplot_system_runtime, only: is_windows
    implicit none

    type(figure_t) :: fig
    type(animation_t) :: anim
    real, dimension(200) :: x, y, z
    integer :: status, i
    character(len=512) :: output_dir, animation_file

    ! Platform check
    if (.not. is_windows()) then
        print *, "This example is optimized for Windows"
        print *, "Use standard animation examples on other platforms"
    end if

    ! Check FFmpeg
    if (.not. check_ffmpeg_available()) then
        print *, "ERROR: FFmpeg required for animation export"
        print *, "Install: choco install ffmpeg"
        print *, "Or download from: https://ffmpeg.org/download.html"
        stop 1
    end if

    ! Setup Windows-safe output directory
    output_dir = "animations"
    call create_directory_if_missing(output_dir)
    
    ! Create data
    do i = 1, 200
        x(i) = real(i-1) * 12.56 / 199.0
        y(i) = sin(x(i))
        z(i) = cos(x(i) * 0.5)
    end do

    ! Initialize figure with good Windows settings
    call fig%initialize(width=800, height=600)
    call fig%add_plot(x, y, label='sin(x)', linestyle='b-')
    call fig%add_plot(x, z, label='cos(x/2)', linestyle='r--')
    call fig%set_title('Windows Animation Demo')
    call fig%set_xlabel('Time')
    call fig%set_ylabel('Amplitude')
    call fig%legend()

    ! Create animation
    anim = FuncAnimation(update_dual_waves, frames=120, interval=42, fig=fig)

    ! Save with Windows-compatible filename
    animation_file = trim(output_dir) // "\dual_wave_demo.mp4"
    
    print *, "Generating Windows animation..."
    print *, "Output file: ", trim(animation_file)
    
    call anim%save(animation_file, fps=24, status=status)
    
    ! Comprehensive error handling
    select case (status)
    case (0)
        print *, "SUCCESS: Animation saved successfully!"
        print *, "File size: ", get_file_size(animation_file), " bytes"
        print *, "Verify with: ffprobe -v error -show_format ", trim(animation_file)
        
    case (-1)
        print *, "ERROR: FFmpeg not found"
        print *, "SOLUTION: Install with 'choco install ffmpeg'"
        print *, "Or add FFmpeg to PATH environment variable"
        
    case (-2)
        print *, "ERROR: Invalid filename"
        print *, "SOLUTION: Check filename for reserved characters"
        
    case (-3)
        print *, "ERROR: Unsupported format"
        print *, "SOLUTION: Use .mp4, .avi, or .mkv extension"
        
    case (-4)
        print *, "ERROR: Failed to open pipe"
        print *, "SOLUTION: Check antivirus settings and permissions"
        print *, "Try running as Administrator"
        
    case (-5)
        print *, "ERROR: Frame generation failed"
        print *, "SOLUTION: Reduce resolution or frame count"
        
    case (-6)
        print *, "ERROR: Frame write failed"
        print *, "SOLUTION: Check disk space and permissions"
        
    case (-7)
        print *, "ERROR: Video validation failed"
        print *, "SOLUTION: Update FFmpeg to latest version"
        
    case default
        print *, "ERROR: Unknown error (", status, ")"
        print *, "Check Windows Event Log for details"
    end select

contains

    subroutine update_dual_waves(frame)
        integer, intent(in) :: frame
        real :: phase1, phase2
        
        phase1 = real(frame-1) * 12.56 / 120.0
        phase2 = phase1 * 0.7
        
        y = sin(x + phase1) * exp(-phase1 * 0.02)
        z = cos(x * 0.5 + phase2) * exp(-phase2 * 0.015)
        
        call fig%set_ydata(1, y)
        call fig%set_ydata(2, z)
    end subroutine

end program
```

## Validation

Verify your Windows animation setup:

```bash
# Test FFmpeg installation
ffmpeg -version

# Validate generated video
ffprobe -v error -show_format animation.mp4

# Check file is not corrupted
dir animation.mp4
# Should show file size > 10KB for valid animation
```

## Advanced Windows Features

### Using Windows Media Foundation
Future versions may support Windows Media Foundation for native encoding without FFmpeg dependency.

### PowerShell Integration
```powershell
# Batch process animations
Get-ChildItem *.f90 | ForEach-Object {
    gfortran $_.Name -o $_.BaseName.exe
    & ".\$($_.BaseName).exe"
}
```

For additional Windows-specific features and troubleshooting, see [Windows Compatibility Guide](doc/windows_compatibility.md).