title: Animation
---

# Animation

This example demonstrates creating animated plots and saving to video files with cross-platform support.

## Quick Start

```fortran
use fortplot
use fortplot_animation

type(figure_t) :: fig
type(animation_t) :: anim
integer :: status

! Create animation
anim = FuncAnimation(update_func, frames=60, interval=50, fig=fig)

! Save with error handling
call anim%save("animation.mp4", fps=24, status=status)
if (status /= 0) print *, "Check FFmpeg installation"
```

## Platform-Specific Setup

### Windows Users
**Quick Install:** `choco install ffmpeg`

**Issue #189 Fixed:** Windows FFmpeg support now fully functional with binary pipe handling.

See [Windows FFmpeg Setup Guide](../windows_ffmpeg_setup.md) for complete installation and troubleshooting.

### Linux/macOS Users
```bash
# Ubuntu/Debian
sudo apt install ffmpeg

# macOS
brew install ffmpeg

# Arch Linux  
sudo pacman -S ffmpeg
```

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
            print *, "Animation saved successfully to '", trim(filename), "'"
        case (-1)
            print *, "ERROR: ffmpeg not found. Please install ffmpeg to save animations."
            print *, "Windows: choco install ffmpeg"
            print *, "Linux: sudo apt install ffmpeg"
            print *, "macOS: brew install ffmpeg"
            print *, "Or visit: https://ffmpeg.org/download.html"
        case (-3)
            print *, "ERROR: Unsupported file format. Use .mp4, .avi, or .mkv"
        case (-4)
            print *, "ERROR: Could not open pipe to ffmpeg. Check ffmpeg installation."
            print *, "Windows: Check antivirus settings and run as Administrator if needed"
        case (-5)
            print *, "ERROR: Failed to generate animation frames."
        case (-6)
            print *, "ERROR: Failed to write frame data to ffmpeg."
        case (-7)
            print *, "ERROR: Generated video failed validation. Check ffmpeg version."
        case default
            print *, "ERROR: Unknown error occurred while saving animation. Status:", status
        end select
    end subroutine save_animation_with_error_handling

end program save_animation_demo
```

## Features Demonstrated

- **Frame generation**: Create individual frames
- **Video export**: Save as MP4 using ffmpeg
- **Time evolution**: Animate changing data
- **Smooth transitions**: Proper frame timing

## Animation Workflow

1. **Initialize animation**: Set frame rate and duration
2. **Generate frames**: Update data for each time step
3. **Save frames**: Store as temporary images
4. **Create video**: Use ffmpeg to combine frames

## Requirements

### All Platforms
- **ffmpeg**: Must be installed and available in PATH
- **Frame rate**: 15-30 fps recommended (24 fps standard)
- **Resolution**: Match your figure size (800x600 default)

### Windows Specific (Issue #189)
- **Binary mode pipes**: Automatically handled
- **Path escaping**: Special characters handled automatically
- **Error reporting**: Windows-specific error codes provided
- **Antivirus**: May need exclusions for FFmpeg and temp directories

## Cross-Platform Differences

| Feature | Windows | Linux/macOS | Notes |
|---------|---------|-------------|-------|
| Pipe Mode | Binary (`_popen`) | Text (`popen`) | Windows requires binary mode |
| Path Separators | `\` or `/` | `/` | Both work on Windows |
| FFmpeg Detection | Multiple methods | Single method | Windows tries `where` command |
| Error Codes | Windows API | POSIX | Platform-specific reporting |

## File Validation

Generated video files should meet these criteria:
- **File size**: >10KB for valid animations (typical: 50KB-5MB)
- **Format validation**: Use `ffprobe -v error -show_format filename.mp4`
- **Playback test**: Compatible with VLC, Windows Media Player, QuickTime
- **Header check**: Contains valid MP4 box signatures (`ftyp`, `mdat`, `moov`)

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

## Troubleshooting

### All Platforms
1. **FFmpeg not found**: Check `ffmpeg -version` works
2. **Small file size**: Indicates encoding failure
3. **Won't play**: Check format compatibility

### Windows-Specific Issues
1. **Pipe errors**: Check antivirus, try running as Administrator
2. **Path issues**: Use forward slashes or double backslashes
3. **Binary corruption**: Update to latest fortplot (Issue #189 fixed)

For complete Windows troubleshooting, see [Windows FFmpeg Setup Guide](../windows_ffmpeg_setup.md)
