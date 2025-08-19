title: Animation
---

# Animation

This example demonstrates creating animated plots and saving to video files.

## Source Files

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
            print *, "Install with: sudo pacman -S ffmpeg (Arch Linux)"
            print *, "Or visit: https://ffmpeg.org/download.html"
        case (-3)
            print *, "ERROR: Unsupported file format. Use .mp4, .avi, or .mkv"
        case (-4)
            print *, "ERROR: Could not open pipe to ffmpeg. Check ffmpeg installation."
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

- **ffmpeg**: Must be installed for video generation
- **Frame rate**: Typically 30 fps for smooth playback
- **Resolution**: Match your figure size

## MPEG File Validation

Generated MPEG files should meet these criteria:
- **File size**: >5KB for multi-frame animations (typical range: 10KB-1MB+)
- **Header validation**: Must contain valid MP4 box signatures (`ftyp`, `mdat`, `moov`)
- **External validation**: Should pass `ffprobe -v error -show_format filename.mp4`
- **Playback**: Should be compatible with standard media players

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

## Output

## Troubleshooting MPEG Issues

If generated MPEG files are unusually small (<5KB) or fail to play:

1. **Check file size**: `ls -la *.mp4` - should be >5KB for valid video
2. **Validate with ffprobe**: `ffprobe -v error -show_format filename.mp4`
3. **Test playback**: Open in VLC, mpv, or other media players
4. **Check encoding**: Ensure proper frame rate and resolution settings

**Common Issues:**
- Files <1KB usually indicate encoding failure
- Missing headers suggest incomplete MPEG-1 format compliance
- Playback failures often indicate quantization or DCT encoding problems
