title: Animation
---

# Animation

Source: [save_animation_demo.f90](../../sourcefile/save_animation_demo.f90.html)

This example demonstrates creating animated plots and saving to video files.

## Files

- `save_animation_demo.f90` - Animation saving example
- **[📹 Download animation.mp4](../media/examples/animation/animation.mp4)** - Example output video

## Running

```bash
make example ARGS="save_animation_demo"
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
