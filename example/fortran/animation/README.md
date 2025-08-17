title: Animation Example
---

# Animation

This example demonstrates creating animated plots and saving to video files.

## Files

- `save_animation_demo.f90` - Animation saving example
- `wave_animation.mp4` - Example output video

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

## Output Examples

The animation example generates an MP4 video file showing a wave that changes over time.

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