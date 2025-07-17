title: Animation
---

# Animation

This example demonstrates creating animated plots and saving to video files.

## Source Files

## Source Code

**Fortran:** [save_animation_demo.f90](https://github.com/krystophny/fortplotlib/blob/main/example/fortran/animation/save_animation_demo.f90)

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
    call anim%save("example/fortran/animation/wave_animation.mp4", fps=24)

    print *, "Animation saved successfully!"

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

### Wave Animation

<video width="800" height="600" controls>
  <source src="../../media/examples/wave_animation.mp4" type="video/mp4">
  Your browser does not support the video tag.
</video>

[Download MP4](../../media/examples/wave_animation.mp4)

