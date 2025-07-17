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
    call anim%save("build/example/animation/wave_animation.mp4", fps=24)
    
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