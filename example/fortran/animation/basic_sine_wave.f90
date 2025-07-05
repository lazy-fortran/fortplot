program animated_sine
    use fortplot
    use fortplot_animation
    implicit none

    integer, parameter :: NFRAMES = 25

    type(figure_t) :: fig
    type(animation_t) :: anim
    real(wp) :: x(NFRAMES), y(NFRAMES)
    integer :: i

    ! Create x data
    do i = 1, NFRAMES
        x(i) = real(i-1, wp) * 2.0_wp * 3.14159_wp / (NFRAMES-1)  ! 0 to 2*pi
    end do

    ! Initial y data
    y = sin(x)

    call fig%initialize()
    call fig%add_plot(x, y, label='sin wave')
    call fig%set_title('Animated Sine Wave')
    call fig%set_xlabel('x')
    call fig%set_ylabel('sin(x + phase)')
    call fig%set_xlim(0.0_wp, 2.0_wp * 3.14159_wp)
    call fig%set_ylim(-1.5_wp, 1.5_wp)

    anim = FuncAnimation(animate_sine, frames=NFRAMES, interval=50, fig=fig)
    
    ! Save animation as MP4
    print *, "Saving animation as MP4..."
    call anim%save("example/fortran/animation/sine_wave.mp4", fps=15)
    
    ! Also run the animation to generate individual frames
    call anim%run()

    print *, "Animation completed. Check frame_*.png files."

contains

    subroutine animate_sine(frame)
        integer, intent(in) :: frame
        character(len=128) :: filename
        real(wp) :: phase
        integer :: i

        phase = real(frame, wp) / 10.0_wp
        y = sin(x + phase)

        call fig%set_ydata(1, y)

        write(filename, '("example/fortran/animation/frame_", I3.3, ".png")') frame
        call fig%savefig(filename)

        print *, "Generated frame", frame
    end subroutine animate_sine

end program animated_sine
