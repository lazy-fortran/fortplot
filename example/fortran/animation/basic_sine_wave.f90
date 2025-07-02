program animated_sine
    use fortplot
    use fortplot_animation
    implicit none

    type(figure_t) :: fig
    type(animation_t) :: anim
    real(wp) :: x(100), y(100)
    integer :: i

    ! Create x data
    do i = 1, 100
        x(i) = real(i-1, wp) * 2.0_wp * 3.14159_wp / 99.0_wp  ! 0 to 2*pi
    end do

    ! Initial y data
    y = sin(x)

    ! Setup figure once before animation (matplotlib style)
    call fig%initialize(800, 600)
    call fig%add_plot(x, y, label='sin wave')
    call fig%set_title('Animated Sine Wave')
    call fig%set_xlabel('x')
    call fig%set_ylabel('sin(x + phase)')
    call fig%set_xlim(0.0_wp, 2.0_wp * 3.14159_wp)
    call fig%set_ylim(-1.5_wp, 1.5_wp)

    print *, "Creating animation with 100 frames..."

    ! Create and run animation
    anim = FuncAnimation(animate_sine, frames=25, interval=50)
    call anim%run()

    print *, "Animation completed. Check frame_*.png files."

contains

    subroutine animate_sine(frame)
        integer, intent(in) :: frame
        character(len=128) :: filename
        real(wp) :: phase
        integer :: i

        ! Calculate phase shift
        phase = real(frame, wp) / 10.0_wp

        ! Update y data with phase shift
        do i = 1, 100
            y(i) = sin(x(i) + phase)
        end do

        ! Update existing plot data efficiently (matplotlib style: line.set_ydata())
        call fig%set_ydata(1, y)

        ! Save frame as PNG in correct directory
        write(filename, '("example/fortran/animation/frame_", I3.3, ".png")') frame
        call fig%savefig(filename)

        if (mod(frame, 10) == 0) then
            print *, "Generated frame", frame, "of 100"
        end if
    end subroutine animate_sine

end program animated_sine
