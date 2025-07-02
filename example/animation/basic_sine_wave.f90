program animated_sine
    use fortplot
    use fortplot_animation
    implicit none
    
    type(animation_t) :: anim
    real(wp) :: x(100), y(100)
    integer :: i
    
    ! Create x data
    do i = 1, 100
        x(i) = real(i-1, wp) * 2.0_wp * 3.14159_wp / 99.0_wp  ! 0 to 2*pi
    end do
    
    ! Initial y data
    y = sin(x)
    
    ! Setup initial figure
    call figure(800, 600)
    call plot(x, y, label='sin wave')
    call title('Animated Sine Wave')
    call xlabel('x')
    call ylabel('sin(x + phase)')
    
    print *, "Creating animation with 100 frames..."
    
    ! Create and run animation  
    anim = FuncAnimation(animate_sine, frames=100, interval=50)
    call anim%run()
    
    print *, "Animation completed. Check frame_*.png files."

contains

    subroutine animate_sine(frame)
        integer, intent(in) :: frame
        character(len=50) :: filename
        real(wp) :: phase
        integer :: i
        
        ! Calculate phase shift
        phase = real(frame, wp) / 10.0_wp
        
        ! Update y data with phase shift
        do i = 1, 100
            y(i) = sin(x(i) + phase)
        end do
        
        ! Clear and redraw (for now - will optimize later with set_ydata)
        call figure(800, 600)  ! Reset figure
        call plot(x, y, label='sin wave')
        call title('Animated Sine Wave')
        call xlabel('x')
        call ylabel('sin(x + phase)')
        
        ! Save frame as PNG (user explicitly calls savefig - matplotlib style)
        write(filename, '("frame_", I3.3, ".png")') frame
        call savefig(filename)
        
        if (mod(frame, 10) == 0) then
            print *, "Generated frame", frame, "of 100"
        end if
    end subroutine animate_sine

end program animated_sine