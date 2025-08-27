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
    
    call figure(figsize=[8.0_wp, 6.0_wp])
    call add_plot(x, y, label='animated wave')
    call title('Animation Save Demo')
    call xlabel('x')
    call ylabel('y')
    call xlim(0.0_wp, 2.0_wp * 3.14159_wp)
    call ylim(-1.5_wp, 1.5_wp)
    
    ! Create animation with figure reference
    anim = FuncAnimation(update_wave, frames=NFRAMES, interval=50, fig=fig)
    
    ! Save as MP4 video with 24 fps to GitHub Pages structure
    print *, "Saving animation as MP4..."
    call create_output_directory()
    call save_animation_with_error_handling(anim, "output/example/fortran/animation/animation.mp4", 24)
    
contains

    subroutine update_wave(frame)
        integer, intent(in) :: frame
        real(wp) :: phase
        
        ! Calculate phase based on frame number
        phase = real(frame - 1, wp) * 2.0_wp * 3.14159_wp / real(NFRAMES, wp)
        
        ! Update y data with animated wave
        y = sin(x + phase) * cos(phase * 0.5_wp)
        
        ! Update plot data
        call set_ydata(y)  ! Note: plot index parameter not supported in pyplot API
    end subroutine update_wave
    
    subroutine save_animation_with_error_handling(anim, filename, fps)
        type(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer :: status
        
        call save_animation(anim, filename, fps, status)
        
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
            print *, "✗ Pipe write failed - enhanced recovery attempted (Issue #186: exponential backoff exhausted)"
        case (-7)
            print *, "ERROR: Generated video failed validation. Check ffmpeg version."
        case default
            print *, "ERROR: Unknown error occurred while saving animation. Status:", status
        end select
    end subroutine save_animation_with_error_handling
    
    subroutine create_output_directory()
        ! SECURITY: Directory creation using execute_command_line disabled for security compliance
        ! Create directory structure would require secure alternatives
        print *, "Info: Output directory creation disabled for security compliance"
    end subroutine create_output_directory
    
end program save_animation_demo