program validation_complex_animation
    use fortplot
    use fortplot_animation
    use fortplot_security, only: secure_file_remove
    implicit none

    integer, parameter :: NFRAMES = 120  ! More frames
    integer, parameter :: NPOINTS = 200  ! More data points
    
    type(figure_t) :: fig
    type(animation_t) :: anim
    real(wp), dimension(NPOINTS) :: x, y1, y2, y3
    integer :: i
    character(len=200) :: filename = "complex_validation_test.mp4"
    logical :: file_exists
    integer :: file_size_bytes
    
    ! Create x data
    do i = 1, NPOINTS
        x(i) = real(i-1, wp) * 4.0_wp * 3.14159_wp / real(NPOINTS-1, wp)
    end do
    
    ! Initial y data for multiple complex plots
    y1 = sin(x)
    y2 = cos(x) * 0.7_wp
    y3 = sin(x * 2.0_wp) * 0.5_wp
    
    call fig%initialize(width=1280, height=720)  ! Larger resolution
    call fig%add_plot(x, y1, label='sin(x)')
    call fig%add_plot(x, y2, label='0.7*cos(x)')
    call fig%add_plot(x, y3, label='0.5*sin(2x)')
    call fig%set_title('Complex Validation Animation - Multiple Functions')
    call fig%set_xlabel('Phase (radians)')
    call fig%set_ylabel('Amplitude')
    call fig%set_xlim(0.0_wp, 4.0_wp * 3.14159_wp)
    call fig%set_ylim(-1.5_wp, 1.5_wp)
    ! Skip legend for validation test
    
    ! Create animation with more complex behavior
    anim = FuncAnimation(update_complex_wave, frames=NFRAMES, interval=40, fig=fig)
    
    ! Save as MP4 video with higher quality settings
    print *, "Saving complex animation as MP4..."
    call anim%save(filename, fps=25)
    
    ! Validation
    inquire(file=filename, exist=file_exists)
    if (.not. file_exists) then
        print *, "ERROR: Animation file was not created"
        stop 1
    end if
    
    inquire(file=filename, size=file_size_bytes)
    print *, "File created successfully:"
    print *, "  Size:", file_size_bytes, "bytes (~", file_size_bytes/1024, "KB)"
    
    if (file_size_bytes < 5000) then
        print *, "WARNING: File size suspiciously small for complex animation"
    else
        print *, "✅ File size appropriate for complex content"
    end if
    
    ! Cleanup
    if (.not. secure_file_remove(filename)) then
        print *, "Warning: Could not remove temporary file: " // trim(filename)
    end if
    
contains

    subroutine update_complex_wave(frame)
        integer, intent(in) :: frame
        real(wp) :: phase, amp_mod
        
        ! Calculate phase and amplitude modulation
        phase = real(frame - 1, wp) * 2.0_wp * 3.14159_wp / real(NFRAMES, wp)
        amp_mod = 0.5_wp + 0.5_wp * cos(phase * 0.3_wp)
        
        ! Update all three plot datasets with complex interactions
        y1 = sin(x + phase) * amp_mod
        y2 = cos(x - phase * 0.7_wp) * 0.7_wp * (1.0_wp + 0.3_wp * sin(phase * 2.0_wp))
        y3 = sin(x * 2.0_wp + phase * 1.5_wp) * 0.5_wp * cos(phase * 0.8_wp)
        
        ! Update all plots
        call fig%set_ydata(1, y1)
        call fig%set_ydata(2, y2)
        call fig%set_ydata(3, y3)
    end subroutine update_complex_wave
    
end program validation_complex_animation