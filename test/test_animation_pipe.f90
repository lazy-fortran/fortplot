program test_animation_pipe
    use fortplot
    use iso_fortran_env, only: real64
    implicit none
    
    call test_ffmpeg_pipe_streaming()
    call test_pipe_with_different_formats()
    
contains

    subroutine test_ffmpeg_pipe_streaming()
        type(figure_t) :: fig
        type(animation_t) :: anim
        real(real64), dimension(100) :: x, y
        integer :: i, status
        
        print *, "Testing ffmpeg pipe streaming..."
        
        ! Create test data
        do i = 1, 100
            x(i) = real(i-1, real64) * 0.1_real64
        end do
        
        ! Initialize figure
        call fig%initialize(640, 480)
        
        ! Create animation
        anim = FuncAnimation(animate_sine_wave_pipe, frames=30, interval=50, fig=fig)
        
        ! Test saving with pipe streaming (no temp files)
        call anim%save("test_pipe_output.mp4", fps=10, status=status)
        
        if (status == 0) then
            print *, "PASS: Pipe streaming to ffmpeg succeeded"
        else
            print *, "FAIL: Pipe streaming to ffmpeg failed with status:", status
        end if
        
    end subroutine test_ffmpeg_pipe_streaming
    
    subroutine test_pipe_with_different_formats()
        type(figure_t) :: fig
        type(animation_t) :: anim
        real(real64), dimension(50) :: x, y
        integer :: i, status
        character(len=*), dimension(3), parameter :: formats = ["mp4", "avi", "mkv"]
        character(len=256) :: filename
        
        print *, "Testing pipe streaming with different formats..."
        
        ! Create simple test data
        do i = 1, 50
            x(i) = real(i-1, real64)
            y(i) = real(i-1, real64) ** 2
        end do
        
        call fig%initialize(640, 480)
        
        do i = 1, size(formats)
            write(filename, '(A,A,A)') "test_pipe_", trim(formats(i)), "." // trim(formats(i))
            
            anim = FuncAnimation(animate_parabola_pipe, frames=10, fig=fig)
            call anim%save(trim(filename), fps=5, status=status)
            
            if (status == 0) then
                print *, "PASS: Pipe streaming to", trim(formats(i)), "succeeded"
            else
                print *, "FAIL: Pipe streaming to", trim(formats(i)), "failed"
            end if
        end do
        
    end subroutine test_pipe_with_different_formats
    
    subroutine animate_sine_wave_pipe(frame)
        integer, intent(in) :: frame
        ! This will be defined separately
        print *, "animate_sine_wave_pipe called for frame", frame
    end subroutine animate_sine_wave_pipe
    
    subroutine animate_parabola_pipe(frame)
        integer, intent(in) :: frame
        ! This will be defined separately
        print *, "animate_parabola_pipe called for frame", frame
    end subroutine animate_parabola_pipe
    
    function int_to_str(num) result(str)
        integer, intent(in) :: num
        character(len=:), allocatable :: str
        character(len=20) :: temp
        
        write(temp, '(I0)') num
        str = trim(temp)
    end function int_to_str

end program test_animation_pipe