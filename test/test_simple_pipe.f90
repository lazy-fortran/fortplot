program test_simple_pipe
    use fortplot
    use iso_fortran_env, only: real64
    implicit none
    
    type(figure_t) :: fig
    type(animation_t) :: anim
    real(real64), dimension(10) :: x, y
    integer :: i, status
    
    print *, "Testing simple pipe streaming..."
    
    ! Create simple test data
    do i = 1, 10
        x(i) = real(i-1, real64)
        y(i) = real(i-1, real64) ** 2
    end do
    
    ! Initialize figure
    call fig%initialize(400, 300)
    call fig%add_plot(x, y, label='test data')
    call fig%set_title('Pipe Test')
    
    ! Create animation with just 3 frames
    anim = FuncAnimation(animate_simple, frames=3, fig=fig)
    
    ! Test saving with pipe streaming
    call anim%save("test_simple_pipe.mp4", fps=2, status=status)
    
    if (status == 0) then
        print *, "SUCCESS: Pipe streaming to ffmpeg worked!"
    else
        print *, "FAILED: Pipe streaming failed with status:", status
    end if
    
contains
    
    subroutine animate_simple(frame)
        integer, intent(in) :: frame
        character(len=20) :: title
        real(real64), dimension(10) :: frame_y
        
        write(title, '(A,I0)') "Frame ", frame
        
        ! Update the data for this frame
        frame_y = y * real(frame, real64)
        call fig%set_ydata(1, frame_y)
        call fig%set_title(trim(title))
    end subroutine animate_simple

end program test_simple_pipe