program test_animation_save
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    ! Module variables for accessing from nested procedures
    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    
    call test_save_animation_mp4()
    call test_save_animation_avi()
    call test_save_animation_mkv()
    call test_save_animation_with_fps()
    call test_save_animation_invalid_format()
    call test_save_animation_ffmpeg_not_found()
    call test_save_animation_frame_sequence()

    print *, "All animation save tests passed!"

contains

    subroutine test_save_animation_mp4()
        type(animation_t) :: anim
        integer :: i
        logical :: file_exists
        character(len=200) :: test_file

        test_file = "test_animation.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_test_data_mp4, frames=5, interval=50, fig=test_fig)
        
        call anim%save(test_file)
        
        inquire(file=test_file, exist=file_exists)
        if (.not. file_exists) then
            error stop "test_save_animation_mp4: Failed to create MP4 file"
        end if
        
        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_test_data_mp4(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_save_animation_avi()
        type(animation_t) :: anim
        integer :: i
        logical :: file_exists
        character(len=200) :: test_file

        test_file = "test_animation.avi"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_sine_data, frames=5, interval=50, fig=test_fig)
        
        call anim%save(test_file)
        
        inquire(file=test_file, exist=file_exists)
        if (.not. file_exists) then
            error stop "test_save_animation_avi: Failed to create AVI file"
        end if
        
        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_sine_data(frame)
        integer, intent(in) :: frame
        real(real64) :: phase
        phase = real(frame, real64) * 0.1_real64
        test_y = sin(test_x + phase)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_save_animation_mkv()
        type(animation_t) :: anim
        integer :: i
        logical :: file_exists
        character(len=200) :: test_file

        test_file = "test_animation.mkv"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = cos(test_x)
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_cosine_data, frames=5, interval=50, fig=test_fig)
        
        call anim%save(test_file)
        
        inquire(file=test_file, exist=file_exists)
        if (.not. file_exists) then
            error stop "test_save_animation_mkv: Failed to create MKV file"
        end if
        
        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_cosine_data(frame)
        integer, intent(in) :: frame
        real(real64) :: phase
        phase = real(frame, real64) * 0.1_real64
        test_y = cos(test_x + phase)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_save_animation_with_fps()
        type(animation_t) :: anim
        integer :: i
        logical :: file_exists
        character(len=200) :: test_file

        test_file = "test_animation_fps.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_linear_data, frames=10, interval=50, fig=test_fig)
        
        call anim%save(test_file, fps=30)
        
        inquire(file=test_file, exist=file_exists)
        if (.not. file_exists) then
            error stop "test_save_animation_with_fps: Failed to create MP4 file with custom fps"
        end if
        
        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_linear_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_save_animation_invalid_format()
        type(animation_t) :: anim
        integer :: i
        integer :: status

        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_update, frames=5, interval=50, fig=test_fig)
        
        call anim%save("test.txt", status=status)
        
        if (status == 0) then
            error stop "test_save_animation_invalid_format: Should have failed for unsupported format"
        end if
    end subroutine

    subroutine dummy_update(frame)
        integer, intent(in) :: frame
        ! Do nothing
    end subroutine

    subroutine test_save_animation_ffmpeg_not_found()
        type(animation_t) :: anim
        integer :: i
        integer :: status
        character(len=200) :: original_path, test_path

        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_update2, frames=5, interval=50, fig=test_fig)
        
        call get_environment_variable("PATH", original_path)
        test_path = "/nonexistent/path"
        call execute_command_line("export PATH=" // trim(test_path))
        
        call anim%save("test.mp4", status=status)
        
        call execute_command_line("export PATH=" // trim(original_path))
    end subroutine

    subroutine dummy_update2(frame)
        integer, intent(in) :: frame
        ! Do nothing
    end subroutine

    subroutine test_save_animation_frame_sequence()
        type(animation_t) :: anim
        integer :: i
        logical :: file_exists

        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_quad_data, frames=3, interval=50, fig=test_fig)
        
        call anim%save_frame_sequence("test_frame_")
        
        do i = 0, 2
            write(*, '(A,I0,A)') "Checking test_frame_", i, ".png"
            inquire(file="test_frame_" // char(48+i) // ".png", exist=file_exists)
            if (.not. file_exists) then
                error stop "test_save_animation_frame_sequence: Failed to create frame sequence"
            end if
            call execute_command_line("rm -f test_frame_" // char(48+i) // ".png")
        end do
    end subroutine

    subroutine update_quad_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 * real(frame + 1, real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

end program test_animation_save