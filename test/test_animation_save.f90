program test_animation_save
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    ! Module variables for accessing from nested procedures
    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    
    call test_save_animation_mp4()
    call test_save_animation_with_fps()
    call test_save_animation_invalid_format()

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


end program test_animation_save