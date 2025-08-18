program test_mpeg_validation_debug
    use fortplot
    use fortplot_security, only: safe_remove_file
    use iso_fortran_env, only: real64
    implicit none

    ! Given: We need to investigate why MPEG files are only 624 bytes
    ! When: We create an animation and check file size
    ! Then: We should see proper file size validation

    type(figure_t) :: test_fig
    type(animation_t) :: anim
    real(real64), dimension(10) :: test_x, test_y
    character(len=200) :: test_file
    logical :: file_exists
    integer :: file_size, file_unit, ios, status
    integer :: i

    call test_check_actual_mpeg_file_size()
    call test_check_ffmpeg_availability()
    call test_mpeg_validation_with_status_codes()

    print *, "MPEG validation debug tests completed"

contains

    subroutine test_check_actual_mpeg_file_size()
        ! Given: Animation system that should create valid MPEG files
        test_file = "debug_animation.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_debug_data, frames=5, interval=50, fig=test_fig)
        
        ! When: We save the animation
        call anim%save(test_file, status=status)
        print *, "Animation save status:", status
        
        ! Then: Check file existence and size
        inquire(file=test_file, exist=file_exists, size=file_size)
        
        print *, "File exists:", file_exists
        print *, "File size (bytes):", file_size
        
        if (file_exists) then
            if (file_size < 1000) then
                print *, "WARNING: File size suspiciously small for video file"
                print *, "Expected: >10KB for valid MPEG, Got:", file_size, "bytes"
            else
                print *, "File size appears reasonable for video content"
            end if
        end if
        
        ! Clean up
        if (file_exists) then
            block
                logical :: remove_success
                call safe_remove_file(test_file, remove_success)
                if (.not. remove_success) then
                    print *, "Warning: Could not remove temporary file: " // trim(test_file)
                        end if
    end block
        end if
    end subroutine

    subroutine update_debug_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 10.0_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_check_ffmpeg_availability()
        ! Given: System that requires ffmpeg for MPEG generation
        logical :: ffmpeg_available
        
        ! When: We check ffmpeg availability
        ffmpeg_available = check_ffmpeg_available_external()
        
        ! Then: Report the status
        print *, "FFmpeg available:", ffmpeg_available
        
        if (.not. ffmpeg_available) then
            print *, "WARNING: FFmpeg not available - MPEG generation will fail"
        end if
    end subroutine

    function check_ffmpeg_available_external() result(available)
        use fortplot_pipe, only: check_ffmpeg_available
        logical :: available
        
        available = check_ffmpeg_available()
    end function

    subroutine test_mpeg_validation_with_status_codes()
        ! Given: Animation save with various scenarios
        character(len=200) :: invalid_file
        real(real64), dimension(5) :: test_x_small, test_y_small
        
        invalid_file = "test_invalid.xyz"
        
        test_x_small = [(real(i, real64), i=1,5)]
        test_y_small = test_x_small
        
        call test_fig%initialize(width=200, height=150)
        call test_fig%add_plot(test_x_small, test_y_small)
        
        anim = FuncAnimation(dummy_update, frames=3, interval=50, fig=test_fig)
        
        ! When: We try to save with invalid format
        call anim%save(invalid_file, status=status)
        
        ! Then: Status should indicate failure
        print *, "Invalid format save status:", status
        
        if (status == 0) then
            print *, "ERROR: Should have failed for invalid format"
        else
            print *, "Correctly rejected invalid format"
        end if
    end subroutine

    subroutine dummy_update(frame)
        integer, intent(in) :: frame
        ! Minimal update for testing
    end subroutine

end program test_mpeg_validation_debug