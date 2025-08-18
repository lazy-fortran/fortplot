program test_animation_ffmpeg_availability
    use fortplot
    use fortplot_pipe, only: check_ffmpeg_available
    use fortplot_security, only: safe_remove_file
    use iso_fortran_env, only: real64
    implicit none

    call test_ffmpeg_detection()
    call test_animation_graceful_degradation()
    call test_animation_fallback_png_sequence()

    ! XFAIL: FFmpeg not available in CI environment - Issue #104
    print *, "All ffmpeg availability tests passed!"

contains

    subroutine test_ffmpeg_detection()
        ! Given: System may or may not have ffmpeg installed
        ! When: We check ffmpeg availability
        ! Then: Function should return logical result without crashing
        
        logical :: ffmpeg_available
        
        ffmpeg_available = check_ffmpeg_available()
        
        ! Test passes regardless of result - we just verify the function works
        if (ffmpeg_available) then
            print *, "FFMPEG DETECTION: ffmpeg is available"
        else
            print *, "FFMPEG DETECTION: ffmpeg is not available (graceful degradation)"
        end if
        
        ! Function call completed successfully
        print *, "✓ PASS: ffmpeg availability detection completed without crash"
    end subroutine test_ffmpeg_detection

    subroutine test_animation_graceful_degradation()
        ! Given: Animation save attempt on system that may not have ffmpeg
        ! When: We try to save animation with status monitoring
        ! Then: Should handle missing ffmpeg gracefully with proper status code
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(5) :: test_x, test_y
        integer :: save_status, i
        logical :: ffmpeg_available
        
        ! Setup test data
        test_x = [(real(i, real64), i=1,5)]
        test_y = test_x**2
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate, frames=3, interval=50, fig=test_fig)
        
        ! Test animation save with status monitoring
        call anim%save("test_graceful.mp4", status=save_status)
        
        ffmpeg_available = check_ffmpeg_available()
        
        if (ffmpeg_available) then
            ! If ffmpeg is available, save should succeed
            if (save_status /= 0) then
                error stop "test_animation_graceful_degradation: Save failed despite ffmpeg being available"
            end if
            print *, "✓ PASS: Animation saved successfully with ffmpeg available"
            ! Cleanup successful save
            block
                logical :: remove_success
                call safe_remove_file("test_graceful.mp4", remove_success)
                if (.not. remove_success) then
                    print *, "Warning: Could not remove temporary file: test_graceful.mp4"
                end if
            end block
        else
            ! If ffmpeg is not available, should get specific error code
            if (save_status /= -1) then
                error stop "test_animation_graceful_degradation: Expected status -1 for missing ffmpeg"
            end if
            print *, "✓ PASS: Animation save gracefully handled missing ffmpeg (status: ", save_status, ")"
        end if
    end subroutine test_animation_graceful_degradation

    subroutine test_animation_fallback_png_sequence()
        ! Given: Animation that needs to be saved regardless of ffmpeg availability
        ! When: We use PNG sequence fallback instead of video format
        ! Then: Should always work as PNG doesn't require ffmpeg
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(3) :: test_x, test_y
        integer :: i
        logical :: frame_exists
        character(len=50) :: frame_filename
        
        ! Setup test data
        test_x = [(real(i, real64), i=1,3)]
        test_y = test_x
        
        call test_fig%initialize(width=200, height=150)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate, frames=2, interval=50, fig=test_fig)
        
        ! Save as PNG sequence (should always work)
        call anim%save_frame_sequence("test_fallback_frame_")
        
        ! Verify first frame was created
        frame_filename = "test_fallback_frame_0.png"
        inquire(file=frame_filename, exist=frame_exists)
        if (.not. frame_exists) then
            error stop "test_animation_fallback_png_sequence: PNG sequence fallback failed"
        end if
        
        print *, "✓ PASS: PNG sequence fallback works regardless of ffmpeg availability"
        
        ! Cleanup PNG files - in secure mode, user must clean manually
        print *, "Note: Please manually clean up test_fallback_frame_*.png files"
    end subroutine test_animation_fallback_png_sequence

    subroutine dummy_animate(frame)
        integer, intent(in) :: frame
        ! Minimal animation callback for testing
        continue
    end subroutine dummy_animate

end program test_animation_ffmpeg_availability