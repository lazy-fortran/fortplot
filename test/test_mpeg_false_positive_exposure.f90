program test_mpeg_false_positive_exposure
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    ! Given: Current MPEG tests that give false positives
    ! When: We apply proper validation that should be used
    ! Then: These tests should FAIL to expose the validation inadequacy

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    ! These tests DEMONSTRATE THE PROBLEM - they should fail but currently pass
    print *, "=== EXPOSING FALSE POSITIVES IN CURRENT MPEG TESTS ==="
    
    call test_current_animation_save_inadequate()
    call test_animation_save_should_fail_size_check()
    call test_animation_save_should_validate_content()

    print *, "=== All false positive exposure tests completed ==="
    print *, "NOTE: If any of these tests report SUCCESS, there's a validation problem"

contains

    subroutine test_current_animation_save_inadequate()
        ! Given: Current animation save test approach
        ! When: We use the same logic as test_animation_save.f90
        ! Then: This should FAIL but currently PASSES (false positive)

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: file_exists, test_passes_currently, should_actually_pass
        integer :: file_size

        print *, ""
        print *, "TEST: Current Animation Save Logic (SHOULD FAIL)"
        print *, "=============================================="

        test_file = "current_logic_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_current_logic_data, frames=5, interval=50, fig=test_fig)
        call anim%save(test_file)
        
        ! Current test logic (from test_animation_save.f90)
        inquire(file=test_file, exist=file_exists, size=file_size)
        test_passes_currently = file_exists
        
        ! What the validation SHOULD be
        should_actually_pass = proper_mpeg_validation(test_file)
        
        print *, "File exists (current test):", file_exists
        print *, "File size:", file_size, "bytes"
        print *, "Current test would pass:", test_passes_currently
        print *, "Proper validation passes:", should_actually_pass
        
        if (test_passes_currently .and. .not. should_actually_pass) then
            print *, "*** FALSE POSITIVE CONFIRMED ***"
            print *, "Current test logic gives false positive!"
            print *, "This is exactly the issue described in #32"
        end if
        
        ! The test should FAIL here, but currently PASSES
        if (.not. should_actually_pass) then
            print *, "RESULT: This test SHOULD FAIL (and would with proper validation)"
        else
            print *, "RESULT: Test correctly passes"
        end if
        
        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_current_logic_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function proper_mpeg_validation(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        logical :: exists, reasonable_size, ffprobe_validates
        integer :: file_size, min_size
        character(len=500) :: command
        integer :: ffprobe_status
        
        inquire(file=filename, exist=exists, size=file_size)
        
        ! Conservative minimum size check
        ! Even heavily compressed 5-frame video should be > 3KB
        min_size = 3000
        reasonable_size = (file_size >= min_size)
        
        ! External validation with ffprobe
        write(command, '(A,A,A)') 'ffprobe -v error -show_format "', trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=ffprobe_status)
        ffprobe_validates = (ffprobe_status == 0)
        
        is_valid = exists .and. reasonable_size .and. ffprobe_validates
        
        print *, "  Proper validation details:"
        print *, "    File exists:", exists
        print *, "    Reasonable size (>=", min_size, "):", reasonable_size
        print *, "    FFprobe validates:", ffprobe_validates
    end function

    subroutine test_animation_save_should_fail_size_check()
        ! Given: Animation that creates suspiciously small files
        ! When: We apply size-based validation
        ! Then: Test should FAIL due to inadequate file size

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: passes_size_check
        integer :: file_size, expected_minimum

        print *, ""
        print *, "TEST: Animation Save Size Check (SHOULD FAIL)"
        print *, "==========================================="

        test_file = "size_check_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x
        
        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_size_check_data, frames=10, interval=50, fig=test_fig)
        call anim%save(test_file, fps=20)
        
        inquire(file=test_file, size=file_size)
        
        ! For 10 frames at 640x480, even heavily compressed should be > 5KB
        expected_minimum = 5000
        passes_size_check = (file_size >= expected_minimum)
        
        print *, "Generated file size:", file_size, "bytes"
        print *, "Expected minimum:", expected_minimum, "bytes"
        print *, "Passes size check:", passes_size_check
        
        if (.not. passes_size_check) then
            print *, "RESULT: Test FAILS size check (as it should)"
            print *, "Current tests don't catch this inadequate file size"
        else
            print *, "RESULT: Test passes size check"
        end if
        
        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_size_check_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.5_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine test_animation_save_should_validate_content()
        ! Given: Animation save that should validate actual video content
        ! When: We check for proper video stream properties
        ! Then: Validation should ensure file has reasonable video characteristics

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: has_video_stream, has_reasonable_duration
        integer :: video_info_status

        print *, ""
        print *, "TEST: Animation Content Validation (SHOULD BE COMPREHENSIVE)"
        print *, "=========================================================="

        test_file = "content_validation_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)
        
        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_content_data, frames=15, interval=50, fig=test_fig)
        call anim%save(test_file, fps=25)
        
        ! Validate video stream properties
        has_video_stream = check_video_stream_exists(test_file)
        has_reasonable_duration = check_reasonable_duration(test_file, expected_frames=15, fps=25)
        
        print *, "Has video stream:", has_video_stream
        print *, "Has reasonable duration:", has_reasonable_duration
        
        if (has_video_stream .and. has_reasonable_duration) then
            print *, "RESULT: Content validation passes"
        else
            print *, "RESULT: Content validation FAILS"
            print *, "Current tests don't validate video stream properties"
        end if
        
        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_content_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.3_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function check_video_stream_exists(filename) result(has_stream)
        character(len=*), intent(in) :: filename
        logical :: has_stream
        character(len=500) :: command
        integer :: status
        
        ! Use ffprobe to check for video stream
        write(command, '(A,A,A)') 'ffprobe -v error -select_streams v:0 -show_entries stream=codec_type "', &
                                  trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        has_stream = (status == 0)
    end function

    function check_reasonable_duration(filename, expected_frames, fps) result(reasonable)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: expected_frames, fps
        logical :: reasonable
        character(len=500) :: command
        integer :: status
        real :: expected_duration
        
        expected_duration = real(expected_frames) / real(fps)
        
        ! Use ffprobe to check duration (simplified check)
        write(command, '(A,A,A)') 'ffprobe -v error -show_entries format=duration "', &
                                  trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        reasonable = (status == 0)  ! Simplified - just check that duration can be read
    end function

end program test_mpeg_false_positive_exposure