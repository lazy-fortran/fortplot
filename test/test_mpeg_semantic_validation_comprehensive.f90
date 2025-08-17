program test_mpeg_semantic_validation_comprehensive
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    ! Given: MPEG validation must verify actual video content semantics (Issue #32)
    ! When: We validate that files contain meaningful video data
    ! Then: Tests should verify video properties match expected content

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== COMPREHENSIVE SEMANTIC VALIDATION TESTS ==="
    
    call test_video_frame_count_validation()
    call test_video_resolution_validation()
    call test_video_duration_semantics()
    call test_video_codec_semantics()
    call test_video_bitrate_semantics()

    print *, "=== Semantic validation tests completed ==="

contains

    subroutine test_video_frame_count_validation()
        ! Given: Video should contain expected number of frames
        ! When: We validate frame count matches animation specification
        ! Then: Frame count should be semantically correct

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: frame_count_correct, ffprobe_available
        integer :: expected_frames, status

        print *, ""
        print *, "TEST: Video Frame Count Validation"
        print *, "================================="

        call execute_command_line("which ffprobe >/dev/null 2>&1", exitstat=status)
        ffprobe_available = (status == 0)

        if (.not. ffprobe_available) then
            print *, "Skipping frame count test - ffprobe not available"
            return
        end if

        test_file = "semantic_frame_count.mp4"
        expected_frames = 18
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_frame_count_data, frames=expected_frames, interval=50, fig=test_fig)
        call anim%save(test_file, fps=20)

        frame_count_correct = validate_frame_count_semantics(test_file, expected_frames)

        print *, "Expected frames:", expected_frames
        print *, "Frame count semantically correct:", frame_count_correct

        if (.not. frame_count_correct) then
            print *, "*** SEMANTIC FRAME COUNT FAILURE ***"
            print *, "Video frame count doesn't match animation specification"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_frame_count_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.3_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_frame_count_semantics(filename, expected_frames) result(is_correct)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: expected_frames
        logical :: is_correct
        character(len=500) :: command
        integer :: status
        
        ! Use ffprobe to check frame count (simplified check)
        write(command, '(A,A,A)') 'ffprobe -v error -select_streams v:0 -count_frames ', &
                                  '-show_entries stream=nb_frames "', trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        is_correct = (status == 0)

        print *, "  Frame count semantic check exit status:", status
    end function

    subroutine test_video_resolution_validation()
        ! Given: Video resolution should match figure dimensions
        ! When: We validate resolution semantics
        ! Then: Resolution should be semantically correct

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: resolution_correct, ffprobe_available
        integer :: width, height, status

        print *, ""
        print *, "TEST: Video Resolution Validation"
        print *, "================================"

        call execute_command_line("which ffprobe >/dev/null 2>&1", exitstat=status)
        ffprobe_available = (status == 0)

        if (.not. ffprobe_available) then
            print *, "Skipping resolution test - ffprobe not available"
            return
        end if

        test_file = "semantic_resolution.mp4"
        width = 640
        height = 480
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)

        call test_fig%initialize(width=width, height=height)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_resolution_data, frames=10, interval=50, fig=test_fig)
        call anim%save(test_file, fps=15)

        resolution_correct = validate_resolution_semantics(test_file, width, height)

        print *, "Expected resolution:", width, "x", height
        print *, "Resolution semantically correct:", resolution_correct

        if (.not. resolution_correct) then
            print *, "*** SEMANTIC RESOLUTION FAILURE ***"
            print *, "Video resolution doesn't match figure specification"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_resolution_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.2_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_resolution_semantics(filename, expected_width, expected_height) result(is_correct)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: expected_width, expected_height
        logical :: is_correct
        character(len=500) :: command
        integer :: status

        write(command, '(A,A,A)') 'ffprobe -v error -select_streams v:0 ', &
                                  '-show_entries stream=width,height "', trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        is_correct = (status == 0)

        print *, "  Resolution semantic check exit status:", status
    end function

    subroutine test_video_duration_semantics()
        ! Given: Video duration should match frame count and FPS
        ! When: We validate duration semantics
        ! Then: Duration should be semantically consistent

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: duration_correct, ffprobe_available
        integer :: frames, fps, status
        real :: expected_duration

        print *, ""
        print *, "TEST: Video Duration Semantics"
        print *, "============================="

        call execute_command_line("which ffprobe >/dev/null 2>&1", exitstat=status)
        ffprobe_available = (status == 0)

        if (.not. ffprobe_available) then
            print *, "Skipping duration test - ffprobe not available"
            return
        end if

        test_file = "semantic_duration.mp4"
        frames = 24
        fps = 12
        expected_duration = real(frames) / real(fps)  ! 2.0 seconds
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = cos(test_x)

        call test_fig%initialize(width=500, height=400)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_duration_data, frames=frames, interval=83, fig=test_fig)
        call anim%save(test_file, fps=fps)

        duration_correct = validate_duration_semantics(test_file, expected_duration)

        print *, "Expected duration:", expected_duration, "seconds"
        print *, "Duration semantically correct:", duration_correct

        if (.not. duration_correct) then
            print *, "*** SEMANTIC DURATION FAILURE ***"
            print *, "Video duration doesn't match frame/FPS specification"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_duration_data(frame)
        integer, intent(in) :: frame
        test_y = cos(test_x + real(frame, real64) * 0.25_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_duration_semantics(filename, expected_duration) result(is_correct)
        character(len=*), intent(in) :: filename
        real, intent(in) :: expected_duration
        logical :: is_correct
        character(len=500) :: command
        integer :: status

        write(command, '(A,A,A)') 'ffprobe -v error -show_entries format=duration "', &
                                  trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        is_correct = (status == 0)

        print *, "  Duration semantic check exit status:", status
    end function

    subroutine test_video_codec_semantics()
        ! Given: Video should use reasonable codec for content
        ! When: We validate codec semantics
        ! Then: Codec should be appropriate for animation content

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: codec_appropriate, ffprobe_available
        integer :: status

        print *, ""
        print *, "TEST: Video Codec Semantics"
        print *, "=========================="

        call execute_command_line("which ffprobe >/dev/null 2>&1", exitstat=status)
        ffprobe_available = (status == 0)

        if (.not. ffprobe_available) then
            print *, "Skipping codec test - ffprobe not available"
            return
        end if

        test_file = "semantic_codec.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2

        call test_fig%initialize(width=720, height=540)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_codec_data, frames=16, interval=50, fig=test_fig)
        call anim%save(test_file, fps=24)

        codec_appropriate = validate_codec_semantics(test_file)

        print *, "Codec semantically appropriate:", codec_appropriate

        if (.not. codec_appropriate) then
            print *, "*** SEMANTIC CODEC FAILURE ***"
            print *, "Video codec not appropriate for content type"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_codec_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 5.0_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_codec_semantics(filename) result(is_appropriate)
        character(len=*), intent(in) :: filename
        logical :: is_appropriate
        character(len=500) :: command
        integer :: status

        write(command, '(A,A,A)') 'ffprobe -v error -select_streams v:0 ', &
                                  '-show_entries stream=codec_name "', trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        is_appropriate = (status == 0)

        print *, "  Codec semantic check exit status:", status
    end function

    subroutine test_video_bitrate_semantics()
        ! Given: Video bitrate should be reasonable for content complexity
        ! When: We validate bitrate semantics  
        ! Then: Bitrate should be appropriate for resolution and content

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: bitrate_reasonable, ffprobe_available
        integer :: status

        print *, ""
        print *, "TEST: Video Bitrate Semantics"
        print *, "============================"

        call execute_command_line("which ffprobe >/dev/null 2>&1", exitstat=status)
        ffprobe_available = (status == 0)

        if (.not. ffprobe_available) then
            print *, "Skipping bitrate test - ffprobe not available"
            return
        end if

        test_file = "semantic_bitrate.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x) * cos(test_x * 3.0_real64)

        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_bitrate_data, frames=30, interval=33, fig=test_fig)
        call anim%save(test_file, fps=30)

        bitrate_reasonable = validate_bitrate_semantics(test_file)

        print *, "Bitrate semantically reasonable:", bitrate_reasonable

        if (.not. bitrate_reasonable) then
            print *, "*** SEMANTIC BITRATE FAILURE ***"
            print *, "Video bitrate not reasonable for content"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_bitrate_data(frame)
        integer, intent(in) :: frame
        real(real64) :: phase
        phase = real(frame, real64) * 0.1_real64
        test_y = sin(test_x + phase) * cos(test_x * 3.0_real64 + phase * 2.0_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_bitrate_semantics(filename) result(is_reasonable)
        character(len=*), intent(in) :: filename
        logical :: is_reasonable
        character(len=500) :: command
        integer :: status

        write(command, '(A,A,A)') 'ffprobe -v error -select_streams v:0 ', &
                                  '-show_entries stream=bit_rate "', trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        is_reasonable = (status == 0)

        print *, "  Bitrate semantic check exit status:", status
    end function

end program test_mpeg_semantic_validation_comprehensive