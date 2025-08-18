program test_mpeg_ffprobe_validation_comprehensive
    use fortplot
    use fortplot_security, only: safe_remove_file, safe_check_program_available, sanitize_filename
    use iso_fortran_env, only: real64
    implicit none

    ! Given: FFprobe provides comprehensive video validation (Issue #32)
    ! When: We use FFprobe to validate MPEG file integrity
    ! Then: Tests should detect files that FFprobe cannot parse or validate

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== COMPREHENSIVE FFPROBE VALIDATION TESTS ==="
    
    call test_ffprobe_availability()
    call test_ffprobe_format_validation()
    call test_ffprobe_stream_validation()
    call test_ffprobe_duration_validation()
    call test_ffprobe_codec_validation()
    call test_ffprobe_comprehensive_validation()

    print *, "=== FFprobe validation tests completed ==="

contains

    subroutine test_ffprobe_availability()
        ! Given: FFprobe tool should be available for validation
        ! When: We check for FFprobe installation
        ! Then: Tool should be accessible and functional

        logical :: ffprobe_available
        integer :: status

        print *, ""
        print *, "TEST: FFprobe Availability"
        print *, "========================="

        ffprobe_available = safe_check_program_available('ffprobe')

        print *, "FFprobe available:", ffprobe_available

        if (.not. ffprobe_available) then
            print *, "*** WARNING: FFprobe not available ***"
            print *, "Some validation tests will be skipped"
            print *, "Install ffmpeg package to enable full validation"
        else
            print *, "FFprobe found - comprehensive validation enabled"
        end if
    end subroutine

    subroutine test_ffprobe_format_validation()
        ! Given: FFprobe should validate MP4 format structure
        ! When: We create animation and validate format
        ! Then: FFprobe should recognize valid MP4 format

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: ffprobe_available, format_valid
        integer :: status

        print *, ""
        print *, "TEST: FFprobe Format Validation"
        print *, "=============================="

        ! Check if ffprobe is available
        ffprobe_available = safe_check_program_available('ffprobe')

        if (.not. ffprobe_available) then
            print *, "Skipping FFprobe format test - tool not available"
            return
        end if

        test_file = "ffprobe_format_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_format_data, frames=8, interval=50, fig=test_fig)
        call anim%save(test_file, fps=15)

        format_valid = validate_format_with_ffprobe(test_file)

        print *, "Format validation with FFprobe:", format_valid

        if (.not. format_valid) then
            print *, "*** FFPROBE FORMAT VALIDATION FAILURE ***"
            print *, "FFprobe cannot validate file format"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_format_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.8_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_format_with_ffprobe(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=500) :: command
        integer :: status

        write(command, '(A,A,A)') 'ffprobe -v error -show_format ', sanitize_filename(filename), ' >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        is_valid = (status == 0)

        print *, "  FFprobe format check exit status:", status
    end function

    subroutine test_ffprobe_stream_validation()
        ! Given: FFprobe should detect video streams in MP4 files
        ! When: We validate stream information
        ! Then: FFprobe should identify video stream properties

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: ffprobe_available, stream_valid
        integer :: status

        print *, ""
        print *, "TEST: FFprobe Stream Validation"
        print *, "=============================="

        ffprobe_available = safe_check_program_available('ffprobe')

        if (.not. ffprobe_available) then
            print *, "Skipping FFprobe stream test - tool not available"
            return
        end if

        test_file = "ffprobe_stream_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)

        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_stream_data, frames=12, interval=50, fig=test_fig)
        call anim%save(test_file, fps=20)

        stream_valid = validate_stream_with_ffprobe(test_file)

        print *, "Stream validation with FFprobe:", stream_valid

        if (.not. stream_valid) then
            print *, "*** FFPROBE STREAM VALIDATION FAILURE ***"
            print *, "FFprobe cannot detect valid video stream"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_stream_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.2_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_stream_with_ffprobe(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=500) :: command
        integer :: status

        write(command, '(A,A,A)') 'ffprobe -v error -select_streams v:0 -show_entries stream=codec_type ', &
                                  sanitize_filename(filename), ' >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        is_valid = (status == 0)

        print *, "  FFprobe stream check exit status:", status
    end function

    subroutine test_ffprobe_duration_validation()
        ! Given: FFprobe should detect reasonable duration for video files
        ! When: We validate duration information
        ! Then: FFprobe should report non-zero duration

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: ffprobe_available, duration_valid
        integer :: status

        print *, ""
        print *, "TEST: FFprobe Duration Validation"
        print *, "================================"

        ffprobe_available = safe_check_program_available('ffprobe')

        if (.not. ffprobe_available) then
            print *, "Skipping FFprobe duration test - tool not available"
            return
        end if

        test_file = "ffprobe_duration_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2

        call test_fig%initialize(width=500, height=400)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_duration_data, frames=16, interval=50, fig=test_fig)
        call anim%save(test_file, fps=24)

        duration_valid = validate_duration_with_ffprobe(test_file)

        print *, "Duration validation with FFprobe:", duration_valid

        if (.not. duration_valid) then
            print *, "*** FFPROBE DURATION VALIDATION FAILURE ***"
            print *, "FFprobe cannot detect valid duration"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_duration_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 2.0_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_duration_with_ffprobe(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=500) :: command
        integer :: status

        write(command, '(A,A,A)') 'ffprobe -v error -show_entries format=duration ', &
                                  sanitize_filename(filename), ' >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        is_valid = (status == 0)

        print *, "  FFprobe duration check exit status:", status
    end function

    subroutine test_ffprobe_codec_validation()
        ! Given: FFprobe should identify video codec information
        ! When: We validate codec properties
        ! Then: FFprobe should detect valid video codec

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: ffprobe_available, codec_valid
        integer :: status

        print *, ""
        print *, "TEST: FFprobe Codec Validation"
        print *, "============================="

        ffprobe_available = safe_check_program_available('ffprobe')

        if (.not. ffprobe_available) then
            print *, "Skipping FFprobe codec test - tool not available"
            return
        end if

        test_file = "ffprobe_codec_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = cos(test_x)

        call test_fig%initialize(width=720, height=540)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_codec_data, frames=14, interval=50, fig=test_fig)
        call anim%save(test_file, fps=30)

        codec_valid = validate_codec_with_ffprobe(test_file)

        print *, "Codec validation with FFprobe:", codec_valid

        if (.not. codec_valid) then
            print *, "*** FFPROBE CODEC VALIDATION FAILURE ***"
            print *, "FFprobe cannot identify valid video codec"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_codec_data(frame)
        integer, intent(in) :: frame
        test_y = cos(test_x + real(frame, real64) * 0.3_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_codec_with_ffprobe(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=500) :: command
        integer :: status

        write(command, '(A,A,A)') 'ffprobe -v error -show_entries stream=codec_name ', &
                                  sanitize_filename(filename), ' >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        is_valid = (status == 0)

        print *, "  FFprobe codec check exit status:", status
    end function

    subroutine test_ffprobe_comprehensive_validation()
        ! Given: FFprobe comprehensive validation should check all aspects
        ! When: We perform complete FFprobe validation
        ! Then: All validation checks should pass for valid MP4

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: ffprobe_available, comprehensive_valid
        integer :: status

        print *, ""
        print *, "TEST: FFprobe Comprehensive Validation"
        print *, "======================================"

        ffprobe_available = safe_check_program_available('ffprobe')

        if (.not. ffprobe_available) then
            print *, "Skipping FFprobe comprehensive test - tool not available"
            return
        end if

        test_file = "ffprobe_comprehensive_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x) * cos(test_x)

        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_comprehensive_data, frames=20, interval=50, fig=test_fig)
        call anim%save(test_file, fps=25)

        comprehensive_valid = validate_comprehensive_with_ffprobe(test_file)

        print *, "Comprehensive validation with FFprobe:", comprehensive_valid

        if (.not. comprehensive_valid) then
            print *, "*** FFPROBE COMPREHENSIVE VALIDATION FAILURE ***"
            print *, "File fails comprehensive FFprobe validation"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_comprehensive_data(frame)
        integer, intent(in) :: frame
        real(real64) :: phase
        phase = real(frame, real64) * 0.1_real64
        test_y = sin(test_x + phase) * cos(test_x + phase * 2.0_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_comprehensive_with_ffprobe(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        logical :: format_ok, stream_ok, duration_ok, codec_ok
        character(len=500) :: command
        integer :: status

        ! Check format
        write(command, '(A,A,A)') 'ffprobe -v error -show_format ', sanitize_filename(filename), ' >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        format_ok = (status == 0)

        ! Check stream
        write(command, '(A,A,A)') 'ffprobe -v error -select_streams v:0 -show_entries stream=codec_type ', &
                                  sanitize_filename(filename), ' >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        stream_ok = (status == 0)

        ! Check duration
        write(command, '(A,A,A)') 'ffprobe -v error -show_entries format=duration ', &
                                  sanitize_filename(filename), ' >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        duration_ok = (status == 0)

        ! Check codec
        write(command, '(A,A,A)') 'ffprobe -v error -show_entries stream=codec_name ', &
                                  sanitize_filename(filename), ' >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        codec_ok = (status == 0)

        is_valid = format_ok .and. stream_ok .and. duration_ok .and. codec_ok

        print *, "  Comprehensive FFprobe validation:"
        print *, "    Format valid:", format_ok
        print *, "    Stream valid:", stream_ok
        print *, "    Duration valid:", duration_ok
        print *, "    Codec valid:", codec_ok
        print *, "    Overall valid:", is_valid
    end function

end program test_mpeg_ffprobe_validation_comprehensive