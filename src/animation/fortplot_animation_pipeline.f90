module fortplot_animation_pipeline
    use iso_fortran_env, only: real64, wp => real64
    use fortplot_animation_core, only: animation_t, DEFAULT_FRAME_INTERVAL_MS, DEFAULT_ANIMATION_FPS, &
        MIN_VALID_VIDEO_SIZE, MIN_EXPECTED_VIDEO_SIZE, MAX_FILENAME_LENGTH, &
        MAX_RETRY_ATTEMPTS, BASE_RETRY_DELAY_MS
    use fortplot_constants, only: MILLISECONDS_PER_SECOND
    use fortplot_animation_rendering, only: render_frame_to_png
    use fortplot_animation_validation, only: validate_generated_video_enhanced
    use fortplot_pipe, only: open_ffmpeg_pipe, write_png_to_pipe, close_ffmpeg_pipe
    use fortplot_logging, only: log_error, log_info, log_warning
    implicit none
    private

    public :: save_animation_with_pipeline
    public :: save_animation_full

contains

    subroutine save_animation_full(anim, filename, fps, status)
        use fortplot_animation_validation, only: is_supported_video_format
        use fortplot_utils, only: ensure_directory_exists
        
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in), optional :: fps
        integer, intent(out), optional :: status
        
        integer :: actual_fps, stat
        
        if (.not. is_supported_video_format(filename)) then
            if (present(status)) status = -3
            call log_error_with_remediation("Unsupported file format: " // trim(filename), &
                                           "Change filename extension to .mp4, .avi, or .mkv")
            return
        end if
        
        if (.not. check_ffmpeg_available()) then
            ! PNG sequence fallback when FFmpeg not available
            call log_warning("FFmpeg not found - falling back to PNG sequence")
            call save_with_png_sequence_fallback(anim, filename, stat)
            if (present(status)) status = stat
            return
        end if
        
        actual_fps = get_fps_or_default(fps)
        call ensure_directory_exists(filename)
        call save_animation_with_pipeline(anim, filename, actual_fps, stat)
        
        ! If FFmpeg fails, fallback to PNG sequence
        if (stat /= 0) then
            call log_warning("FFmpeg animation failed - falling back to PNG sequence")
            call save_with_png_sequence_fallback(anim, filename, stat)
        end if
        
        if (present(status)) status = stat
    end subroutine save_animation_full

    function check_ffmpeg_available() result(available)
        use fortplot_pipe, only: check_ffmpeg_available_pipe => check_ffmpeg_available
        logical :: available
        
        available = check_ffmpeg_available_pipe()
    end function check_ffmpeg_available

    function get_fps_or_default(fps) result(actual_fps)
        integer, intent(in), optional :: fps
        integer :: actual_fps
        
        if (present(fps)) then
            actual_fps = fps
        else
            actual_fps = DEFAULT_ANIMATION_FPS
        end if
    end function get_fps_or_default

    subroutine save_with_png_sequence_fallback(anim, filename, status)
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        
        character(len=:), allocatable :: base_name, pattern
        integer :: dot_pos
        
        ! Extract base name from video filename
        dot_pos = index(filename, ".", back=.true.)
        if (dot_pos > 0) then
            base_name = filename(1:dot_pos-1)
        else
            base_name = filename
        end if
        
        ! Create PNG sequence pattern
        pattern = base_name // "_frame_"
        
        call log_info("Saving PNG sequence: " // pattern // "*.png")
        call anim%save_frame_sequence(pattern)
        
        status = 0  ! PNG sequence always succeeds if frames can be generated
    end subroutine save_with_png_sequence_fallback

    subroutine save_animation_with_pipeline(anim, filename, fps, status)
        class(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer, intent(out) :: status
        
        integer :: open_stat, close_stat
        
        ! Enhanced pipe opening with better error reporting
        open_stat = open_ffmpeg_pipe(filename, fps)
        if (open_stat /= 0) then
            status = -4
            call log_error_with_remediation("Could not open pipe to FFmpeg", &
                                           "Check that FFmpeg is installed and accessible in PATH, " // &
                                           "or set FORTPLOT_ENABLE_FFMPEG=1 environment variable")
            return
        end if
        
        ! Write frames with enhanced error handling
        call write_all_frames_to_pipe(anim, status)
        if (status /= 0) then
            close_stat = close_ffmpeg_pipe()  ! Always try to close
            return
        end if
        
        ! Enhanced pipe closing with better status handling
        close_stat = close_ffmpeg_pipe()
        if (close_stat /= 0) then
            call log_warning("Pipe close returned non-zero status, but continuing validation")
        end if
        
        ! Validate output with enhanced checking
        call validate_output_video_enhanced(filename, status)
    end subroutine save_animation_with_pipeline

    subroutine write_all_frames_to_pipe(anim, status)
        class(animation_t), intent(inout) :: anim
        integer, intent(out) :: status
        
        integer :: frame_idx, stat
        integer(1), allocatable :: png_data(:)
        
        do frame_idx = 1, anim%frames
            call generate_png_frame_data(anim, frame_idx, png_data, stat)
            if (stat /= 0) then
                status = -5
                call log_error_with_remediation("Failed to generate frame data", &
                                               "Check figure data validity and ensure backend is properly initialized")
                return
            end if
            
            ! Enhanced pipe write with exponential backoff retry mechanism
            call write_frame_with_exponential_backoff(png_data, stat)
            if (stat /= 0) then
                status = -6
                call log_error_with_remediation("Failed to write frame to pipe after retry attempts", &
                                               "FFmpeg pipe may be broken. Try reducing animation complexity " // &
                                               "or increasing system memory")
                return
            end if
            
            if (allocated(png_data)) deallocate(png_data)
        end do
        
        status = 0
    end subroutine write_all_frames_to_pipe

    subroutine write_frame_with_exponential_backoff(png_data, status)
        integer(1), allocatable, intent(in) :: png_data(:)
        integer, intent(out) :: status
        
        integer :: attempt, write_stat, delay_ms
        
        status = -6  ! Default to pipe write failure
        
        do attempt = 1, MAX_RETRY_ATTEMPTS
            write_stat = write_png_to_pipe(png_data)
            
            if (write_stat == 0) then
                ! Success - frame written successfully
                status = 0
                if (attempt > 1) then
                    call log_info("Frame write succeeded after enhanced recovery")
                end if
                return
            else if (write_stat == -6) then
                ! Status -6: Pipe flush failed - enhanced recovery with backoff
                delay_ms = BASE_RETRY_DELAY_MS * (2 ** (attempt - 1))
                
                if (attempt < MAX_RETRY_ATTEMPTS) then
                    write(*, '(A,I0,A,I0,A,I0,A)') &
                        "Pipe write failed - attempting recovery (retry ", &
                        attempt, "/", MAX_RETRY_ATTEMPTS, &
                        " with ", delay_ms, "ms exponential backoff)..."
                    call exponential_backoff_delay(delay_ms)
                else
                    call log_error_with_remediation("Pipe write failed after all retry attempts", &
                                                   "FFmpeg pipe is unresponsive. Check system resources or try PNG fallback")
                end if
            else
                ! Other write errors - immediate failure
                write(*, '(A,I0)') "Pipe write failed with unrecoverable error: ", write_stat
                status = write_stat
                return
            end if
        end do
    end subroutine write_frame_with_exponential_backoff

    subroutine exponential_backoff_delay(delay_ms)
        integer, intent(in) :: delay_ms
        real(wp) :: delay_seconds
        
        delay_seconds = real(delay_ms, wp) / MILLISECONDS_PER_SECOND
        call cpu_time_delay(delay_seconds)
    end subroutine exponential_backoff_delay

    subroutine validate_output_video_enhanced(filename, status)
        character(len=*), intent(in) :: filename
        integer, intent(out) :: status
        
        logical :: file_exists
        integer :: file_size
        
        ! Basic file existence and size check first
        inquire(file=filename, exist=file_exists, size=file_size)
        
        if (.not. file_exists) then
            status = -7
            call log_error_with_remediation("Output file was not created: " // trim(filename), &
                                           "Check directory permissions and disk space availability")
            return
        end if
        
        if (file_size <= 0) then
            status = -8
            call log_error_with_remediation("Output file exists but has invalid size", &
                                           "FFmpeg may have encountered an error. Check FFmpeg installation and codecs")
            return
        end if
        
        ! Enhanced validation for video content
        if (validate_generated_video_enhanced(filename, file_size)) then
            status = 0
            call log_info("Video validation successful")
        else
            status = -7
            call log_error_with_remediation("Generated video failed content validation", &
                                           "Video format may be corrupted. Try different format (.mp4, .avi, .mkv) " // &
                                           "or check FFmpeg codec support")
        end if
    end subroutine validate_output_video_enhanced

    subroutine generate_png_frame_data(anim, frame_idx, png_data, status)
        class(animation_t), intent(inout) :: anim
        integer, intent(in) :: frame_idx
        integer(1), allocatable, intent(out) :: png_data(:)
        integer, intent(out) :: status
        
        if (.not. associated(anim%fig)) then
            status = -1
            return
        end if
        
        call update_frame_data(anim, frame_idx)
        call render_frame_to_png(anim%fig, png_data, status)
    end subroutine generate_png_frame_data

    subroutine update_frame_data(anim, frame_idx)
        class(animation_t), intent(inout) :: anim
        integer, intent(in) :: frame_idx
        
        call anim%animate_func(frame_idx)
    end subroutine update_frame_data

    subroutine cpu_time_delay(seconds)
        real(wp), intent(in) :: seconds
        real(wp) :: start_time, current_time
        
        call cpu_time(start_time)
        do
            call cpu_time(current_time)
            if (current_time - start_time >= seconds) exit
        end do
    end subroutine cpu_time_delay

    subroutine log_error_with_remediation(error_msg, remediation_msg)
        character(len=*), intent(in) :: error_msg, remediation_msg
        
        call log_error("ERROR: " // error_msg)
        call log_info("REMEDIATION: " // remediation_msg)
    end subroutine log_error_with_remediation

end module fortplot_animation_pipeline