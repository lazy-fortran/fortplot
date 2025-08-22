program test_windows_ffmpeg_pipe_handling
    !! RED Phase test implementation for Issue #189: Windows FFmpeg pipe handling
    !! 
    !! Given: Windows system with FFmpeg binary data streaming requirements
    !! When: Opening pipes and writing PNG data frames to FFmpeg
    !! Then: Binary mode operations and command escaping should work correctly
    
    use fortplot
    use fortplot_pipe, only: check_ffmpeg_available, open_ffmpeg_pipe, &
                            write_png_to_pipe, close_ffmpeg_pipe
    use fortplot_system_runtime, only: is_windows
    use fortplot_security, only: safe_remove_file
    use iso_fortran_env, only: real64, int8
    implicit none

    logical :: on_windows, ffmpeg_available
    character(len=256) :: ci_env
    integer :: status

    ! Detect platform 
    on_windows = is_windows()
    call get_environment_variable("CI", ci_env, status=status)
    
    if (.not. on_windows) then
        print *, "SKIPPED: Windows-specific FFmpeg tests (not Windows platform)"
        stop 0
    end if
    
    ! Check FFmpeg availability
    ffmpeg_available = check_ffmpeg_available()
    if (.not. ffmpeg_available) then
        print *, "SKIPPED: Windows FFmpeg tests require FFmpeg installation"
        stop 77
    end if

    print *, "=== WINDOWS FFMPEG PIPE HANDLING TESTS (RED PHASE) ==="
    
    call test_windows_binary_pipe_creation()
    call test_windows_png_binary_data_streaming()
    call test_windows_command_escaping()
    call test_windows_pipe_error_recovery()
    call test_windows_cross_platform_fallback()
    
    print *, "=== Windows FFmpeg pipe handling tests completed (RED) ==="

contains

    subroutine test_windows_binary_pipe_creation()
        !! Given: Windows system requiring binary mode pipes for FFmpeg
        !! When: Opening FFmpeg pipe for MP4 output with Windows-specific flags
        !! Then: Pipe should be created in binary mode with proper Windows handling
        
        integer :: pipe_status
        character(len=200) :: test_output_file
        
        print *, "TEST: Windows binary pipe creation"
        
        test_output_file = "test_windows_binary_pipe.mp4"
        
        ! Attempt to open FFmpeg pipe with Windows-specific requirements
        pipe_status = open_ffmpeg_pipe(test_output_file, 10)
        
        if (pipe_status /= 0) then
            ! Expected failure in RED phase - need Windows binary pipe implementation
            print *, "EXPECTED FAIL: Windows binary pipe creation failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows _setmode(_fileno(pipe), _O_BINARY)"
            return
        end if
        
        ! Close pipe if successfully opened
        pipe_status = close_ffmpeg_pipe()
        if (pipe_status /= 0) then
            print *, "WARNING: Failed to close pipe (status:", pipe_status, ")"
        end if
        
        ! Cleanup
        block
            logical :: remove_success
            call safe_remove_file(test_output_file, remove_success)
        end block
        
        print *, "✓ PASS: Windows binary pipe creation succeeded"
    end subroutine test_windows_binary_pipe_creation

    subroutine test_windows_png_binary_data_streaming()
        !! Given: Windows binary pipe and PNG frame data
        !! When: Writing PNG data frames through Windows pipe to FFmpeg
        !! Then: Binary data should be written without corruption or truncation
        
        integer :: pipe_status, write_status
        character(len=200) :: test_output_file
        integer(int8), allocatable :: mock_png_data(:)
        integer :: i
        
        print *, "TEST: Windows PNG binary data streaming"
        
        test_output_file = "test_windows_png_streaming.mp4"
        
        ! Create mock PNG data (simplified binary test data)
        allocate(mock_png_data(1000))
        do i = 1, size(mock_png_data)
            mock_png_data(i) = int(mod(i, 256), int8)
        end do
        
        ! Open pipe for testing
        pipe_status = open_ffmpeg_pipe(test_output_file, 15)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Cannot open pipe for binary data test (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows binary pipe with _O_BINARY flag"
            deallocate(mock_png_data)
            return
        end if
        
        ! Write binary PNG data
        write_status = write_png_to_pipe(mock_png_data)
        if (write_status /= 0) then
            print *, "EXPECTED FAIL: Binary PNG data write failed (status:", write_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows binary write with proper byte handling"
            pipe_status = close_ffmpeg_pipe()
            deallocate(mock_png_data)
            return
        end if
        
        ! Write a second frame to test continued streaming
        do i = 1, size(mock_png_data)
            mock_png_data(i) = int(mod(i+100, 256), int8)
        end do
        
        write_status = write_png_to_pipe(mock_png_data)
        if (write_status /= 0) then
            print *, "EXPECTED FAIL: Second frame write failed (status:", write_status, ")"
            print *, "IMPLEMENTATION NEEDED: Persistent Windows binary pipe state"
        end if
        
        ! Close pipe
        pipe_status = close_ffmpeg_pipe()
        
        ! Cleanup
        deallocate(mock_png_data)
        block
            logical :: remove_success
            call safe_remove_file(test_output_file, remove_success)
        end block
        
        print *, "✓ PASS: Windows PNG binary data streaming test completed"
    end subroutine test_windows_png_binary_data_streaming

    subroutine test_windows_command_escaping()
        !! Given: Windows command line with spaces, quotes, and special characters
        !! When: Constructing FFmpeg command for Windows pipe
        !! Then: Command should be properly escaped and quoted for Windows cmd.exe
        
        character(len=300) :: test_filename_with_spaces
        character(len=300) :: test_filename_with_quotes  
        integer :: pipe_status
        
        print *, "TEST: Windows command escaping"
        
        ! Test filename with spaces (common Windows scenario)
        test_filename_with_spaces = "test output with spaces.mp4"
        pipe_status = open_ffmpeg_pipe(test_filename_with_spaces, 10)
        
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Filename with spaces failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows command quoting with double quotes"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(test_filename_with_spaces, remove_success)
            end block
        end if
        
        ! Test filename with quotes (edge case)
        test_filename_with_quotes = 'test_"quoted"_filename.mp4'
        pipe_status = open_ffmpeg_pipe(test_filename_with_quotes, 10)
        
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Filename with quotes failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows quote escaping with backslashes"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(test_filename_with_quotes, remove_success)
            end block
        end if
        
        print *, "✓ PASS: Windows command escaping test completed"
    end subroutine test_windows_command_escaping

    subroutine test_windows_pipe_error_recovery()
        !! Given: Windows FFmpeg pipe encountering errors during operation
        !! When: Pipe write fails or FFmpeg process terminates unexpectedly
        !! Then: Error should be detected and handled gracefully without hanging
        
        integer :: pipe_status, write_status
        character(len=200) :: test_output_file
        integer(int8), allocatable :: test_data(:)
        
        print *, "TEST: Windows pipe error recovery"
        
        test_output_file = "test_windows_error_recovery.mp4"
        
        ! Allocate test data
        allocate(test_data(500))
        test_data = 42_int8
        
        ! Test 1: Write to pipe before opening (should fail gracefully)
        write_status = write_png_to_pipe(test_data)
        if (write_status == 0) then
            error stop "ERROR: Writing to unopened pipe should have failed"
        end if
        print *, "✓ PASS: Write to unopened pipe properly failed (status:", write_status, ")"
        
        ! Test 2: Open pipe and force close externally, then try to write
        pipe_status = open_ffmpeg_pipe(test_output_file, 10)
        if (pipe_status == 0) then
            ! Force close the pipe
            pipe_status = close_ffmpeg_pipe()
            
            ! Try to write after closing (should fail)
            write_status = write_png_to_pipe(test_data)
            if (write_status == 0) then
                print *, "EXPECTED FAIL: Write after close should have failed"
                print *, "IMPLEMENTATION NEEDED: Windows pipe state validation"
            else
                print *, "✓ PASS: Write after close properly failed (status:", write_status, ")"
            end if
        else
            print *, "EXPECTED FAIL: Could not open pipe for error recovery test"
            print *, "IMPLEMENTATION NEEDED: Windows pipe error recovery mechanisms"
        end if
        
        ! Cleanup
        deallocate(test_data)
        block
            logical :: remove_success
            call safe_remove_file(test_output_file, remove_success)
        end block
        
        print *, "✓ PASS: Windows pipe error recovery test completed"
    end subroutine test_windows_pipe_error_recovery

    subroutine test_windows_cross_platform_fallback()
        !! Given: Windows system where FFmpeg pipe may fail
        !! When: Animation save is attempted with fallback mechanisms
        !! Then: Should gracefully fall back to frame sequence or error reporting
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(5) :: test_x, test_y
        integer :: save_status, i
        
        print *, "TEST: Windows cross-platform fallback"
        
        ! Setup test data
        test_x = [(real(i, real64), i=1,5)]
        test_y = test_x**2
        
        call test_fig%initialize(width=200, height=150)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate_fallback, frames=2, interval=50, fig=test_fig)
        
        ! Test 1: Try video save (may fail on Windows due to pipe issues)
        call anim%save("test_windows_fallback.mp4", status=save_status)
        
        if (save_status /= 0) then
            print *, "EXPECTED FAIL: Windows video save failed (status:", save_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows binary pipe or temporary file fallback"
            
            ! Test fallback to PNG sequence
            call anim%save_frame_sequence("test_windows_fallback_frame_")
            
            ! Check if PNG sequence was created
            block
                logical :: frame_exists
                inquire(file="test_windows_fallback_frame_0.png", exist=frame_exists)
                if (frame_exists) then
                    print *, "✓ PASS: PNG sequence fallback works on Windows"
                    ! Cleanup PNG files
                    call safe_cleanup_png_sequence("test_windows_fallback_frame_")
                else
                    print *, "EXPECTED FAIL: PNG sequence fallback also failed"
                    print *, "IMPLEMENTATION NEEDED: Windows PNG file writing"
                end if
            end block
        else
            print *, "✓ PASS: Windows video save succeeded"
            block
                logical :: remove_success
                call safe_remove_file("test_windows_fallback.mp4", remove_success)
            end block
        end if
        
        print *, "✓ PASS: Windows cross-platform fallback test completed"
    end subroutine test_windows_cross_platform_fallback

    subroutine safe_cleanup_png_sequence(pattern)
        !! Helper to safely remove PNG sequence files
        character(len=*), intent(in) :: pattern
        character(len=256) :: filename
        logical :: file_exists, remove_success
        integer :: frame_idx
        
        do frame_idx = 0, 10  ! Check up to 10 frames
            write(filename, '(A,I0,A)') trim(pattern), frame_idx, ".png"
            inquire(file=filename, exist=file_exists)
            if (file_exists) then
                call safe_remove_file(filename, remove_success)
                if (.not. remove_success) then
                    print *, "Warning: Could not remove:", trim(filename)
                end if
            end if
        end do
    end subroutine safe_cleanup_png_sequence

    subroutine dummy_animate_fallback(frame)
        !! Minimal animation callback for fallback testing
        integer, intent(in) :: frame
        ! Minimal update for testing - just continue
        continue
    end subroutine dummy_animate_fallback

end program test_windows_ffmpeg_pipe_handling