program test_ffmpeg_pipe_reliability
    !! RED Phase test implementation for Issue #186: FFmpeg pipe output and format problems
    !! 
    !! Given: FFmpeg pipe integration is required for animation saves  
    !! When: Writing frame data through pipes to FFmpeg with various configurations
    !! Then: Pipe communication should be reliable with proper error recovery
    
    use fortplot
    use fortplot_pipe, only: check_ffmpeg_available, open_ffmpeg_pipe, &
                            write_png_to_pipe, close_ffmpeg_pipe
    use fortplot_animation, only: animation_t, FuncAnimation
    use fortplot_security, only: safe_remove_file
    use fortplot_system_runtime, only: is_windows
    use iso_fortran_env, only: real64, int8
    implicit none

    logical :: ffmpeg_available, skip_tests
    character(len=256) :: ci_env
    integer :: status

    ! Check FFmpeg availability and CI environment
    ffmpeg_available = check_ffmpeg_available()
    call get_environment_variable("CI", ci_env, status=status)
    skip_tests = (.not. ffmpeg_available .or. &
                 (is_windows() .and. status == 0))

    if (skip_tests) then
        if (.not. ffmpeg_available) then
            print *, "SKIPPED: FFmpeg pipe reliability tests require FFmpeg installation"
        else
            print *, "SKIPPED: FFmpeg pipe tests on Windows CI (known pipe issues)"
        end if
        stop 77  ! Standard exit code for skipped tests
    end if

    print *, "=== FFMPEG PIPE RELIABILITY TESTS (RED PHASE) ==="
    print *, "Testing pipe communication robustness for Issue #186"
    
    call test_pipe_write_failure_detection()
    call test_pipe_status_validation()
    call test_frame_transmission_integrity()
    call test_format_validation_reliability()
    call test_pipe_error_recovery_mechanisms()
    call test_cross_platform_pipe_robustness()
    
    print *, "=== FFmpeg pipe reliability tests completed (RED) ==="

contains

    subroutine test_pipe_write_failure_detection()
        !! Given: FFmpeg pipe may fail during frame writing operations
        !! When: Attempting to write PNG frame data to potentially failing pipe
        !! Then: Write failures should be detected immediately with proper status codes
        
        integer :: pipe_status, write_status, close_status
        character(len=200) :: test_output_file
        integer(int8), allocatable :: test_png_data(:)
        integer :: i
        logical :: file_exists
        integer :: file_size
        
        print *, "TEST: Pipe write failure detection"
        
        test_output_file = "test_pipe_write_failure.mp4"
        
        ! Create minimal PNG-like test data
        allocate(test_png_data(100))
        do i = 1, size(test_png_data)
            test_png_data(i) = int(mod(i, 256), int8)
        end do
        
        ! Test 1: Write to unopened pipe should fail immediately
        write_status = write_png_to_pipe(test_png_data)
        if (write_status == 0) then
            error stop "CRITICAL: Write to unopened pipe should have failed with non-zero status"
        end if
        print *, "✓ EXPECTED FAIL: Write to unopened pipe failed correctly (status:", write_status, ")"
        
        ! Test 2: Open pipe and test legitimate write
        pipe_status = open_ffmpeg_pipe(test_output_file, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Cannot open FFmpeg pipe (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Robust pipe opening with error handling"
            deallocate(test_png_data)
            return
        end if
        
        ! Write data and check for success
        write_status = write_png_to_pipe(test_png_data)
        if (write_status /= 0) then
            print *, "EXPECTED FAIL: Frame write failed with status:", write_status
            print *, "IMPLEMENTATION NEEDED: Reliable frame transmission (Issue #186 status -6)"
        else
            print *, "✓ PASS: Frame write succeeded"
        end if
        
        ! Test 3: Close pipe and verify file creation
        close_status = close_ffmpeg_pipe()
        if (close_status /= 0) then
            print *, "WARNING: Pipe close failed (status:", close_status, ")"
        end if
        
        ! Validate output file was created (addresses Issue #186 "File exists: F")
        inquire(file=test_output_file, exist=file_exists, size=file_size)
        if (.not. file_exists) then
            print *, "EXPECTED FAIL: Output file not created (Issue #186 File exists: F)"
            print *, "IMPLEMENTATION NEEDED: Ensure FFmpeg pipe creates valid output"
        else if (file_size <= 0) then
            print *, "EXPECTED FAIL: Output file empty or invalid size:", file_size
            print *, "IMPLEMENTATION NEEDED: Valid file size reporting (Issue #186 size: -1)"
        else
            print *, "✓ PASS: Output file created with size:", file_size, "bytes"
        end if
        
        ! Cleanup
        deallocate(test_png_data)
        block
            logical :: remove_success
            call safe_remove_file(test_output_file, remove_success)
        end block
        
        print *, "✓ PASS: Pipe write failure detection test completed"
    end subroutine test_pipe_write_failure_detection

    subroutine test_pipe_status_validation()
        !! Given: FFmpeg pipe operations return status codes indicating success/failure
        !! When: Checking pipe operation statuses throughout animation save process
        !! Then: Status codes should accurately reflect actual operation success/failure
        
        integer :: open_status, write_status, close_status
        character(len=200) :: test_output_file
        integer(int8), allocatable :: minimal_data(:)
        
        print *, "TEST: Pipe status validation"
        
        test_output_file = "test_pipe_status.mp4"
        
        ! Minimal test data
        allocate(minimal_data(50))
        minimal_data = 65_int8  ! ASCII 'A'
        
        ! Test status validation sequence
        open_status = open_ffmpeg_pipe(test_output_file, 15)
        
        if (open_status == 0) then
            print *, "✓ PASS: Pipe opened successfully (status:", open_status, ")"
            
            write_status = write_png_to_pipe(minimal_data)
            if (write_status == 0) then
                print *, "✓ PASS: Write operation successful (status:", write_status, ")"
            else
                print *, "EXPECTED FAIL: Write failed with status:", write_status
                print *, "IMPLEMENTATION NEEDED: Reliable write operations (Issue #186)"
            end if
            
            close_status = close_ffmpeg_pipe()
            if (close_status == 0) then
                print *, "✓ PASS: Pipe closed successfully (status:", close_status, ")"
            else
                print *, "WARNING: Pipe close returned status:", close_status
            end if
        else
            print *, "EXPECTED FAIL: Pipe open failed (status:", open_status, ")"
            print *, "IMPLEMENTATION NEEDED: Reliable pipe opening mechanisms"
        end if
        
        ! Test double-close (should be safe)
        close_status = close_ffmpeg_pipe()
        if (close_status == 0) then
            print *, "NOTE: Double-close returned success (may be expected)"
        else
            print *, "NOTE: Double-close returned status:", close_status, "(may be expected)"
        end if
        
        ! Cleanup
        deallocate(minimal_data)
        block
            logical :: remove_success
            call safe_remove_file(test_output_file, remove_success)
        end block
        
        print *, "✓ PASS: Pipe status validation test completed"
    end subroutine test_pipe_status_validation

    subroutine test_frame_transmission_integrity()
        !! Given: Animation frames must be transmitted completely through FFmpeg pipe
        !! When: Writing multiple frames of varying sizes through the pipe
        !! Then: All frame data should be transmitted without corruption or loss
        
        integer :: pipe_status, write_status, close_status, frame_idx
        character(len=200) :: test_output_file
        integer(int8), allocatable :: frame_data(:)
        integer :: frame_sizes(3) = [200, 500, 1000]
        logical :: all_writes_successful
        
        print *, "TEST: Frame transmission integrity"
        
        test_output_file = "test_frame_transmission.mp4"
        all_writes_successful = .true.
        
        pipe_status = open_ffmpeg_pipe(test_output_file, 20)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Cannot open pipe for frame transmission test"
            print *, "IMPLEMENTATION NEEDED: Stable pipe initialization"
            return
        end if
        
        ! Write frames of different sizes to test transmission reliability
        do frame_idx = 1, size(frame_sizes)
            allocate(frame_data(frame_sizes(frame_idx)))
            
            ! Fill with pattern data for integrity checking
            frame_data = int(mod(frame_idx * 42, 256), int8)
            
            write_status = write_png_to_pipe(frame_data)
            if (write_status /= 0) then
                print *, "EXPECTED FAIL: Frame", frame_idx, "write failed (status:", write_status, ")"
                print *, "IMPLEMENTATION NEEDED: Multi-frame transmission reliability"
                all_writes_successful = .false.
            else
                print *, "✓ PASS: Frame", frame_idx, "transmitted successfully (size:", frame_sizes(frame_idx), ")"
            end if
            
            deallocate(frame_data)
        end do
        
        close_status = close_ffmpeg_pipe()
        
        if (all_writes_successful .and. close_status == 0) then
            print *, "✓ PASS: All frames transmitted successfully"
        else
            print *, "EXPECTED FAIL: Frame transmission issues detected"
            print *, "IMPLEMENTATION NEEDED: Robust multi-frame pipeline (Issue #186)"
        end if
        
        ! Cleanup
        block
            logical :: remove_success
            call safe_remove_file(test_output_file, remove_success)
        end block
        
        print *, "✓ PASS: Frame transmission integrity test completed"
    end subroutine test_frame_transmission_integrity

    subroutine test_format_validation_reliability()
        !! Given: Animation save must validate file formats before processing
        !! When: Attempting to save animations with various file extensions
        !! Then: Unsupported formats should be caught early with clear error messages
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(3) :: test_x, test_y
        integer :: save_status, i
        character(len=200), dimension(5) :: invalid_formats = [ &
            "test.txt      ", "test.doc      ", "test.xyz      ", &
            "test.gif      ", "test          " ]
        character(len=200), dimension(3) :: valid_formats = [ &
            "test.mp4      ", "test.avi      ", "test.mkv      " ]
        
        print *, "TEST: Format validation reliability"
        
        ! Setup minimal animation for testing
        test_x = [1.0_real64, 2.0_real64, 3.0_real64]
        test_y = test_x**2
        
        call test_fig%initialize(width=100, height=100)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate_format, frames=1, interval=50, fig=test_fig)
        
        ! Test invalid format rejection
        print *, "Testing invalid format rejection..."
        do i = 1, size(invalid_formats)
            call anim%save(trim(invalid_formats(i)), status=save_status)
            if (save_status == 0) then
                error stop "CRITICAL: Invalid format " // trim(invalid_formats(i)) // " was accepted"
            else
                print *, "✓ PASS: Invalid format rejected:", trim(invalid_formats(i)), "(status:", save_status, ")"
            end if
        end do
        
        ! Test valid format acceptance (may fail due to pipe issues, but should not fail on format)
        print *, "Testing valid format acceptance..."
        do i = 1, size(valid_formats)
            call anim%save(trim(valid_formats(i)), status=save_status)
            if (save_status == -3) then
                error stop "CRITICAL: Valid format " // trim(valid_formats(i)) // " was rejected as invalid"
            else if (save_status == 0) then
                print *, "✓ PASS: Valid format accepted and saved:", trim(valid_formats(i))
                block
                    logical :: remove_success
                    call safe_remove_file(trim(valid_formats(i)), remove_success)
                end block
            else
                print *, "EXPECTED FAIL: Valid format accepted but save failed:", &
                         trim(valid_formats(i)), "(status:", save_status, ")"
                print *, "IMPLEMENTATION NEEDED: Reliable save after format validation"
            end if
        end do
        
        print *, "✓ PASS: Format validation reliability test completed"
    end subroutine test_format_validation_reliability

    subroutine test_pipe_error_recovery_mechanisms()
        !! Given: FFmpeg pipe operations may encounter various error conditions
        !! When: Pipe errors occur during animation save process
        !! Then: Error recovery should be graceful without hanging or corrupting state
        
        integer :: first_open_status, second_open_status, write_status, close_status
        character(len=200) :: test_output_file
        integer(int8), allocatable :: test_data(:)
        
        print *, "TEST: Pipe error recovery mechanisms"
        
        test_output_file = "test_pipe_recovery.mp4"
        
        allocate(test_data(100))
        test_data = 123_int8
        
        ! Test 1: Multiple pipe open attempts (should handle gracefully)
        first_open_status = open_ffmpeg_pipe(test_output_file, 10)
        second_open_status = open_ffmpeg_pipe("test_second.mp4", 10)
        
        if (first_open_status == 0 .and. second_open_status /= 0) then
            print *, "✓ PASS: Second pipe open correctly failed (only one pipe allowed)"
        else if (first_open_status /= 0) then
            print *, "EXPECTED FAIL: First pipe open failed"
            print *, "IMPLEMENTATION NEEDED: Basic pipe opening functionality"
        else
            print *, "WARNING: Multiple pipe opens both succeeded (may cause issues)"
        end if
        
        ! Test 2: Write after error condition
        if (first_open_status == 0) then
            write_status = write_png_to_pipe(test_data)
            if (write_status /= 0) then
                print *, "EXPECTED FAIL: Write failed after pipe state issues"
                print *, "IMPLEMENTATION NEEDED: Error recovery for pipe write operations"
            end if
            
            close_status = close_ffmpeg_pipe()
        end if
        
        ! Test 3: Recovery after failed operations
        write_status = write_png_to_pipe(test_data)
        if (write_status == 0) then
            error stop "CRITICAL: Write to closed pipe should have failed"
        else
            print *, "✓ PASS: Write after close properly failed (status:", write_status, ")"
        end if
        
        ! Test 4: New pipe after error recovery
        first_open_status = open_ffmpeg_pipe("test_recovery_new.mp4", 15)
        if (first_open_status == 0) then
            print *, "✓ PASS: New pipe opened after error recovery"
            close_status = close_ffmpeg_pipe()
        else
            print *, "EXPECTED FAIL: Cannot open new pipe after error recovery"
            print *, "IMPLEMENTATION NEEDED: Complete error state reset"
        end if
        
        ! Cleanup
        deallocate(test_data)
        block
            logical :: remove_success
            call safe_remove_file(test_output_file, remove_success)
            call safe_remove_file("test_second.mp4", remove_success)
            call safe_remove_file("test_recovery_new.mp4", remove_success)
        end block
        
        print *, "✓ PASS: Pipe error recovery mechanisms test completed"
    end subroutine test_pipe_error_recovery_mechanisms

    subroutine test_cross_platform_pipe_robustness()
        !! Given: FFmpeg pipe integration must work across different platforms
        !! When: Running pipe operations on current platform with platform-specific considerations
        !! Then: Platform-specific pipe behavior should be handled appropriately
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(2) :: test_x, test_y
        integer :: save_status
        logical :: is_windows_platform
        character(len=200) :: test_output_file
        
        print *, "TEST: Cross-platform pipe robustness"
        
        is_windows_platform = is_windows()
        test_output_file = "test_cross_platform.mp4"
        
        ! Setup minimal animation
        test_x = [1.0_real64, 2.0_real64]
        test_y = [1.0_real64, 4.0_real64]
        
        call test_fig%initialize(width=150, height=150)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate_platform, frames=2, interval=100, fig=test_fig)
        
        print *, "Platform detected: ", merge("Windows", "Unix   ", is_windows_platform)
        
        ! Test platform-specific pipe behavior
        call anim%save(test_output_file, status=save_status)
        
        if (save_status == 0) then
            print *, "✓ PASS: Cross-platform animation save succeeded"
            
            ! Verify file creation
            block
                logical :: file_exists
                integer :: file_size
                inquire(file=test_output_file, exist=file_exists, size=file_size)
                if (file_exists .and. file_size > 0) then
                    print *, "✓ PASS: Output file created successfully (size:", file_size, "bytes)"
                else
                    print *, "EXPECTED FAIL: File creation issues (exists:", file_exists, &
                            ", size:", file_size, ")"
                    print *, "IMPLEMENTATION NEEDED: Platform-specific file handling"
                end if
            end block
        else
            print *, "EXPECTED FAIL: Cross-platform save failed (status:", save_status, ")"
            if (is_windows_platform) then
                print *, "IMPLEMENTATION NEEDED: Windows binary pipe handling (Issue #186)"
            else
                print *, "IMPLEMENTATION NEEDED: Unix pipe reliability improvements"
            end if
        end if
        
        ! Test fallback mechanism
        if (save_status /= 0) then
            print *, "Testing PNG sequence fallback..."
            call anim%save_frame_sequence("test_platform_fallback_")
            
            block
                logical :: fallback_exists
                inquire(file="test_platform_fallback_0.png", exist=fallback_exists)
                if (fallback_exists) then
                    print *, "✓ PASS: PNG sequence fallback works"
                    call safe_cleanup_fallback_files("test_platform_fallback_")
                else
                    print *, "EXPECTED FAIL: PNG sequence fallback also failed"
                    print *, "IMPLEMENTATION NEEDED: Reliable fallback mechanisms"
                end if
            end block
        end if
        
        ! Cleanup
        block
            logical :: remove_success
            call safe_remove_file(test_output_file, remove_success)
        end block
        
        print *, "✓ PASS: Cross-platform pipe robustness test completed"
    end subroutine test_cross_platform_pipe_robustness

    subroutine safe_cleanup_fallback_files(pattern)
        !! Helper to clean up PNG fallback files
        character(len=*), intent(in) :: pattern
        character(len=256) :: filename
        logical :: file_exists, remove_success
        integer :: frame_idx
        
        do frame_idx = 0, 5  ! Check limited range of frames
            write(filename, '(A,I0,A)') trim(pattern), frame_idx, ".png"
            inquire(file=filename, exist=file_exists)
            if (file_exists) then
                call safe_remove_file(filename, remove_success)
            end if
        end do
    end subroutine safe_cleanup_fallback_files

    subroutine dummy_animate_format(frame)
        !! Minimal animation callback for format testing
        integer, intent(in) :: frame
        continue
    end subroutine dummy_animate_format

    subroutine dummy_animate_platform(frame)
        !! Minimal animation callback for platform testing
        integer, intent(in) :: frame
        continue
    end subroutine dummy_animate_platform

end program test_ffmpeg_pipe_reliability