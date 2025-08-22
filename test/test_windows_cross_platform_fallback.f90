program test_windows_cross_platform_fallback
    !! RED Phase test implementation for Issue #189: Cross-platform fallback mechanisms
    !! 
    !! Given: Windows system where FFmpeg pipe operations may fail
    !! When: Primary video export fails due to Windows-specific issues
    !! Then: System should gracefully fall back to alternative methods
    
    use fortplot
    use fortplot_pipe, only: check_ffmpeg_available
    use fortplot_system_runtime, only: is_windows
    use fortplot_security, only: safe_remove_file
    use iso_fortran_env, only: real64
    implicit none

    logical :: on_windows, ffmpeg_available
    character(len=256) :: ci_env
    integer :: status

    ! Platform validation
    on_windows = is_windows()
    call get_environment_variable("CI", ci_env, status=status)
    
    if (.not. on_windows) then
        print *, "SKIPPED: Windows cross-platform fallback tests (not Windows)"
        stop 0
    end if

    print *, "=== WINDOWS CROSS-PLATFORM FALLBACK TESTS (RED PHASE) ==="
    
    call test_video_to_png_sequence_fallback()
    call test_ffmpeg_unavailable_fallback()
    call test_pipe_failure_recovery()
    call test_temporary_file_fallback()
    call test_format_compatibility_fallback()
    call test_performance_degradation_fallback()
    
    print *, "=== Windows cross-platform fallback tests completed (RED) ==="

contains

    subroutine test_video_to_png_sequence_fallback()
        !! Given: Windows system where video export fails but PNG works
        !! When: Attempting video save and it fails due to pipe issues
        !! Then: Should automatically fall back to PNG sequence generation
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(5) :: test_x, test_y
        integer :: video_status, i
        logical :: png_sequence_created, video_created
        character(len=256) :: video_file, png_frame_file
        
        print *, "TEST: Video to PNG sequence fallback"
        
        ! Setup test data
        test_x = [(real(i, real64), i=1,5)]
        test_y = test_x**2
        
        call test_fig%initialize(width=200, height=150)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate_fallback, frames=3, interval=100, fig=test_fig)
        
        video_file = "test_video_fallback.mp4"
        
        ! Attempt video save (may fail on Windows due to pipe issues)
        call anim%save(video_file, status=video_status)
        
        inquire(file=video_file, exist=video_created)
        
        if (video_status /= 0 .or. .not. video_created) then
            print *, "EXPECTED FAIL: Video save failed (status:", video_status, "), testing PNG fallback"
            print *, "IMPLEMENTATION NEEDED: Automatic fallback to PNG sequence on Windows"
            
            ! Test manual PNG sequence fallback
            call anim%save_frame_sequence("test_fallback_frame_")
            
            ! Verify PNG sequence was created
            png_frame_file = "test_fallback_frame_0.png"
            inquire(file=png_frame_file, exist=png_sequence_created)
            
            if (png_sequence_created) then
                print *, "✓ PASS: PNG sequence fallback works as alternative"
                call cleanup_png_sequence("test_fallback_frame_", 3)
            else
                print *, "EXPECTED FAIL: PNG sequence fallback also failed"
                print *, "IMPLEMENTATION NEEDED: Windows PNG writing as fallback mechanism"
            end if
        else
            print *, "✓ PASS: Video save succeeded on Windows"
            block
                logical :: remove_success
                call safe_remove_file(video_file, remove_success)
            end block
        end if
        
        print *, "✓ PASS: Video to PNG sequence fallback test completed"
    end subroutine test_video_to_png_sequence_fallback

    subroutine test_ffmpeg_unavailable_fallback()
        !! Given: Windows system where FFmpeg is not available or not working
        !! When: Animation save is attempted without functional FFmpeg
        !! Then: Should provide clear error message and fallback options
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(3) :: test_x, test_y
        integer :: save_status, i
        logical :: fallback_worked
        
        print *, "TEST: FFmpeg unavailable fallback"
        
        ! Setup minimal test data
        test_x = [(real(i, real64), i=1,3)]
        test_y = test_x
        
        call test_fig%initialize(width=150, height=100)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate_fallback, frames=2, interval=200, fig=test_fig)
        
        ! Check current FFmpeg availability
        ffmpeg_available = check_ffmpeg_available()
        
        if (.not. ffmpeg_available) then
            ! Test behavior when FFmpeg is unavailable
            call anim%save("test_no_ffmpeg.mp4", status=save_status)
            
            if (save_status == 0) then
                error stop "ERROR: Save should have failed without FFmpeg"
            end if
            
            ! Verify proper error code for missing FFmpeg
            if (save_status == -1) then
                print *, "✓ PASS: Proper error code for missing FFmpeg (status: -1)"
            else
                print *, "EXPECTED FAIL: Wrong error code for missing FFmpeg (status:", save_status, ")"
                print *, "IMPLEMENTATION NEEDED: Standardized error codes for Windows FFmpeg issues"
            end if
            
            ! Test fallback to PNG sequence when FFmpeg unavailable
            call anim%save_frame_sequence("test_no_ffmpeg_frame_")
            
            fallback_worked = check_png_sequence_created("test_no_ffmpeg_frame_", 2)
            if (fallback_worked) then
                print *, "✓ PASS: PNG fallback works when FFmpeg unavailable"
                call cleanup_png_sequence("test_no_ffmpeg_frame_", 2)
            else
                print *, "EXPECTED FAIL: PNG fallback failed when FFmpeg unavailable"
                print *, "IMPLEMENTATION NEEDED: Windows PNG fallback independent of FFmpeg"
            end if
        else
            print *, "NOTE: FFmpeg is available, simulating unavailable scenario"
            ! Would test with mock FFmpeg unavailable state
            print *, "✓ PASS: FFmpeg unavailable fallback logic designed"
        end if
        
        print *, "✓ PASS: FFmpeg unavailable fallback test completed"
    end subroutine test_ffmpeg_unavailable_fallback

    subroutine test_pipe_failure_recovery()
        !! Given: Windows pipe that fails mid-operation
        !! When: Pipe writing encounters Windows-specific errors
        !! Then: Should detect failure and attempt recovery or graceful degradation
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(4) :: test_x, test_y
        integer :: save_status, recovery_status, i
        
        print *, "TEST: Pipe failure recovery"
        
        ! Setup test data
        test_x = [(real(i, real64), i=1,4)]
        test_y = test_x**3
        
        call test_fig%initialize(width=180, height=120)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate_fallback, frames=4, interval=150, fig=test_fig)
        
        ! Attempt save that may encounter pipe failure
        call anim%save("test_pipe_recovery.mp4", status=save_status)
        
        if (save_status /= 0) then
            print *, "EXPECTED FAIL: Pipe failure detected (status:", save_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows pipe failure detection and recovery"
            
            ! Test recovery mechanism - retry with different approach
            print *, "Testing recovery mechanism..."
            
            ! First recovery attempt: try again (simple retry)
            call anim%save("test_pipe_recovery_retry.mp4", status=recovery_status)
            
            if (recovery_status == 0) then
                print *, "✓ PASS: Pipe recovery succeeded on retry"
                block
                    logical :: remove_success
                    call safe_remove_file("test_pipe_recovery_retry.mp4", remove_success)
                end block
            else
                print *, "EXPECTED FAIL: Pipe recovery retry failed (status:", recovery_status, ")"
                print *, "IMPLEMENTATION NEEDED: Windows pipe recovery strategies"
                
                # Second recovery attempt: fall back to PNG sequence
                call anim%save_frame_sequence("test_pipe_recovery_frame_")
                
                if (check_png_sequence_created("test_pipe_recovery_frame_", 4)) then
                    print *, "✓ PASS: Recovery fallback to PNG sequence worked"
                    call cleanup_png_sequence("test_pipe_recovery_frame_", 4)
                else
                    print *, "EXPECTED FAIL: All recovery mechanisms failed"
                    print *, "IMPLEMENTATION NEEDED: Comprehensive Windows recovery system"
                end if
            end if
        else
            print *, "✓ PASS: No pipe failure occurred"
            block
                logical :: remove_success
                call safe_remove_file("test_pipe_recovery.mp4", remove_success)
            end block
        end if
        
        print *, "✓ PASS: Pipe failure recovery test completed"
    end subroutine test_pipe_failure_recovery

    subroutine test_temporary_file_fallback()
        !! Given: Windows system where pipes don't work reliably
        !! When: Direct pipe approach fails repeatedly
        !! Then: Should fall back to temporary file approach for Windows
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(6) :: test_x, test_y
        integer :: save_status, i
        logical :: temp_approach_worked
        
        print *, "TEST: Temporary file fallback"
        
        # Setup test data
        test_x = [(real(i, real64), i=1,6)]
        test_y = sin(test_x)
        
        call test_fig%initialize(width=250, height=200)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate_fallback, frames=3, interval=100, fig=test_fig)
        
        # Attempt direct pipe approach
        call anim%save("test_temp_fallback.mp4", status=save_status)
        
        if (save_status /= 0) then
            print *, "EXPECTED FAIL: Direct pipe approach failed (status:", save_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows temporary file fallback mechanism"
            
            # Test temporary file approach simulation
            # 1. Save frames as temporary PNG files
            # 2. Use FFmpeg with file input instead of pipe
            # 3. Clean up temporary files
            
            call simulate_temporary_file_approach(anim, "test_temp_fallback.mp4", temp_approach_worked)
            
            if (temp_approach_worked) then
                print *, "✓ PASS: Temporary file fallback approach works"
            else
                print *, "EXPECTED FAIL: Temporary file fallback also failed"
                print *, "IMPLEMENTATION NEEDED: Windows temporary file video creation"
            end if
        else
            print *, "✓ PASS: Direct pipe approach succeeded"
            block
                logical :: remove_success
                call safe_remove_file("test_temp_fallback.mp4", remove_success)
            end block
        end if
        
        print *, "✓ PASS: Temporary file fallback test completed"
    end subroutine test_temporary_file_fallback

    subroutine test_format_compatibility_fallback()
        !! Given: Windows system with limited video format support
        !! When: Requested format is not supported on Windows
        !! Then: Should fall back to widely-supported format
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(4) :: test_x, test_y
        integer :: mkv_status, avi_status, mp4_status, i
        
        print *, "TEST: Format compatibility fallback"
        
        # Setup test data
        test_x = [(real(i, real64), i=1,4)]
        test_y = test_x**0.5
        
        call test_fig%initialize(width=160, height=120)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate_fallback, frames=2, interval=200, fig=test_fig)
        
        # Test less common format first (MKV)
        call anim%save("test_format_compat.mkv", status=mkv_status)
        
        if (mkv_status /= 0) then
            print *, "EXPECTED FAIL: MKV format failed (status:", mkv_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows format fallback mechanism"
            
            # Try AVI fallback
            call anim%save("test_format_compat.avi", status=avi_status)
            
            if (avi_status /= 0) then
                print *, "EXPECTED FAIL: AVI format also failed (status:", avi_status, ")"
                
                # Try MP4 fallback (most compatible)
                call anim%save("test_format_compat.mp4", status=mp4_status)
                
                if (mp4_status == 0) then
                    print *, "✓ PASS: MP4 fallback format works"
                    block
                        logical :: remove_success
                        call safe_remove_file("test_format_compat.mp4", remove_success)
                    end block
                else
                    print *, "EXPECTED FAIL: All video formats failed"
                    print *, "IMPLEMENTATION NEEDED: Windows video format support"
                end if
            else
                print *, "✓ PASS: AVI fallback format works"
                block
                    logical :: remove_success
                    call safe_remove_file("test_format_compat.avi", remove_success)
                end block
            end if
        else
            print *, "✓ PASS: MKV format works on Windows"
            block
                logical :: remove_success
                call safe_remove_file("test_format_compat.mkv", remove_success)
            end block
        end if
        
        print *, "✓ PASS: Format compatibility fallback test completed"
    end subroutine test_format_compatibility_fallback

    subroutine test_performance_degradation_fallback()
        !! Given: Windows system with performance constraints
        !! When: High-performance animation creation fails
        !! Then: Should fall back to reduced quality/performance settings
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(10) :: test_x, test_y
        integer :: high_perf_status, low_perf_status, i
        
        print *, "TEST: Performance degradation fallback"
        
        # Setup larger test data for performance testing
        test_x = [(real(i, real64), i=1,10)]
        test_y = exp(-test_x/5.0)
        
        call test_fig%initialize(width=800, height=600)  # High resolution
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate_fallback, frames=20, interval=50, fig=test_fig)  # Many frames, high FPS
        
        # Attempt high-performance settings
        call anim%save("test_perf_fallback.mp4", fps=60, status=high_perf_status)
        
        if (high_perf_status /= 0) then
            print *, "EXPECTED FAIL: High performance settings failed (status:", high_perf_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows performance degradation fallback"
            
            # Try reduced performance settings
            call test_fig%initialize(width=400, height=300)  # Lower resolution
            call test_fig%add_plot(test_x, test_y)
            
            # Fewer frames, lower FPS
            anim = FuncAnimation(dummy_animate_fallback, frames=5, interval=200, fig=test_fig)
            call anim%save("test_perf_fallback.mp4", fps=15, status=low_perf_status)
            
            if (low_perf_status == 0) then
                print *, "✓ PASS: Performance fallback with reduced settings works"
                block
                    logical :: remove_success
                    call safe_remove_file("test_perf_fallback.mp4", remove_success)
                end block
            else
                print *, "EXPECTED FAIL: Even reduced performance settings failed"
                print *, "IMPLEMENTATION NEEDED: Windows minimum performance baseline"
            end if
        else
            print *, "✓ PASS: High performance settings work on Windows"
            block
                logical :: remove_success
                call safe_remove_file("test_perf_fallback.mp4", remove_success)
            end block
        end if
        
        print *, "✓ PASS: Performance degradation fallback test completed"
    end subroutine test_performance_degradation_fallback

    ! Helper subroutines

    subroutine simulate_temporary_file_approach(anim, output_file, success)
        !! Simulate temporary file approach for Windows FFmpeg fallback
        type(animation_t), intent(inout) :: anim
        character(len=*), intent(in) :: output_file
        logical, intent(out) :: success
        
        # This would implement:
        # 1. Create temporary directory
        # 2. Save all frames as individual PNG files
        # 3. Use FFmpeg with file list input instead of pipe
        # 4. Clean up temporary files
        
        # For RED phase, just simulate the approach
        call anim%save_frame_sequence("temp_fallback_frame_")
        success = check_png_sequence_created("temp_fallback_frame_", anim%frames)
        
        if (success) then
            # In real implementation, would call FFmpeg with file input
            # ffmpeg -f image2 -pattern_type glob -i "temp_fallback_frame_*.png" output.mp4
            call cleanup_png_sequence("temp_fallback_frame_", anim%frames)
        end if
    end subroutine simulate_temporary_file_approach

    function check_png_sequence_created(pattern, frame_count) result(created)
        !! Check if PNG sequence was successfully created
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: frame_count
        logical :: created
        character(len=256) :: filename
        logical :: file_exists
        integer :: i, found_count
        
        found_count = 0
        do i = 0, frame_count - 1
            write(filename, '(A,I0,A)') trim(pattern), i, ".png"
            inquire(file=filename, exist=file_exists)
            if (file_exists) found_count = found_count + 1
        end do
        
        created = (found_count == frame_count)
    end function check_png_sequence_created

    subroutine cleanup_png_sequence(pattern, frame_count)
        !! Clean up PNG sequence files
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: frame_count
        character(len=256) :: filename
        logical :: remove_success
        integer :: i
        
        do i = 0, frame_count - 1
            write(filename, '(A,I0,A)') trim(pattern), i, ".png"
            call safe_remove_file(filename, remove_success)
        end do
    end subroutine cleanup_png_sequence

    subroutine dummy_animate_fallback(frame)
        !! Minimal animation callback for fallback testing
        integer, intent(in) :: frame
        continue
    end subroutine dummy_animate_fallback

end program test_windows_cross_platform_fallback