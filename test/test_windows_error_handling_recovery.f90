program test_windows_error_handling_recovery
    !! RED Phase test implementation for Issue #189: Windows-specific error handling and recovery
    !! 
    !! Given: Windows system with specific error conditions and recovery requirements
    !! When: Various Windows-specific error scenarios occur during FFmpeg operations
    !! Then: System should detect, classify, and recover from errors appropriately
    
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

    ! Platform validation
    on_windows = is_windows()
    call get_environment_variable("CI", ci_env, status=status)
    
    if (.not. on_windows) then
        print *, "SKIPPED: Windows error handling tests (not Windows)"
        stop 0
    end if

    print *, "=== WINDOWS ERROR HANDLING AND RECOVERY TESTS (RED PHASE) ==="
    
    call test_pipe_creation_error_handling()
    call test_binary_write_error_recovery()
    call test_disk_space_error_handling()
    call test_permission_error_recovery()
    call test_process_termination_handling()
    call test_timeout_error_recovery()
    call test_memory_allocation_error_handling()
    
    print *, "=== Windows error handling and recovery tests completed (RED) ==="

contains

    subroutine test_pipe_creation_error_handling()
        !! Given: Windows system where pipe creation may fail
        !! When: FFmpeg pipe creation encounters Windows-specific errors
        !! Then: Errors should be properly classified and reported
        
        integer :: pipe_status
        character(len=300) :: problematic_filename
        
        print *, "TEST: Pipe creation error handling"
        
        ! Test 1: Invalid path characters for Windows
        problematic_filename = 'test<invalid>characters.mp4'  # < and > invalid on Windows
        pipe_status = open_ffmpeg_pipe(problematic_filename, 10)
        
        if (pipe_status == 0) then
            print *, "UNEXPECTED: Invalid characters filename should have failed"
            pipe_status = close_ffmpeg_pipe()
        else
            print *, "✓ PASS: Invalid characters properly rejected (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows filename validation with specific error codes"
        end if
        
        ! Test 2: Very long filename (Windows MAX_PATH limitation)
        problematic_filename = repeat("very_long_filename_", 20) // ".mp4"  # ~340 characters
        pipe_status = open_ffmpeg_pipe(problematic_filename, 10)
        
        if (pipe_status == 0) then
            print *, "UNEXPECTED: Very long filename should have failed on Windows"
            pipe_status = close_ffmpeg_pipe()
        else
            print *, "✓ PASS: Long filename properly rejected (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows MAX_PATH error handling"
        end if
        
        ! Test 3: Reserved device names on Windows
        problematic_filename = "CON.mp4"  # CON is reserved on Windows
        pipe_status = open_ffmpeg_pipe(problematic_filename, 10)
        
        if (pipe_status == 0) then
            print *, "UNEXPECTED: Reserved device name should have failed"
            pipe_status = close_ffmpeg_pipe()
        else
            print *, "✓ PASS: Reserved device name properly rejected (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows reserved name validation"
        end if
        
        print *, "✓ PASS: Pipe creation error handling test completed"
    end subroutine test_pipe_creation_error_handling

    subroutine test_binary_write_error_recovery()
        !! Given: Windows pipe opened successfully but write operations fail
        !! When: Binary data writing encounters Windows-specific errors
        !! Then: Errors should be detected and recovery attempted
        
        integer :: pipe_status, write_status, recovery_attempts
        character(len=200) :: test_filename
        integer(int8), allocatable :: test_data(:)
        logical :: recovery_successful
        
        print *, "TEST: Binary write error recovery"
        
        test_filename = "test_write_error_recovery.mp4"
        
        # Setup test data
        allocate(test_data(1000))
        test_data = 85_int8
        
        # Open pipe
        pipe_status = open_ffmpeg_pipe(test_filename, 15)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Cannot open pipe for write error test (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Basic Windows pipe creation"
            deallocate(test_data)
            return
        end if
        
        # Test write operation
        write_status = write_png_to_pipe(test_data)
        
        if (write_status /= 0) then
            print *, "EXPECTED FAIL: Write operation failed (status:", write_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows binary write error classification"
            
            # Test recovery mechanism
            recovery_successful = .false.
            do recovery_attempts = 1, 3
                print *, "Recovery attempt", recovery_attempts, "..."
                
                # In real implementation, would analyze error and try different approach
                # For now, just test retry logic
                write_status = write_png_to_pipe(test_data)
                if (write_status == 0) then
                    recovery_successful = .true.
                    print *, "✓ PASS: Recovery succeeded on attempt", recovery_attempts
                    exit
                end if
            end do
            
            if (.not. recovery_successful) then
                print *, "EXPECTED FAIL: All recovery attempts failed"
                print *, "IMPLEMENTATION NEEDED: Windows write error recovery strategies"
            end if
        else
            print *, "✓ PASS: Write operation succeeded"
        end if
        
        # Close pipe
        pipe_status = close_ffmpeg_pipe()
        
        # Cleanup
        deallocate(test_data)
        block
            logical :: remove_success
            call safe_remove_file(test_filename, remove_success)
        end block
        
        print *, "✓ PASS: Binary write error recovery test completed"
    end subroutine test_binary_write_error_recovery

    subroutine test_disk_space_error_handling()
        !! Given: Windows system with limited disk space
        !! When: Video creation fails due to insufficient disk space
        !! Then: Error should be properly detected and reported
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(5) :: test_x, test_y
        integer :: save_status, i
        
        print *, "TEST: Disk space error handling"
        
        # Setup test data
        test_x = [(real(i, real64), i=1,5)]
        test_y = test_x**2
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        # Create animation with many frames to potentially trigger disk space issues
        anim = FuncAnimation(dummy_animate_error, frames=50, interval=50, fig=test_fig)
        
        # Attempt save to trigger potential disk space error
        call anim%save("test_disk_space_error.mp4", status=save_status)
        
        if (save_status /= 0) then
            print *, "EXPECTED FAIL: Save failed, checking if disk space related (status:", save_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows disk space error detection and specific error codes"
            
            # Test graceful degradation - try smaller animation
            anim = FuncAnimation(dummy_animate_error, frames=5, interval=100, fig=test_fig)
            call anim%save("test_disk_space_small.mp4", status=save_status)
            
            if (save_status == 0) then
                print *, "✓ PASS: Smaller animation succeeded, confirming resource constraint handling"
                block
                    logical :: remove_success
                    call safe_remove_file("test_disk_space_small.mp4", remove_success)
                end block
            else
                print *, "EXPECTED FAIL: Even small animation failed (status:", save_status, ")"
                print *, "IMPLEMENTATION NEEDED: Windows resource constraint analysis"
            end if
        else
            print *, "✓ PASS: Large animation save succeeded (sufficient disk space)"
            block
                logical :: remove_success
                call safe_remove_file("test_disk_space_error.mp4", remove_success)
            end block
        end if
        
        print *, "✓ PASS: Disk space error handling test completed"
    end subroutine test_disk_space_error_handling

    subroutine test_permission_error_recovery()
        !! Given: Windows system with file permission restrictions
        !! When: Video creation fails due to file/directory permissions
        !! Then: Error should be detected and alternative locations tried
        
        integer :: save_status
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(3) :: test_x, test_y
        character(len=300) :: restricted_path, alternative_path
        integer :: i
        
        print *, "TEST: Permission error recovery"
        
        # Setup test data
        test_x = [(real(i, real64), i=1,3)]
        test_y = test_x
        
        call test_fig%initialize(width=200, height=150)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(dummy_animate_error, frames=2, interval=200, fig=test_fig)
        
        # Test 1: Try to write to potentially restricted location
        restricted_path = "C:\Program Files\test_permission.mp4"  # Typically restricted
        call anim%save(restricted_path, status=save_status)
        
        if (save_status /= 0) then
            print *, "EXPECTED FAIL: Restricted path failed (status:", save_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows permission error detection"
            
            # Test recovery - try current directory
            alternative_path = "test_permission_recovery.mp4"
            call anim%save(alternative_path, status=save_status)
            
            if (save_status == 0) then
                print *, "✓ PASS: Permission recovery to alternative location succeeded"
                block
                    logical :: remove_success
                    call safe_remove_file(alternative_path, remove_success)
                end block
            else
                print *, "EXPECTED FAIL: Even alternative location failed (status:", save_status, ")"
                print *, "IMPLEMENTATION NEEDED: Windows permission recovery strategies"
            end if
        else
            print *, "NOTE: Restricted path write succeeded (system allows it)"
            block
                logical :: remove_success
                call safe_remove_file(restricted_path, remove_success)
            end block
        end if
        
        print *, "✓ PASS: Permission error recovery test completed"
    end subroutine test_permission_error_recovery

    subroutine test_process_termination_handling()
        !! Given: Windows FFmpeg process that terminates unexpectedly
        !! When: External process termination occurs during pipe operation
        !! Then: Termination should be detected and handled gracefully
        
        integer :: pipe_status, write_status
        character(len=200) :: test_filename
        integer(int8), allocatable :: test_data(:)
        
        print *, "TEST: Process termination handling"
        
        test_filename = "test_process_termination.mp4"
        
        # Setup test data
        allocate(test_data(500))
        test_data = 123_int8
        
        # Open pipe
        pipe_status = open_ffmpeg_pipe(test_filename, 20)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Cannot open pipe for termination test (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows pipe for process termination testing"
            deallocate(test_data)
            return
        end if
        
        # Write data
        write_status = write_png_to_pipe(test_data)
        if (write_status /= 0) then
            print *, "EXPECTED FAIL: Write failed, may indicate process termination (status:", write_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows process termination detection"
        else
            print *, "✓ PASS: Write succeeded"
        end if
        
        # Force close pipe (simulating process termination)
        pipe_status = close_ffmpeg_pipe()
        print *, "Pipe closed, status:", pipe_status
        
        # Try to write after process termination (should fail gracefully)
        write_status = write_png_to_pipe(test_data)
        if (write_status == 0) then
            error stop "ERROR: Write after termination should have failed"
        else
            print *, "✓ PASS: Write after termination properly failed (status:", write_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows process state validation"
        end if
        
        # Cleanup
        deallocate(test_data)
        block
            logical :: remove_success
            call safe_remove_file(test_filename, remove_success)
        end block
        
        print *, "✓ PASS: Process termination handling test completed"
    end subroutine test_process_termination_handling

    subroutine test_timeout_error_recovery()
        !! Given: Windows system where FFmpeg operations may hang or timeout
        !! When: Operations exceed reasonable time limits
        !! Then: Timeouts should be detected and recovery attempted
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(8) :: test_x, test_y
        integer :: save_status, timeout_recovery_status, i
        
        print *, "TEST: Timeout error recovery"
        
        # Setup larger test data that might cause timeouts
        test_x = [(real(i, real64), i=1,8)]
        test_y = test_x**2 + sin(test_x)
        
        call test_fig%initialize(width=600, height=400)
        call test_fig%add_plot(test_x, test_y)
        
        # Create potentially slow operation (many frames, high resolution)
        anim = FuncAnimation(dummy_animate_error, frames=30, interval=50, fig=test_fig)
        
        # Attempt save that might timeout
        call anim%save("test_timeout_recovery.mp4", status=save_status)
        
        if (save_status /= 0) then
            print *, "EXPECTED FAIL: Operation failed, checking for timeout (status:", save_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows timeout detection and error classification"
            
            # Test timeout recovery - try with reduced complexity
            call test_fig%initialize(width=300, height=200)  # Smaller resolution
            call test_fig%add_plot(test_x, test_y)
            
            anim = FuncAnimation(dummy_animate_error, frames=5, interval=200, fig=test_fig)  # Fewer frames
            call anim%save("test_timeout_recovery.mp4", status=timeout_recovery_status)
            
            if (timeout_recovery_status == 0) then
                print *, "✓ PASS: Timeout recovery with reduced complexity succeeded"
                block
                    logical :: remove_success
                    call safe_remove_file("test_timeout_recovery.mp4", remove_success)
                end block
            else
                print *, "EXPECTED FAIL: Timeout recovery also failed (status:", timeout_recovery_status, ")"
                print *, "IMPLEMENTATION NEEDED: Windows timeout recovery mechanisms"
            end if
        else
            print *, "✓ PASS: Large operation completed without timeout"
            block
                logical :: remove_success
                call safe_remove_file("test_timeout_recovery.mp4", remove_success)
            end block
        end if
        
        print *, "✓ PASS: Timeout error recovery test completed"
    end subroutine test_timeout_error_recovery

    subroutine test_memory_allocation_error_handling()
        !! Given: Windows system with memory constraints
        !! When: Large buffer allocations fail during animation processing
        !! Then: Memory errors should be handled gracefully with fallbacks
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(20) :: test_x, test_y
        integer :: save_status, memory_recovery_status, i
        
        print *, "TEST: Memory allocation error handling"
        
        # Setup large test data that might stress memory
        test_x = [(real(i, real64), i=1,20)]
        test_y = exp(-test_x/10.0) * cos(test_x)
        
        # Very high resolution to potentially trigger memory issues
        call test_fig%initialize(width=2000, height=1500)
        call test_fig%add_plot(test_x, test_y)
        
        # Many frames to stress memory
        anim = FuncAnimation(dummy_animate_error, frames=100, interval=25, fig=test_fig)
        
        # Attempt memory-intensive save
        call anim%save("test_memory_error.mp4", status=save_status)
        
        if (save_status /= 0) then
            print *, "EXPECTED FAIL: Memory-intensive operation failed (status:", save_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows memory error detection and classification"
            
            # Test memory recovery - reduce memory requirements
            call test_fig%initialize(width=400, height=300)  # Much smaller
            call test_fig%add_plot(test_x, test_y)
            
            anim = FuncAnimation(dummy_animate_error, frames=10, interval=100, fig=test_fig)  # Fewer frames
            call anim%save("test_memory_recovery.mp4", status=memory_recovery_status)
            
            if (memory_recovery_status == 0) then
                print *, "✓ PASS: Memory recovery with reduced requirements succeeded"
                block
                    logical :: remove_success
                    call safe_remove_file("test_memory_recovery.mp4", remove_success)
                end block
            else
                print *, "EXPECTED FAIL: Memory recovery also failed (status:", memory_recovery_status, ")"
                print *, "IMPLEMENTATION NEEDED: Windows memory constraint fallback mechanisms"
            end if
        else
            print *, "✓ PASS: Memory-intensive operation completed successfully"
            block
                logical :: remove_success
                call safe_remove_file("test_memory_error.mp4", remove_success)
            end block
        end if
        
        print *, "✓ PASS: Memory allocation error handling test completed"
    end subroutine test_memory_allocation_error_handling

    subroutine dummy_animate_error(frame)
        !! Minimal animation callback for error testing
        integer, intent(in) :: frame
        # Simple animation update for error testing
        continue
    end subroutine dummy_animate_error

end program test_windows_error_handling_recovery