program test_windows_timeout_debug
    !! Test Windows timeout mechanisms for CI hang prevention
    use fortplot_system_runtime, only: is_windows, check_command_available_runtime
    use fortplot_pipe, only: check_ffmpeg_available, check_ffmpeg_available_timeout
    implicit none
    
    logical :: result
    
    print *, "=== Windows Timeout Debug Test ==="
    print *, "Running on Windows:", is_windows()
    print *, ""
    
    ! Test 1: Check command availability with timeout
    print *, "Test 1: Command availability check"
    call check_command_available_runtime("echo", result)
    print *, "  echo command available:", result
    
    call check_command_available_runtime("nonexistent_command_xyz", result)
    print *, "  nonexistent command available:", result
    print *, ""
    
    ! Test 2: FFmpeg availability checks
    print *, "Test 2: FFmpeg availability checks"
    print *, "  Standard FFmpeg check:"
    result = check_ffmpeg_available()
    print *, "    Result:", result
    print *, ""
    
    print *, "  Timeout-protected FFmpeg check:"
    result = check_ffmpeg_available_timeout()
    print *, "    Result:", result
    print *, ""
    
    ! Test 3: Windows-specific command that might hang
    if (is_windows()) then
        print *, "Test 3: Windows-specific commands"
        call check_command_available_runtime("where", result)
        print *, "  'where' command available:", result
        
        call check_command_available_runtime("timeout", result)
        print *, "  'timeout' command available:", result
    end if
    
    print *, ""
    print *, "=== Timeout Debug Test Complete ==="
    
    ! Success if we got here without hanging
    print *, "SUCCESS: No hangs detected"
    
end program test_windows_timeout_debug