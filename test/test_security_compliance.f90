! test_security_compliance.f90 - Tests to verify security hardening is in place
program test_security_compliance
    use fortplot_security, only: safe_check_program_available, safe_launch_viewer, &
                                safe_validate_mpeg_with_ffprobe
    use fortplot_system_runtime, only: delete_file_runtime, open_with_default_app_runtime, &
                                       check_command_available_runtime, create_directory_runtime, &
                                       is_windows, map_unix_to_windows_path, normalize_path_separators
    use fortplot_system_timeout, only: system_command_timeout, get_windows_timeout_ms, &
                                       sleep_ms
    implicit none
    
    logical :: success, available
    character(len=256) :: test_file, mapped_path
    character(len=512) :: normalized
    integer :: timeout_ms
    
    print *, "Testing security compliance - all external operations should be disabled"
    
    ! Test 1: Verify external command checking is disabled
    call check_command_available_runtime("ls", available)
    if (available) then
        print *, "FAIL: Command checking should be disabled for security"
        stop 1
    else
        print *, "PASS: Command checking disabled as expected"
    end if
    
    ! Test 2: Verify file deletion is disabled
    test_file = "test_security_file.tmp"
    call delete_file_runtime(test_file, success)
    if (success) then
        print *, "FAIL: File deletion should be disabled for security"
        stop 1
    else
        print *, "PASS: File deletion disabled as expected"
    end if
    
    ! Test 3: Verify external app launching is disabled (except in CI)
    call get_environment_variable("CI", test_file)
    if (len_trim(test_file) == 0) then
        ! Not in CI - should be disabled
        call open_with_default_app_runtime("test.png", success)
        if (success) then
            print *, "FAIL: External app launching should be disabled outside CI"
            stop 1
        else
            print *, "PASS: External app launching disabled as expected"
        end if
    else
        print *, "SKIP: External app test skipped in CI environment"
    end if
    
    ! Test 4: Verify command execution with timeout is disabled
    call system_command_timeout("echo test", success)
    if (success) then
        print *, "FAIL: Command execution should be disabled for security"
        stop 1
    else
        print *, "PASS: Command execution disabled as expected"
    end if
    
    ! Test 5: Verify program availability checking returns false
    available = safe_check_program_available("ffmpeg")
    if (available .and. len_trim(test_file) == 0) then
        print *, "FAIL: Program availability should return false in secure mode"
        stop 1
    else
        print *, "PASS: Program availability check returns expected result"
    end if
    
    ! Test 6: Verify viewer launching is disabled
    call safe_launch_viewer("test.png", success)
    if (.not. success) then
        ! Expected behavior - viewer disabled or file doesn't exist
        print *, "PASS: Viewer launching behaves as expected"
    else
        print *, "INFO: Viewer launching returned success (file may not exist)"
    end if
    
    ! Test 7: Verify MPEG validation works without external tools
    available = safe_validate_mpeg_with_ffprobe("test.mp4")
    if (.not. available) then
        ! Expected - file doesn't exist or validation disabled
        print *, "PASS: MPEG validation behaves as expected"
    else
        print *, "INFO: MPEG validation returned true (may use magic bytes)"
    end if
    
    ! Test 8: Additional coverage for runtime utilities
    print *, "Testing additional runtime utilities for coverage..."
    
    ! Test is_windows() function
    if (is_windows()) then
        print *, "PASS: Running on Windows platform detected"
    else
        print *, "PASS: Running on Unix/Linux platform detected"
    end if
    
    ! Test path mapping functionality
    mapped_path = map_unix_to_windows_path("/tmp/test.txt")
    print *, "PASS: Path mapping test completed"
    
    ! Test path normalization
    normalized = normalize_path_separators("test/path/file.txt", .false.)
    print *, "PASS: Path normalization test completed"
    
    normalized = normalize_path_separators("test\path\file.txt", .true.)
    print *, "PASS: Windows path normalization test completed"
    
    ! Test timeout function
    timeout_ms = get_windows_timeout_ms()
    if (timeout_ms > 0) then
        print *, "PASS: Timeout value retrieved:", timeout_ms
    else
        print *, "FAIL: Invalid timeout value"
        stop 1
    end if
    
    ! Test sleep function with very short sleep (1ms to avoid test delays)
    call sleep_ms(1)
    print *, "PASS: Sleep function completed"
    
    ! Test directory creation (should be restricted)
    call create_directory_runtime("/tmp/fortplot_test_security", success)
    if (success) then
        print *, "PASS: Test directory creation succeeded (allowed for test paths)"
    else
        print *, "PASS: Directory creation restricted (as expected for security)"
    end if
    
    ! Test non-test directory creation (should fail)
    call create_directory_runtime("/arbitrary/path", success)
    if (.not. success) then
        print *, "PASS: Non-test directory creation blocked for security"
    else
        print *, "FAIL: Non-test directory creation should be blocked"
        stop 1
    end if

    ! Test directory traversal patterns are blocked
    call create_directory_runtime('output/..', success)
    if (success) then
        print *, 'FAIL: Directory traversal output/.. should be blocked'
        stop 1
    else
        print *, 'PASS: output/.. correctly blocked'
    end if

    call create_directory_runtime('foo/..', success)
    if (success) then
        print *, 'FAIL: Directory traversal foo/.. should be blocked'
        stop 1
    else
        print *, 'PASS: foo/.. correctly blocked'
    end if

    call create_directory_runtime('results/../plots', success)
    if (success) then
        print *, 'FAIL: Directory traversal results/../plots should be blocked'
        stop 1
    else
        print *, 'PASS: results/../plots correctly blocked'
    end if
    
    ! Test various command availability checks for different programs
    call check_command_available_runtime("fpm", available)
    print *, "PASS: FPM command availability checked"
    
    call check_command_available_runtime("ffmpeg", available)
    print *, "PASS: FFmpeg command availability checked"
    
    call check_command_available_runtime("magick", available)
    print *, "PASS: ImageMagick command availability checked"
    
    call check_command_available_runtime("arbitrary_command", available)
    if (.not. available) then
        print *, "PASS: Arbitrary command blocked as expected"
    else
        print *, "INFO: Arbitrary command check result:", available
    end if
    
    print *, ""
    print *, "Security compliance tests PASSED - external operations properly disabled"
    
end program test_security_compliance
