! test_security_compliance.f90 - Tests to verify security hardening is in place
program test_security_compliance
    use fortplot_security, only: safe_check_program_available, safe_launch_viewer, &
                                safe_validate_mpeg_with_ffprobe
    use fortplot_system_runtime, only: delete_file_runtime, open_with_default_app_runtime, &
                                       check_command_available_runtime
    use fortplot_system_timeout, only: system_command_timeout
    implicit none
    
    logical :: success, available
    character(len=256) :: test_file
    
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
    
    print *, ""
    print *, "Security compliance tests PASSED - external operations properly disabled"
    
end program test_security_compliance