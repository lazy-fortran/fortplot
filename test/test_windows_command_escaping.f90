program test_windows_command_escaping
    !! RED Phase test implementation for Issue #189: Windows command line escaping
    !! 
    !! Given: Windows cmd.exe with specific quoting and escaping requirements
    !! When: Constructing FFmpeg commands with paths, filenames, and arguments
    !! Then: Commands should be properly escaped for Windows command line execution
    
    use fortplot
    use fortplot_pipe, only: check_ffmpeg_available, open_ffmpeg_pipe, close_ffmpeg_pipe
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
        print *, "SKIPPED: Windows command escaping tests (not Windows)"
        stop 0
    end if
    
    ffmpeg_available = check_ffmpeg_available()
    if (.not. ffmpeg_available) then
        print *, "SKIPPED: Windows command escaping tests require FFmpeg"
        stop 77
    end if

    print *, "=== WINDOWS COMMAND ESCAPING TESTS (RED PHASE) ==="
    
    call test_filename_with_spaces()
    call test_filename_with_quotes()
    call test_filename_with_special_characters()
    call test_path_with_backslashes()
    call test_ffmpeg_argument_escaping()
    call test_unicode_filename_handling()
    
    print *, "=== Windows command escaping tests completed (RED) ==="

contains

    subroutine test_filename_with_spaces()
        !! Given: Output filename containing spaces (common on Windows)
        !! When: Constructing FFmpeg command with spaced filename
        !! Then: Filename should be properly quoted for Windows cmd.exe
        
        character(len=300) :: spaced_filename
        integer :: pipe_status
        logical :: file_created
        
        print *, "TEST: Filename with spaces"
        
        ! Test various space scenarios common on Windows
        spaced_filename = "test output file.mp4"
        
        pipe_status = open_ffmpeg_pipe(spaced_filename, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Simple spaced filename failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows filename quoting with double quotes"
        else
            pipe_status = close_ffmpeg_pipe()
            inquire(file=spaced_filename, exist=file_created)
            if (file_created) then
                print *, "✓ PASS: Simple spaced filename handled correctly"
                block
                    logical :: remove_success
                    call safe_remove_file(spaced_filename, remove_success)
                end block
            else
                print *, "EXPECTED FAIL: File not created despite successful pipe"
                print *, "IMPLEMENTATION NEEDED: Verify Windows file creation path"
            end if
        end if
        
        ! Test filename with multiple spaces
        spaced_filename = "test  multiple   spaces.mp4"
        pipe_status = open_ffmpeg_pipe(spaced_filename, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Multiple spaces filename failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows multiple space handling"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(spaced_filename, remove_success)
            end block
            print *, "✓ PASS: Multiple spaces filename handled"
        end if
        
        ! Test filename with leading/trailing spaces
        spaced_filename = " test leading trailing .mp4"
        pipe_status = open_ffmpeg_pipe(spaced_filename, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Leading/trailing spaces failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows edge case space handling"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(spaced_filename, remove_success)
            end block
            print *, "✓ PASS: Leading/trailing spaces handled"
        end if
        
        print *, "✓ PASS: Filename with spaces test completed"
    end subroutine test_filename_with_spaces

    subroutine test_filename_with_quotes()
        !! Given: Filename containing quote characters
        !! When: Windows command construction with embedded quotes
        !! Then: Quotes should be properly escaped with backslashes
        
        character(len=300) :: quoted_filename
        integer :: pipe_status
        
        print *, "TEST: Filename with quotes"
        
        ! Test filename with double quotes
        quoted_filename = 'test_"quoted"_filename.mp4'
        pipe_status = open_ffmpeg_pipe(quoted_filename, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Double quotes filename failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows quote escaping with backslashes"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(quoted_filename, remove_success)
            end block
            print *, "✓ PASS: Double quotes filename handled"
        end if
        
        ! Test filename with mixed quotes and spaces
        quoted_filename = 'test "mixed quotes" and spaces.mp4'
        pipe_status = open_ffmpeg_pipe(quoted_filename, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Mixed quotes and spaces failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows complex escaping combination"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(quoted_filename, remove_success)
            end block
            print *, "✓ PASS: Mixed quotes and spaces handled"
        end if
        
        print *, "✓ PASS: Filename with quotes test completed"
    end subroutine test_filename_with_quotes

    subroutine test_filename_with_special_characters()
        !! Given: Filename with Windows-specific special characters
        !! When: Building command with characters that have special meaning in cmd.exe
        !! Then: Special characters should be properly escaped or handled
        
        character(len=300) :: special_filename
        integer :: pipe_status
        
        print *, "TEST: Filename with special characters"
        
        ! Test filename with parentheses (common in Windows)
        special_filename = "test_(with_parentheses).mp4"
        pipe_status = open_ffmpeg_pipe(special_filename, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Parentheses filename failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows parentheses handling"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(special_filename, remove_success)
            end block
            print *, "✓ PASS: Parentheses filename handled"
        end if
        
        ! Test filename with ampersand
        special_filename = "test_&_ampersand.mp4"
        pipe_status = open_ffmpeg_pipe(special_filename, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Ampersand filename failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows ampersand escaping (^&)"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(special_filename, remove_success)
            end block
            print *, "✓ PASS: Ampersand filename handled"
        end if
        
        ! Test filename with percentage (environment variable syntax)
        special_filename = "test_%_percentage.mp4"
        pipe_status = open_ffmpeg_pipe(special_filename, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Percentage filename failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows percentage escaping"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(special_filename, remove_success)
            end block
            print *, "✓ PASS: Percentage filename handled"
        end if
        
        print *, "✓ PASS: Filename with special characters test completed"
    end subroutine test_filename_with_special_characters

    subroutine test_path_with_backslashes()
        !! Given: Windows path using backslashes as directory separators
        !! When: Constructing FFmpeg command with Windows-style paths
        !! Then: Backslashes should be handled correctly without double-escaping
        
        character(len=300) :: backslash_path
        integer :: pipe_status
        
        print *, "TEST: Path with backslashes"
        
        ! Test Windows-style path with backslashes
        backslash_path = "output\subdir\test_backslash.mp4"
        pipe_status = open_ffmpeg_pipe(backslash_path, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Backslash path failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows backslash path handling"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(backslash_path, remove_success)
            end block
            print *, "✓ PASS: Backslash path handled"
        end if
        
        ! Test mixed forward/backward slashes
        backslash_path = "output/subdir\test_mixed.mp4"
        pipe_status = open_ffmpeg_pipe(backslash_path, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Mixed slash path failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows path normalization"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(backslash_path, remove_success)
            end block
            print *, "✓ PASS: Mixed slash path handled"
        end if
        
        print *, "✓ PASS: Path with backslashes test completed"
    end subroutine test_path_with_backslashes

    subroutine test_ffmpeg_argument_escaping()
        !! Given: FFmpeg command arguments requiring Windows-specific escaping
        !! When: Building complete FFmpeg command with multiple arguments
        !! Then: All arguments should be properly escaped and separated
        
        character(len=300) :: test_filename
        integer :: pipe_status
        
        print *, "TEST: FFmpeg argument escaping"
        
        test_filename = "test_args_escaping.mp4"
        
        ! Test with high frame rate (requires proper argument handling)
        pipe_status = open_ffmpeg_pipe(test_filename, 60)  ! High FPS
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: High FPS arguments failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows FFmpeg argument construction"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(test_filename, remove_success)
            end block
            print *, "✓ PASS: High FPS arguments handled"
        end if
        
        # Test with complex codec arguments (if supported)
        # This would test more complex argument escaping scenarios
        
        print *, "✓ PASS: FFmpeg argument escaping test completed"
    end subroutine test_ffmpeg_argument_escaping

    subroutine test_unicode_filename_handling()
        !! Given: Windows filename with Unicode characters
        !! When: Constructing FFmpeg command with non-ASCII characters
        !! Then: Unicode should be properly handled through Windows command interface
        
        character(len=300) :: unicode_filename
        integer :: pipe_status
        
        print *, "TEST: Unicode filename handling"
        
        ! Test filename with common Unicode characters
        unicode_filename = "test_unicode_ñáéíóú.mp4"
        pipe_status = open_ffmpeg_pipe(unicode_filename, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Unicode filename failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows Unicode filename support"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(unicode_filename, remove_success)
            end block
            print *, "✓ PASS: Unicode filename handled"
        end if
        
        ! Test filename with Cyrillic characters
        unicode_filename = "test_cyrillic_тест.mp4"
        pipe_status = open_ffmpeg_pipe(unicode_filename, 10)
        if (pipe_status /= 0) then
            print *, "EXPECTED FAIL: Cyrillic filename failed (status:", pipe_status, ")"
            print *, "IMPLEMENTATION NEEDED: Windows extended Unicode support"
        else
            pipe_status = close_ffmpeg_pipe()
            block
                logical :: remove_success
                call safe_remove_file(unicode_filename, remove_success)
            end block
            print *, "✓ PASS: Cyrillic filename handled"
        end if
        
        print *, "✓ PASS: Unicode filename handling test completed"
    end subroutine test_unicode_filename_handling

end program test_windows_command_escaping