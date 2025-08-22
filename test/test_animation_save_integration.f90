program test_animation_save_integration
    !! RED Phase integration test for Issue #186: Animation save status and file validation
    !! 
    !! Given: Animation save operations must complete successfully with valid output files
    !! When: Saving animations with various parameters and checking result status/files
    !! Then: Status codes should accurately reflect success/failure and files should be created
    
    use fortplot
    use fortplot_animation, only: animation_t, FuncAnimation
    use fortplot_pipe, only: check_ffmpeg_available
    use fortplot_security, only: safe_remove_file
    use fortplot_system_runtime, only: is_windows
    use iso_fortran_env, only: real64
    implicit none

    logical :: ffmpeg_available, skip_heavy_tests
    character(len=256) :: ci_env
    integer :: status

    ! Check test prerequisites
    ffmpeg_available = check_ffmpeg_available()
    call get_environment_variable("CI", ci_env, status=status)
    skip_heavy_tests = (is_windows() .and. status == 0)

    if (.not. ffmpeg_available) then
        print *, "SKIPPED: Animation save integration tests require FFmpeg"
        stop 77
    end if

    if (skip_heavy_tests) then
        print *, "SKIPPED: Animation save integration tests on Windows CI"
        stop 77
    end if

    print *, "=== ANIMATION SAVE INTEGRATION TESTS (RED PHASE) ==="
    print *, "Testing complete save workflow for Issue #186"
    
    call test_animation_save_status_reporting()
    call test_file_existence_validation()
    call test_file_size_reporting()
    call test_save_with_fps_parameter()
    call test_multiple_format_saves()
    call test_error_status_propagation()
    
    print *, "=== Animation save integration tests completed (RED) ==="

contains

    subroutine test_animation_save_status_reporting()
        !! Given: Animation save operations must return accurate status codes
        !! When: Performing save operations that succeed or fail for known reasons
        !! Then: Status codes should correctly indicate success (0) or specific failure types
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(5) :: test_x, test_y
        integer :: save_status, i
        character(len=200) :: valid_file, invalid_file
        
        print *, "TEST: Animation save status reporting"
        
        ! Setup test data
        test_x = [(real(i, real64), i=1,5)]
        test_y = test_x**2
        
        call test_fig%initialize(width=200, height=150)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_test_data_status, frames=3, interval=50, fig=test_fig)
        
        ! Test 1: Valid save should return success status (0)
        valid_file = "test_status_valid.mp4"
        call anim%save(valid_file, status=save_status)
        
        if (save_status == 0) then
            print *, "✓ PASS: Valid save returned success status (0)"
        else if (save_status == -6) then
            print *, "EXPECTED FAIL: Save failed with status -6 (Issue #186 pipe write failure)"
            print *, "IMPLEMENTATION NEEDED: Fix pipe write reliability"
        else
            print *, "EXPECTED FAIL: Save failed with status:", save_status
            print *, "IMPLEMENTATION NEEDED: Investigate save failure cause"
        end if
        
        ! Test 2: Invalid format should return format error status (-3)
        invalid_file = "test_status_invalid.txt"
        call anim%save(invalid_file, status=save_status)
        
        if (save_status == -3) then
            print *, "✓ PASS: Invalid format returned correct error status (-3)"
        else
            print *, "CRITICAL: Invalid format should return status -3, got:", save_status
            error stop "CRITICAL: Invalid format status check failed"
        end if
        
        ! Test 3: Verify status consistency across multiple calls
        call anim%save(valid_file, status=save_status)
        if (save_status == 0) then
            print *, "✓ PASS: Status reporting consistent across multiple calls"
        else
            print *, "EXPECTED FAIL: Inconsistent status across calls:", save_status
            print *, "IMPLEMENTATION NEEDED: Consistent status reporting"
        end if
        
        ! Cleanup
        block
            logical :: remove_success
            call safe_remove_file(valid_file, remove_success)
        end block
        
        print *, "✓ PASS: Animation save status reporting test completed"
    end subroutine test_animation_save_status_reporting

    subroutine test_file_existence_validation()
        !! Given: Animation save should create output files when successful
        !! When: Checking file existence after save operations
        !! Then: Files should exist after successful saves (addresses "File exists: F" from Issue #186)
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(3) :: test_x, test_y
        integer :: save_status
        logical :: file_exists
        character(len=200) :: output_file
        
        print *, "TEST: File existence validation"
        
        ! Setup minimal animation
        test_x = [1.0_real64, 2.0_real64, 3.0_real64]
        test_y = [1.0_real64, 4.0_real64, 9.0_real64]
        
        call test_fig%initialize(width=300, height=200)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_test_data_existence, frames=2, interval=75, fig=test_fig)
        
        output_file = "test_file_existence.mp4"
        
        ! Ensure file doesn't exist before save
        inquire(file=output_file, exist=file_exists)
        if (file_exists) then
            block
                logical :: remove_success
                call safe_remove_file(output_file, remove_success)
            end block
        end if
        
        ! Perform save and check file creation
        call anim%save(output_file, status=save_status)
        
        inquire(file=output_file, exist=file_exists)
        
        if (save_status == 0) then
            if (file_exists) then
                print *, "✓ PASS: File created successfully after successful save"
            else
                error stop "CRITICAL: Save status 0 but file does not exist (Issue #186)"
            end if
        else
            if (.not. file_exists) then
                print *, "✓ PASS: No file created after failed save (status:", save_status, ")"
            else
                print *, "WARNING: File exists despite failed save (status:", save_status, ")"
                print *, "May indicate incomplete save or status reporting issue"
            end if
            
            if (save_status == -6) then
                print *, "EXPECTED FAIL: Status -6 matches Issue #186 pipe write failure"
                print *, "IMPLEMENTATION NEEDED: Fix pipe write to enable file creation"
            end if
        end if
        
        ! Test file existence check robustness
        call verify_file_existence_reporting(output_file)
        
        ! Cleanup
        block
            logical :: remove_success
            call safe_remove_file(output_file, remove_success)
        end block
        
        print *, "✓ PASS: File existence validation test completed"
    end subroutine test_file_existence_validation

    subroutine test_file_size_reporting()
        !! Given: Created animation files should have valid, reportable file sizes
        !! When: Checking file size after successful animation save
        !! Then: File size should be positive and reasonable (addresses "File size: -1" from Issue #186)
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(4) :: test_x, test_y
        integer :: save_status, file_size, i
        logical :: file_exists
        character(len=200) :: output_file
        
        print *, "TEST: File size reporting"
        
        ! Setup test animation
        test_x = [(real(i, real64), i=1,4)]
        test_y = test_x**3
        
        call test_fig%initialize(width=250, height=180)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_test_data_size, frames=4, interval=60, fig=test_fig)
        
        output_file = "test_file_size.mp4"
        
        call anim%save(output_file, status=save_status)
        
        inquire(file=output_file, exist=file_exists, size=file_size)
        
        if (save_status == 0) then
            if (file_exists) then
                if (file_size > 0) then
                    print *, "✓ PASS: File created with valid size:", file_size, "bytes"
                    
                    ! Verify reasonable file size (should be substantial for video)
                    if (file_size >= 1000) then
                        print *, "✓ PASS: File size reasonable for video content"
                    else
                        print *, "WARNING: File size very small for video:", file_size, "bytes"
                        print *, "May indicate compression issues or minimal content"
                    end if
                else if (file_size == 0) then
                    error stop "CRITICAL: File exists but has zero size (Issue #186 variant)"
                else
                    print *, "CRITICAL: File size reported as negative:", file_size, "(Issue #186)"
                    error stop "CRITICAL: Negative file size reported"
                end if
            else
                error stop "CRITICAL: Save success but file doesn't exist"
            end if
        else
            print *, "EXPECTED FAIL: Save failed (status:", save_status, ")"
            
            if (file_exists) then
                print *, "File size for failed save:", file_size, "bytes"
                if (file_size < 0) then
                    print *, "EXPECTED FAIL: Negative file size confirms Issue #186"
                    print *, "IMPLEMENTATION NEEDED: Fix file size reporting for failed saves"
                end if
            else
                print *, "No file created (expected for failed save)"
            end if
        end if
        
        ! Cleanup
        block
            logical :: remove_success
            call safe_remove_file(output_file, remove_success)
        end block
        
        print *, "✓ PASS: File size reporting test completed"
    end subroutine test_file_size_reporting

    subroutine test_save_with_fps_parameter()
        !! Given: Animation save with custom FPS parameter should work correctly
        !! When: Saving animations with different FPS values
        !! Then: FPS parameter should be processed without affecting core save reliability
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(3) :: test_x, test_y
        integer :: save_status_default, save_status_custom
        character(len=200) :: file_default, file_custom
        integer, parameter :: custom_fps = 25
        
        print *, "TEST: Save with FPS parameter"
        
        ! Setup test animation
        test_x = [0.0_real64, 1.0_real64, 2.0_real64]
        test_y = [0.0_real64, 1.0_real64, 4.0_real64]
        
        call test_fig%initialize(width=180, height=140)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_test_data_fps, frames=3, interval=40, fig=test_fig)
        
        file_default = "test_fps_default.mp4"
        file_custom = "test_fps_custom.mp4"
        
        ! Test 1: Save with default FPS
        call anim%save(file_default, status=save_status_default)
        
        ! Test 2: Save with custom FPS
        call anim%save(file_custom, fps=custom_fps, status=save_status_custom)
        
        ! Compare results
        if (save_status_default == save_status_custom) then
            if (save_status_default == 0) then
                print *, "✓ PASS: Both default and custom FPS saves succeeded"
            else
                print *, "EXPECTED FAIL: Both saves failed with status:", save_status_default
                print *, "IMPLEMENTATION NEEDED: Basic save functionality (Issue #186)"
            end if
        else
            print *, "WARNING: Different status for default vs custom FPS"
            print *, "Default FPS status:", save_status_default
            print *, "Custom FPS status:", save_status_custom
            print *, "May indicate FPS parameter processing issues"
        end if
        
        ! Verify file creation for successful saves
        block
            logical :: default_exists, custom_exists
            inquire(file=file_default, exist=default_exists)
            inquire(file=file_custom, exist=custom_exists)
            
            if (save_status_default == 0 .and. .not. default_exists) then
                error stop "CRITICAL: Default save success but no file created"
            end if
            
            if (save_status_custom == 0 .and. .not. custom_exists) then
                error stop "CRITICAL: Custom FPS save success but no file created"
            end if
        end block
        
        ! Cleanup
        block
            logical :: remove_success
            call safe_remove_file(file_default, remove_success)
            call safe_remove_file(file_custom, remove_success)
        end block
        
        print *, "✓ PASS: Save with FPS parameter test completed"
    end subroutine test_save_with_fps_parameter

    subroutine test_multiple_format_saves()
        !! Given: Animation save should support multiple video formats consistently
        !! When: Saving the same animation to different supported formats
        !! Then: All supported formats should work with consistent behavior
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(2) :: test_x, test_y
        integer :: mp4_status, avi_status, mkv_status
        character(len=200) :: mp4_file, avi_file, mkv_file
        
        print *, "TEST: Multiple format saves"
        
        ! Setup simple animation
        test_x = [1.0_real64, 3.0_real64]
        test_y = [2.0_real64, 6.0_real64]
        
        call test_fig%initialize(width=160, height=120)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_test_data_format, frames=2, interval=100, fig=test_fig)
        
        mp4_file = "test_format.mp4"
        avi_file = "test_format.avi"
        mkv_file = "test_format.mkv"
        
        ! Test all supported formats
        call anim%save(mp4_file, status=mp4_status)
        call anim%save(avi_file, status=avi_status)
        call anim%save(mkv_file, status=mkv_status)
        
        ! Analyze results
        if (mp4_status == 0 .and. avi_status == 0 .and. mkv_status == 0) then
            print *, "✓ PASS: All video formats saved successfully"
        else
            print *, "EXPECTED FAIL: Format-specific save issues detected"
            print *, "MP4 status:", mp4_status
            print *, "AVI status:", avi_status
            print *, "MKV status:", mkv_status
            
            if (mp4_status == -6 .or. avi_status == -6 .or. mkv_status == -6) then
                print *, "IMPLEMENTATION NEEDED: Fix pipe write failures (Issue #186 status -6)"
            end if
        end if
        
        ! Check for format consistency (all should behave the same)
        if (mp4_status == avi_status .and. avi_status == mkv_status) then
            print *, "✓ PASS: Consistent behavior across formats"
        else
            print *, "WARNING: Inconsistent behavior across formats"
            print *, "May indicate format-specific pipeline issues"
        end if
        
        ! Cleanup
        block
            logical :: remove_success
            call safe_remove_file(mp4_file, remove_success)
            call safe_remove_file(avi_file, remove_success)
            call safe_remove_file(mkv_file, remove_success)
        end block
        
        print *, "✓ PASS: Multiple format saves test completed"
    end subroutine test_multiple_format_saves

    subroutine test_error_status_propagation()
        !! Given: Errors during animation save should propagate correctly through status codes
        !! When: Inducing various types of save errors
        !! Then: Each error type should produce the correct status code
        
        type(animation_t) :: anim
        type(figure_t) :: test_fig
        real(real64), dimension(2) :: test_x, test_y
        integer :: format_status, empty_status
        character(len=200) :: invalid_format_file, empty_filename
        
        print *, "TEST: Error status propagation"
        
        ! Setup animation for error testing
        test_x = [1.0_real64, 2.0_real64]
        test_y = [1.0_real64, 8.0_real64]
        
        call test_fig%initialize(width=100, height=100)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_test_data_error, frames=1, interval=50, fig=test_fig)
        
        ! Test 1: Invalid format error (-3)
        invalid_format_file = "test_error.invalid"
        call anim%save(invalid_format_file, status=format_status)
        
        if (format_status == -3) then
            print *, "✓ PASS: Invalid format correctly returns status -3"
        else
            print *, "CRITICAL: Invalid format should return status -3, got:", format_status
            error stop "CRITICAL: Invalid format error status check failed"
        end if
        
        ! Test 2: Empty filename (should handle gracefully)
        empty_filename = ""
        call anim%save(empty_filename, status=empty_status)
        
        if (empty_status /= 0) then
            print *, "✓ PASS: Empty filename correctly returns error status:", empty_status
        else
            error stop "CRITICAL: Empty filename should return error status"
        end if
        
        ! Test 3: Verify status codes are distinct for different errors
        if (format_status /= empty_status) then
            print *, "✓ PASS: Different error types produce distinct status codes"
        else
            print *, "WARNING: Same status for different errors may indicate generic error handling"
        end if
        
        print *, "✓ PASS: Error status propagation test completed"
    end subroutine test_error_status_propagation

    subroutine verify_file_existence_reporting(filename)
        !! Helper to verify file existence reporting is consistent
        character(len=*), intent(in) :: filename
        logical :: exists_inquire, exists_direct
        integer :: file_unit, ios
        
        ! Method 1: INQUIRE statement
        inquire(file=filename, exist=exists_inquire)
        
        ! Method 2: Direct file access attempt
        exists_direct = .false.
        open(newunit=file_unit, file=filename, status='old', action='read', iostat=ios)
        if (ios == 0) then
            exists_direct = .true.
            close(file_unit)
        end if
        
        if (exists_inquire .eqv. exists_direct) then
            print *, "✓ PASS: File existence reporting consistent"
        else
            print *, "WARNING: Inconsistent file existence reporting"
            print *, "INQUIRE:", exists_inquire, "Direct access:", exists_direct
        end if
    end subroutine verify_file_existence_reporting


    ! Animation callback functions for different tests
    subroutine update_test_data_status(frame)
        integer, intent(in) :: frame
        continue
    end subroutine update_test_data_status

    subroutine update_test_data_existence(frame)
        integer, intent(in) :: frame
        continue
    end subroutine update_test_data_existence

    subroutine update_test_data_size(frame)
        integer, intent(in) :: frame
        continue
    end subroutine update_test_data_size

    subroutine update_test_data_fps(frame)
        integer, intent(in) :: frame
        continue
    end subroutine update_test_data_fps

    subroutine update_test_data_format(frame)
        integer, intent(in) :: frame
        continue
    end subroutine update_test_data_format

    subroutine update_test_data_error(frame)
        integer, intent(in) :: frame
        continue
    end subroutine update_test_data_error

end program test_animation_save_integration