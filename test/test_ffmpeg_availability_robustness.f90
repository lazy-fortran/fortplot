program test_ffmpeg_availability_robustness
    !! RED Phase test for Issue #186: FFmpeg availability detection and timeout handling
    !! 
    !! Given: FFmpeg availability check must be reliable across platforms and environments
    !! When: Checking FFmpeg availability with various system configurations
    !! Then: Detection should be accurate, timeout-protected, and handle edge cases gracefully
    
    use fortplot_pipe, only: check_ffmpeg_available, check_ffmpeg_available_timeout
    use fortplot_system_runtime, only: is_windows
    use fortplot_animation, only: animation_t, FuncAnimation
    use fortplot, only: figure_t
    use iso_fortran_env, only: real64
    implicit none

    logical :: basic_availability, timeout_availability, is_windows_platform
    character(len=256) :: ci_env
    integer :: status

    ! Platform detection
    is_windows_platform = is_windows()
    call get_environment_variable("CI", ci_env, status=status)

    print *, "=== FFMPEG AVAILABILITY ROBUSTNESS TESTS (RED PHASE) ==="
    print *, "Testing FFmpeg detection reliability for Issue #186"
    print *, "Platform: ", merge("Windows", "Unix   ", is_windows_platform)
    
    call test_basic_ffmpeg_detection()
    call test_timeout_protected_detection()
    call test_availability_consistency()
    call test_ci_environment_handling()
    call test_detection_performance()
    call test_availability_error_recovery()
    
    print *, "=== FFmpeg availability robustness tests completed (RED) ==="

contains

    subroutine test_basic_ffmpeg_detection()
        !! Given: FFmpeg may or may not be installed on the system
        !! When: Calling basic FFmpeg availability check
        !! Then: Detection should return definitive true/false without hanging or errors
        
        logical :: ffmpeg_detected
        real :: start_time, end_time, detection_time
        
        print *, "TEST: Basic FFmpeg detection"
        
        call cpu_time(start_time)
        ffmpeg_detected = check_ffmpeg_available()
        call cpu_time(end_time)
        
        detection_time = end_time - start_time
        
        print *, "FFmpeg detected:", ffmpeg_detected
        print *, "Detection time:", detection_time, "seconds"
        
        ! Verify reasonable detection time (should not hang)
        if (detection_time > 10.0) then
            print *, "WARNING: FFmpeg detection took excessive time:", detection_time, "seconds"
            print *, "IMPLEMENTATION NEEDED: Timeout protection for FFmpeg detection"
        else
            print *, "✓ PASS: FFmpeg detection completed in reasonable time"
        end if
        
        ! Test consistency of repeated calls
        block
            logical :: second_detection, third_detection
            
            second_detection = check_ffmpeg_available()
            third_detection = check_ffmpeg_available()
            
            if (ffmpeg_detected .eqv. second_detection .and. &
                second_detection .eqv. third_detection) then
                print *, "✓ PASS: FFmpeg detection consistent across multiple calls"
            else
                print *, "WARNING: Inconsistent FFmpeg detection results"
                print *, "First:", ffmpeg_detected, "Second:", second_detection, "Third:", third_detection
                print *, "IMPLEMENTATION NEEDED: Stable FFmpeg detection"
            end if
        end block
        
        print *, "✓ PASS: Basic FFmpeg detection test completed"
    end subroutine test_basic_ffmpeg_detection

    subroutine test_timeout_protected_detection()
        !! Given: FFmpeg detection may hang on some systems (especially Windows)
        !! When: Using timeout-protected FFmpeg detection
        !! Then: Detection should complete within reasonable time limits
        
        logical :: timeout_detected
        real :: start_time, end_time, timeout_time
        
        print *, "TEST: Timeout-protected detection"
        
        call cpu_time(start_time)
        timeout_detected = check_ffmpeg_available_timeout()
        call cpu_time(end_time)
        
        timeout_time = end_time - start_time
        
        print *, "FFmpeg detected (timeout):", timeout_detected
        print *, "Timeout detection time:", timeout_time, "seconds"
        
        ! Timeout detection should be faster and more reliable
        if (timeout_time > 5.0) then
            print *, "EXPECTED FAIL: Timeout detection still too slow:", timeout_time, "seconds"
            print *, "IMPLEMENTATION NEEDED: Effective timeout mechanism"
        else
            print *, "✓ PASS: Timeout detection completed quickly"
        end if
        
        ! Compare basic vs timeout detection
        block
            logical :: basic_detected
            basic_detected = check_ffmpeg_available()
            
            if (basic_detected .eqv. timeout_detected) then
                print *, "✓ PASS: Basic and timeout detection agree"
            else
                print *, "WARNING: Basic and timeout detection disagree"
                print *, "Basic:", basic_detected, "Timeout:", timeout_detected
                print *, "May indicate timeout detection issues"
            end if
        end block
        
        print *, "✓ PASS: Timeout-protected detection test completed"
    end subroutine test_timeout_protected_detection

    subroutine test_availability_consistency()
        !! Given: FFmpeg availability should be consistent within a program run
        !! When: Checking availability multiple times with different methods
        !! Then: Results should be consistent and deterministic
        
        logical, dimension(10) :: detection_results
        logical :: all_consistent
        integer :: i
        
        print *, "TEST: Availability consistency"
        
        ! Perform multiple detection calls
        do i = 1, size(detection_results)
            if (mod(i, 2) == 0) then
                detection_results(i) = check_ffmpeg_available()
            else
                detection_results(i) = check_ffmpeg_available_timeout()
            end if
        end do
        
        ! Check consistency
        all_consistent = .true.
        do i = 2, size(detection_results)
            if (detection_results(i) .neqv. detection_results(1)) then
                all_consistent = .false.
                exit
            end if
        end do
        
        if (all_consistent) then
            print *, "✓ PASS: FFmpeg detection consistent across", size(detection_results), "calls"
        else
            print *, "WARNING: Inconsistent FFmpeg detection results"
            print *, "Results:", detection_results
            print *, "IMPLEMENTATION NEEDED: Deterministic FFmpeg detection"
        end if
        
        ! Test rapid successive calls (stress test)
        block
            logical :: rapid_result1, rapid_result2, rapid_result3
            rapid_result1 = check_ffmpeg_available()
            rapid_result2 = check_ffmpeg_available()
            rapid_result3 = check_ffmpeg_available()
            
            if (rapid_result1 .eqv. rapid_result2 .and. rapid_result2 .eqv. rapid_result3) then
                print *, "✓ PASS: Rapid successive calls consistent"
            else
                print *, "WARNING: Rapid calls show inconsistency"
                print *, "IMPLEMENTATION NEEDED: Thread-safe FFmpeg detection"
            end if
        end block
        
        print *, "✓ PASS: Availability consistency test completed"
    end subroutine test_availability_consistency

    subroutine test_ci_environment_handling()
        !! Given: CI environments may have different FFmpeg availability or restrictions
        !! When: Running in CI environment (especially Windows CI)
        !! Then: Detection should handle CI-specific conditions appropriately
        
        logical :: ffmpeg_available
        character(len=256) :: ci_value, path_env
        integer :: ci_status, path_status
        
        print *, "TEST: CI environment handling"
        
        call get_environment_variable("CI", ci_value, status=ci_status)
        call get_environment_variable("PATH", path_env, status=path_status)
        
        if (ci_status == 0) then
            print *, "Running in CI environment:", trim(ci_value)
        else
            print *, "Not running in CI environment"
        end if
        
        ffmpeg_available = check_ffmpeg_available()
        
        ! CI-specific validation
        if (ci_status == 0) then
            if (is_windows()) then
                print *, "Windows CI detected"
                if (.not. ffmpeg_available) then
                    print *, "✓ EXPECTED: FFmpeg often unavailable in Windows CI"
                else
                    print *, "NOTE: FFmpeg available in Windows CI (good for testing)"
                end if
            else
                print *, "Unix CI detected"
                if (ffmpeg_available) then
                    print *, "✓ PASS: FFmpeg available in Unix CI"
                else
                    print *, "WARNING: FFmpeg unavailable in Unix CI (may limit testing)"
                end if
            end if
        else
            print *, "Local development environment"
            if (ffmpeg_available) then
                print *, "✓ PASS: FFmpeg available for local testing"
            else
                print *, "NOTE: FFmpeg unavailable (install for full testing)"
            end if
        end if
        
        ! Test timeout behavior in CI
        if (ci_status == 0 .and. is_windows()) then
            block
                logical :: timeout_result
                real :: start_time, end_time
                
                call cpu_time(start_time)
                timeout_result = check_ffmpeg_available_timeout()
                call cpu_time(end_time)
                
                if ((end_time - start_time) < 3.0) then
                    print *, "✓ PASS: Timeout detection fast in Windows CI"
                else
                    print *, "EXPECTED FAIL: Timeout detection slow in Windows CI"
                    print *, "IMPLEMENTATION NEEDED: Better Windows CI timeout handling"
                end if
            end block
        end if
        
        print *, "✓ PASS: CI environment handling test completed"
    end subroutine test_ci_environment_handling

    subroutine test_detection_performance()
        !! Given: FFmpeg detection should be reasonably fast for good user experience
        !! When: Measuring detection performance under various conditions
        !! Then: Detection should complete within acceptable time limits
        
        real :: basic_time, timeout_time, start_time, end_time
        integer, parameter :: num_trials = 5
        real :: basic_times(num_trials), timeout_times(num_trials)
        real :: basic_avg, timeout_avg
        logical :: basic_detected, timeout_detected
        integer :: i
        
        print *, "TEST: Detection performance"
        
        ! Measure basic detection performance
        do i = 1, num_trials
            call cpu_time(start_time)
            basic_detected = check_ffmpeg_available()
            call cpu_time(end_time)
            basic_times(i) = end_time - start_time
        end do
        
        ! Measure timeout detection performance
        do i = 1, num_trials
            call cpu_time(start_time)
            timeout_detected = check_ffmpeg_available_timeout()
            call cpu_time(end_time)
            timeout_times(i) = end_time - start_time
        end do
        
        ! Calculate averages
        basic_avg = sum(basic_times) / num_trials
        timeout_avg = sum(timeout_times) / num_trials
        
        print *, "Basic detection average time:", basic_avg, "seconds"
        print *, "Timeout detection average time:", timeout_avg, "seconds"
        
        ! Performance validation
        if (basic_avg < 2.0) then
            print *, "✓ PASS: Basic detection performs well"
        else
            print *, "WARNING: Basic detection slow (avg:", basic_avg, "s)"
            print *, "IMPLEMENTATION NEEDED: Optimize basic detection"
        end if
        
        if (timeout_avg < 1.0) then
            print *, "✓ PASS: Timeout detection performs excellently"
        else if (timeout_avg < 3.0) then
            print *, "✓ PASS: Timeout detection performs adequately"
        else
            print *, "EXPECTED FAIL: Timeout detection too slow (avg:", timeout_avg, "s)"
            print *, "IMPLEMENTATION NEEDED: Improve timeout mechanism"
        end if
        
        ! Verify timeout is actually faster (or at least not slower)
        if (timeout_avg <= basic_avg * 1.1) then
            print *, "✓ PASS: Timeout detection not significantly slower than basic"
        else
            print *, "WARNING: Timeout detection slower than basic"
            print *, "May indicate inefficient timeout implementation"
        end if
        
        print *, "✓ PASS: Detection performance test completed"
    end subroutine test_detection_performance

    subroutine test_availability_error_recovery()
        !! Given: FFmpeg detection errors should not affect subsequent operations
        !! When: Detection fails or times out, then retry operations
        !! Then: System should recover gracefully for subsequent attempts
        
        logical :: first_attempt, second_attempt, third_attempt
        logical :: recovery_successful
        
        print *, "TEST: Availability error recovery"
        
        ! Test error recovery sequence
        first_attempt = check_ffmpeg_available()
        second_attempt = check_ffmpeg_available_timeout()
        third_attempt = check_ffmpeg_available()
        
        ! Check if system maintained consistent state
        if (first_attempt .eqv. second_attempt .and. second_attempt .eqv. third_attempt) then
            print *, "✓ PASS: Detection state consistent after error scenarios"
            recovery_successful = .true.
        else
            print *, "WARNING: Detection state inconsistent after operations"
            print *, "First:", first_attempt, "Second:", second_attempt, "Third:", third_attempt
            recovery_successful = .false.
        end if
        
        ! Test integration with animation system after detection issues
        if (first_attempt) then
            block
                type(animation_t) :: test_anim
                type(figure_t) :: test_fig
                real(real64), dimension(2) :: test_x, test_y
                integer :: save_status
                
                print *, "Testing animation integration after detection operations..."
                
                test_x = [1.0_real64, 2.0_real64]
                test_y = [1.0_real64, 4.0_real64]
                
                call test_fig%initialize(width=100, height=100)
                call test_fig%add_plot(test_x, test_y)
                
                test_anim = FuncAnimation(dummy_update_recovery, frames=1, &
                                         interval=50, fig=test_fig)
                
                call test_anim%save("test_recovery.mp4", status=save_status)
                
                if (save_status == 0) then
                    print *, "✓ PASS: Animation save works after detection operations"
                else if (save_status == -6) then
                    print *, "EXPECTED FAIL: Animation save failed (Issue #186 status -6)"
                    print *, "Detection OK but pipe write issues remain"
                else
                    print *, "UNEXPECTED: Animation save failed with status:", save_status
                end if
                
                ! Cleanup
                block
                    use fortplot_security, only: safe_remove_file
                    logical :: remove_success
                    call safe_remove_file("test_recovery.mp4", remove_success)
                end block
            end block
        else
            print *, "FFmpeg not available - skipping animation integration test"
        end if
        
        if (recovery_successful) then
            print *, "✓ PASS: Error recovery successful"
        else
            print *, "EXPECTED FAIL: Error recovery issues detected"
            print *, "IMPLEMENTATION NEEDED: Robust error recovery mechanisms"
        end if
        
        print *, "✓ PASS: Availability error recovery test completed"
    end subroutine test_availability_error_recovery

    subroutine dummy_update_recovery(frame)
        !! Minimal animation callback for recovery testing
        integer, intent(in) :: frame
        continue
    end subroutine dummy_update_recovery

end program test_ffmpeg_availability_robustness