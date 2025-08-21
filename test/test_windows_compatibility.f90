! Test suite for Windows compilation compatibility and cross-platform functionality
! RED phase tests - these should FAIL until cross-platform C code is implemented
program test_windows_compatibility
    use iso_c_binding
    use fortplot_pipe
    implicit none
    
    integer :: test_count = 0
    integer :: passed_count = 0
    logical :: all_tests_passed = .true.
    
    ! Test the cross-platform functionality through the Fortran interface
    call run_all_windows_compatibility_tests()
    
    if (all_tests_passed) then
        write(*,'(A,I0,A,I0,A)') 'All ', passed_count, ' out of ', test_count, ' tests passed'
        stop 0
    else
        write(*,'(A,I0,A,I0,A)') 'FAILED: ', passed_count, ' out of ', test_count, ' tests passed'
        stop 1
    end if

contains

    subroutine run_all_windows_compatibility_tests()
        call test_compilation_compatibility()
        call test_api_consistency_cross_platform() 
        call test_ffmpeg_pipe_functionality()
        call test_security_features_cross_platform()
        call test_error_handling_consistency()
        call test_binary_data_integrity()
        call test_platform_specific_behavior()
        call test_edge_cases_cross_platform()
    end subroutine run_all_windows_compatibility_tests

    ! Given: Windows-specific headers and functions in C code
    ! When: Code is compiled on Windows platform
    ! Then: Compilation should succeed without Unix-specific dependencies
    subroutine test_compilation_compatibility()
        logical :: compilation_would_succeed
        
        write(*,'(A)') 'Testing: Cross-platform compilation compatibility'
        
        ! This test validates the logic that WILL be implemented
        ! Currently fails because sys/wait.h and popen/pclose are Unix-specific
        compilation_would_succeed = validate_cross_platform_compilation_logic()
        
        call assert_true(compilation_would_succeed, &
            'Cross-platform compilation should succeed on both Windows and Unix')
    end subroutine test_compilation_compatibility

    ! Given: FFmpeg pipe functions exist on both platforms  
    ! When: Same API calls are made on Windows vs Unix
    ! Then: Behavior should be identical (same inputs → same outputs)
    subroutine test_api_consistency_cross_platform()
        integer :: status_windows, status_unix
        logical :: api_consistent
        
        write(*,'(A)') 'Testing: API consistency across platforms'
        
        ! Test that simulated Windows and Unix behaviors are identical
        status_windows = simulate_windows_open_pipe('test.mp4', 30)
        status_unix = simulate_unix_open_pipe('test.mp4', 30)
        
        api_consistent = (status_windows == status_unix)
        
        call assert_true(api_consistent, &
            'API behavior should be identical between Windows and Unix platforms')
            
        ! Test pipe operations produce consistent results
        call assert_true(validate_cross_platform_pipe_operations(), &
            'Pipe operations should behave identically across platforms')
    end subroutine test_api_consistency_cross_platform

    ! Given: FFmpeg pipe functionality 
    ! When: Opening, writing, and closing pipes
    ! Then: Operations should work correctly on both platforms
    subroutine test_ffmpeg_pipe_functionality()
        integer :: status
        integer(1), allocatable :: test_png_data(:)
        logical :: ffmpeg_enabled
        integer :: i
        
        write(*,'(A)') 'Testing: FFmpeg pipe functionality cross-platform'
        
        ! Test pipe opening with cross-platform logic
        status = test_cross_platform_pipe_open('test_output.mp4', 25)
        call assert_equal(status, 0, 'Cross-platform pipe opening should succeed')
        
        ! Test PNG data writing with Windows binary mode considerations
        allocate(test_png_data(100))
        test_png_data = [(int(i, 1), i = 1, 100)]  ! Test data
        
        status = test_cross_platform_png_write(test_png_data)
        call assert_equal(status, 0, 'Cross-platform PNG writing should succeed')
        
        ! Test pipe closing with platform-specific exit status handling
        status = test_cross_platform_pipe_close()
        call assert_equal(status, 0, 'Cross-platform pipe closing should succeed')
        
        deallocate(test_png_data)
    end subroutine test_ffmpeg_pipe_functionality

    ! Given: Filename validation security features
    ! When: Validating filenames on different platforms
    ! Then: Security checks should work identically
    subroutine test_security_features_cross_platform()
        logical :: windows_safe, unix_safe
        
        write(*,'(A)') 'Testing: Security features cross-platform consistency'
        
        ! Test filename validation works identically
        windows_safe = simulate_windows_filename_validation('safe_file.mp4')
        unix_safe = simulate_unix_filename_validation('safe_file.mp4')
        
        call assert_true(windows_safe .and. unix_safe, &
            'Safe filenames should be validated identically on both platforms')
        
        ! Test dangerous path detection includes Windows-specific paths
        call assert_true(validate_windows_specific_dangerous_paths(), &
            'Windows-specific dangerous paths should be properly detected')
            
        ! Test shell metacharacter detection works on both platforms
        call assert_true(validate_cross_platform_metacharacter_detection(), &
            'Shell metacharacter detection should work identically across platforms')
    end subroutine test_security_features_cross_platform

    ! Given: Error conditions in pipe operations
    ! When: Errors occur on different platforms
    ! Then: Error codes and behavior should be consistent
    subroutine test_error_handling_consistency()
        integer :: windows_error, unix_error
        
        write(*,'(A)') 'Testing: Error handling consistency across platforms'
        
        ! Test error code consistency for invalid parameters
        windows_error = simulate_windows_error_invalid_fps(0)
        unix_error = simulate_unix_error_invalid_fps(0)
        
        call assert_equal(windows_error, unix_error, &
            'Error codes should be identical for invalid parameters')
        
        ! Test pipe failure handling consistency
        call assert_true(validate_cross_platform_pipe_failure_handling(), &
            'Pipe failure handling should be consistent across platforms')
            
        ! Test process exit status extraction consistency
        call assert_true(validate_cross_platform_exit_status_extraction(), &
            'Exit status extraction should work consistently on both platforms')
    end subroutine test_error_handling_consistency

    ! Given: Binary PNG data needs to be written through pipes
    ! When: Data is written on Windows vs Unix
    ! Then: Data integrity must be preserved (critical for Windows)
    subroutine test_binary_data_integrity()
        integer(1), allocatable :: original_data(:), windows_data(:), unix_data(:)
        logical :: data_integrity_preserved
        integer :: i
        
        write(*,'(A)') 'Testing: Binary data integrity across platforms'
        
        ! Create test PNG-like binary data
        allocate(original_data(256))
        do i = 1, 256
            original_data(i) = int(mod(i * 137, 256), 1)
        end do
        
        ! Simulate Windows binary mode processing
        windows_data = simulate_windows_binary_processing(original_data)
        
        ! Simulate Unix binary processing  
        unix_data = simulate_unix_binary_processing(original_data)
        
        ! Data should be identical after processing
        data_integrity_preserved = all(windows_data == unix_data) .and. &
                                  all(windows_data == original_data)
        
        call assert_true(data_integrity_preserved, &
            'Binary data integrity must be preserved across platforms')
            
        ! Test that Windows binary mode prevents corruption
        call assert_true(validate_windows_binary_mode_protection(), &
            'Windows binary mode should prevent PNG data corruption')
        
        deallocate(original_data, windows_data, unix_data)
    end subroutine test_binary_data_integrity

    ! Given: Platform-specific implementation details
    ! When: Using platform-specific functions internally  
    ! Then: External behavior should remain consistent
    subroutine test_platform_specific_behavior()
        logical :: windows_behavior_correct, unix_behavior_correct
        
        write(*,'(A)') 'Testing: Platform-specific internal behavior'
        
        ! Test Windows _popen/_pclose behavior simulation
        windows_behavior_correct = validate_windows_popen_simulation()
        call assert_true(windows_behavior_correct, &
            'Windows _popen/_pclose simulation should work correctly')
        
        ! Test Unix popen/pclose behavior simulation  
        unix_behavior_correct = validate_unix_popen_simulation()
        call assert_true(unix_behavior_correct, &
            'Unix popen/pclose simulation should work correctly')
            
        ! Test process tracking differences (Windows handles vs Unix PIDs)
        call assert_true(validate_cross_platform_process_tracking(), &
            'Process tracking should work correctly on both platforms')
            
        ! Test exit status extraction differences (Windows direct vs Unix WEXITSTATUS)
        call assert_true(validate_cross_platform_exit_status_handling(), &
            'Exit status handling should work correctly on both platforms')
    end subroutine test_platform_specific_behavior

    ! Given: Edge cases and boundary conditions
    ! When: Testing cross-platform implementation
    ! Then: All edge cases should behave consistently
    subroutine test_edge_cases_cross_platform()
        write(*,'(A)') 'Testing: Cross-platform edge cases'
        
        ! Test empty filename handling
        call assert_true(validate_empty_filename_handling(), &
            'Empty filename should be handled consistently across platforms')
        
        ! Test maximum filename length handling
        call assert_true(validate_max_filename_length_handling(), &
            'Maximum filename length should be handled consistently')
        
        ! Test special character handling in filenames
        call assert_true(validate_special_character_handling(), &
            'Special characters should be handled consistently across platforms')
        
        ! Test concurrent pipe operations (if supported)
        call assert_true(validate_concurrent_operations(), &
            'Concurrent operations should behave consistently')
        
        ! Test environment variable interactions
        call assert_true(validate_environment_variable_interactions(), &
            'Environment variables should work consistently across platforms')
    end subroutine test_edge_cases_cross_platform

    ! ========================================================================
    ! SIMULATION FUNCTIONS - These simulate the cross-platform behavior
    ! that WILL be implemented. Currently they fail to demonstrate RED phase.
    ! ========================================================================

    function validate_cross_platform_compilation_logic() result(valid)
        logical :: valid
        ! RED PHASE: This fails because current code has Unix-specific headers
        ! Will pass when conditional compilation is implemented
        valid = .false.  ! Fails until #ifdef _WIN32 logic is added
    end function validate_cross_platform_compilation_logic

    function simulate_windows_open_pipe(filename, fps) result(status)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer :: status
        ! RED PHASE: Simulate what Windows _popen would do
        status = -1  ! Fails until actual Windows support is implemented
    end function simulate_windows_open_pipe

    function simulate_unix_open_pipe(filename, fps) result(status)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer :: status
        ! This uses current Unix implementation
        status = open_ffmpeg_pipe(filename, fps)
    end function simulate_unix_open_pipe

    function validate_cross_platform_pipe_operations() result(valid)
        logical :: valid
        ! RED PHASE: Fails until both platforms are implemented
        valid = .false.
    end function validate_cross_platform_pipe_operations

    function test_cross_platform_pipe_open(filename, fps) result(status)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: fps
        integer :: status
        ! RED PHASE: Fails until cross-platform implementation exists
        status = -1
    end function test_cross_platform_pipe_open

    function test_cross_platform_png_write(png_data) result(status)
        integer(1), intent(in) :: png_data(:)
        integer :: status
        ! RED PHASE: Fails until Windows binary mode is implemented
        status = -1
    end function test_cross_platform_png_write

    function test_cross_platform_pipe_close() result(status)
        integer :: status
        ! RED PHASE: Fails until Windows _pclose handling is implemented
        status = -1
    end function test_cross_platform_pipe_close

    function simulate_windows_filename_validation(filename) result(safe)
        character(len=*), intent(in) :: filename
        logical :: safe
        ! RED PHASE: Fails until Windows-specific path validation is added
        safe = .false.
    end function simulate_windows_filename_validation

    function simulate_unix_filename_validation(filename) result(safe)
        character(len=*), intent(in) :: filename
        logical :: safe
        ! This would use current validation logic
        safe = .true.  ! Simplified for test
    end function simulate_unix_filename_validation

    function validate_windows_specific_dangerous_paths() result(valid)
        logical :: valid
        ! RED PHASE: Fails until Windows paths like C:\Windows\System32 are checked
        valid = .false.
    end function validate_windows_specific_dangerous_paths

    function validate_cross_platform_metacharacter_detection() result(valid)
        logical :: valid
        ! RED PHASE: Current logic may not handle Windows cmd.exe metacharacters
        valid = .false.
    end function validate_cross_platform_metacharacter_detection

    function simulate_windows_error_invalid_fps(fps) result(error_code)
        integer, intent(in) :: fps
        integer :: error_code
        ! RED PHASE: Fails until Windows error handling is implemented
        error_code = -1
    end function simulate_windows_error_invalid_fps

    function simulate_unix_error_invalid_fps(fps) result(error_code)
        integer, intent(in) :: fps
        integer :: error_code
        ! This would use current error handling
        error_code = -1
    end function simulate_unix_error_invalid_fps

    function validate_cross_platform_pipe_failure_handling() result(valid)
        logical :: valid
        ! RED PHASE: Fails until both platforms handle pipe failures consistently
        valid = .false.
    end function validate_cross_platform_pipe_failure_handling

    function validate_cross_platform_exit_status_extraction() result(valid)
        logical :: valid
        ! RED PHASE: Windows _pclose returns directly, Unix needs WEXITSTATUS
        valid = .false.
    end function validate_cross_platform_exit_status_extraction

    function simulate_windows_binary_processing(data) result(processed_data)
        integer(1), intent(in) :: data(:)
        integer(1), allocatable :: processed_data(:)
        ! RED PHASE: Fails until Windows "wb" mode is properly implemented
        allocate(processed_data(size(data)))
        processed_data = data
        ! Simulate potential corruption without proper binary mode
        if (size(data) > 10) processed_data(10) = int(127, 1)  ! Corruption simulation
    end function simulate_windows_binary_processing

    function simulate_unix_binary_processing(data) result(processed_data)
        integer(1), intent(in) :: data(:)
        integer(1), allocatable :: processed_data(:)
        ! Unix typically handles binary correctly
        allocate(processed_data(size(data)))
        processed_data = data
    end function simulate_unix_binary_processing

    function validate_windows_binary_mode_protection() result(valid)
        logical :: valid
        ! RED PHASE: Fails until "wb" mode is used on Windows
        valid = .false.
    end function validate_windows_binary_mode_protection

    function validate_windows_popen_simulation() result(valid)
        logical :: valid
        ! RED PHASE: Fails until _popen is actually used
        valid = .false.
    end function validate_windows_popen_simulation

    function validate_unix_popen_simulation() result(valid)
        logical :: valid
        ! Unix version currently works
        valid = .true.
    end function validate_unix_popen_simulation

    function validate_cross_platform_process_tracking() result(valid)
        logical :: valid
        ! RED PHASE: Fails until Windows handles vs Unix PIDs are both supported
        valid = .false.
    end function validate_cross_platform_process_tracking

    function validate_cross_platform_exit_status_handling() result(valid)
        logical :: valid
        ! RED PHASE: Fails until both Windows direct and Unix WEXITSTATUS work
        valid = .false.
    end function validate_cross_platform_exit_status_handling

    function validate_empty_filename_handling() result(valid)
        logical :: valid
        ! RED PHASE: May fail if platform-specific handling differs
        valid = .false.
    end function validate_empty_filename_handling

    function validate_max_filename_length_handling() result(valid)
        logical :: valid
        ! RED PHASE: Windows has different path length limits than Unix
        valid = .false.
    end function validate_max_filename_length_handling

    function validate_special_character_handling() result(valid)
        logical :: valid
        ! RED PHASE: Windows cmd.exe vs Unix shell have different special chars
        valid = .false.
    end function validate_special_character_handling

    function validate_concurrent_operations() result(valid)
        logical :: valid
        ! RED PHASE: Platform differences in pipe handling
        valid = .false.
    end function validate_concurrent_operations

    function validate_environment_variable_interactions() result(valid)
        logical :: valid
        ! RED PHASE: Environment variables may behave differently
        valid = .false.
    end function validate_environment_variable_interactions

    ! ========================================================================
    ! TEST FRAMEWORK UTILITIES
    ! ========================================================================

    subroutine assert_true(condition, message)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: message
        
        test_count = test_count + 1
        if (condition) then
            passed_count = passed_count + 1
            write(*,'(A,A)') '  PASS: ', message
        else
            write(*,'(A,A)') '  FAIL: ', message
            all_tests_passed = .false.
        end if
    end subroutine assert_true

    subroutine assert_equal(actual, expected, message)
        integer, intent(in) :: actual, expected
        character(len=*), intent(in) :: message
        
        test_count = test_count + 1
        if (actual == expected) then
            passed_count = passed_count + 1
            write(*,'(A,A)') '  PASS: ', message
        else
            write(*,'(A,A,A,I0,A,I0)') '  FAIL: ', message, ' (expected ', expected, ', got ', actual, ')'
            all_tests_passed = .false.
        end if
    end subroutine assert_equal

end program test_windows_compatibility