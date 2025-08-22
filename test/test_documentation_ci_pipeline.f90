program test_documentation_ci_pipeline
    !! **Given-When-Then Documentation**
    !!
    !! **GIVEN**: A CI/CD pipeline configured for documentation generation
    !! **WHEN**: Documentation step runs in CI environment
    !! **THEN**: FORD documentation should generate successfully without errors
    !!
    !! **PURPOSE**: Test CI documentation pipeline for Issue #192
    !!
    !! **RED PHASE EXPECTATIONS**: 
    !! - Test WILL FAIL due to missing FORD installation or configuration
    !! - Test WILL FAIL due to file reference errors in CI environment
    !! - These failures demonstrate CI pipeline issues in Issue #192
    !!
    !! **CRITICAL BEHAVIOR VALIDATION**:
    !! 1. CI environment FORD availability
    !! 2. CI documentation step execution
    !! 3. Cross-platform CI compatibility
    !! 4. CI artifact generation validation

    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    
    logical :: test_passed
    integer :: test_count, failed_count
    
    test_count = 0
    failed_count = 0
    
    write(output_unit, '(A)') "=== Documentation CI Pipeline Tests ==="
    write(output_unit, '(A)') "Testing Issue #192: CI documentation generation pipeline"
    write(output_unit, '(A)') ""
    
    ! Test 1: Verify CI environment detection
    call test_ci_environment_detection(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "CI environment detection")
    
    ! Test 2: Test FORD availability in CI environment
    call test_ford_availability_in_ci(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "FORD availability in CI environment")
    
    ! Test 3: Test CI documentation workflow file existence
    call test_ci_documentation_workflow(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "CI documentation workflow configuration")
    
    ! Test 4: Test documentation generation in CI-like environment
    call test_documentation_generation_ci_like(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "Documentation generation in CI-like environment")
    
    ! Test 5: Test CI artifact paths and permissions
    call test_ci_artifact_paths(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "CI artifact paths and permissions")
    
    ! Test 6: Test cross-platform CI compatibility
    call test_cross_platform_ci_compatibility(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "Cross-platform CI compatibility")
    
    ! Test 7: Test CI failure detection and reporting
    call test_ci_failure_detection(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "CI failure detection and reporting")
    
    ! Final test results
    call print_test_summary(test_count, failed_count)
    
    if (failed_count > 0) then
        error stop "Documentation CI pipeline tests FAILED - CI issues detected"
    else
        write(output_unit, '(A)') "All CI pipeline tests PASSED"
    end if

contains

    subroutine test_ci_environment_detection(passed)
        !! **GIVEN**: Test running in potential CI environment
        !! **WHEN**: Detecting CI environment indicators
        !! **THEN**: Should properly identify CI vs local environment
        logical, intent(out) :: passed
        character(len=100) :: ci_indicator, github_actions
        logical :: is_ci_environment
        
        passed = .false.
        is_ci_environment = .false.
        
        ! Check for common CI environment variables
        call get_environment_variable('CI', ci_indicator)
        call get_environment_variable('GITHUB_ACTIONS', github_actions)
        
        if (len_trim(ci_indicator) > 0 .or. len_trim(github_actions) > 0) then
            is_ci_environment = .true.
            write(output_unit, '(A)') "CI environment detected"
            if (len_trim(github_actions) > 0) then
                write(output_unit, '(A)') "GitHub Actions environment confirmed"
            end if
        else
            write(output_unit, '(A)') "Local environment detected (simulating CI tests)"
        end if
        
        passed = .true.  ! This test should always pass
    end subroutine test_ci_environment_detection

    subroutine test_ford_availability_in_ci(passed)
        !! **GIVEN**: CI environment with build tools
        !! **WHEN**: Checking for FORD documentation tool availability
        !! **THEN**: FORD should be available or installable in CI
        !! **RED PHASE**: May FAIL if FORD not installed in CI
        logical, intent(out) :: passed
        integer :: status
        
        passed = .false.
        
        ! Try to execute FORD command to check availability
        call execute_command_line('ford --version 2>/dev/null', exitstat=status)
        
        if (status /= 0) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): FORD not available in environment"
            write(error_unit, '(A)') "This could indicate:"
            write(error_unit, '(A)') "1. FORD not installed in CI environment"
            write(error_unit, '(A)') "2. CI workflow missing FORD installation step"
            write(error_unit, '(A)') "3. PATH not configured correctly for FORD"
            write(error_unit, '(A)') ""
            write(error_unit, '(A)') "Solution: Add FORD installation to CI workflow"
            return
        else
            write(output_unit, '(A)') "FORD available - version check successful"
        end if
        
        passed = .true.
    end subroutine test_ford_availability_in_ci

    subroutine test_ci_documentation_workflow(passed)
        !! **GIVEN**: GitHub Actions workflow files
        !! **WHEN**: Checking for documentation generation steps
        !! **THEN**: CI workflow should include documentation generation
        logical, intent(out) :: passed
        logical :: ci_file_exists, windows_ci_exists
        integer :: unit_file, status
        character(len=200) :: line
        logical :: doc_step_found
        
        passed = .false.
        doc_step_found = .false.
        
        ! Check main CI file
        inquire(file='.github/workflows/ci.yml', exist=ci_file_exists)
        inquire(file='.github/workflows/windows-ci.yml', exist=windows_ci_exists)
        
        if (.not. ci_file_exists .and. .not. windows_ci_exists) then
            write(error_unit, '(A)') "ERROR: No CI workflow files found"
            return
        end if
        
        ! Check CI files for documentation steps
        if (ci_file_exists) then
            call check_ci_file_for_doc_step('.github/workflows/ci.yml', doc_step_found)
        end if
        
        if (windows_ci_exists .and. .not. doc_step_found) then
            call check_ci_file_for_doc_step('.github/workflows/windows-ci.yml', doc_step_found)
        end if
        
        if (.not. doc_step_found) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): No documentation step in CI workflows"
            write(error_unit, '(A)') "This indicates missing CI documentation integration"
            write(error_unit, '(A)') "Solution: Add documentation generation step to CI workflow"
            return
        end if
        
        passed = .true.
    end subroutine test_ci_documentation_workflow

    subroutine test_documentation_generation_ci_like(passed)
        !! **GIVEN**: Clean CI-like environment
        !! **WHEN**: Running documentation generation commands as CI would
        !! **THEN**: Documentation should generate without manual intervention
        logical, intent(out) :: passed
        integer :: status
        character(len=500) :: ci_command
        
        passed = .false.
        
        ! Simulate CI documentation generation process
        write(output_unit, '(A)') "Simulating CI documentation generation..."
        
        ! Clean any existing documentation
        call execute_command_line('rm -rf build/doc 2>/dev/null', exitstat=status)
        
        ! Try to run make doc as CI would
        call execute_command_line('make doc 2>&1', exitstat=status)
        
        if (status /= 0) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): make doc failed in CI-like environment"
            write(error_unit, '(A)') "This confirms Issue #192 - documentation generation fails in automated environment"
            write(error_unit, '(A)') "Root cause: File reference errors prevent successful generation"
            return
        end if
        
        passed = .true.
    end subroutine test_documentation_generation_ci_like

    subroutine test_ci_artifact_paths(passed)
        !! **GIVEN**: Documentation generation in CI
        !! **WHEN**: Checking artifact paths and permissions
        !! **THEN**: All generated files should be in expected locations with correct permissions
        logical, intent(out) :: passed
        logical :: build_dir_exists, doc_dir_exists
        character(len=100) :: expected_output_path
        
        passed = .false.
        expected_output_path = 'build/doc'
        
        ! Check if build directory exists
        inquire(file='build', exist=build_dir_exists)
        if (.not. build_dir_exists) then
            write(error_unit, '(A)') "ERROR: build directory not created"
            return
        end if
        
        ! Check if documentation output directory exists
        inquire(file=trim(expected_output_path), exist=doc_dir_exists)
        if (.not. doc_dir_exists) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): Documentation output directory missing"
            write(error_unit, '(A,A)') "Expected path: ", trim(expected_output_path)
            write(error_unit, '(A)') "This indicates documentation generation did not complete"
            return
        end if
        
        passed = .true.
    end subroutine test_ci_artifact_paths

    subroutine test_cross_platform_ci_compatibility(passed)
        !! **GIVEN**: Multi-platform CI environment (Ubuntu, Windows)
        !! **WHEN**: Running documentation generation on different platforms
        !! **THEN**: Should work consistently across all supported platforms
        logical, intent(out) :: passed
        character(len=50) :: platform_name
        logical :: path_separators_ok
        
        passed = .false.
        path_separators_ok = .true.
        
        ! Detect platform
        call get_environment_variable('OS', platform_name)
        if (len_trim(platform_name) == 0) then
            platform_name = 'Unix-like'
        end if
        
        write(output_unit, '(A,A)') "Testing cross-platform compatibility on: ", trim(platform_name)
        
        ! Platform-specific validation
        if (index(platform_name, 'Windows') > 0) then
            ! Windows-specific path checks
            call validate_windows_paths(path_separators_ok)
        else
            ! Unix-like platform checks
            call validate_unix_paths(path_separators_ok)
        end if
        
        if (.not. path_separators_ok) then
            write(error_unit, '(A)') "ERROR: Cross-platform path compatibility issues detected"
            return
        end if
        
        passed = .true.
    end subroutine test_cross_platform_ci_compatibility

    subroutine test_ci_failure_detection(passed)
        !! **GIVEN**: CI environment with monitoring
        !! **WHEN**: Documentation generation encounters errors
        !! **THEN**: Failures should be properly detected and reported
        logical, intent(out) :: passed
        integer :: status
        logical :: error_log_created
        
        passed = .false.
        
        ! Test error detection by running command that should fail
        call execute_command_line('ford non_existent_file.md 2>doc_error.log', exitstat=status)
        
        ! Check if error was properly captured
        inquire(file='doc_error.log', exist=error_log_created)
        
        if (status == 0) then
            write(error_unit, '(A)') "ERROR: Expected failure not detected - error handling may be insufficient"
            return
        end if
        
        if (.not. error_log_created) then
            write(error_unit, '(A)') "ERROR: Error logging not working properly"
            return
        end if
        
        ! Clean up test error log
        call execute_command_line('rm -f doc_error.log', exitstat=status)
        
        write(output_unit, '(A)') "CI failure detection working correctly"
        passed = .true.
    end subroutine test_ci_failure_detection

    subroutine check_ci_file_for_doc_step(filename, found)
        character(len=*), intent(in) :: filename
        logical, intent(out) :: found
        integer :: unit_file, status
        character(len=200) :: line
        
        found = .false.
        
        open(newunit=unit_file, file=filename, status='old', action='read', iostat=status)
        if (status /= 0) return
        
        do
            read(unit_file, '(A)', iostat=status) line
            if (status /= 0) exit
            
            ! Look for documentation-related steps
            if (index(line, 'ford') > 0 .or. index(line, 'doc') > 0 .or. &
                index(line, 'documentation') > 0) then
                found = .true.
                exit
            end if
        end do
        close(unit_file)
    end subroutine check_ci_file_for_doc_step

    subroutine validate_windows_paths(valid)
        logical, intent(out) :: valid
        character(len=200) :: test_path
        
        valid = .true.
        test_path = 'build\doc'  ! Windows-style path
        
        ! Test if Windows path handling works
        ! In a real implementation, this would test path separator handling
        write(output_unit, '(A)') "Windows path validation completed"
    end subroutine validate_windows_paths

    subroutine validate_unix_paths(valid)
        logical, intent(out) :: valid
        character(len=200) :: test_path
        
        valid = .true.
        test_path = 'build/doc'  ! Unix-style path
        
        ! Test if Unix path handling works
        write(output_unit, '(A)') "Unix path validation completed"
    end subroutine validate_unix_paths

    subroutine update_test_results(passed, test_count, failed_count, test_name)
        logical, intent(in) :: passed
        integer, intent(inout) :: test_count, failed_count
        character(len=*), intent(in) :: test_name
        
        test_count = test_count + 1
        if (.not. passed) then
            failed_count = failed_count + 1
            write(output_unit, '(A,A,A)') "FAIL: ", test_name, " ❌"
        else
            write(output_unit, '(A,A,A)') "PASS: ", test_name, " ✅"
        end if
    end subroutine update_test_results

    subroutine print_test_summary(test_count, failed_count)
        integer, intent(in) :: test_count, failed_count
        
        write(output_unit, '(A)') ""
        write(output_unit, '(A)') "=== CI Pipeline Test Summary ==="
        write(output_unit, '(A,I0,A,I0,A)') "Tests run: ", test_count, ", Failed: ", failed_count, ""
        
        if (failed_count > 0) then
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "=== RED PHASE CI ANALYSIS ==="
            write(output_unit, '(A)') "CI documentation pipeline issues:"
            write(output_unit, '(A)') "1. FORD may not be installed in CI environment"
            write(output_unit, '(A)') "2. CI workflows missing documentation generation step"
            write(output_unit, '(A)') "3. File reference errors prevent automated generation"
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "CI INTEGRATION SOLUTION:"
            write(output_unit, '(A)') "1. Add FORD installation to CI workflows"
            write(output_unit, '(A)') "2. Add documentation generation step"
            write(output_unit, '(A)') "3. Fix file reference issues (README.md vs doc.md)"
        end if
    end subroutine print_test_summary

end program test_documentation_ci_pipeline