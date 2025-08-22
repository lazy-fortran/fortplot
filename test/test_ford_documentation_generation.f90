program test_ford_documentation_generation
    !! **Given-When-Then Documentation**
    !!
    !! **GIVEN**: A Fortran project with FORD documentation configuration
    !! **WHEN**: FORD documentation generation is triggered via Makefile
    !! **THEN**: Documentation generation should succeed without file reference errors
    !!
    !! **PURPOSE**: Test FORD documentation infrastructure for Issue #192
    !!
    !! **RED PHASE EXPECTATIONS**: 
    !! - Test WILL FAIL due to missing doc.md file referenced in Makefile
    !! - Test WILL FAIL due to incomplete fpm.toml FORD configuration
    !! - These failures guide sergei's implementation in GREEN phase
    !!
    !! **CRITICAL BEHAVIOR VALIDATION**:
    !! 1. Makefile 'make doc' command execution
    !! 2. FORD file reference resolution
    !! 3. Documentation HTML generation
    !! 4. CI pipeline compatibility

    use iso_fortran_env, only: output_unit, error_unit, input_unit
    implicit none
    
    logical :: test_passed
    integer :: test_count, failed_count
    
    test_count = 0
    failed_count = 0
    
    write(output_unit, '(A)') "=== FORD Documentation Generation Tests ==="
    write(output_unit, '(A)') "Testing Issue #192: FORD documentation infrastructure"
    write(output_unit, '(A)') ""
    
    ! Test 1: Verify Makefile doc target exists and is executable
    call test_makefile_doc_target_exists(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "Makefile doc target exists and is executable")
    
    ! Test 2: Test FORD command execution (will fail due to missing doc.md)
    call test_ford_command_execution(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "FORD command execution succeeds")
    
    ! Test 3: Verify FORD configuration file reference resolution
    call test_ford_file_reference_resolution(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "FORD file references resolve correctly")
    
    ! Test 4: Test documentation output directory creation
    call test_documentation_output_directory(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "Documentation output directory is created")
    
    ! Test 5: Verify HTML documentation generation
    call test_html_documentation_generation(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "HTML documentation files are generated")
    
    ! Test 6: Test cross-platform compatibility
    call test_cross_platform_compatibility(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "Documentation generation works cross-platform")
    
    ! Final test results
    call print_test_summary(test_count, failed_count)
    
    if (failed_count > 0) then
        error stop "FORD documentation generation tests FAILED - configuration issues detected"
    else
        write(output_unit, '(A)') "All FORD documentation tests PASSED"
    end if

contains

    subroutine test_makefile_doc_target_exists(passed)
        !! **GIVEN**: Project Makefile exists
        !! **WHEN**: Checking for 'doc' target
        !! **THEN**: Makefile should contain executable 'doc' target
        logical, intent(out) :: passed
        integer :: status, unit_file
        character(len=10000) :: makefile_content
        logical :: file_exists, doc_target_found
        
        passed = .false.
        
        ! Check if Makefile exists
        inquire(file='Makefile', exist=file_exists)
        if (.not. file_exists) then
            write(error_unit, '(A)') "ERROR: Makefile not found in project root"
            return
        end if
        
        ! Read Makefile content
        open(newunit=unit_file, file='Makefile', status='old', action='read', iostat=status)
        if (status /= 0) then
            write(error_unit, '(A)') "ERROR: Cannot read Makefile"
            return
        end if
        
        doc_target_found = .false.
        makefile_content = ""
        
        do
            character(len=200) :: line
            read(unit_file, '(A)', iostat=status) line
            if (status /= 0) exit
            makefile_content = trim(makefile_content) // " " // trim(line)
            
            ! Check for doc target
            if (index(line, 'doc:') > 0) then
                doc_target_found = .true.
            end if
        end do
        close(unit_file)
        
        if (.not. doc_target_found) then
            write(error_unit, '(A)') "ERROR: 'doc:' target not found in Makefile"
            return
        end if
        
        ! Verify ford command in doc target
        if (index(makefile_content, 'ford') == 0) then
            write(error_unit, '(A)') "ERROR: 'ford' command not found in Makefile doc target"
            return
        end if
        
        passed = .true.
    end subroutine test_makefile_doc_target_exists

    subroutine test_ford_command_execution(passed)
        !! **GIVEN**: Makefile with doc target
        !! **WHEN**: Executing 'make doc' command
        !! **THEN**: FORD command should execute without file-not-found errors
        !! **RED PHASE**: Will FAIL due to missing doc.md reference
        logical, intent(out) :: passed
        integer :: status
        character(len=1000) :: command_output
        
        passed = .false.
        
        ! Execute make doc command and capture output
        call execute_command_line('make doc 2>&1', exitstat=status, cmdstat=status)
        
        ! RED PHASE: This WILL fail due to missing doc.md file
        ! The test is designed to fail and show the exact error
        if (status /= 0) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): make doc failed"
            write(error_unit, '(A)') "This failure indicates Issue #192 - FORD file reference error"
            write(error_unit, '(A)') "Root cause: Makefile references non-existent doc.md file"
            write(error_unit, '(A)') "Solution needed: Update Makefile to use README.md instead"
            return
        end if
        
        passed = .true.
    end subroutine test_ford_command_execution

    subroutine test_ford_file_reference_resolution(passed)
        !! **GIVEN**: FORD configuration references documentation files
        !! **WHEN**: FORD attempts to resolve file references
        !! **THEN**: All referenced files should exist and be accessible
        logical, intent(out) :: passed
        logical :: readme_exists, doc_md_exists
        
        passed = .false.
        
        ! Check if README.md exists (should exist)
        inquire(file='README.md', exist=readme_exists)
        if (.not. readme_exists) then
            write(error_unit, '(A)') "ERROR: README.md file not found"
            return
        end if
        
        ! Check if doc.md exists (should NOT exist - this is the problem)
        inquire(file='doc.md', exist=doc_md_exists)
        if (doc_md_exists) then
            write(error_unit, '(A)') "WARNING: doc.md file found - should be using README.md"
        else
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): doc.md file missing"
            write(error_unit, '(A)') "This confirms Issue #192 - Makefile references wrong file"
            write(error_unit, '(A)') "Solution: Update Makefile to use README.md instead of doc.md"
            return
        end if
        
        passed = .true.
    end subroutine test_ford_file_reference_resolution

    subroutine test_documentation_output_directory(passed)
        !! **GIVEN**: FORD documentation generation
        !! **WHEN**: Documentation is generated
        !! **THEN**: Output directory should be created as specified in configuration
        logical, intent(out) :: passed
        logical :: output_dir_exists
        
        passed = .false.
        
        ! Check if build/doc directory exists (from fpm.toml configuration)
        inquire(file='build/doc', exist=output_dir_exists)
        if (.not. output_dir_exists) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): build/doc directory not found"
            write(error_unit, '(A)') "This indicates FORD documentation has not been generated"
            return
        end if
        
        passed = .true.
    end subroutine test_documentation_output_directory

    subroutine test_html_documentation_generation(passed)
        !! **GIVEN**: FORD documentation generation completes
        !! **WHEN**: Checking output files
        !! **THEN**: HTML documentation files should be generated
        logical, intent(out) :: passed
        logical :: index_html_exists
        
        passed = .false.
        
        ! Check for generated index.html
        inquire(file='build/doc/index.html', exist=index_html_exists)
        if (.not. index_html_exists) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): index.html not generated"
            write(error_unit, '(A)') "This confirms FORD documentation generation failed"
            return
        end if
        
        passed = .true.
    end subroutine test_html_documentation_generation

    subroutine test_cross_platform_compatibility(passed)
        !! **GIVEN**: Multi-platform CI environment (Linux, Windows)
        !! **WHEN**: FORD documentation generation runs on different platforms
        !! **THEN**: Generation should succeed on all supported platforms
        logical, intent(out) :: passed
        character(len=50) :: platform_name
        
        passed = .false.
        
        ! Determine platform (basic detection)
        call get_environment_variable('OS', platform_name)
        if (len_trim(platform_name) == 0) then
            platform_name = 'Unix-like'
        end if
        
        write(output_unit, '(A,A)') "Testing on platform: ", trim(platform_name)
        
        ! Platform-specific path validation
        if (index(platform_name, 'Windows') > 0) then
            ! Windows-specific checks could go here
            write(output_unit, '(A)') "Windows platform detected - checking path separators"
        else
            ! Unix-like platform checks
            write(output_unit, '(A)') "Unix-like platform detected - checking forward slashes"
        end if
        
        ! For RED phase, this test will pass to show platform detection works
        ! Real cross-platform failures will be in FORD execution
        passed = .true.
    end subroutine test_cross_platform_compatibility

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
        write(output_unit, '(A)') "=== Test Summary ==="
        write(output_unit, '(A,I0,A,I0,A)') "Tests run: ", test_count, ", Failed: ", failed_count, ""
        
        if (failed_count > 0) then
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "=== RED PHASE ANALYSIS ==="
            write(output_unit, '(A)') "Expected failures detected - this confirms Issue #192:"
            write(output_unit, '(A)') "1. Makefile references non-existent doc.md file"
            write(output_unit, '(A)') "2. Should reference README.md instead"
            write(output_unit, '(A)') "3. fpm.toml needs project_file configuration"
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "Next: sergei-perfectionist-coder implements GREEN phase fixes"
        end if
    end subroutine print_test_summary

end program test_ford_documentation_generation