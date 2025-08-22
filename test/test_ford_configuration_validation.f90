program test_ford_configuration_validation
    !! **Given-When-Then Documentation**
    !!
    !! **GIVEN**: A Fortran project using FPM with FORD documentation configuration
    !! **WHEN**: Validating FORD configuration settings in fpm.toml
    !! **THEN**: Configuration should be complete and reference correct files
    !!
    !! **PURPOSE**: Test FORD configuration completeness for Issue #192
    !!
    !! **RED PHASE EXPECTATIONS**: 
    !! - Test WILL FAIL due to missing project_file setting in fpm.toml
    !! - Test WILL FAIL due to incomplete FORD configuration
    !! - These failures guide the specific fixes needed in GREEN phase
    !!
    !! **CRITICAL BEHAVIOR VALIDATION**:
    !! 1. fmp.toml [extra.ford] section completeness
    !! 2. project_file = "README.md" setting presence
    !! 3. FORD configuration parameter validation
    !! 4. File reference correctness

    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    
    logical :: test_passed
    integer :: test_count, failed_count
    
    test_count = 0
    failed_count = 0
    
    write(output_unit, '(A)') "=== FORD Configuration Validation Tests ==="
    write(output_unit, '(A)') "Testing Issue #192: fpm.toml FORD configuration completeness"
    write(output_unit, '(A)') ""
    
    ! Test 1: Verify fpm.toml exists and is readable
    call test_fpm_toml_exists_and_readable(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "fpm.toml exists and is readable")
    
    ! Test 2: Verify [extra.ford] section exists
    call test_ford_section_exists(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "[extra.ford] section exists in fpm.toml")
    
    ! Test 3: Test project_file setting (will fail - this is the main issue)
    call test_project_file_setting(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "project_file setting configured correctly")
    
    ! Test 4: Verify FORD project name configuration
    call test_ford_project_name(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "FORD project name matches package name")
    
    ! Test 5: Test FORD directory configurations
    call test_ford_directory_settings(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "FORD directory settings are valid")
    
    ! Test 6: Validate referenced files exist
    call test_ford_referenced_files_exist(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "All FORD referenced files exist")
    
    ! Test 7: Test FORD configuration completeness
    call test_ford_configuration_completeness(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "FORD configuration is complete")
    
    ! Final test results
    call print_test_summary(test_count, failed_count)
    
    if (failed_count > 0) then
        error stop "FORD configuration validation FAILED - missing settings detected"
    else
        write(output_unit, '(A)') "All FORD configuration tests PASSED"
    end if

contains

    subroutine test_fmp_toml_exists_and_readable(passed)
        !! **GIVEN**: Fortran project with FPM
        !! **WHEN**: Checking for fpm.toml file
        !! **THEN**: File should exist and be readable
        logical, intent(out) :: passed
        logical :: file_exists
        integer :: status, unit_file
        
        passed = .false.
        
        inquire(file='fpm.toml', exist=file_exists)
        if (.not. file_exists) then
            write(error_unit, '(A)') "ERROR: fpm.toml file not found"
            return
        end if
        
        ! Test file readability
        open(newunit=unit_file, file='fpm.toml', status='old', action='read', iostat=status)
        if (status /= 0) then
            write(error_unit, '(A)') "ERROR: Cannot read fpm.toml file"
            return
        end if
        close(unit_file)
        
        passed = .true.
    end subroutine test_fpm_toml_exists_and_readable

    subroutine test_ford_section_exists(passed)
        !! **GIVEN**: fpm.toml file exists
        !! **WHEN**: Searching for [extra.ford] section
        !! **THEN**: Section should be present with configuration
        logical, intent(out) :: passed
        integer :: status, unit_file
        character(len=200) :: line
        logical :: ford_section_found
        
        passed = .false.
        ford_section_found = .false.
        
        open(newunit=unit_file, file='fpm.toml', status='old', action='read')
        
        do
            read(unit_file, '(A)', iostat=status) line
            if (status /= 0) exit
            
            ! Check for [extra.ford] section
            if (index(line, '[extra.ford]') > 0) then
                ford_section_found = .true.
                exit
            end if
        end do
        close(unit_file)
        
        if (.not. ford_section_found) then
            write(error_unit, '(A)') "ERROR: [extra.ford] section not found in fmp.toml"
            return
        end if
        
        passed = .true.
    end subroutine test_ford_section_exists

    subroutine test_project_file_setting(passed)
        !! **GIVEN**: [extra.ford] section exists in fpm.toml
        !! **WHEN**: Checking for project_file = "README.md" setting
        !! **THEN**: Setting should exist and point to README.md
        !! **RED PHASE**: Will FAIL - this is the main missing configuration
        logical, intent(out) :: passed
        integer :: status, unit_file
        character(len=200) :: line
        logical :: project_file_found, in_ford_section
        
        passed = .false.
        project_file_found = .false.
        in_ford_section = .false.
        
        open(newunit=unit_file, file='fpm.toml', status='old', action='read')
        
        do
            read(unit_file, '(A)', iostat=status) line
            if (status /= 0) exit
            
            ! Check if we're in the [extra.ford] section
            if (index(line, '[extra.ford]') > 0) then
                in_ford_section = .true.
                cycle
            end if
            
            ! Check if we've left the ford section
            if (in_ford_section .and. index(line, '[') > 0 .and. index(line, '[extra.ford]') == 0) then
                in_ford_section = .false.
            end if
            
            ! Look for project_file setting within ford section
            if (in_ford_section .and. index(line, 'project_file') > 0) then
                if (index(line, 'README.md') > 0) then
                    project_file_found = .true.
                    exit
                else
                    write(error_unit, '(A)') "ERROR: project_file found but not set to README.md"
                    write(error_unit, '(A,A)') "Current setting: ", trim(line)
                    close(unit_file)
                    return
                end if
            end if
        end do
        close(unit_file)
        
        if (.not. project_file_found) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): project_file setting missing"
            write(error_unit, '(A)') "This is the ROOT CAUSE of Issue #192"
            write(error_unit, '(A)') "Solution needed: Add 'project_file = ""README.md""' to [extra.ford] section"
            return
        end if
        
        passed = .true.
    end subroutine test_project_file_setting

    subroutine test_ford_project_name(passed)
        !! **GIVEN**: fmp.toml with name and [extra.ford] section
        !! **WHEN**: Comparing project names
        !! **THEN**: FORD project name should match package name
        logical, intent(out) :: passed
        integer :: status, unit_file
        character(len=200) :: line
        character(len=50) :: package_name, ford_project_name
        logical :: package_name_found, ford_project_found
        
        passed = .false.
        package_name_found = .false.
        ford_project_found = .false.
        package_name = ""
        ford_project_name = ""
        
        open(newunit=unit_file, file='fpm.toml', status='old', action='read')
        
        do
            read(unit_file, '(A)', iostat=status) line
            if (status /= 0) exit
            
            ! Look for package name
            if (index(line, 'name =') > 0 .and. .not. package_name_found) then
                call extract_quoted_value(line, package_name)
                package_name_found = .true.
            end if
            
            ! Look for FORD project name
            if (index(line, 'project =') > 0) then
                call extract_quoted_value(line, ford_project_name)
                ford_project_found = .true.
            end if
        end do
        close(unit_file)
        
        if (.not. package_name_found) then
            write(error_unit, '(A)') "ERROR: Package name not found in fpm.toml"
            return
        end if
        
        if (.not. ford_project_found) then
            write(error_unit, '(A)') "ERROR: FORD project name not found"
            return
        end if
        
        if (trim(package_name) /= trim(ford_project_name)) then
            write(error_unit, '(A,A,A,A)') "ERROR: Project name mismatch. Package: '", &
                trim(package_name), "', FORD: '", trim(ford_project_name), "'"
            return
        end if
        
        passed = .true.
    end subroutine test_ford_project_name

    subroutine test_ford_directory_settings(passed)
        !! **GIVEN**: FORD configuration with directory settings
        !! **WHEN**: Validating directory paths
        !! **THEN**: All specified directories should be valid paths
        logical, intent(out) :: passed
        integer :: status, unit_file
        character(len=200) :: line
        character(len=100) :: src_dir, output_dir, page_dir
        logical :: src_dir_found, output_dir_found, page_dir_found
        logical :: in_ford_section
        
        passed = .false.
        src_dir_found = .false.
        output_dir_found = .false.
        page_dir_found = .false.
        in_ford_section = .false.
        
        open(newunit=unit_file, file='fpm.toml', status='old', action='read')
        
        do
            read(unit_file, '(A)', iostat=status) line
            if (status /= 0) exit
            
            if (index(line, '[extra.ford]') > 0) then
                in_ford_section = .true.
                cycle
            end if
            
            if (in_ford_section .and. index(line, '[') > 0 .and. index(line, '[extra.ford]') == 0) then
                in_ford_section = .false.
            end if
            
            if (in_ford_section) then
                if (index(line, 'src_dir =') > 0) then
                    call extract_quoted_value(line, src_dir)
                    src_dir_found = .true.
                end if
                
                if (index(line, 'output_dir =') > 0) then
                    call extract_quoted_value(line, output_dir)
                    output_dir_found = .true.
                end if
                
                if (index(line, 'page_dir =') > 0) then
                    call extract_quoted_value(line, page_dir)
                    page_dir_found = .true.
                end if
            end if
        end do
        close(unit_file)
        
        ! Validate found directories exist
        if (src_dir_found) then
            logical :: dir_exists
            inquire(file=trim(src_dir), exist=dir_exists)
            if (.not. dir_exists) then
                write(error_unit, '(A,A)') "ERROR: src_dir does not exist: ", trim(src_dir)
                return
            end if
        end if
        
        if (page_dir_found) then
            logical :: dir_exists
            inquire(file=trim(page_dir), exist=dir_exists)
            if (.not. dir_exists) then
                write(error_unit, '(A,A)') "ERROR: page_dir does not exist: ", trim(page_dir)
                return
            end if
        end if
        
        passed = .true.
    end subroutine test_ford_directory_settings

    subroutine test_ford_referenced_files_exist(passed)
        !! **GIVEN**: FORD configuration references files and directories
        !! **WHEN**: Validating all referenced paths exist
        !! **THEN**: All references should resolve to existing files/directories
        logical, intent(out) :: passed
        logical :: readme_exists, src_exists, doc_dir_exists
        
        passed = .false.
        
        ! Check README.md (should be the project file)
        inquire(file='README.md', exist=readme_exists)
        if (.not. readme_exists) then
            write(error_unit, '(A)') "ERROR: README.md does not exist (needed for project_file)"
            return
        end if
        
        ! Check src directory
        inquire(file='src', exist=src_exists)
        if (.not. src_exists) then
            write(error_unit, '(A)') "ERROR: src directory does not exist"
            return
        end if
        
        ! Check doc directory
        inquire(file='doc', exist=doc_dir_exists)
        if (.not. doc_dir_exists) then
            write(error_unit, '(A)') "ERROR: doc directory does not exist (needed for page_dir)"
            return
        end if
        
        passed = .true.
    end subroutine test_ford_referenced_files_exist

    subroutine test_ford_configuration_completeness(passed)
        !! **GIVEN**: FORD configuration section in fpm.toml
        !! **WHEN**: Checking for all essential configuration parameters
        !! **THEN**: All required parameters should be present
        logical, intent(out) :: passed
        integer :: config_count
        logical :: has_project, has_src_dir, has_output_dir, has_page_dir, has_project_file
        
        passed = .false.
        config_count = 0
        
        call check_ford_config_parameter('project', has_project)
        call check_ford_config_parameter('src_dir', has_src_dir)
        call check_ford_config_parameter('output_dir', has_output_dir)
        call check_ford_config_parameter('page_dir', has_page_dir)
        call check_ford_config_parameter('project_file', has_project_file)
        
        if (has_project) config_count = config_count + 1
        if (has_src_dir) config_count = config_count + 1
        if (has_output_dir) config_count = config_count + 1
        if (has_page_dir) config_count = config_count + 1
        if (has_project_file) config_count = config_count + 1
        
        write(output_unit, '(A,I0,A)') "FORD configuration completeness: ", config_count, "/5 parameters found"
        
        if (.not. has_project_file) then
            write(error_unit, '(A)') "CRITICAL: project_file parameter missing (main Issue #192 cause)"
        end if
        
        ! Minimum required: project, src_dir, output_dir, project_file
        if (config_count < 4 .or. .not. has_project_file) then
            write(error_unit, '(A)') "ERROR: Incomplete FORD configuration"
            write(error_unit, '(A)') "Required: project, src_dir, output_dir, project_file"
            return
        end if
        
        passed = .true.
    end subroutine test_ford_configuration_completeness

    subroutine check_ford_config_parameter(param_name, found)
        character(len=*), intent(in) :: param_name
        logical, intent(out) :: found
        integer :: status, unit_file
        character(len=200) :: line
        logical :: in_ford_section
        
        found = .false.
        in_ford_section = .false.
        
        open(newunit=unit_file, file='fpm.toml', status='old', action='read')
        
        do
            read(unit_file, '(A)', iostat=status) line
            if (status /= 0) exit
            
            if (index(line, '[extra.ford]') > 0) then
                in_ford_section = .true.
                cycle
            end if
            
            if (in_ford_section .and. index(line, '[') > 0 .and. index(line, '[extra.ford]') == 0) then
                in_ford_section = .false.
            end if
            
            if (in_ford_section .and. index(line, trim(param_name) // ' =') > 0) then
                found = .true.
                exit
            end if
        end do
        close(unit_file)
    end subroutine check_ford_config_parameter

    subroutine extract_quoted_value(line, value)
        character(len=*), intent(in) :: line
        character(len=*), intent(out) :: value
        integer :: start_quote, end_quote
        
        start_quote = index(line, '"')
        if (start_quote > 0) then
            end_quote = index(line(start_quote+1:), '"') + start_quote
            value = line(start_quote+1:end_quote-1)
        else
            value = ""
        end if
    end subroutine extract_quoted_value

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
        write(output_unit, '(A)') "=== FORD Configuration Test Summary ==="
        write(output_unit, '(A,I0,A,I0,A)') "Tests run: ", test_count, ", Failed: ", failed_count, ""
        
        if (failed_count > 0) then
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "=== RED PHASE DIAGNOSIS ==="
            write(output_unit, '(A)') "FORD configuration issues detected:"
            write(output_unit, '(A)') "1. Missing 'project_file = ""README.md""' in [extra.ford]"
            write(output_unit, '(A)') "2. This causes FORD to look for non-existent doc.md"
            write(output_unit, '(A)') "3. CI documentation pipeline fails with file-not-found errors"
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "GREEN PHASE SOLUTION:"
            write(output_unit, '(A)') "Add 'project_file = ""README.md""' to fpm.toml [extra.ford] section"
        end if
    end subroutine print_test_summary

end program test_ford_configuration_validation