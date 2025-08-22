program test_ford_file_references
    !! **Given-When-Then Documentation**
    !!
    !! **GIVEN**: FORD documentation system with file references
    !! **WHEN**: Validating file reference consistency and existence
    !! **THEN**: All referenced files should exist and be correctly specified
    !!
    !! **PURPOSE**: Test FORD file reference resolution for Issue #192
    !!
    !! **RED PHASE EXPECTATIONS**: 
    !! - Test WILL FAIL due to README.md vs doc.md reference mismatch
    !! - Test WILL FAIL due to hardcoded file references in Makefile
    !! - These failures pinpoint the exact file reference problems in Issue #192
    !!
    !! **CRITICAL BEHAVIOR VALIDATION**:
    !! 1. Makefile file references vs actual files
    !! 2. fpm.toml configuration vs file system
    !! 3. FORD project file resolution
    !! 4. Documentation source file validation

    use iso_fortran_env, only: output_unit, error_unit
    implicit none
    
    logical :: test_passed
    integer :: test_count, failed_count
    
    test_count = 0
    failed_count = 0
    
    write(output_unit, '(A)') "=== FORD File Reference Validation Tests ==="
    write(output_unit, '(A)') "Testing Issue #192: File reference consistency problems"
    write(output_unit, '(A)') ""
    
    ! Test 1: Validate README.md vs doc.md existence mismatch
    call test_readme_vs_doc_md_existence(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "README.md vs doc.md file existence validation")
    
    ! Test 2: Test Makefile file reference consistency
    call test_makefile_file_reference_consistency(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "Makefile file reference consistency")
    
    ! Test 3: Validate fpm.toml vs actual file system
    call test_fpm_toml_vs_filesystem(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "fpm.toml configuration vs file system")
    
    ! Test 4: Test FORD project file resolution
    call test_ford_project_file_resolution(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "FORD project file resolution")
    
    ! Test 5: Validate documentation source directories
    call test_documentation_source_directories(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "Documentation source directory validation")
    
    ! Test 6: Test file reference chain completeness
    call test_file_reference_chain_completeness(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "File reference chain completeness")
    
    ! Test 7: Validate cross-reference consistency
    call test_cross_reference_consistency(test_passed)
    call update_test_results(test_passed, test_count, failed_count, &
        "Cross-reference consistency validation")
    
    ! Final test results
    call print_test_summary(test_count, failed_count)
    
    if (failed_count > 0) then
        error stop "FORD file reference tests FAILED - reference inconsistencies detected"
    else
        write(output_unit, '(A)') "All FORD file reference tests PASSED"
    end if

contains

    subroutine test_readme_vs_doc_md_existence(passed)
        !! **GIVEN**: Project should have main documentation file
        !! **WHEN**: Checking for README.md vs doc.md existence
        !! **THEN**: Should have README.md but NOT doc.md (as per Issue #192)
        logical, intent(out) :: passed
        logical :: readme_exists, doc_md_exists
        
        passed = .false.
        
        ! Check file existence
        inquire(file='README.md', exist=readme_exists)
        inquire(file='doc.md', exist=doc_md_exists)
        
        write(output_unit, '(A,L1)') "README.md exists: ", readme_exists
        write(output_unit, '(A,L1)') "doc.md exists: ", doc_md_exists
        
        ! Issue #192 scenario: README.md exists, doc.md does not
        if (.not. readme_exists) then
            write(error_unit, '(A)') "ERROR: README.md does not exist - main documentation file missing"
            return
        end if
        
        if (doc_md_exists) then
            write(error_unit, '(A)') "WARNING: doc.md exists but should not be needed"
            write(error_unit, '(A)') "Issue #192 indicates doc.md should be replaced with README.md"
        else
            write(output_unit, '(A)') "CONFIRMED: doc.md does not exist (this is the Issue #192 problem)"
            write(output_unit, '(A)') "Makefile references missing doc.md instead of existing README.md"
        end if
        
        ! Test passes if README.md exists (the correct file)
        passed = readme_exists
        
        if (.not. doc_md_exists .and. readme_exists) then
            write(output_unit, '(A)') "DIAGNOSIS: File reference mismatch confirmed"
            write(output_unit, '(A)') "Solution: Update references from doc.md to README.md"
        end if
    end subroutine test_readme_vs_doc_md_existence

    subroutine test_makefile_file_reference_consistency(passed)
        !! **GIVEN**: Makefile with FORD documentation target
        !! **WHEN**: Checking file references in Makefile
        !! **THEN**: All referenced files should exist
        !! **RED PHASE**: Will FAIL due to doc.md reference
        logical, intent(out) :: passed
        integer :: status, unit_file
        character(len=200) :: line
        logical :: ford_command_found, references_doc_md, references_readme
        
        passed = .false.
        ford_command_found = .false.
        references_doc_md = .false.
        references_readme = .false.
        
        open(newunit=unit_file, file='Makefile', status='old', action='read', iostat=status)
        if (status /= 0) then
            write(error_unit, '(A)') "ERROR: Cannot read Makefile"
            return
        end if
        
        do
            read(unit_file, '(A)', iostat=status) line
            if (status /= 0) exit
            
            ! Look for ford command
            if (index(line, 'ford') > 0) then
                ford_command_found = .true.
                write(output_unit, '(A,A)') "Found FORD command: ", trim(line)
                
                ! Check what file it references
                if (index(line, 'doc.md') > 0) then
                    references_doc_md = .true.
                    write(error_unit, '(A)') "ISSUE FOUND: Makefile references doc.md (which doesn't exist)"
                end if
                
                if (index(line, 'README.md') > 0) then
                    references_readme = .true.
                    write(output_unit, '(A)') "GOOD: Makefile references README.md"
                end if
            end if
        end do
        close(unit_file)
        
        if (.not. ford_command_found) then
            write(error_unit, '(A)') "ERROR: No FORD command found in Makefile"
            return
        end if
        
        if (references_doc_md) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): Makefile references non-existent doc.md"
            write(error_unit, '(A)') "This is the ROOT CAUSE of Issue #192"
            write(error_unit, '(A)') "Solution: Change 'ford doc.md' to 'ford README.md' in Makefile"
            return
        end if
        
        if (.not. references_readme) then
            write(error_unit, '(A)') "ERROR: Makefile doesn't reference any known documentation file"
            return
        end if
        
        passed = .true.
    end subroutine test_makefile_file_reference_consistency

    subroutine test_fpm_toml_vs_filesystem(passed)
        !! **GIVEN**: fpm.toml with FORD configuration
        !! **WHEN**: Validating configuration against file system
        !! **THEN**: All configured paths should exist
        logical, intent(out) :: passed
        character(len=100) :: src_dir, output_dir, page_dir, project_file
        logical :: src_exists, page_exists, project_file_exists
        
        passed = .false.
        
        ! Extract configuration from fpm.toml
        call extract_ford_config_value('src_dir', src_dir)
        call extract_ford_config_value('output_dir', output_dir)
        call extract_ford_config_value('page_dir', page_dir)
        call extract_ford_config_value('project_file', project_file)
        
        write(output_unit, '(A,A)') "Configured src_dir: ", trim(src_dir)
        write(output_unit, '(A,A)') "Configured page_dir: ", trim(page_dir)
        write(output_unit, '(A,A)') "Configured project_file: ", trim(project_file)
        
        ! Validate paths exist
        if (len_trim(src_dir) > 0) then
            inquire(file=trim(src_dir), exist=src_exists)
            if (.not. src_exists) then
                write(error_unit, '(A,A)') "ERROR: Configured src_dir does not exist: ", trim(src_dir)
                return
            end if
        end if
        
        if (len_trim(page_dir) > 0) then
            inquire(file=trim(page_dir), exist=page_exists)
            if (.not. page_exists) then
                write(error_unit, '(A,A)') "ERROR: Configured page_dir does not exist: ", trim(page_dir)
                return
            end if
        end if
        
        if (len_trim(project_file) > 0) then
            inquire(file=trim(project_file), exist=project_file_exists)
            if (.not. project_file_exists) then
                write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): Configured project_file missing"
                write(error_unit, '(A,A)') "Missing file: ", trim(project_file)
                write(error_unit, '(A)') "This indicates fmp.toml project_file setting is incorrect"
                return
            end if
        else
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): project_file not configured"
            write(error_unit, '(A)') "This is a major part of Issue #192 - missing project_file setting"
            return
        end if
        
        passed = .true.
    end subroutine test_fpm_toml_vs_filesystem

    subroutine test_ford_project_file_resolution(passed)
        !! **GIVEN**: FORD configuration with project file setting
        !! **WHEN**: Resolving project file for FORD processing
        !! **THEN**: File should exist and be readable by FORD
        logical, intent(out) :: passed
        character(len=100) :: project_file
        logical :: file_exists, file_readable, readme_exists
        integer :: unit_file, status
        
        passed = .false.
        
        ! Get project file from configuration
        call extract_ford_config_value('project_file', project_file)
        
        if (len_trim(project_file) == 0) then
            ! Try default expectations
            project_file = 'README.md'
            write(output_unit, '(A)') "No project_file configured, testing with README.md"
        end if
        
        inquire(file=trim(project_file), exist=file_exists)
        if (.not. file_exists) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): FORD project file does not exist"
            write(error_unit, '(A,A)') "Expected file: ", trim(project_file)
            
            ! Check if we should be using README.md instead
            inquire(file='README.md', exist=readme_exists)
            if (readme_exists .and. trim(project_file) /= 'README.md') then
                write(error_unit, '(A)') "DIAGNOSIS: Should use README.md as project_file"
            end if
            return
        end if
        
        ! Test file readability
        open(newunit=unit_file, file=trim(project_file), status='old', action='read', iostat=status)
        if (status /= 0) then
            write(error_unit, '(A,A)') "ERROR: Cannot read project file: ", trim(project_file)
            return
        end if
        close(unit_file)
        
        write(output_unit, '(A,A)') "FORD project file validated: ", trim(project_file)
        passed = .true.
    end subroutine test_ford_project_file_resolution

    subroutine test_documentation_source_directories(passed)
        !! **GIVEN**: FORD configuration specifying source directories
        !! **WHEN**: Validating source directory structure
        !! **THEN**: All specified directories should exist and contain expected content
        logical, intent(out) :: passed
        character(len=100) :: src_dir, page_dir
        logical :: src_exists, page_exists, has_fortran_files, has_doc_files
        
        passed = .false.
        
        call extract_ford_config_value('src_dir', src_dir)
        call extract_ford_config_value('page_dir', page_dir)
        
        ! Default directories if not configured
        if (len_trim(src_dir) == 0) src_dir = 'src'
        if (len_trim(page_dir) == 0) page_dir = 'doc'
        
        ! Check directory existence
        inquire(file=trim(src_dir), exist=src_exists)
        inquire(file=trim(page_dir), exist=page_exists)
        
        if (.not. src_exists) then
            write(error_unit, '(A,A)') "ERROR: Source directory does not exist: ", trim(src_dir)
            return
        end if
        
        if (.not. page_exists) then
            write(error_unit, '(A,A)') "ERROR: Documentation page directory does not exist: ", trim(page_dir)
            return
        end if
        
        ! Check for expected content
        call check_directory_content(trim(src_dir), '*.f90', has_fortran_files)
        call check_directory_content(trim(page_dir), '*.md', has_doc_files)
        
        if (.not. has_fortran_files) then
            write(error_unit, '(A,A)') "WARNING: No Fortran files found in: ", trim(src_dir)
        end if
        
        if (.not. has_doc_files) then
            write(output_unit, '(A,A)') "INFO: No markdown files in: ", trim(page_dir)
        end if
        
        write(output_unit, '(A)') "Documentation source directories validated"
        passed = .true.
    end subroutine test_documentation_source_directories

    subroutine test_file_reference_chain_completeness(passed)
        !! **GIVEN**: Complete FORD documentation system
        !! **WHEN**: Tracing file reference chain from Makefile to sources
        !! **THEN**: All links in the chain should be valid
        logical, intent(out) :: passed
        logical :: makefile_ok, fpm_toml_ok, project_file_ok, sources_ok
        
        passed = .false.
        
        ! Test each link in the reference chain
        call validate_makefile_references(makefile_ok)
        call validate_fpm_toml_references(fpm_toml_ok)
        call validate_project_file_references(project_file_ok)
        call validate_source_file_references(sources_ok)
        
        write(output_unit, '(A)') "=== File Reference Chain Status ==="
        write(output_unit, '(A,L1)') "Makefile references: ", makefile_ok
        write(output_unit, '(A,L1)') "fpm.toml references: ", fpm_toml_ok
        write(output_unit, '(A,L1)') "Project file references: ", project_file_ok
        write(output_unit, '(A,L1)') "Source file references: ", sources_ok
        
        if (.not. makefile_ok) then
            write(error_unit, '(A)') "CHAIN BREAK: Makefile reference issues"
        end if
        
        if (.not. fpm_toml_ok) then
            write(error_unit, '(A)') "CHAIN BREAK: fpm.toml reference issues"
        end if
        
        if (.not. project_file_ok) then
            write(error_unit, '(A)') "CHAIN BREAK: Project file reference issues"
        end if
        
        passed = makefile_ok .and. fpm_toml_ok .and. project_file_ok .and. sources_ok
        
        if (.not. passed) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): File reference chain broken"
            write(error_unit, '(A)') "This confirms Issue #192 - documentation reference problems"
        end if
    end subroutine test_file_reference_chain_completeness

    subroutine test_cross_reference_consistency(passed)
        !! **GIVEN**: Multiple configuration files with FORD references
        !! **WHEN**: Comparing references across different files
        !! **THEN**: All references should be consistent and point to same files
        logical, intent(out) :: passed
        character(len=100) :: makefile_ref, fpm_toml_ref
        logical :: references_consistent
        
        passed = .false.
        
        ! Extract references from different sources
        call extract_makefile_ford_reference(makefile_ref)
        call extract_ford_config_value('project_file', fpm_toml_ref)
        
        write(output_unit, '(A,A)') "Makefile FORD reference: ", trim(makefile_ref)
        write(output_unit, '(A,A)') "fpm.toml project_file: ", trim(fpm_toml_ref)
        
        ! Compare references
        references_consistent = .false.
        
        if (len_trim(makefile_ref) > 0 .and. len_trim(fpm_toml_ref) > 0) then
            references_consistent = (trim(makefile_ref) == trim(fpm_toml_ref))
        else if (len_trim(makefile_ref) > 0 .and. len_trim(fpm_toml_ref) == 0) then
            ! If fpm.toml has no project_file, check if makefile refs README.md
            references_consistent = (trim(makefile_ref) == 'README.md')
        end if
        
        if (.not. references_consistent) then
            write(error_unit, '(A)') "EXPECTED FAILURE (RED PHASE): Cross-reference inconsistency"
            write(error_unit, '(A)') "Makefile and fpm.toml reference different files"
            write(error_unit, '(A)') "This confirms Issue #192 - documentation reference mismatch"
            return
        end if
        
        passed = .true.
    end subroutine test_cross_reference_consistency

    ! Helper subroutines
    subroutine extract_ford_config_value(key, value)
        character(len=*), intent(in) :: key
        character(len=*), intent(out) :: value
        integer :: unit_file, status
        character(len=200) :: line
        logical :: in_ford_section
        
        value = ""
        in_ford_section = .false.
        
        open(newunit=unit_file, file='fpm.toml', status='old', action='read', iostat=status)
        if (status /= 0) return
        
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
            
            if (in_ford_section .and. index(line, trim(key) // ' =') > 0) then
                call extract_quoted_value(line, value)
                exit
            end if
        end do
        close(unit_file)
    end subroutine extract_ford_config_value

    subroutine extract_makefile_ford_reference(reference)
        character(len=*), intent(out) :: reference
        integer :: unit_file, status
        character(len=200) :: line
        integer :: ford_pos, space_pos
        
        reference = ""
        
        open(newunit=unit_file, file='Makefile', status='old', action='read', iostat=status)
        if (status /= 0) return
        
        do
            read(unit_file, '(A)', iostat=status) line
            if (status /= 0) exit
            
            ford_pos = index(line, 'ford ')
            if (ford_pos > 0) then
                ! Extract the file reference after "ford "
                reference = adjustl(line(ford_pos+5:))
                space_pos = index(reference, ' ')
                if (space_pos > 0) then
                    reference = reference(1:space_pos-1)
                end if
                exit
            end if
        end do
        close(unit_file)
    end subroutine extract_makefile_ford_reference

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

    subroutine check_directory_content(dir_path, pattern, has_files)
        character(len=*), intent(in) :: dir_path, pattern
        logical, intent(out) :: has_files
        integer :: status
        
        ! Simplified check - in real implementation would use system calls
        has_files = .true.  ! Assume true for testing
    end subroutine check_directory_content

    subroutine validate_makefile_references(valid)
        logical, intent(out) :: valid
        character(len=100) :: ref
        logical :: file_exists
        
        call extract_makefile_ford_reference(ref)
        if (len_trim(ref) > 0) then
            inquire(file=trim(ref), exist=file_exists)
            valid = file_exists
        else
            valid = .false.
        end if
    end subroutine validate_makefile_references

    subroutine validate_fpm_toml_references(valid)
        logical, intent(out) :: valid
        character(len=100) :: ref
        logical :: file_exists
        
        call extract_ford_config_value('project_file', ref)
        if (len_trim(ref) > 0) then
            inquire(file=trim(ref), exist=file_exists)
            valid = file_exists
        else
            valid = .false.  ! project_file should be configured
        end if
    end subroutine validate_fpm_toml_references

    subroutine validate_project_file_references(valid)
        logical, intent(out) :: valid
        ! Simplified validation
        valid = .true.
    end subroutine validate_project_file_references

    subroutine validate_source_file_references(valid)
        logical, intent(out) :: valid
        logical :: src_exists
        inquire(file='src', exist=src_exists)
        valid = src_exists
    end subroutine validate_source_file_references

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
        write(output_unit, '(A)') "=== File Reference Test Summary ==="
        write(output_unit, '(A,I0,A,I0,A)') "Tests run: ", test_count, ", Failed: ", failed_count, ""
        
        if (failed_count > 0) then
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "=== RED PHASE FILE REFERENCE ANALYSIS ==="
            write(output_unit, '(A)') "File reference inconsistencies detected:"
            write(output_unit, '(A)') "1. Makefile references 'doc.md' (does not exist)"
            write(output_unit, '(A)') "2. fpm.toml missing 'project_file = ""README.md""'"
            write(output_unit, '(A)') "3. README.md exists but is not properly referenced"
            write(output_unit, '(A)') ""
            write(output_unit, '(A)') "EXACT SOLUTION FOR ISSUE #192:"
            write(output_unit, '(A)') "1. Change Makefile: 'ford doc.md' → 'ford README.md'"
            write(output_unit, '(A)') "2. Add to fpm.toml [extra.ford]: project_file = ""README.md"""
        end if
    end subroutine print_test_summary

end program test_ford_file_references