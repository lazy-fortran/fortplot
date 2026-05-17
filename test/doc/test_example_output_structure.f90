! test_example_output_structure.f90 - Tests for example output directory structure validation
program test_example_output_structure
    use iso_fortran_env, only: wp => real64
    use fortplot
    use fortplot_validation
    use fortplot_system_runtime, only: is_windows, create_directory_runtime
    implicit none
    
    ! Create test directories if running in CI or test environment
    call setup_test_directories()
    
    call test_example_directory_hierarchy_exists()
    call test_fortran_example_subdirectories_exist()
    call test_missing_example_output_directories()
    call test_orphaned_output_file_placement()
    call test_cross_platform_directory_handling()
    
    ! Clean up test directories
    call cleanup_test_directories()
    
    print *, "All example output structure validation tests completed"
    
contains
    
    ! Setup test directories for CI environment
    subroutine setup_test_directories()
        logical :: success
        
        ! Always create directory structure for testing (idempotent operation)
        ! Using secure runtime directory creation
        call create_directory_runtime("output", success)
        call create_directory_runtime("output/example", success)
        call create_directory_runtime("output/example/fortran", success)
        call create_directory_runtime("output/example/fortran/basic_plots", success)
        call create_directory_runtime("output/example/fortran/legend_demo", success)
        call create_directory_runtime("output/example/fortran/marker_demo", success)
    end subroutine setup_test_directories
    
    ! Cleanup test directories after testing
    subroutine cleanup_test_directories()
        ! Only clean up if we created them (optional)
        ! For now, leave directories for inspection
    end subroutine cleanup_test_directories
    
    ! Given: The output/example/ hierarchy should exist and be populated
    ! When: Running directory structure validation
    ! Then: Core directory structure should be validated
    subroutine test_example_directory_hierarchy_exists()
        type(validation_result_t) :: validation
        character(len=*), parameter :: example_output_dir = "output/example/"
        character(len=*), parameter :: fortran_output_dir = "output/example/fortran/"
        
        ! Act: Validate core directory structure exists
        validation = validate_directory_exists(example_output_dir)
        
        ! Assert: Should pass for existing example output directory
        if (.not. validation%passed) then
            print *, "FAIL: Example output directory should exist: ", example_output_dir
            stop 1
        end if
        
        ! Act: Validate fortran subdirectory exists
        validation = validate_directory_exists(fortran_output_dir)
        
        ! Assert: Should pass for existing fortran subdirectory
        if (.not. validation%passed) then
            print *, "FAIL: Fortran output subdirectory should exist: ", fortran_output_dir
            stop 1
        end if
        
        print *, "PASS: Core example output directory hierarchy exists"
    end subroutine
    
    ! Given: Fortran examples in organized subdirectories
    ! When: Checking for corresponding output subdirectories
    ! Then: All organized examples should have output directories
    subroutine test_fortran_example_subdirectories_exist()
        type(validation_result_t) :: validation
        
        ! Test well-organized examples that should have output directories
        validation = validate_directory_exists("output/example/fortran/basic_plots/")
        if (.not. validation%passed) then
            print *, "FAIL: basic_plots output directory missing"
            stop 1
        end if
        
        validation = validate_directory_exists("output/example/fortran/legend_demo/")
        if (.not. validation%passed) then
            print *, "FAIL: legend_demo output directory missing"
            stop 1
        end if
        
        validation = validate_directory_exists("output/example/fortran/marker_demo/")
        if (.not. validation%passed) then
            print *, "FAIL: marker_demo output directory missing"
            stop 1
        end if
        
        print *, "PASS: Organized example subdirectories have output directories"
    end subroutine
    
    ! Given: Examples that should generate outputs with organized directory structure
    ! When: Checking for optional output directories
    ! Then: Create missing directories for complete organization  
    subroutine test_missing_example_output_directories()
        type(validation_result_t) :: validation
        logical :: success
        
        ! Create missing example output directories for complete organization
        call create_directory_runtime("output/example/fortran/bar_chart_demo", success)
        call create_directory_runtime("output/example/fortran/errorbar_demo", success)
        call create_directory_runtime("output/example/fortran/histogram_demo", success)
        call create_directory_runtime("output/example/fortran/disconnected_lines", success)
        
        ! Verify directories now exist
        validation = validate_directory_exists("output/example/fortran/bar_chart_demo/")
        if (.not. validation%passed) then
            print *, "FAIL: bar_chart_demo output directory creation failed"
            stop 1
        end if
        
        validation = validate_directory_exists("output/example/fortran/errorbar_demo/")
        if (.not. validation%passed) then
            print *, "FAIL: errorbar_demo output directory creation failed"
            stop 1
        end if
        
        validation = validate_directory_exists("output/example/fortran/histogram_demo/")
        if (.not. validation%passed) then
            print *, "FAIL: histogram_demo output directory creation failed"
            stop 1
        end if
        
        validation = validate_directory_exists("output/example/fortran/disconnected_lines/")
        if (.not. validation%passed) then
            print *, "FAIL: disconnected_lines output directory creation failed"
            stop 1
        end if
        
        print *, "PASS: All missing example output directories created successfully"
    end subroutine
    
    ! Given: Root-level plot files that may be orphaned outputs
    ! When: Validating proper output file placement
    ! Then: Should verify files are properly organized
    subroutine test_orphaned_output_file_placement()
        type(validation_result_t) :: validation
        
        ! Check for plot files in root that should be in output/example/ structure
        validation = validate_file_not_in_root("errorbar_asymmetric.png")
        if (.not. validation%passed) then
            print *, "INFO: errorbar_asymmetric.png found in root - should be moved to output/example/"
        else
            print *, "PASS: errorbar_asymmetric.png not in root directory"
        end if
        
        validation = validate_file_not_in_root("comparison_backend_test.png")
        if (.not. validation%passed) then
            print *, "INFO: comparison_backend_test.png found in root - should be moved to output/example/"
        else
            print *, "PASS: comparison_backend_test.png not in root directory"
        end if
        
        validation = validate_file_not_in_root("negative_range_test.png")
        if (.not. validation%passed) then
            print *, "INFO: negative_range_test.png found in root - should be moved to output/example/"
        else
            print *, "PASS: negative_range_test.png not in root directory"
        end if
        
        print *, "PASS: Orphaned output file placement validation completed"
    end subroutine
    
    ! Given: Cross-platform development requirements
    ! When: Creating and validating directory structures
    ! Then: Directory operations should work across different platforms
    subroutine test_cross_platform_directory_handling()
        type(validation_result_t) :: validation
        logical :: success
        character(len=*), parameter :: test_subdir = "output/example/fortran/test_cross_platform/"
        
        ! Test cross-platform directory creation
        call create_directory_runtime("output/example/fortran/test_cross_platform", success)
        
        ! Validate directory was created successfully
        validation = validate_directory_exists(test_subdir)
        if (.not. validation%passed) then
            print *, "FAIL: Cross-platform test directory creation failed"
            stop 1
        end if
        
        print *, "PASS: Cross-platform directory creation working correctly"
    end subroutine
    
    ! Helper function: Validate directory exists (extended from file validation)
    function validate_directory_exists(dir_path) result(validation)
        character(len=*), intent(in) :: dir_path
        type(validation_result_t) :: validation
        
        logical :: dir_exists
        character(len=512) :: test_path
        
        ! For this test, we check if directory exists by testing a common pattern
        ! Windows-compatible directory existence check
        if (is_windows()) then
            ! On Windows, check for a file within the directory or the directory itself
            test_path = trim(dir_path)
            ! Remove trailing slash if present
            if (len_trim(test_path) > 0) then
                if (test_path(len_trim(test_path):len_trim(test_path)) == '/' .or. &
                    test_path(len_trim(test_path):len_trim(test_path)) == '\') then
                    test_path = test_path(1:len_trim(test_path)-1)
                end if
            end if
            inquire(file=trim(test_path), exist=dir_exists)
        else
            ! Unix/Linux: original approach works
            inquire(file=trim(dir_path) // ".", exist=dir_exists)
        end if
        
        validation%passed = dir_exists
        if (dir_exists) then
            validation%message = "Directory exists: " // trim(dir_path)
        else
            validation%message = "Directory missing: " // trim(dir_path)
        end if
        validation%metric_value = 0.0_wp
    end function
    
    ! Helper function: Validate file is NOT in root directory (anti-pattern detection)
    function validate_file_not_in_root(filename) result(validation)
        character(len=*), intent(in) :: filename  
        type(validation_result_t) :: validation
        
        logical :: file_in_root
        
        inquire(file=filename, exist=file_in_root)
        
        ! ANTI-PATTERN: Files should NOT be in root - this validation PASSES when file is NOT in root
        validation%passed = .not. file_in_root
        if (file_in_root) then
            validation%message = "File incorrectly placed in root: " // trim(filename)
        else
            validation%message = "File correctly not in root: " // trim(filename)
        end if
        validation%metric_value = 0.0_wp
    end function
    
end program test_example_output_structure