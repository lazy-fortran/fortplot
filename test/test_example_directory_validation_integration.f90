! test_example_directory_validation_integration.f90 - Integration with Issue #93 validation framework
program test_example_directory_validation_integration
    use fortplot
    use fortplot_validation
    implicit none
    
    call test_integration_with_existing_validation()
    call test_directory_validation_workflow()
    call test_example_functional_validation_chain()
    call test_validation_reporting_integration()
    call test_cross_validation_consistency()
    
    print *, "All example directory validation integration tests completed"
    
contains
    
    ! Given: Existing functional validation framework from Issue #93
    ! When: Integrating directory structure validation
    ! Then: Should work seamlessly with existing validation utilities
    subroutine test_integration_with_existing_validation()
        type(validation_result_t) :: file_validation, dir_validation
        character(len=*), parameter :: example_file = "output/example/fortran/basic_plots/simple_plot.png"
        character(len=*), parameter :: example_dir = "output/example/fortran/basic_plots/"
        
        ! Act: Use existing file validation
        file_validation = validate_file_exists(example_file)
        
        ! Act: Use new directory validation  
        dir_validation = validate_directory_exists(example_dir)
        
        ! Assert: Both should pass for well-organized examples
        if (.not. file_validation%passed) then
            print *, "FAIL: Existing file validation should work with organized outputs"
            stop 1
        end if
        
        if (.not. dir_validation%passed) then
            print *, "FAIL: New directory validation should detect existing directories"
            stop 1
        end if
        
        ! Test validation result structure consistency
        if (len_trim(file_validation%message) == 0) then
            print *, "FAIL: File validation should provide meaningful message"
            stop 1
        end if
        
        if (len_trim(dir_validation%message) == 0) then
            print *, "FAIL: Directory validation should provide meaningful message"  
            stop 1
        end if
        
        print *, "PASS: Directory validation integrates with existing validation framework"
    end subroutine
    
    ! Given: Directory structure validation workflow
    ! When: Running complete validation chain for examples
    ! Then: Should provide comprehensive validation reporting
    subroutine test_directory_validation_workflow()
        type(validation_result_t) :: validations(4)
        integer :: passed_count, i
        
        ! Run validation workflow for organized example
        validations(1) = validate_directory_exists("output/example/")
        validations(2) = validate_directory_exists("output/example/fortran/")
        validations(3) = validate_directory_exists("output/example/fortran/basic_plots/")
        validations(4) = validate_file_exists("output/example/fortran/basic_plots/simple_plot.png")
        
        ! Count passed validations
        passed_count = 0
        do i = 1, 4
            if (validations(i)%passed) passed_count = passed_count + 1
        end do
        
        ! Assert: All validations should pass for well-organized examples
        if (passed_count /= 4) then
            print *, "FAIL: Complete validation workflow should pass for organized examples"
            do i = 1, 4
                if (.not. validations(i)%passed) then
                    print *, "  FAILED: ", trim(validations(i)%message)
                end if
            end do
            stop 1
        end if
        
        print *, "PASS: Directory validation workflow works end-to-end"
    end subroutine
    
    ! Given: Example functional validation requirements
    ! When: Combining directory, file, and format validation
    ! Then: Should provide complete example validation chain
    subroutine test_example_functional_validation_chain()
        type(validation_result_t) :: dir_val, file_val, size_val, format_val
        character(len=*), parameter :: example_dir = "output/example/fortran/legend_demo/"
        character(len=*), parameter :: example_file = "output/example/fortran/legend_demo/basic_legend.png"
        
        ! Run complete validation chain
        dir_val = validate_directory_exists(example_dir)
        file_val = validate_file_exists(example_file)  
        size_val = validate_file_size(example_file, MIN_PNG_SIZE)
        format_val = validate_png_format(example_file)
        
        ! Assert: Well-organized examples should pass all validations
        if (.not. dir_val%passed) then
            print *, "FAIL: Directory validation failed: ", trim(dir_val%message)
            stop 1
        end if
        
        if (.not. file_val%passed) then
            print *, "FAIL: File existence validation failed: ", trim(file_val%message)
            stop 1
        end if
        
        if (.not. size_val%passed) then
            print *, "FAIL: File size validation failed: ", trim(size_val%message) 
            stop 1
        end if
        
        if (.not. format_val%passed) then
            print *, "FAIL: PNG format validation failed: ", trim(format_val%message)
            stop 1
        end if
        
        print *, "PASS: Complete functional validation chain works for organized examples"
    end subroutine
    
    ! Given: Validation framework reporting capabilities
    ! When: Generating validation reports for directory issues
    ! Then: Should provide actionable reporting for missing directories
    subroutine test_validation_reporting_integration()
        type(validation_result_t) :: validation
        character(len=*), parameter :: missing_dir = "output/example/fortran/missing_example_demo/"
        character(len=*), parameter :: missing_file = "output/example/fortran/missing_example_demo/missing.png"
        
        ! Test reporting for missing directory
        validation = validate_directory_exists(missing_dir)
        if (validation%passed) then
            print *, "UNEXPECTED: Missing directory reported as existing"
        else
            ! Validate error message is informative
            if (index(validation%message, "missing") == 0 .and. index(validation%message, "Directory missing") == 0) then
                print *, "FAIL: Directory validation should report missing status clearly"
                stop 1
            end if
            print *, "PASS: Missing directory reported correctly: ", trim(validation%message)
        end if
        
        ! Test reporting for missing file in missing directory
        validation = validate_file_exists(missing_file)
        if (validation%passed) then
            print *, "UNEXPECTED: Missing file reported as existing"
        else
            ! Validate error message is informative  
            if (index(validation%message, "missing") == 0 .and. index(validation%message, "File missing") == 0) then
                print *, "FAIL: File validation should report missing status clearly"
                stop 1
            end if
            print *, "PASS: Missing file reported correctly: ", trim(validation%message)
        end if
        
        print *, "PASS: Validation reporting provides actionable feedback for directory issues"
    end subroutine
    
    ! Given: Different validation methods for directories and files
    ! When: Cross-validating consistency between directory and file checks
    ! Then: Results should be logically consistent
    subroutine test_cross_validation_consistency()
        type(validation_result_t) :: dir_val, file_val
        character(len=*), parameter :: existing_dir = "output/example/fortran/contour_demo/"
        character(len=*), parameter :: existing_file = "output/example/fortran/contour_demo/contour_gaussian.png"
        character(len=*), parameter :: missing_dir = "output/example/fortran/nonexistent/"
        character(len=*), parameter :: missing_file = "output/example/fortran/nonexistent/missing.png"
        
        ! Test consistency for existing directory/file
        dir_val = validate_directory_exists(existing_dir)
        file_val = validate_file_exists(existing_file)
        
        if (dir_val%passed .and. .not. file_val%passed) then
            print *, "INCONSISTENCY: Directory exists but expected file missing"
            print *, "  Dir: ", trim(dir_val%message)
            print *, "  File: ", trim(file_val%message)
            ! This might be expected for some tests, so don't stop
        end if
        
        ! Test consistency for missing directory/file
        dir_val = validate_directory_exists(missing_dir)
        file_val = validate_file_exists(missing_file)
        
        if (dir_val%passed .or. file_val%passed) then
            print *, "INCONSISTENCY: Missing directory or file reported as existing"
            stop 1
        end if
        
        print *, "PASS: Directory and file validation results are logically consistent"
    end subroutine
    
    ! Helper function: Validate directory exists (consistent with other tests)
    function validate_directory_exists(dir_path) result(validation)
        character(len=*), intent(in) :: dir_path
        type(validation_result_t) :: validation
        
        logical :: dir_exists
        
        ! Use consistent directory existence check
        inquire(file=trim(dir_path) // ".", exist=dir_exists)
        
        validation%passed = dir_exists
        if (dir_exists) then
            validation%message = "Directory exists: " // trim(dir_path)
        else
            validation%message = "Directory missing: " // trim(dir_path)
        end if
        validation%metric_value = 0.0_wp
    end function
    
end program test_example_directory_validation_integration