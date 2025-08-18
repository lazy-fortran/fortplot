! test_example_output_generation.f90 - Tests that examples generate outputs in correct locations
program test_example_output_generation
    use fortplot
    use fortplot_validation
    implicit none
    
    call test_example_generates_organized_output()
    call test_missing_example_output_generation()
    call test_example_output_file_consistency()
    call test_output_directory_auto_creation()
    call test_example_backend_output_consistency()
    
    print *, "All example output generation validation tests completed"
    
contains
    
    ! Given: Well-organized examples in subdirectories
    ! When: Running examples that should generate organized outputs
    ! Then: Outputs should appear in corresponding output/example/fortran/ subdirectories
    subroutine test_example_generates_organized_output()
        type(validation_result_t) :: validation
        
        ! Test that organized examples generate files in correct locations
        validation = validate_file_exists("output/example/fortran/basic_plots/simple_plot.png")
        if (.not. validation%passed) then
            print *, "FAIL: basic_plots should generate simple_plot.png in organized output directory"
            stop 1
        end if
        
        validation = validate_file_exists("output/example/fortran/legend_demo/basic_legend.png")
        if (.not. validation%passed) then
            print *, "FAIL: legend_demo should generate basic_legend.png in organized output directory" 
            stop 1
        end if
        
        validation = validate_file_exists("output/example/fortran/marker_demo/scatter_plot.png")
        if (.not. validation%passed) then
            print *, "FAIL: marker_demo should generate scatter_plot.png in organized output directory"
            stop 1
        end if
        
        print *, "PASS: Organized examples generate outputs in correct subdirectories"
    end subroutine
    
    ! Given: Examples that exist but lack organized output generation  
    ! When: Checking for expected output files in organized structure
    ! Then: Should FAIL for examples not generating organized outputs
    subroutine test_missing_example_output_generation()
        type(validation_result_t) :: validation
        
        ! These should FAIL initially - examples exist but don't generate organized outputs
        validation = validate_file_exists("output/example/fortran/bar_chart_demo/bar_chart_basic.png")
        if (validation%passed) then
            print *, "UNEXPECTED: bar_chart_demo generates organized output (should be missing initially)"
        else
            print *, "EXPECTED FAIL: bar_chart_demo doesn't generate organized output - needs fix"
        end if
        
        validation = validate_file_exists("output/example/fortran/errorbar_demo/errorbar_basic.png") 
        if (validation%passed) then
            print *, "UNEXPECTED: errorbar_demo generates organized output (should be missing initially)"
        else
            print *, "EXPECTED FAIL: errorbar_demo doesn't generate organized output - needs fix"
        end if
        
        validation = validate_file_exists("output/example/fortran/histogram_demo/histogram_basic.png")
        if (validation%passed) then
            print *, "UNEXPECTED: histogram_demo generates organized output (should be missing initially)"
        else
            print *, "EXPECTED FAIL: histogram_demo doesn't generate organized output - needs fix"
        end if
        
        ! disconnected_lines.f90 is at root level but should generate organized output
        validation = validate_file_exists("output/example/fortran/disconnected_lines/disconnected_basic.png")
        if (validation%passed) then
            print *, "UNEXPECTED: disconnected_lines generates organized output (should be missing initially)"
        else
            print *, "EXPECTED FAIL: disconnected_lines doesn't generate organized output - needs fix"
        end if
        
        print *, "EXPECTED FAILURES: Missing example output generation detected correctly"
    end subroutine
    
    ! Given: Examples that generate multiple output files
    ! When: Validating output file consistency across backends
    ! Then: All backend outputs should be in consistent organized locations
    subroutine test_example_output_file_consistency()
        type(validation_result_t) :: validation
        
        ! Test multi-backend consistency for organized examples
        validation = validate_file_exists("output/example/fortran/basic_plots/simple_plot.png")
        if (.not. validation%passed) then
            print *, "FAIL: basic_plots PNG output missing from organized location"
            stop 1
        end if
        
        validation = validate_file_exists("output/example/fortran/basic_plots/simple_plot.pdf")
        if (.not. validation%passed) then
            print *, "FAIL: basic_plots PDF output missing from organized location"
            stop 1
        end if
        
        validation = validate_file_exists("output/example/fortran/basic_plots/simple_plot.txt")
        if (.not. validation%passed) then
            print *, "FAIL: basic_plots ASCII output missing from organized location"
            stop 1
        end if
        
        print *, "PASS: Multi-backend outputs consistently organized"
    end subroutine
    
    ! Given: Examples that should auto-create output directories
    ! When: Running examples that need directory structure
    ! Then: Should automatically create necessary output subdirectories  
    subroutine test_output_directory_auto_creation()
        type(validation_result_t) :: validation
        character(len=*), parameter :: test_output_dir = "output/example/fortran/test_auto_create/"
        character(len=*), parameter :: test_output_file = "output/example/fortran/test_auto_create/test_plot.png"
        
        ! This tests the auto-creation logic that should be implemented
        ! Initially should fail since directory doesn't exist
        validation = validate_directory_exists(test_output_dir)
        if (validation%passed) then
            print *, "UNEXPECTED: Auto-create test directory exists (should be missing initially)"
        else
            print *, "EXPECTED FAIL: Auto-create test directory missing - needs implementation"
        end if
        
        ! Simulate what should happen: example creates directory and generates file
        ! This should fail initially since the auto-creation isn't implemented
        validation = validate_file_exists(test_output_file)
        if (validation%passed) then
            print *, "UNEXPECTED: Auto-create test file exists (should be missing initially)"
        else
            print *, "EXPECTED FAIL: Auto-create test file missing - needs directory creation logic"
        end if
        
        print *, "EXPECTED FAILURES: Auto-directory creation needs implementation"
    end subroutine
    
    ! Given: Examples that should work across PNG, PDF, ASCII backends
    ! When: Generating outputs with consistent organization
    ! Then: All backends should respect organized output structure
    subroutine test_example_backend_output_consistency()
        type(validation_result_t) :: validation
        
        ! Test that backend selection doesn't break organization
        ! Check format_string_demo which should have all three backends
        validation = validate_file_exists("output/example/fortran/format_string_demo/format_string_demo.png")
        if (.not. validation%passed) then
            print *, "FAIL: format_string_demo PNG missing from organized location"
            stop 1  
        end if
        
        validation = validate_file_exists("output/example/fortran/format_string_demo/format_string_demo.pdf")
        if (.not. validation%passed) then
            print *, "FAIL: format_string_demo PDF missing from organized location"
            stop 1
        end if
        
        validation = validate_file_exists("output/example/fortran/format_string_demo/format_string_demo.txt")
        if (.not. validation%passed) then
            print *, "FAIL: format_string_demo ASCII missing from organized location"
            stop 1
        end if
        
        print *, "PASS: Backend output consistency maintained in organized structure"
    end subroutine
    
    ! Helper function: Validate directory exists (reused from structure test)
    function validate_directory_exists(dir_path) result(validation)
        character(len=*), intent(in) :: dir_path
        type(validation_result_t) :: validation
        
        logical :: dir_exists
        
        ! Check if directory exists by testing for directory marker
        inquire(file=trim(dir_path) // ".", exist=dir_exists)
        
        validation%passed = dir_exists
        if (dir_exists) then
            validation%message = "Directory exists: " // trim(dir_path)
        else
            validation%message = "Directory missing: " // trim(dir_path)
        end if
        validation%metric_value = 0.0_wp
    end function
    
end program test_example_output_generation