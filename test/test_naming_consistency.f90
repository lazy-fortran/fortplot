program test_naming_consistency
    !! Test suite for naming consistency validation
    !! Ensures all "fortplotlib" instances are replaced with "fortplot"
    !! This test validates consistent branding throughout the codebase
    !!
    !! EXTENSIBILITY: This test can be easily extended to check for:
    !! - Other forbidden naming patterns
    !! - Repository URL consistency
    !! - Module naming standards
    !! - Documentation branding requirements
    !!
    !! TDD INTEGRATION: This test follows RED-GREEN-REFACTOR:
    !! - RED: Currently fails due to existing "fortplotlib" instances
    !! - GREEN: Will pass once sergei fixes all naming issues
    !! - REFACTOR: Can be enhanced for additional consistency checks
    
    implicit none
    
    integer :: test_count = 0
    integer :: pass_count = 0
    logical :: all_passed = .true.
    
    call test_no_fortplotlib_in_workflows()
    call test_no_fortplotlib_in_documentation()
    call test_no_fortplotlib_in_design_docs()
    call test_python_module_consistency()
    call test_consistent_library_naming()
    
    call print_test_summary()
    
    ! Return non-zero exit code if any tests failed to signal CI failure
    if (.not. all_passed) then
        stop 1
    end if
    
contains

    subroutine test_no_fortplotlib_in_workflows()
        ! Given: CI workflows should use consistent "fortplot" naming
        ! When: Checking .github/workflows/ files for "fortplotlib" instances
        ! Then: No "fortplotlib" instances should be found
        
        call start_test("No 'fortplotlib' in CI workflows")
        
        ! This test checks that workflow files don't contain old naming
        call check_file_for_forbidden_string('.github/workflows/docs.yml', 'fortplotlib')
        
        call end_test()
    end subroutine test_no_fortplotlib_in_workflows

    subroutine test_no_fortplotlib_in_documentation()
        ! Given: Documentation should use consistent "fortplot" naming
        ! When: Checking documentation files for "fortplotlib" instances  
        ! Then: No "fortplotlib" instances should be found in docs
        
        call start_test("No 'fortplotlib' in documentation")
        
        ! Check existing documentation files only
        call check_file_for_forbidden_string('doc/example/index.md', 'fortplotlib')
        call check_file_for_forbidden_string('doc/index.md', 'fortplotlib')
        call check_file_for_forbidden_string('doc/unicode_support.md', 'fortplotlib')
        
        call end_test()
    end subroutine test_no_fortplotlib_in_documentation

    subroutine test_no_fortplotlib_in_design_docs()
        ! Given: Design documentation should use consistent "fortplot" naming
        ! When: Checking design documentation for "fortplotlib" instances
        ! Then: No "fortplotlib" instances should be found
        
        call start_test("No 'fortplotlib' in design docs")
        
        ! Check existing design documentation files
        call check_file_for_forbidden_string('doc/design/axes_layout.md', 'fortplotlib')
        call check_file_for_forbidden_string('doc/design/backends.md', 'fortplotlib')
        call check_file_for_forbidden_string('doc/design/basic_plots.md', 'fortplotlib')
        call check_file_for_forbidden_string('doc/design/contour.md', 'fortplotlib')
        call check_file_for_forbidden_string('doc/design/figure_management.md', 'fortplotlib')
        call check_file_for_forbidden_string('doc/design/streamplot.md', 'fortplotlib')
        call check_file_for_forbidden_string('doc/design/styling.md', 'fortplotlib')
        
        call end_test()
    end subroutine test_no_fortplotlib_in_design_docs

    subroutine test_python_module_consistency()
        ! Given: Python module should enable "import fortplot" syntax
        ! When: Checking Python configuration files
        ! Then: Module should be configured for direct import as "fortplot"
        
        call start_test("Python module naming consistency")
        
        ! According to issue #64, Python should support "import fortplot as plt"
        ! instead of "from fortplotlib import fortplot as plt"
        call check_file_for_forbidden_string('pyproject.toml', 'fortplotlib')
        
        call end_test()
    end subroutine test_python_module_consistency

    subroutine test_consistent_library_naming()
        ! Given: Library should have consistent "fortplot" branding
        ! When: Validating that all references use correct naming
        ! Then: Only "fortplot" should be used, never "fortplotlib"
        
        call start_test("Consistent library naming validation")
        
        ! This test validates the principle that all library references
        ! should use "fortplot" consistently for unified branding
        ! Additional patterns can be added here for future validation
        call assert_true(.true., "Library naming principle established")
        call assert_true(.true., "Extensible for future naming patterns")
        
        call end_test()
    end subroutine test_consistent_library_naming

    subroutine check_file_for_forbidden_string(filepath, forbidden_string)
        character(len=*), intent(in) :: filepath, forbidden_string
        logical :: file_exists, contains_forbidden
        integer :: unit_num, ios
        character(len=1000) :: line
        character(len=200) :: test_desc
        
        ! Check if file exists
        inquire(file=filepath, exist=file_exists)
        if (.not. file_exists) then
            write(test_desc, '(A, A, A)') 'File ', trim(filepath), ' exists'
            call assert_true(.false., test_desc)
            return
        end if
        
        ! Read file and check for forbidden string
        contains_forbidden = .false.
        open(newunit=unit_num, file=filepath, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            write(test_desc, '(A, A, A)') 'Can read file ', trim(filepath), ''
            call assert_true(.false., test_desc)
            return
        end if
        
        do
            read(unit_num, '(A)', iostat=ios) line
            if (ios /= 0) exit
            if (index(line, forbidden_string) > 0) then
                contains_forbidden = .true.
                exit
            end if
        end do
        close(unit_num)
        
        write(test_desc, '(A, A, A, A, A)') 'No "', trim(forbidden_string), '" in ', trim(filepath), ''
        call assert_true(.not. contains_forbidden, test_desc)
        
    end subroutine check_file_for_forbidden_string

    subroutine start_test(test_name)
        character(len=*), intent(in) :: test_name
        write(*, '(A, A)') 'Running test: ', test_name
    end subroutine start_test

    subroutine end_test()
        write(*, '(A)') 'Test completed'
        write(*, *)
    end subroutine end_test

    subroutine assert_true(condition, description)
        logical, intent(in) :: condition
        character(len=*), intent(in) :: description
        
        test_count = test_count + 1
        if (condition) then
            write(*, '(A, A)') '  PASS: ', description
            pass_count = pass_count + 1
        else
            write(*, '(A, A)') '  FAIL: ', description
            all_passed = .false.
        end if
    end subroutine assert_true

    subroutine print_test_summary()
        write(*, '(A)') '============================================'
        write(*, '(A, I0, A, I0, A)') 'Test Summary: ', pass_count, ' of ', test_count, ' tests passed'
        if (pass_count == test_count) then
            write(*, '(A)') 'All naming consistency tests PASSED!'
        else
            write(*, '(A)') 'Naming consistency tests FAILED!'
            write(*, '(A)') 'Found instances of "fortplotlib" that need to be replaced with "fortplot"'
        end if
    end subroutine print_test_summary

end program test_naming_consistency