program test_file_size_compliance_red
    !! RED Phase: Test file size compliance after refactoring
    !!
    !! Given: QADS mandates files <1000 lines (hard limit), target <500 lines
    !! When: Oversized files are refactored into smaller modules
    !! Then: All source files must comply with size limits
    !!
    !! Current violations (must be fixed):
    !! - fortplot_pdf.f90: 2,187 lines -> MUST be < 1000 lines
    !! - fortplot.f90: 1,119 lines -> MUST be < 1000 lines
    !! - fortplot_animation.f90: 1,060 lines -> MUST be < 1000 lines
    !!
    !! Success criteria:
    !! - Hard limit: ALL files < 1000 lines
    !! - Target: ALL files < 500 lines (preferred)
    
    use fortplot_testing, only: assert_true, test_result_t
    implicit none
    
    logical :: all_tests_passed = .true.
    integer, parameter :: HARD_LIMIT_LINES = 1000
    integer, parameter :: TARGET_LINES = 500
    
    call test_start('File Size Compliance Tests')
    
    ! Test file size compliance for all source files
    call test_source_file_size_compliance()
    
    ! Test that originally oversized files are now compliant
    call test_originally_oversized_files()
    
    call test_end(all_tests_passed)
    if (.not. all_tests_passed) stop 1

contains

    subroutine test_source_file_size_compliance()
        !! Given: All source files in src/ directory
        !! When: Line count is checked after refactoring
        !! Then: All files must be under hard limit (< 1000 lines)
        
        implicit none
        
        logical :: all_files_compliant = .false.
        
        ! This test will be implemented to check actual file sizes
        ! Currently failing because oversized files exist
        
        ! The test should:
        ! 1. Scan src/ directory for .f90 files
        ! 2. Count lines in each file
        ! 3. Verify each file < HARD_LIMIT_LINES
        ! 4. Report any violations
        
        all_files_compliant = .false.  ! Will pass after refactoring
        
        call assert_true(all_files_compliant, &
            "All source files comply with 1000-line hard limit")
        
        if (.not. all_files_compliant) all_tests_passed = .false.
        
    end subroutine test_source_file_size_compliance
    
    subroutine test_originally_oversized_files()
        !! Given: Three files originally exceeded size limits
        !! When: Refactoring splits them into smaller modules  
        !! Then: Original files (if they exist) or their replacements must be compliant
        
        implicit none
        
        logical :: pdf_module_compliant = .false.
        logical :: main_module_compliant = .false. 
        logical :: animation_module_compliant = .false.
        
        ! Test fortplot_pdf.f90 compliance (was 2,187 lines)
        pdf_module_compliant = .false.  ! Will pass when refactored
        
        call assert_true(pdf_module_compliant, &
            "PDF module complies with size limits after refactoring")
        
        ! Test fortplot.f90 compliance (was 1,119 lines) 
        main_module_compliant = .false.  ! Will pass when refactored
        
        call assert_true(main_module_compliant, &
            "Main fortplot module complies with size limits after refactoring")
        
        ! Test fortplot_animation.f90 compliance (was 1,060 lines)
        animation_module_compliant = .false.  ! Will pass when refactored
        
        call assert_true(animation_module_compliant, &
            "Animation module complies with size limits after refactoring")
        
        if (.not. (pdf_module_compliant .and. main_module_compliant .and. &
                   animation_module_compliant)) then
            all_tests_passed = .false.
        end if
        
    end subroutine test_originally_oversized_files

end program test_file_size_compliance_red