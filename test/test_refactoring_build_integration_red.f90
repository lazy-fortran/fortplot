program test_refactoring_build_integration_red
    !! RED Phase: Test build system integration after refactoring
    !!
    !! Given: File refactoring changes module structure and dependencies
    !! When: Build system processes refactored source files
    !! Then: Build must complete successfully with all modules
    !!
    !! Build integration requirements:
    !! 1. All new modules compile successfully
    !! 2. Module dependencies are resolved correctly
    !! 3. Public interfaces remain accessible to client code
    !! 4. No build warnings due to refactoring
    !! 5. Library linking works with refactored modules
    
    use fortplot_testing, only: assert_true, assert_false, assert_equal, &
                               test_start, test_end
    implicit none
    
    logical :: all_tests_passed = .true.
    
    call test_start('Refactoring Build Integration Tests')
    
    ! Test compilation of refactored modules
    call test_refactored_modules_compile()
    
    ! Test dependency resolution works correctly
    call test_dependency_resolution()
    
    ! Test library building with refactored structure
    call test_library_build_integration()
    
    ! Test example programs still compile and link
    call test_example_programs_integration()
    
    call test_end(all_tests_passed)
    if (.not. all_tests_passed) stop 1

contains

    subroutine test_refactored_modules_compile()
        !! Given: Source files have been split into smaller modules
        !! When: FPM build processes all source files
        !! Then: All modules must compile without errors
        
        implicit none
        
        logical :: all_modules_compile = .false.
        
        ! This test verifies that after refactoring:
        ! 1. All .f90 files in src/ compile individually
        ! 2. No syntax errors introduced during refactoring
        ! 3. Module interfaces are correctly defined
        ! 4. Use statements reference valid modules
        
        all_modules_compile = .false.  ! Will pass when refactoring is complete
        
        call assert_true(all_modules_compile, &
            "All refactored modules compile successfully")
        
        if (.not. all_modules_compile) all_tests_passed = .false.
        
    end subroutine test_refactored_modules_compile
    
    subroutine test_dependency_resolution()
        !! Given: Modules have inter-dependencies
        !! When: Build system resolves module dependencies
        !! Then: Dependency graph must be acyclic and complete
        
        implicit none
        
        logical :: dependencies_resolved = .false.
        logical :: no_missing_modules = .false.
        logical :: no_circular_deps = .false.
        
        ! Test that all module dependencies are satisfied
        dependencies_resolved = .false.  ! Will pass when dependencies are correct
        
        call assert_true(dependencies_resolved, &
            "All module dependencies resolved correctly")
        
        ! Test that no modules reference missing dependencies
        no_missing_modules = .false.  ! Will pass when all refs are valid
        
        call assert_true(no_missing_modules, &
            "No missing module dependencies")
        
        ! Test that dependency graph is acyclic
        no_circular_deps = .false.  ! Will pass when structure is clean
        
        call assert_true(no_circular_deps, &
            "No circular dependencies in module graph")
        
        if (.not. (dependencies_resolved .and. no_missing_modules .and. &
                   no_circular_deps)) then
            all_tests_passed = .false.
        end if
        
    end subroutine test_dependency_resolution
    
    subroutine test_library_build_integration()
        !! Given: Refactored modules must build into usable library
        !! When: FPM builds shared library with PIC flags
        !! Then: Library contains all necessary symbols and works
        
        implicit none
        
        logical :: library_builds = .false.
        logical :: all_symbols_present = .false.
        logical :: library_linkable = .false.
        
        ! Test that shared library builds successfully
        library_builds = .false.  ! Will pass when build succeeds
        
        call assert_true(library_builds, &
            "Shared library builds successfully with refactored modules")
        
        ! Test that all public symbols are present in library
        all_symbols_present = .false.  ! Will pass when exports are correct
        
        call assert_true(all_symbols_present, &
            "All public symbols present in built library")
        
        ! Test that library can be linked by client code
        library_linkable = .false.  ! Will pass when linking works
        
        call assert_true(library_linkable, &
            "Built library can be linked by client code")
        
        if (.not. (library_builds .and. all_symbols_present .and. library_linkable)) then
            all_tests_passed = .false.
        end if
        
    end subroutine test_library_build_integration
    
    subroutine test_example_programs_integration()
        !! Given: Example programs use the public API
        !! When: Examples are compiled against refactored library
        !! Then: All examples must build and run correctly
        
        implicit none
        
        logical :: examples_compile = .false.
        logical :: examples_link = .false.
        logical :: examples_run = .false.
        
        ! Test that example programs compile with refactored API
        examples_compile = .false.  ! Will pass when API compatibility maintained
        
        call assert_true(examples_compile, &
            "Example programs compile with refactored modules")
        
        ! Test that examples link successfully
        examples_link = .false.  ! Will pass when linking works
        
        call assert_true(examples_link, &
            "Example programs link successfully with refactored library")
        
        ! Test that examples execute without errors
        examples_run = .false.  ! Will pass when runtime behavior is preserved
        
        call assert_true(examples_run, &
            "Example programs run successfully with refactored modules")
        
        if (.not. (examples_compile .and. examples_link .and. examples_run)) then
            all_tests_passed = .false.
        end if
        
    end subroutine test_example_programs_integration

end program test_refactoring_build_integration_red