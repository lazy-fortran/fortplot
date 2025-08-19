program test_github_pages_integration
    !! Given: GitHub Pages deployment CI workflow and documentation structure
    !! When: Documentation is built and deployed to GitHub Pages
    !! Then: Example outputs must be visible and accessible on the deployed site
    !!
    !! This test validates end-to-end GitHub Pages deployment functionality
    !! to ensure example plots are visible on https://lazy-fortran.github.io/fortplot/
    
    use iso_fortran_env, only: stdout => output_unit, stderr => error_unit
    implicit none

    integer :: test_count = 0
    integer :: failures = 0

    call test_github_pages_deployment_pipeline()
    call test_documentation_artifact_presence()
    call test_example_output_integration()
    call test_ford_media_directory_configuration()
    call test_ci_workflow_artifact_generation()

    call print_test_summary()

contains

    subroutine test_github_pages_deployment_pipeline()
        !! Given: GitHub Pages CI workflow exists and documentation builds
        !! When: CI completes successfully
        !! Then: Documentation must contain accessible example images
        
        logical :: ci_config_exists, doc_config_exists, media_dir_exists
        
        test_count = test_count + 1
        write(stdout, '(A)') 'Testing GitHub Pages deployment pipeline...'
        
        ! Verify CI configuration exists
        inquire(file='.github/workflows/docs.yml', exist=ci_config_exists)
        if (.not. ci_config_exists) then
            write(stderr, '(A)') 'FAIL: GitHub Pages CI workflow configuration missing'
            failures = failures + 1
            return
        end if
        
        ! Verify FORD documentation configuration
        inquire(file='doc.md', exist=doc_config_exists)
        if (.not. doc_config_exists) then
            write(stderr, '(A)') 'FAIL: FORD documentation configuration missing'
            failures = failures + 1
            return
        end if
        
        ! Verify media directory structure exists
        inquire(file='doc/media', exist=media_dir_exists)
        if (.not. media_dir_exists) then
            write(stderr, '(A)') 'FAIL: Documentation media directory missing'
            failures = failures + 1
            return
        end if
        
        ! This test MUST FAIL until GitHub Pages deployment is fixed
        write(stderr, '(A)') 'FAIL: GitHub Pages deployment verification not implemented'
        failures = failures + 1
        
        write(stdout, '(A)') 'GitHub Pages deployment pipeline test completed'
    end subroutine test_github_pages_deployment_pipeline

    subroutine test_documentation_artifact_presence()
        !! Given: Examples generate output files in output/example/fortran/
        !! When: CI copies files to doc/media/examples/
        !! Then: All generated examples must be present in documentation artifacts
        
        logical :: output_dir_exists, basic_plots_exist
        
        test_count = test_count + 1
        write(stdout, '(A)') 'Testing documentation artifact presence...'
        
        ! Verify example output directory exists
        inquire(file='output/example/fortran/basic_plots', exist=output_dir_exists)
        if (.not. output_dir_exists) then
            write(stderr, '(A)') 'FAIL: Example output directory missing - run make example first'
            failures = failures + 1
            return
        end if
        
        ! Check for specific example files
        inquire(file='output/example/fortran/basic_plots/simple_plot.png', exist=basic_plots_exist)
        if (.not. basic_plots_exist) then
            write(stderr, '(A)') 'FAIL: Basic plots example output missing'
            failures = failures + 1
            return
        end if
        
        ! This test MUST FAIL until artifact copying is verified
        write(stderr, '(A)') 'FAIL: Documentation artifact verification not implemented'
        failures = failures + 1
        
        write(stdout, '(A)') 'Documentation artifact presence test completed'
    end subroutine test_documentation_artifact_presence

    subroutine test_example_output_integration()
        !! Given: Documentation markdown files reference example outputs
        !! When: FORD processes documentation
        !! Then: Generated HTML must contain valid image links to example outputs
        
        logical :: basic_plots_md_exists, build_doc_exists
        
        test_count = test_count + 1
        write(stdout, '(A)') 'Testing example output integration...'
        
        ! Verify example documentation exists
        inquire(file='doc/example/basic_plots.md', exist=basic_plots_md_exists)
        if (.not. basic_plots_md_exists) then
            write(stderr, '(A)') 'FAIL: Basic plots documentation missing'
            failures = failures + 1
            return
        end if
        
        ! Check if documentation build directory exists
        inquire(file='build/doc', exist=build_doc_exists)
        if (.not. build_doc_exists) then
            write(stderr, '(A)') 'FAIL: Documentation build directory missing - run make doc first'
            failures = failures + 1
            return
        end if
        
        ! This test MUST FAIL until image integration is verified
        write(stderr, '(A)') 'FAIL: Example output integration verification not implemented'
        failures = failures + 1
        
        write(stdout, '(A)') 'Example output integration test completed'
    end subroutine test_example_output_integration

    subroutine test_ford_media_directory_configuration()
        !! Given: FORD documentation generator configuration
        !! When: FORD processes documentation with media files
        !! Then: Media files must be accessible in generated documentation
        
        logical :: ford_config_exists
        
        test_count = test_count + 1
        write(stdout, '(A)') 'Testing FORD media directory configuration...'
        
        ! Verify FORD configuration file
        inquire(file='doc.md', exist=ford_config_exists)
        if (.not. ford_config_exists) then
            write(stderr, '(A)') 'FAIL: FORD configuration file missing'
            failures = failures + 1
            return
        end if
        
        ! This test MUST FAIL until FORD media configuration is verified
        write(stderr, '(A)') 'FAIL: FORD media directory configuration not verified'
        failures = failures + 1
        
        write(stdout, '(A)') 'FORD media directory configuration test completed'
    end subroutine test_ford_media_directory_configuration

    subroutine test_ci_workflow_artifact_generation()
        !! Given: GitHub Actions CI workflow
        !! When: Workflow runs example generation and documentation build
        !! Then: Artifacts must contain all example outputs in correct locations
        
        logical :: workflow_exists
        
        test_count = test_count + 1
        write(stdout, '(A)') 'Testing CI workflow artifact generation...'
        
        ! Verify GitHub Actions workflow exists
        inquire(file='.github/workflows/docs.yml', exist=workflow_exists)
        if (.not. workflow_exists) then
            write(stderr, '(A)') 'FAIL: GitHub Actions workflow missing'
            failures = failures + 1
            return
        end if
        
        ! This test MUST FAIL until CI artifact generation is verified
        write(stderr, '(A)') 'FAIL: CI workflow artifact generation not verified'
        failures = failures + 1
        
        write(stdout, '(A)') 'CI workflow artifact generation test completed'
    end subroutine test_ci_workflow_artifact_generation

    subroutine print_test_summary()
        write(stdout, '(A)') ''
        write(stdout, '(A)') '=== GitHub Pages Integration Test Summary ==='
        write(stdout, '(A,I0)') 'Total tests: ', test_count
        write(stdout, '(A,I0)') 'Failures: ', failures
        
        if (failures > 0) then
            write(stderr, '(A)') 'GitHub Pages deployment tests FAILED - deployment regression confirmed'
            call exit(1)
        else
            write(stdout, '(A)') 'All GitHub Pages deployment tests PASSED'
        end if
    end subroutine print_test_summary

end program test_github_pages_integration