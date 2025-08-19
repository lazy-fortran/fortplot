program test_documentation_content_integration
    !! Given: Documentation markdown files contain references to example outputs
    !! When: Documentation is processed and deployed
    !! Then: All image references must point to accessible example outputs
    !!
    !! This test validates that documentation content correctly integrates
    !! with generated example outputs for GitHub Pages deployment
    
    use iso_fortran_env, only: stdout => output_unit, stderr => error_unit
    implicit none

    integer :: test_count = 0
    integer :: failures = 0

    call test_markdown_image_references()
    call test_documentation_output_mapping()
    call test_relative_path_correctness()
    call test_example_output_completeness()
    call test_documentation_build_integration()

    call print_test_summary()

contains

    subroutine test_markdown_image_references()
        !! Given: Documentation markdown files reference example images
        !! When: Markdown is processed by FORD
        !! Then: Image references must be valid and point to existing files
        
        logical :: basic_plots_md_exists, line_styles_md_exists, marker_demo_md_exists
        
        test_count = test_count + 1
        write(stdout, '(A)') 'Testing markdown image references...'
        
        ! Verify key documentation files exist
        inquire(file='doc/example/basic_plots.md', exist=basic_plots_md_exists)
        inquire(file='doc/example/line_styles.md', exist=line_styles_md_exists)
        inquire(file='doc/example/marker_demo.md', exist=marker_demo_md_exists)
        
        if (.not. basic_plots_md_exists) then
            write(stderr, '(A)') 'FAIL: basic_plots.md documentation missing'
            failures = failures + 1
            return
        end if
        
        if (.not. line_styles_md_exists) then
            write(stderr, '(A)') 'FAIL: line_styles.md documentation missing'
            failures = failures + 1
            return
        end if
        
        if (.not. marker_demo_md_exists) then
            write(stderr, '(A)') 'FAIL: marker_demo.md documentation missing'
            failures = failures + 1
            return
        end if
        
        ! This test MUST FAIL until image reference validation is implemented
        write(stderr, '(A)') 'FAIL: Markdown image reference validation not implemented'
        failures = failures + 1
        
        write(stdout, '(A)') 'Markdown image references test completed'
    end subroutine test_markdown_image_references

    subroutine test_documentation_output_mapping()
        !! Given: Examples generate specific output files
        !! When: Documentation references these outputs
        !! Then: Every referenced output file must exist in the correct location
        
        logical :: basic_simple_png, basic_multi_png, line_styles_png
        logical :: marker_all_types_png, contour_gaussian_png
        
        test_count = test_count + 1
        write(stdout, '(A)') 'Testing documentation output mapping...'
        
        ! Check for specific example outputs that should be referenced in documentation
        inquire(file='output/example/fortran/basic_plots/simple_plot.png', exist=basic_simple_png)
        inquire(file='output/example/fortran/basic_plots/multi_line.png', exist=basic_multi_png)
        inquire(file='output/example/fortran/line_styles/line_styles.png', exist=line_styles_png)
        inquire(file='output/example/fortran/marker_demo/all_marker_types.png', exist=marker_all_types_png)
        inquire(file='output/example/fortran/contour_demo/contour_gaussian.png', exist=contour_gaussian_png)
        
        if (.not. basic_simple_png) then
            write(stderr, '(A)') 'FAIL: basic_plots simple_plot.png missing - run make example first'
            failures = failures + 1
            return
        end if
        
        if (.not. basic_multi_png) then
            write(stderr, '(A)') 'FAIL: basic_plots multi_line.png missing'
            failures = failures + 1
            return
        end if
        
        if (.not. line_styles_png) then
            write(stderr, '(A)') 'FAIL: line_styles.png missing'
            failures = failures + 1
            return
        end if
        
        if (.not. marker_all_types_png) then
            write(stderr, '(A)') 'FAIL: marker_demo all_marker_types.png missing'
            failures = failures + 1
            return
        end if
        
        if (.not. contour_gaussian_png) then
            write(stderr, '(A)') 'FAIL: contour_demo contour_gaussian.png missing'
            failures = failures + 1
            return
        end if
        
        ! This test MUST FAIL until output mapping verification is implemented
        write(stderr, '(A)') 'FAIL: Documentation output mapping verification not implemented'
        failures = failures + 1
        
        write(stdout, '(A)') 'Documentation output mapping test completed'
    end subroutine test_documentation_output_mapping

    subroutine test_relative_path_correctness()
        !! Given: Documentation uses relative paths to reference images
        !! When: FORD generates HTML documentation
        !! Then: Relative paths must be correct for GitHub Pages deployment
        
        test_count = test_count + 1
        write(stdout, '(A)') 'Testing relative path correctness...'
        
        ! This test MUST FAIL until relative path validation is implemented
        write(stderr, '(A)') 'FAIL: Relative path correctness validation not implemented'
        failures = failures + 1
        
        write(stdout, '(A)') 'Relative path correctness test completed'
    end subroutine test_relative_path_correctness

    subroutine test_example_output_completeness()
        !! Given: All examples should generate corresponding outputs
        !! When: Documentation is built
        !! Then: Every documented example must have generated output files
        
        logical :: all_examples_present
        
        test_count = test_count + 1
        write(stdout, '(A)') 'Testing example output completeness...'
        
        ! Check that all critical example categories have outputs
        all_examples_present = .true.
        
        ! Verify basic plotting examples
        inquire(file='output/example/fortran/basic_plots', exist=all_examples_present)
        if (.not. all_examples_present) then
            write(stderr, '(A)') 'FAIL: Basic plots outputs missing'
            failures = failures + 1
            return
        end if
        
        ! Verify line styles examples
        inquire(file='output/example/fortran/line_styles', exist=all_examples_present)
        if (.not. all_examples_present) then
            write(stderr, '(A)') 'FAIL: Line styles outputs missing'
            failures = failures + 1
            return
        end if
        
        ! Verify marker demo examples
        inquire(file='output/example/fortran/marker_demo', exist=all_examples_present)
        if (.not. all_examples_present) then
            write(stderr, '(A)') 'FAIL: Marker demo outputs missing'
            failures = failures + 1
            return
        end if
        
        ! This test MUST FAIL until completeness validation is implemented
        write(stderr, '(A)') 'FAIL: Example output completeness validation not implemented'
        failures = failures + 1
        
        write(stdout, '(A)') 'Example output completeness test completed'
    end subroutine test_example_output_completeness

    subroutine test_documentation_build_integration()
        !! Given: Documentation build process copies example outputs
        !! When: FORD generates documentation
        !! Then: Built documentation must contain accessible example images
        
        logical :: build_doc_exists, build_media_exists
        
        test_count = test_count + 1
        write(stdout, '(A)') 'Testing documentation build integration...'
        
        ! Check if documentation build directory exists
        inquire(file='build/doc', exist=build_doc_exists)
        if (.not. build_doc_exists) then
            write(stderr, '(A)') 'FAIL: Documentation build directory missing - run make doc first'
            failures = failures + 1
            return
        end if
        
        ! Check if media directory exists in build
        inquire(file='build/doc/media', exist=build_media_exists)
        if (.not. build_media_exists) then
            write(stderr, '(A)') 'FAIL: Built documentation media directory missing'
            failures = failures + 1
            return
        end if
        
        ! This test MUST FAIL until build integration is verified
        write(stderr, '(A)') 'FAIL: Documentation build integration verification not implemented'
        failures = failures + 1
        
        write(stdout, '(A)') 'Documentation build integration test completed'
    end subroutine test_documentation_build_integration

    subroutine print_test_summary()
        write(stdout, '(A)') ''
        write(stdout, '(A)') '=== Documentation Content Integration Test Summary ==='
        write(stdout, '(A,I0)') 'Total tests: ', test_count
        write(stdout, '(A,I0)') 'Failures: ', failures
        
        if (failures > 0) then
            write(stderr, '(A)') 'Documentation content integration tests FAILED'
            call exit(1)
        else
            write(stdout, '(A)') 'All documentation content integration tests PASSED'
        end if
    end subroutine print_test_summary

end program test_documentation_content_integration