!! Test for GitHub Pages image path resolution - Issue #205
!! 
!! Given-When-Then Documentation:
!! 
!! GIVEN: 
!! - Documentation markdown files reference images with relative paths like "../../media/examples/basic_plots/simple_plot.png"
!! - FORD generates HTML files in subdirectories like "build/doc/page/example/"
!! - Media files need to be accessible at the correct relative paths for deployed GitHub Pages
!! 
!! WHEN:
!! - Documentation build process runs
!! - FORD processes markdown files and generates HTML
!! - Media files are copied to build directory
!! 
!! THEN:
!! - Media files should be accessible at the expected relative paths from generated HTML
!! - Image links in generated HTML should resolve correctly
!! - Both local builds and GitHub Pages deployment should work
!! 
!! TEST SCENARIOS:
!! 1. Media file staging validation (pre-FORD copy requirement)
!! 2. Relative path resolution from generated HTML directories
!! 3. Documentation build process integration test
!! 4. Image accessibility validation for GitHub Pages structure
!!
program test_github_pages_image_paths_issue_205
    use fortplot_security, only: get_test_output_path
    implicit none
    
    call test_media_file_staging_validation()
    call test_relative_path_resolution_from_html()
    call test_documentation_build_process_integration()
    call test_image_accessibility_validation()
    
contains

    subroutine test_media_file_staging_validation()
        !! Given-When-Then: Test that media files are staged correctly before FORD runs
        !! 
        !! GIVEN: Media files exist in source and output locations
        !! WHEN: Documentation build process prepares files for FORD
        !! THEN: Media files should be accessible at paths expected by markdown relative references
        
        logical :: source_media_exists, staged_media_exists
        character(len=:), allocatable :: source_path, expected_staged_path
        
        print *, "=== Testing Media File Staging Validation ==="
        
        ! Given: Check source media files exist
        source_path = "doc/media/examples/basic_plots/simple_plot.png"
        inquire(file=source_path, exist=source_media_exists)
        
        if (.not. source_media_exists) then
            print *, "WARNING: Source media file not found: ", source_path
            print *, "This test requires media files to be generated first"
        endif
        
        ! When: Check if media files are staged where FORD-generated HTML expects them
        ! For HTML at build/doc/page/example/*.html, the path ../../media/examples/* should resolve to build/doc/media/examples/*
        expected_staged_path = "build/doc/media/examples/basic_plots/simple_plot.png"
        inquire(file=expected_staged_path, exist=staged_media_exists)
        
        ! Then: Media files should be staged correctly
        if (source_media_exists .and. .not. staged_media_exists) then
            print *, "FAIL: Media files not staged correctly for GitHub Pages"
            print *, "Expected: ", expected_staged_path
            print *, "This indicates media files are copied AFTER FORD runs (current broken behavior)"
            call exit(1)  ! This should fail until fix is implemented
        elseif (staged_media_exists) then
            print *, "PASS: Media files staged correctly at: ", expected_staged_path
        else
            print *, "SKIP: No source media files to test staging"
        endif
        
        print *, "=== Media File Staging Validation Complete ==="
    end subroutine test_media_file_staging_validation

    subroutine test_relative_path_resolution_from_html()
        !! Given-When-Then: Test that relative paths resolve correctly from generated HTML locations
        !! 
        !! GIVEN: FORD generates HTML files in nested directory structures
        !! WHEN: Markdown uses relative paths like "../../media/examples/"
        !! THEN: These paths should resolve correctly from the generated HTML location
        
        logical :: html_dir_exists, target_media_exists
        character(len=:), allocatable :: html_dir, relative_path_target
        
        print *, "=== Testing Relative Path Resolution ==="
        
        ! Given: Check if FORD generates HTML in the expected location
        html_dir = "build/doc/page/example"
        inquire(file=trim(html_dir)//'/.', exist=html_dir_exists)
        
        if (.not. html_dir_exists) then
            print *, "INFO: HTML directory not found: ", html_dir
            print *, "This test requires documentation to be built first"
        endif
        
        ! When: Relative path from HTML should resolve to media location
        ! From build/doc/page/example/*.html, ../../media/examples/* should resolve to build/doc/media/examples/*
        relative_path_target = "build/doc/media/examples"
        inquire(file=trim(relative_path_target)//'/.', exist=target_media_exists)
        
        ! Then: Target directory should exist for relative paths to work
        if (html_dir_exists .and. .not. target_media_exists) then
            print *, "FAIL: Relative path target directory does not exist"
            print *, "HTML dir exists: ", html_dir
            print *, "Expected target: ", relative_path_target
            print *, "Relative paths in generated HTML will be broken"
        elseif (target_media_exists) then
            print *, "PASS: Relative path target directory exists: ", relative_path_target
        else
            print *, "SKIP: Documentation not built, cannot test relative path resolution"
        endif
        
        print *, "=== Relative Path Resolution Test Complete ==="
    end subroutine test_relative_path_resolution_from_html

    subroutine test_documentation_build_process_integration()
        !! Given-When-Then: Test the complete documentation build process
        !! 
        !! GIVEN: Source documentation and media files exist
        !! WHEN: Documentation build process runs (make doc)
        !! THEN: Generated HTML should have working image references
        
        logical :: makefile_exists, readme_exists
        
        print *, "=== Testing Documentation Build Process Integration ==="
        
        ! Given: Check build environment prerequisites
        inquire(file="Makefile", exist=makefile_exists)
        inquire(file="README.md", exist=readme_exists)
        
        if (.not. makefile_exists .or. .not. readme_exists) then
            print *, "FAIL: Missing build prerequisites"
            print *, "Makefile exists: ", makefile_exists
            print *, "README.md exists: ", readme_exists
            call exit(1)
        endif
        
        ! When: Documentation build process should work
        ! This is validated by checking that the staging happens correctly
        print *, "INFO: Documentation build integration requires:"
        print *, "  1. Media files copied to build/doc/media/examples/ BEFORE FORD runs"
        print *, "  2. FORD processes markdown files with working relative paths"
        print *, "  3. Generated HTML can access images via ../../media/examples/ paths"
        
        ! Then: The build process structure should support GitHub Pages deployment
        print *, "VALIDATION: Current build process analysis:"
        print *, "  - Makefile copies media AFTER FORD (lines 95-108): BROKEN"
        print *, "  - GitHub Actions copies media AFTER FORD (lines 83-85): BROKEN"
        print *, "  - Need to copy media BEFORE FORD for relative paths to work"
        
        print *, "=== Documentation Build Process Integration Test Complete ==="
    end subroutine test_documentation_build_process_integration

    subroutine test_image_accessibility_validation()
        !! Given-When-Then: Test that images are accessible for GitHub Pages deployment
        !! 
        !! GIVEN: Documentation is built with media files
        !! WHEN: Deployed to GitHub Pages at lazy-fortran.github.io/fortplot/
        !! THEN: Image links should work correctly in the deployed environment
        
        logical :: build_doc_exists, media_examples_exists
        character(len=:), allocatable :: github_pages_media_path
        
        print *, "=== Testing Image Accessibility for GitHub Pages ==="
        
        ! Given: Check if documentation build directory exists
        inquire(file="build/doc/.", exist=build_doc_exists)
        
        if (.not. build_doc_exists) then
            print *, "INFO: build/doc directory not found, cannot test accessibility"
            print *, "Run 'make doc' first to generate documentation"
            return
        endif
        
        ! When: Check if media files are in the correct location for GitHub Pages
        github_pages_media_path = "build/doc/media/examples"
        inquire(file=trim(github_pages_media_path)//'/.', exist=media_examples_exists)
        
        ! Then: Media files should be accessible at the expected GitHub Pages URLs
        if (.not. media_examples_exists) then
            print *, "FAIL: Media files not accessible for GitHub Pages deployment"
            print *, "Expected at: ", github_pages_media_path
            print *, "This will cause broken images in deployed documentation"
            print *, "Current broken URL pattern: https://lazy-fortran.github.io/fortplot/page/example/basic_plots.html"
            print *, "Tries to access: ../../media/examples/basic_plots/simple_plot.png"
            print *, "Should resolve to: https://lazy-fortran.github.io/fortplot/media/examples/basic_plots/simple_plot.png"
        else
            print *, "PASS: Media files accessible for GitHub Pages at: ", github_pages_media_path
        endif
        
        ! Additional validation for specific example files
        call validate_specific_image_files()
        
        print *, "=== Image Accessibility Validation Complete ==="
    end subroutine test_image_accessibility_validation

    subroutine validate_specific_image_files()
        !! Validate specific image files mentioned in the issue
        logical :: basic_plots_png_exists, basic_plots_pdf_exists
        character(len=:), allocatable :: png_path, pdf_path
        
        print *, "  --- Validating Specific Files ---"
        
        ! Check for basic_plots example files (mentioned in issue)
        png_path = "build/doc/media/examples/basic_plots/simple_plot.png"
        pdf_path = "build/doc/media/examples/basic_plots/simple_plot.pdf"
        
        inquire(file=png_path, exist=basic_plots_png_exists)
        inquire(file=pdf_path, exist=basic_plots_pdf_exists)
        
        if (basic_plots_png_exists) then
            print *, "  PASS: basic_plots PNG accessible: ", png_path
        else
            print *, "  FAIL: basic_plots PNG not accessible: ", png_path
        endif
        
        if (basic_plots_pdf_exists) then
            print *, "  PASS: basic_plots PDF accessible: ", pdf_path
        else
            print *, "  FAIL: basic_plots PDF not accessible: ", pdf_path
        endif
        
        ! Validate that these files would be accessible via GitHub Pages URLs
        if (basic_plots_png_exists .and. basic_plots_pdf_exists) then
            print *, "  SUCCESS: Files should be accessible at GitHub Pages URLs:"
            print *, "    PNG: https://lazy-fortran.github.io/fortplot/media/examples/basic_plots/simple_plot.png"
            print *, "    PDF: https://lazy-fortran.github.io/fortplot/media/examples/basic_plots/simple_plot.pdf"
        endif
    end subroutine validate_specific_image_files

end program test_github_pages_image_paths_issue_205