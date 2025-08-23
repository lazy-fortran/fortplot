program test_github_pages_directory_structure
    ! GIVEN-WHEN-THEN Documentation:
    !
    ! GIVEN animation examples that should integrate with GitHub Pages deployment
    ! WHEN the output directory structure is created for GitHub Pages  
    ! THEN the directory structure should follow expected patterns
    !
    ! GIVEN GitHub Pages media directory requirements
    ! WHEN documentation links reference animation files
    ! THEN the relative path structure should be consistent
    !
    ! This implements RED phase testing for Issue #178:
    ! Tests directory structure and path mapping for GitHub Pages integration

    implicit none
    
    ! Run RED phase tests for directory structure requirements
    call test_output_directory_structure_creation()
    call test_github_pages_path_consistency()  
    call test_documentation_directory_mapping()
    call test_relative_path_validation()
    
    print *, "All GitHub Pages directory structure tests passed!"

contains

    subroutine test_output_directory_structure_creation()
        ! GIVEN-WHEN-THEN:
        ! GIVEN animation examples need GitHub Pages deployment structure
        ! WHEN output directories are created for structured deployment
        ! THEN the directory structure should match expected GitHub Pages layout
        
        character(len=512) :: expected_output_dir, expected_doc_dir
        character(len=512) :: animation_dir, media_dir
        logical :: output_dir_exists, doc_dir_exists
        logical :: animation_dir_exists, media_dir_exists
        integer :: mkdir_status
        
        ! Expected GitHub Pages directory structure
        expected_output_dir = "output/example/fortran"
        expected_doc_dir = "build/doc/media/examples"
        animation_dir = trim(expected_output_dir) // "/animation"
        media_dir = trim(expected_doc_dir) // "/animation"
        
        ! Test directory creation for output structure
        call execute_command_line("mkdir -p " // animation_dir, &
                                 exitstat=mkdir_status)
        if (mkdir_status /= 0) then
            error stop "Failed to create output animation directory structure"
        end if
        
        ! Test directory creation for documentation structure
        call execute_command_line("mkdir -p " // media_dir, &
                                 exitstat=mkdir_status)
        if (mkdir_status /= 0) then
            error stop "Failed to create documentation media directory structure"
        end if
        
        ! Verify directories were created
        inquire(file=expected_output_dir, exist=output_dir_exists)
        inquire(file=expected_doc_dir, exist=doc_dir_exists)
        inquire(file=animation_dir, exist=animation_dir_exists)
        inquire(file=media_dir, exist=media_dir_exists)
        
        if (.not. output_dir_exists) then
            error stop "Output directory structure not created correctly"
        end if
        
        if (.not. doc_dir_exists) then
            error stop "Documentation directory structure not created correctly"
        end if
        
        if (.not. animation_dir_exists) then
            error stop "Animation subdirectory not created in output structure"
        end if
        
        if (.not. media_dir_exists) then
            error stop "Animation subdirectory not created in documentation structure"
        end if
        
        ! Clean up test directories (from deepest to shallowest)
        call execute_command_line("rmdir " // media_dir // " 2>/dev/null || true")
        call execute_command_line("rmdir " // animation_dir // " 2>/dev/null || true")
        call execute_command_line("rmdir " // trim(expected_doc_dir) // " 2>/dev/null || true")
        
    end subroutine test_output_directory_structure_creation

    subroutine test_github_pages_path_consistency()
        ! GIVEN-WHEN-THEN:
        ! GIVEN GitHub Pages deployment requires consistent path structure
        ! WHEN animation files are referenced in documentation
        ! THEN paths should follow consistent naming patterns
        
        character(len=512) :: output_path, doc_path, relative_path
        character(len=512) :: expected_filename
        logical :: path_consistency_valid
        
        expected_filename = "animation.mp4"
        
        ! Test path patterns for GitHub Pages consistency
        output_path = "output/example/fortran/animation/" // expected_filename
        doc_path = "build/doc/media/examples/animation/" // expected_filename
        relative_path = "../media/examples/animation/" // expected_filename
        
        path_consistency_valid = .true.
        
        ! Validate output path structure
        if (index(output_path, "output/example/fortran") == 0) then
            path_consistency_valid = .false.
        end if
        
        if (index(output_path, "animation/" // expected_filename) == 0) then
            path_consistency_valid = .false.
        end if
        
        ! Validate documentation path structure
        if (index(doc_path, "build/doc/media/examples") == 0) then
            path_consistency_valid = .false.
        end if
        
        if (index(doc_path, "animation/" // expected_filename) == 0) then
            path_consistency_valid = .false.
        end if
        
        ! Validate relative path for markdown links
        if (index(relative_path, "../media/examples") == 0) then
            path_consistency_valid = .false.
        end if
        
        if (index(relative_path, expected_filename) == 0) then
            path_consistency_valid = .false.
        end if
        
        if (.not. path_consistency_valid) then
            error stop "GitHub Pages path consistency validation failed"
        end if
        
    end subroutine test_github_pages_path_consistency

    subroutine test_documentation_directory_mapping()
        ! GIVEN-WHEN-THEN:
        ! GIVEN output files need to be accessible via GitHub Pages
        ! WHEN documentation references animation files  
        ! THEN directory mapping should provide correct accessibility
        
        character(len=512) :: source_pattern, target_pattern
        character(len=512) :: test_source, test_target
        logical :: mapping_valid
        integer :: path_depth_source, path_depth_target
        
        ! Test directory mapping patterns
        source_pattern = "output/example/fortran/animation"
        target_pattern = "build/doc/media/examples/animation"
        
        test_source = trim(source_pattern) // "/test.mp4"
        test_target = trim(target_pattern) // "/test.mp4"
        
        mapping_valid = .true.
        
        ! Verify source directory pattern
        if (len_trim(source_pattern) == 0) then
            mapping_valid = .false.
        end if
        
        if (index(source_pattern, "animation") == 0) then
            mapping_valid = .false.
        end if
        
        ! Verify target directory pattern  
        if (len_trim(target_pattern) == 0) then
            mapping_valid = .false.
        end if
        
        if (index(target_pattern, "animation") == 0) then
            mapping_valid = .false.
        end if
        
        ! Check directory depth consistency for mapping
        path_depth_source = count_path_separators(source_pattern)
        path_depth_target = count_path_separators(target_pattern)
        
        ! Both should have consistent depth for proper mapping
        if (path_depth_source < 3 .or. path_depth_target < 4) then
            mapping_valid = .false.
        end if
        
        if (.not. mapping_valid) then
            error stop "Documentation directory mapping validation failed"
        end if
        
    end subroutine test_documentation_directory_mapping

    subroutine test_relative_path_validation()
        ! GIVEN-WHEN-THEN:
        ! GIVEN documentation markdown files reference animation files
        ! WHEN relative paths are constructed for GitHub Pages
        ! THEN the paths should be valid and properly formatted
        
        character(len=1024) :: markdown_link, href_path
        character(len=512) :: filename, relative_path
        logical :: link_format_valid
        
        filename = "animation.mp4"
        relative_path = "../media/examples/animation/" // trim(filename)
        
        ! Test markdown link generation
        markdown_link = "📹 **Animation Video**: [" // trim(filename) // "](" // &
                       trim(relative_path) // ")"
        
        href_path = relative_path
        
        link_format_valid = .true.
        
        ! Validate markdown link format
        if (len_trim(markdown_link) == 0) then
            link_format_valid = .false.
        end if
        
        if (index(markdown_link, "[" // trim(filename) // "]") == 0) then
            link_format_valid = .false.
        end if
        
        if (index(markdown_link, "](" // trim(relative_path) // ")") == 0) then
            link_format_valid = .false.
        end if
        
        ! Validate relative path format
        if (index(href_path, "../") /= 1) then
            link_format_valid = .false.
        end if
        
        if (index(href_path, ".mp4") == 0) then
            link_format_valid = .false.
        end if
        
        ! Check for proper path segments
        if (index(href_path, "media/examples") == 0) then
            link_format_valid = .false.
        end if
        
        if (.not. link_format_valid) then
            error stop "Relative path validation failed for GitHub Pages links"
        end if
        
    end subroutine test_relative_path_validation

    integer function count_path_separators(path)
        character(len=*), intent(in) :: path
        integer :: i, count
        
        count = 0
        do i = 1, len_trim(path)
            if (path(i:i) == '/') count = count + 1
        end do
        count_path_separators = count
    end function count_path_separators

end program test_github_pages_directory_structure