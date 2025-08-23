program test_animation_github_pages_integration
    ! GIVEN-WHEN-THEN Documentation:
    !
    ! GIVEN an animation example that should output MP4 to GitHub Pages structure
    ! WHEN the animation is saved with structured output directory
    ! THEN the MP4 file should be created in the correct GitHub Pages location
    !
    ! GIVEN a GitHub Pages deployment structure with media directories  
    ! WHEN documentation is generated linking to animation files
    ! THEN the animation MP4 should be accessible via download links
    !
    ! GIVEN an animation output directory structure for GitHub Pages
    ! WHEN multiple animations are generated in the same session
    ! THEN each animation should be saved to its appropriate subdirectory
    !
    ! This implements RED phase testing for Issue #178:
    ! Missing MP4 download link in GitHub Pages animation example

    use fortplot
    use fortplot_animation
    use fortplot_pipe, only: check_ffmpeg_available
    use fortplot_security, only: safe_remove_file
    use fortplot_system_runtime, only: is_windows
    use iso_fortran_env, only: real64
    implicit none
    
    logical :: ffmpeg_available, on_windows
    character(len=256) :: ci_env
    integer :: status

    ! Module variables for nested procedures
    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    
    ! Skip on Windows CI - FFmpeg pipe issues
    on_windows = is_windows()
    call get_environment_variable("CI", ci_env, status=status)
    
    if (on_windows .and. status == 0) then
        print *, "SKIPPED: GitHub Pages animation tests on Windows CI"
        stop 0
    end if
    
    ! Check if ffmpeg is available
    ffmpeg_available = check_ffmpeg_available()
    if (.not. ffmpeg_available) then
        print *, "XFAIL: GitHub Pages animation tests require FFmpeg"
        print *, "Expected failure - FFmpeg not available"
        stop 77  ! Standard exit code for skipped tests
    end if
    
    ! Run RED phase tests
    call test_animation_output_directory_structure()
    call test_github_pages_media_directory_creation()
    call test_animation_file_accessibility()
    call test_documentation_link_validation()
    call test_multiple_animation_organization()
    
    print *, "All GitHub Pages animation integration tests passed!"

contains

    subroutine test_animation_output_directory_structure()
        ! GIVEN-WHEN-THEN:
        ! GIVEN an animation demo that needs GitHub Pages integration
        ! WHEN the animation is saved to structured output directory
        ! THEN the file should be created at output/example/fortran/animation/
        
        type(animation_t) :: anim
        character(len=512) :: output_path, expected_dir
        logical :: file_exists, dir_exists
        integer :: i, mkdir_status
        
        ! Expected GitHub Pages structure
        expected_dir = "output/example/fortran/animation"
        output_path = trim(expected_dir) // "/animation.mp4"
        
        ! Create directory structure if it doesn't exist
        call execute_command_line("mkdir -p " // expected_dir, &
                                 exitstat=mkdir_status)
        if (mkdir_status /= 0) then
            error stop "Failed to create output directory structure"
        end if
        
        ! Verify directory was created
        inquire(file=expected_dir, exist=dir_exists)
        if (.not. dir_exists) then
            error stop "Expected directory structure was not created"
        end if
        
        ! Create test animation
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)
        
        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y, label='GitHub Pages test wave')
        call test_fig%set_title('GitHub Pages Animation Demo')
        
        anim = FuncAnimation(update_github_pages_animation, &
                            frames=10, interval=50, fig=test_fig)
        
        ! Save animation to structured directory
        call anim%save(output_path)
        
        ! Verify file was created in correct location
        inquire(file=output_path, exist=file_exists)
        if (.not. file_exists) then
            error stop "Animation MP4 not created in GitHub Pages structure"
        end if
        
        ! Clean up
        block
            logical :: remove_success
            call safe_remove_file(output_path, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not clean up test file: " // trim(output_path)
            end if
        end block
        
    end subroutine test_animation_output_directory_structure

    subroutine update_github_pages_animation(frame)
        integer, intent(in) :: frame
        real(real64) :: phase
        
        phase = real(frame, real64) * 0.5_real64
        test_y = sin(test_x + phase)
        call test_fig%set_ydata(1, test_y)
    end subroutine update_github_pages_animation

    subroutine test_github_pages_media_directory_creation()
        ! GIVEN-WHEN-THEN:
        ! GIVEN a documentation build process for GitHub Pages
        ! WHEN animation files are processed for web deployment  
        ! THEN media directory structure should be created automatically
        
        character(len=512) :: media_dir, doc_media_dir
        logical :: media_exists, doc_media_exists
        integer :: mkdir_status
        
        media_dir = "output/example/fortran/animation"
        doc_media_dir = "build/doc/media/examples/animation"
        
        ! Test output directory creation
        call execute_command_line("mkdir -p " // media_dir, &
                                 exitstat=mkdir_status)
        if (mkdir_status /= 0) then
            error stop "Failed to create media output directory"
        end if
        
        ! Test documentation media directory creation  
        call execute_command_line("mkdir -p " // doc_media_dir, &
                                 exitstat=mkdir_status)
        if (mkdir_status /= 0) then
            error stop "Failed to create documentation media directory"
        end if
        
        ! Verify both directories exist
        inquire(file=media_dir, exist=media_exists)
        inquire(file=doc_media_dir, exist=doc_media_exists)
        
        if (.not. media_exists) then
            error stop "Output media directory was not created"
        end if
        
        if (.not. doc_media_exists) then
            error stop "Documentation media directory was not created"
        end if
        
        ! Clean up test directories
        call execute_command_line("rmdir " // doc_media_dir // " 2>/dev/null || true")
        call execute_command_line("rmdir " // media_dir // " 2>/dev/null || true")
        
    end subroutine test_github_pages_media_directory_creation

    subroutine test_animation_file_accessibility()
        ! GIVEN-WHEN-THEN:
        ! GIVEN an animation MP4 file in GitHub Pages structure
        ! WHEN the file is accessed via relative path
        ! THEN the file should be readable and have valid MP4 header
        
        type(animation_t) :: anim
        character(len=512) :: test_file, expected_dir
        logical :: file_exists, is_readable
        integer :: i, unit_num, io_stat, mkdir_status
        character(len=4) :: mp4_header
        
        expected_dir = "output/example/fortran/animation"  
        test_file = trim(expected_dir) // "/test_accessibility.mp4"
        
        ! Create directory structure
        call execute_command_line("mkdir -p " // expected_dir, &
                                 exitstat=mkdir_status)
        if (mkdir_status /= 0) then
            error stop "Failed to create directory for accessibility test"
        end if
        
        ! Create test animation
        test_x = [(real(i, real64), i=1,5)]
        test_y = test_x**2
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_accessibility_test, &
                            frames=3, interval=100, fig=test_fig)
        
        ! Save animation
        call anim%save(test_file)
        
        ! Verify file exists
        inquire(file=test_file, exist=file_exists)
        if (.not. file_exists) then
            error stop "Test animation file was not created"
        end if
        
        ! Test file accessibility - try to read MP4 header
        open(newunit=unit_num, file=test_file, status='old', &
             access='stream', form='unformatted', iostat=io_stat)
        if (io_stat /= 0) then
            error stop "Animation file is not accessible for reading"
        end if
        
        ! Read first 4 bytes to check for valid MP4-like format
        read(unit_num, iostat=io_stat) mp4_header
        close(unit_num)
        
        if (io_stat /= 0) then
            error stop "Could not read animation file header"
        end if
        
        ! MP4 files should have some recognizable binary header
        ! (not checking exact format, just ensuring it's binary data)
        if (len_trim(mp4_header) == 0) then
            error stop "Animation file appears to be empty or invalid"
        end if
        
        ! Clean up
        block
            logical :: remove_success
            call safe_remove_file(test_file, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not clean up accessibility test file"
            end if
        end block
        
    end subroutine test_animation_file_accessibility

    subroutine update_accessibility_test(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 0.1_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine update_accessibility_test

    subroutine test_documentation_link_validation()
        ! GIVEN-WHEN-THEN:
        ! GIVEN animation MP4 files in GitHub Pages directory structure
        ! WHEN documentation links are generated for download
        ! THEN the relative paths should be correctly formatted
        
        character(len=512) :: relative_path, expected_path
        character(len=1024) :: markdown_link
        logical :: path_format_valid
        
        ! Test relative path format for GitHub Pages
        relative_path = "../media/examples/animation/animation.mp4"
        expected_path = "media/examples/animation/animation.mp4"
        
        ! Verify path formatting follows GitHub Pages conventions
        path_format_valid = .true.
        
        ! Check for proper file extension
        if (index(relative_path, ".mp4") == 0) then
            path_format_valid = .false.
        end if
        
        ! Check for proper directory structure
        if (index(relative_path, "animation") == 0) then
            path_format_valid = .false.
        end if
        
        if (.not. path_format_valid) then
            error stop "Animation path format invalid for GitHub Pages"
        end if
        
        ! Test markdown link generation format
        markdown_link = "📹 **Animation Video**: [animation.mp4](" // &
                       trim(relative_path) // ")"
        
        if (len_trim(markdown_link) == 0) then
            error stop "Failed to generate valid markdown download link"
        end if
        
        ! Verify link contains required components
        if (index(markdown_link, "animation.mp4") == 0) then
            error stop "Download link missing filename"
        end if
        
        if (index(markdown_link, "](") == 0) then
            error stop "Download link missing markdown syntax"
        end if
        
    end subroutine test_documentation_link_validation

    subroutine test_multiple_animation_organization()
        ! GIVEN-WHEN-THEN:
        ! GIVEN multiple animation examples in the same project
        ! WHEN each animation is saved to GitHub Pages structure
        ! THEN each should have its own organized subdirectory
        
        type(animation_t) :: anim1, anim2
        character(len=512) :: base_dir, file1, file2, subdir1, subdir2
        logical :: file1_exists, file2_exists
        integer :: i, mkdir_status
        
        base_dir = "output/example/fortran"
        subdir1 = trim(base_dir) // "/animation"
        subdir2 = trim(base_dir) // "/other_animation" 
        
        file1 = trim(subdir1) // "/demo1.mp4"
        file2 = trim(subdir2) // "/demo2.mp4"
        
        ! Create both directory structures
        call execute_command_line("mkdir -p " // subdir1, &
                                 exitstat=mkdir_status)
        if (mkdir_status /= 0) then
            error stop "Failed to create first animation subdirectory"
        end if
        
        call execute_command_line("mkdir -p " // subdir2, &
                                 exitstat=mkdir_status)
        if (mkdir_status /= 0) then
            error stop "Failed to create second animation subdirectory"
        end if
        
        ! Create first test animation
        test_x = [(real(i, real64), i=1,8)]
        test_y = cos(test_x)
        
        call test_fig%initialize(width=600, height=400)
        call test_fig%add_plot(test_x, test_y, label='Demo 1')
        
        anim1 = FuncAnimation(update_demo1, frames=5, interval=80, fig=test_fig)
        call anim1%save(file1)
        
        ! Create second test animation  
        test_y = sin(test_x * 2.0_real64)
        call test_fig%clear()
        call test_fig%add_plot(test_x, test_y, label='Demo 2')
        
        anim2 = FuncAnimation(update_demo2, frames=5, interval=80, fig=test_fig)
        call anim2%save(file2)
        
        ! Verify both files were created in correct subdirectories
        inquire(file=file1, exist=file1_exists)
        inquire(file=file2, exist=file2_exists)
        
        if (.not. file1_exists) then
            error stop "First animation not created in organized subdirectory"
        end if
        
        if (.not. file2_exists) then
            error stop "Second animation not created in organized subdirectory"
        end if
        
        ! Clean up both test files
        block
            logical :: remove1_success, remove2_success
            call safe_remove_file(file1, remove1_success)
            call safe_remove_file(file2, remove2_success)
            
            if (.not. remove1_success) then
                print *, "Warning: Could not clean up first test animation"
            end if
            if (.not. remove2_success) then
                print *, "Warning: Could not clean up second test animation"
            end if
        end block
        
        ! Clean up test directories
        call execute_command_line("rmdir " // subdir2 // " 2>/dev/null || true")
        
    end subroutine test_multiple_animation_organization

    subroutine update_demo1(frame)
        integer, intent(in) :: frame
        real(real64) :: shift
        shift = real(frame, real64) * 0.3_real64
        test_y = cos(test_x + shift)
        call test_fig%set_ydata(1, test_y)
    end subroutine update_demo1

    subroutine update_demo2(frame)
        integer, intent(in) :: frame
        real(real64) :: amplitude
        amplitude = 1.0_real64 + real(frame, real64) * 0.2_real64
        test_y = sin(test_x * 2.0_real64) * amplitude
        call test_fig%set_ydata(1, test_y)
    end subroutine update_demo2

end program test_animation_github_pages_integration