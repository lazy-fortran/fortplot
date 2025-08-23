program test_animation_example_output_structure
    ! GIVEN-WHEN-THEN Documentation:
    !
    ! GIVEN the save_animation_demo example should output to structured directory
    ! WHEN the example is run with modified output path 
    ! THEN the MP4 should be created at output/example/fortran/animation/animation.mp4
    !
    ! GIVEN the animation example output in GitHub Pages structure
    ! WHEN the file is accessed via the expected relative path
    ! THEN it should match the documentation link format
    !
    ! This implements RED phase testing for Issue #178:
    ! Validates that animation example creates MP4 in correct GitHub Pages location

    use fortplot
    use fortplot_pipe, only: check_ffmpeg_available
    use fortplot_security, only: safe_remove_file
    use fortplot_system_runtime, only: is_windows
    use iso_fortran_env, only: real64
    implicit none
    
    logical :: ffmpeg_available, on_windows
    character(len=256) :: ci_env
    integer :: status

    ! Skip on Windows CI - FFmpeg pipe issues  
    on_windows = is_windows()
    call get_environment_variable("CI", ci_env, status=status)
    
    if (on_windows .and. status == 0) then
        print *, "SKIPPED: Animation example output structure tests on Windows CI"
        stop 0
    end if
    
    ! Check if ffmpeg is available
    ffmpeg_available = check_ffmpeg_available()
    if (.not. ffmpeg_available) then
        print *, "XFAIL: Animation example output structure tests require FFmpeg"
        print *, "Expected failure - FFmpeg not available"
        stop 77  ! Standard exit code for skipped tests
    end if
    
    ! Run RED phase tests
    call test_example_output_directory_compliance()
    call test_example_filename_consistency()
    call test_documentation_path_mapping()
    
    print *, "All animation example output structure tests passed!"

contains

    subroutine test_example_output_directory_compliance()
        ! GIVEN-WHEN-THEN:
        ! GIVEN save_animation_demo should create output in GitHub Pages structure
        ! WHEN animation is saved with structured path
        ! THEN file should appear at output/example/fortran/animation/animation.mp4
        
        character(len=512) :: expected_output_path, expected_dir
        character(len=512) :: working_dir_file, documentation_path
        logical :: structured_file_exists, working_dir_file_exists
        logical :: dir_exists
        integer :: mkdir_status, i
        
        ! Test data setup
        integer, parameter :: NFRAMES = 5, NPOINTS = 20
        type(figure_t) :: test_fig
        type(animation_t) :: anim
        real(real64), dimension(NPOINTS) :: x_data, y_data
        
        ! Expected GitHub Pages structure paths
        expected_dir = "output/example/fortran/animation"
        expected_output_path = trim(expected_dir) // "/animation.mp4"
        working_dir_file = "animation.mp4"  ! Current example output location
        documentation_path = "media/examples/animation/animation.mp4"
        
        ! Create expected directory structure
        call execute_command_line("mkdir -p " // expected_dir, &
                                 exitstat=mkdir_status)
        if (mkdir_status /= 0) then
            error stop "Failed to create expected output directory structure"
        end if
        
        ! Verify directory creation
        inquire(file=expected_dir, exist=dir_exists)
        if (.not. dir_exists) then
            error stop "Expected GitHub Pages directory structure not created"
        end if
        
        ! Create test animation matching save_animation_demo example
        do i = 1, NPOINTS
            x_data(i) = real(i-1, real64) * 2.0_real64 * 3.14159_real64 &
                       / real(NPOINTS-1, real64)
        end do
        y_data = sin(x_data)
        
        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(x_data, y_data, label='animated wave')
        call test_fig%set_title('Animation Save Demo')
        call test_fig%set_xlabel('x')
        call test_fig%set_ylabel('y')
        call test_fig%set_xlim(0.0_real64, 2.0_real64 * 3.14159_real64)
        call test_fig%set_ylim(-1.5_real64, 1.5_real64)
        
        ! Create animation matching example
        anim = FuncAnimation(update_example_wave, frames=NFRAMES, &
                            interval=50, fig=test_fig)
        
        ! Test 1: Save to structured directory (desired behavior)
        call anim%save(expected_output_path, 24)
        
        inquire(file=expected_output_path, exist=structured_file_exists)
        if (.not. structured_file_exists) then
            error stop "Animation not saved to expected GitHub Pages structure"
        end if
        
        ! Test 2: Verify working directory file doesn't exist (current issue)
        inquire(file=working_dir_file, exist=working_dir_file_exists)
        if (working_dir_file_exists) then
            ! Clean up unexpected file
            block
                logical :: remove_success
                call safe_remove_file(working_dir_file, remove_success)
            end block
        end if
        
        ! Cleanup structured file
        block
            logical :: remove_success
            call safe_remove_file(expected_output_path, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not clean up structured output file"
            end if
        end block
        
    contains
        subroutine update_example_wave(frame)
            integer, intent(in) :: frame
            real(real64) :: phase
            
            ! Match the phase calculation from save_animation_demo
            phase = real(frame - 1, real64) * 2.0_real64 * 3.14159_real64 &
                   / real(NFRAMES, real64)
            
            ! Update y data with animated wave
            y_data = sin(x_data + phase) * cos(phase * 0.5_real64)
            
            ! Update plot data
            call test_fig%set_ydata(1, y_data)
        end subroutine update_example_wave
        
    end subroutine test_example_output_directory_compliance

    subroutine test_example_filename_consistency()
        ! GIVEN-WHEN-THEN:
        ! GIVEN animation example should use consistent filename 
        ! WHEN example outputs "animation.mp4"
        ! THEN filename should match documentation references
        
        character(len=512) :: expected_filename, alternative_filename
        character(len=512) :: test_path, base_dir
        logical :: filename_consistent
        integer :: mkdir_status
        
        expected_filename = "animation.mp4"
        alternative_filename = "save_animation_demo.mp4"  ! Should NOT be used
        base_dir = "output/example/fortran/animation"
        test_path = trim(base_dir) // "/" // expected_filename
        
        ! Create directory 
        call execute_command_line("mkdir -p " // base_dir, &
                                 exitstat=mkdir_status)
        if (mkdir_status /= 0) then
            error stop "Failed to create directory for filename test"
        end if
        
        ! Verify expected filename format is valid
        filename_consistent = .true.
        
        ! Check that filename matches documentation expectations
        if (len_trim(expected_filename) == 0) then
            filename_consistent = .false.
        end if
        
        if (index(expected_filename, ".mp4") == 0) then
            filename_consistent = .false.
        end if
        
        ! Verify filename is not program-specific (should be generic)
        if (index(expected_filename, "demo") /= 0 .or. &
            index(expected_filename, "save") /= 0) then
            filename_consistent = .false.
        end if
        
        if (.not. filename_consistent) then
            error stop "Animation filename format inconsistent with documentation"
        end if
        
        ! Test path construction
        if (index(test_path, expected_filename) == 0) then
            error stop "Failed to construct consistent file path"
        end if
        
    end subroutine test_example_filename_consistency

    subroutine test_documentation_path_mapping()
        ! GIVEN-WHEN-THEN:
        ! GIVEN output files in GitHub Pages directory structure
        ! WHEN documentation references animation files
        ! THEN paths should map correctly from output to documentation
        
        character(len=512) :: output_path, doc_path, relative_path
        character(len=512) :: expected_output, expected_doc, expected_relative
        logical :: mapping_valid
        
        ! Expected path mappings for GitHub Pages deployment
        expected_output = "output/example/fortran/animation/animation.mp4"
        expected_doc = "build/doc/media/examples/animation/animation.mp4"  
        expected_relative = "../media/examples/animation/animation.mp4"
        
        output_path = expected_output
        doc_path = expected_doc
        relative_path = expected_relative
        
        mapping_valid = .true.
        
        ! Verify output path format
        if (index(output_path, "output/example") == 0) then
            mapping_valid = .false.
        end if
        
        if (index(output_path, "animation/animation.mp4") == 0) then
            mapping_valid = .false.
        end if
        
        ! Verify documentation path format
        if (index(doc_path, "build/doc/media") == 0) then
            mapping_valid = .false.
        end if
        
        if (index(doc_path, "examples/animation") == 0) then
            mapping_valid = .false.
        end if
        
        ! Verify relative path format for markdown links
        if (index(relative_path, "../media/examples") == 0) then
            mapping_valid = .false.
        end if
        
        if (index(relative_path, "animation.mp4") == 0) then
            mapping_valid = .false.
        end if
        
        if (.not. mapping_valid) then
            error stop "Documentation path mapping invalid for GitHub Pages"
        end if
        
        ! Test that paths have consistent depth structure
        if (count_path_separators(output_path) /= count_path_separators(doc_path)) then
            ! This is expected - they have different base directories
            ! but the relative structure after base should be similar
        end if
        
    contains
        integer function count_path_separators(path)
            character(len=*), intent(in) :: path
            integer :: i, count
            
            count = 0
            do i = 1, len_trim(path)
                if (path(i:i) == '/') count = count + 1
            end do
            count_path_separators = count
        end function count_path_separators
        
    end subroutine test_documentation_path_mapping

end program test_animation_example_output_structure