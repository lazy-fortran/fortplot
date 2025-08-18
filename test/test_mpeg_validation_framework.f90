program test_mpeg_validation_framework_fixed
    use fortplot
    use fortplot_security, only: safe_remove_file, safe_check_program_available, sanitize_filename, safe_validate_mpeg_with_ffprobe
    use iso_fortran_env, only: real64
    implicit none

    ! Given: Current MPEG tests give false positives
    ! When: We implement comprehensive validation
    ! Then: Tests should properly detect invalid/corrupt MPEG files

    print *, "=== MPEG Validation Framework Tests (Fixed) ==="

    call test_detect_false_positive_scenario()
    call test_comprehensive_mpeg_validation()
    call test_validation_prevents_false_positives()

    print *, "=== Framework tests completed ==="

contains

    subroutine test_detect_false_positive_scenario()
        ! Given: Current system that produces false positives
        ! When: We apply comprehensive validation
        ! Then: We should detect the issues current tests miss

        type(figure_t) :: test_fig
        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: current_test_passes, comprehensive_validates
        integer :: status, file_size, i
        real(real64), dimension(10) :: test_x, test_y

        print *, ""
        print *, "TEST: Detecting False Positive Scenario"
        print *, "======================================"

        test_file = "false_positive_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        ! Create animation with frame updates
        call create_and_save_test_animation(test_fig, test_file, 5, status)
        
        ! Current test logic (what's currently passing)
        current_test_passes = (status == 0)
        inquire(file=test_file, size=file_size)
        current_test_passes = current_test_passes .and. (file_size > 0)
        
        ! Comprehensive validation (what should be the standard)
        comprehensive_validates = validate_mpeg_comprehensively(test_file)
        
        print *, "Current test approach passes:", current_test_passes
        print *, "Comprehensive validation passes:", comprehensive_validates
        print *, "File size:", file_size, "bytes"
        
        ! This demonstrates the false positive problem
        if (current_test_passes .and. .not. comprehensive_validates) then
            print *, "*** FALSE POSITIVE DETECTED ***"
            print *, "Current tests pass but file is invalid"
            print *, "This is the root cause of issue #32"
        else if (.not. current_test_passes) then
            print *, "Current test correctly failed"
        else
            print *, "Both validations agree - file is valid"
        end if
        
        block
            logical :: remove_success
            call safe_remove_file(test_file, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not remove temporary file: " // trim(test_file)
                    end if
    end block
        end block
    end subroutine

    subroutine create_and_save_test_animation(fig, filename, frames, save_status)
        type(figure_t), intent(inout) :: fig
        character(len=*), intent(in) :: filename
        integer, intent(in) :: frames
        integer, intent(out) :: save_status
        
        ! Simple approach: save individual frames then combine with FFmpeg
        character(len=200) :: frame_pattern, command
        integer :: frame_idx
        
        frame_pattern = "temp_frame_"
        
        ! Generate frames manually
        do frame_idx = 1, frames
            call update_test_data(fig, frame_idx)
            call save_frame(fig, frame_pattern, frame_idx)
        end do
        
        ! Combine frames using FFmpeg - secure mode disabled
        save_status = 1  ! Indicate failure in secure mode
        if (safe_check_program_available('ffmpeg')) then
            print *, "FFmpeg available but external command execution disabled for security"
        else
            print *, "Operating in secure mode - FFmpeg operations disabled"
        end if
        
        ! Clean up frame files using secure file removal
        do frame_idx = 1, frames
            block
                character(len=256) :: frame_file
                logical :: remove_success
                write(frame_file, '(A,I0,A)') trim(frame_pattern), frame_idx, '.png'
                call safe_remove_file(frame_file, remove_success)
                if (.not. remove_success) then
                    print *, "Warning: Could not remove frame file: " // trim(frame_file)
                        end if
    end block
            end block
        end do
    end subroutine create_and_save_test_animation
    
    subroutine update_test_data(fig, frame)
        type(figure_t), intent(inout) :: fig
        integer, intent(in) :: frame
        real(real64), dimension(10) :: new_y
        integer :: i
        
        ! Create animated data
        new_y = [(real(i**2, real64) + real(frame, real64) * 2.0_real64, i=1,10)]
        call fig%set_ydata(1, new_y)
    end subroutine update_test_data
    
    subroutine save_frame(fig, pattern, frame_num)
        type(figure_t), intent(inout) :: fig
        character(len=*), intent(in) :: pattern
        integer, intent(in) :: frame_num
        character(len=200) :: filename
        
        write(filename, '(A,I0,A)') trim(pattern), frame_num, '.png'
        call fig%savefig(trim(filename))
    end subroutine save_frame

    function validate_mpeg_comprehensively(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        logical :: exists, has_content, has_video_header, passes_ffprobe, adequate_size
        integer :: file_size
        
        ! Comprehensive validation checklist
        inquire(file=filename, exist=exists, size=file_size)
        
        has_content = (file_size > 100)  ! Minimum reasonable size
        adequate_size = validate_size_for_content(filename, file_size)
        has_video_header = validate_video_header(filename)
        passes_ffprobe = validate_with_external_tool(filename)
        
        is_valid = exists .and. has_content .and. adequate_size .and. &
                  has_video_header .and. passes_ffprobe
        
        ! Debug output for validation steps
        print *, "  Validation checklist:"
        print *, "    File exists:", exists
        print *, "    Has content (>100 bytes):", has_content
        print *, "    Adequate size for content:", adequate_size
        print *, "    Valid video header:", has_video_header
        print *, "    Passes ffprobe:", passes_ffprobe
        print *, "    Overall valid:", is_valid
    end function

    function validate_size_for_content(filename, file_size) result(adequate)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: adequate
        integer :: min_expected
        
        ! Calculate minimum expected size based on content
        ! For simple animations: ~200-500 bytes per frame minimum
        ! Even heavily compressed H.264 should produce some data per frame
        min_expected = 1000  ! Conservative minimum 1KB for any valid video
        
        adequate = (file_size >= min_expected)
    end function

    function validate_video_header(filename) result(valid_header)
        character(len=*), intent(in) :: filename
        logical :: valid_header
        character(len=12) :: header
        integer :: file_unit, ios
        
        valid_header = .false.
        
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        
        if (ios /= 0) return
        
        ! Look for MP4 box signatures
        valid_header = (index(header, 'ftyp') > 0 .or. &
                       index(header, 'mdat') > 0 .or. &
                       index(header, 'moov') > 0 .or. &
                       index(header, 'mp4') > 0)
    end function

    function validate_with_external_tool(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        character(len=500) :: command
        integer :: status
        
        ! Use secure MPEG validation instead of ffprobe
        valid = safe_validate_mpeg_with_ffprobe(filename)
    end function

    subroutine test_comprehensive_mpeg_validation()
        ! Given: Need for robust MPEG validation
        ! When: We test against various scenarios
        ! Then: Validation should correctly identify valid/invalid files

        type(figure_t) :: test_fig
        character(len=200) :: test_file
        logical :: validation_result
        integer :: frames_list(3) = [2, 5, 8]
        integer :: j, status, i
        real(real64), dimension(10) :: test_x, test_y

        print *, ""
        print *, "TEST: Comprehensive MPEG Validation"
        print *, "=================================="

        do j = 1, size(frames_list)
            write(test_file, '(A,I0,A)') "comprehensive_test_", frames_list(j), ".mp4"
            
            test_x = [(real(i, real64), i=1,10)]
            test_y = sin(test_x)
            
            call test_fig%initialize(width=640, height=480)
            call test_fig%add_plot(test_x, test_y)
            
            call create_and_save_test_animation(test_fig, test_file, frames_list(j), status)
            
            validation_result = validate_mpeg_comprehensively(test_file)
            
            print *, "Frames:", frames_list(j), "- Comprehensive validation:", validation_result
            
            if (.not. validation_result) then
                print *, "  VALIDATION FAILURE for", frames_list(j), "frames"
            end if
            
            block
            logical :: remove_success
            call safe_remove_file(test_file, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not remove temporary file: " // trim(test_file)
                    end if
    end block
        end block
        end do
    end subroutine

    subroutine test_validation_prevents_false_positives()
        ! Given: Enhanced validation framework
        ! When: We test edge cases that cause false positives
        ! Then: Framework should catch issues current tests miss

        character(len=200) :: fake_file, empty_file
        logical :: fake_validates, empty_validates
        integer :: file_unit, ios

        print *, ""
        print *, "TEST: Validation Prevents False Positives"
        print *, "========================================"

        ! Test with fake/empty file
        fake_file = "fake.mp4"
        empty_file = "empty.mp4"
        
        ! Create a fake MP4 file (just some random bytes)
        open(newunit=file_unit, file=fake_file, access='stream', form='unformatted', iostat=ios)
        if (ios == 0) then
            write(file_unit) "fake video data that should not validate"
            close(file_unit)
        end if
        
        ! Create empty file
        open(newunit=file_unit, file=empty_file, access='stream', form='unformatted', iostat=ios)
        if (ios == 0) close(file_unit)
        
        ! Test validation
        fake_validates = validate_mpeg_comprehensively(fake_file)
        empty_validates = validate_mpeg_comprehensively(empty_file)
        
        print *, "Fake file validates:", fake_validates
        print *, "Empty file validates:", empty_validates
        
        if (fake_validates) then
            print *, "ERROR: Framework incorrectly validated fake file"
        else
            print *, "GOOD: Framework correctly rejected fake file"
        end if
        
        if (empty_validates) then
            print *, "ERROR: Framework incorrectly validated empty file"
        else
            print *, "GOOD: Framework correctly rejected empty file"
        end if
        
        block
            logical :: remove_success
            call safe_remove_file(fake_file, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not remove temporary file: " // trim(fake_file)
                    end if
    end block
            call safe_remove_file(empty_file, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not remove temporary file: " // trim(empty_file)
            end if
        end block
    end subroutine

end program test_mpeg_validation_framework_fixed