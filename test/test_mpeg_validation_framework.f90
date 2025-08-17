program test_mpeg_validation_framework
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    ! Given: Current MPEG tests give false positives
    ! When: We implement comprehensive validation
    ! Then: Tests should properly detect invalid/corrupt MPEG files

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== MPEG Validation Framework Tests ==="

    call test_detect_false_positive_scenario()
    call test_comprehensive_mpeg_validation()
    call test_validation_prevents_false_positives()

    print *, "=== Framework tests completed ==="

contains

    subroutine test_detect_false_positive_scenario()
        ! Given: Current system that produces false positives
        ! When: We apply comprehensive validation
        ! Then: We should detect the issues current tests miss

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: current_test_passes, comprehensive_validates
        integer :: status, file_size

        print *, ""
        print *, "TEST: Detecting False Positive Scenario"
        print *, "======================================"

        test_file = "false_positive_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_false_positive_data, frames=5, interval=50, fig=test_fig)
        
        ! When: We save animation (current approach)
        call anim%save(test_file, status=status)
        
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
        
        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_false_positive_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 2.0_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

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
        
        ! Additional heuristics could be added here based on:
        ! - Frame count (if available in metadata)
        ! - Resolution (if available in metadata)
        ! - Duration (if available in metadata)
        
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
        
        ! Use ffprobe to validate
        write(command, '(A,A,A)') 'ffprobe -v error -show_format "', trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        valid = (status == 0)
    end function

    subroutine test_comprehensive_mpeg_validation()
        ! Given: Need for robust MPEG validation
        ! When: We test against various scenarios
        ! Then: Validation should correctly identify valid/invalid files

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: validation_result
        integer :: frames_list(3) = [2, 10, 25]
        integer :: j

        print *, ""
        print *, "TEST: Comprehensive MPEG Validation"
        print *, "=================================="

        do j = 1, size(frames_list)
            write(test_file, '(A,I0,A)') "comprehensive_test_", frames_list(j), ".mp4"
            
            test_x = [(real(i, real64), i=1,10)]
            test_y = sin(test_x)
            
            call test_fig%initialize(width=640, height=480)
            call test_fig%add_plot(test_x, test_y)
            
            anim = FuncAnimation(update_comprehensive_data, frames=frames_list(j), interval=50, fig=test_fig)
            call anim%save(test_file, fps=20)
            
            validation_result = validate_mpeg_comprehensively(test_file)
            
            print *, "Frames:", frames_list(j), "- Comprehensive validation:", validation_result
            
            if (.not. validation_result) then
                print *, "  VALIDATION FAILURE for", frames_list(j), "frames"
            end if
            
            call execute_command_line("rm -f " // trim(test_file))
        end do
    end subroutine

    subroutine update_comprehensive_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.2_real64)
        call test_fig%set_ydata(1, test_y)
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
        
        call execute_command_line("rm -f " // trim(fake_file) // " " // trim(empty_file))
    end subroutine

end program test_mpeg_validation_framework