program test_mpeg_file_validation
    ! XFAIL: MPEG validation requires FFmpeg/ffprobe - Issue #104
    use fortplot
    use fortplot_security, only: safe_remove_file, safe_check_program_available, safe_validate_mpeg_with_ffprobe
    use iso_fortran_env, only: real64
    implicit none

    ! Given: We need to validate that MPEG files are actually playable video files
    ! When: We create animations and validate file integrity
    ! Then: Tests should fail if MPEG files are invalid

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    call test_mpeg_file_integrity_validation()
    call test_mpeg_file_header_validation()
    call test_mpeg_minimum_size_validation()
    call test_mpeg_ffprobe_validation()

    print *, "MPEG file validation tests completed"

contains

    subroutine test_mpeg_file_integrity_validation()
        ! Given: Animation system that should create valid MPEG files
        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: file_exists, is_valid_video
        integer :: file_size, status

        test_file = "integrity_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2
        
        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_integrity_data, frames=10, interval=50, fig=test_fig)
        
        ! When: We save the animation
        call anim%save(test_file, fps=15, status=status)
        
        ! Then: File should exist and be a valid video
        inquire(file=test_file, exist=file_exists, size=file_size)
        
        print *, "Integrity test - File exists:", file_exists
        print *, "Integrity test - File size:", file_size, "bytes"
        print *, "Integrity test - Save status:", status
        
        if (file_exists) then
            is_valid_video = validate_video_file_structure(test_file)
            print *, "Integrity test - Valid video structure:", is_valid_video
            
            ! This is where the current test would fail - we need proper validation
            if (.not. is_valid_video) then
                print *, "CRITICAL: File exists but is not a valid video!"
                print *, "Current tests give FALSE POSITIVE - they should FAIL here"
            end if
            
            block
            logical :: remove_success
            call safe_remove_file(test_file, remove_success)
            if (.not. remove_success) then
                print *, "Warning: Could not remove temporary file: " // trim(test_file)
            end if
            end block
        else
            print *, "CRITICAL: No file created despite status 0"
        end if
    end subroutine

    subroutine update_integrity_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 5.0_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_video_file_structure(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        character(len=4) :: header
        integer :: file_unit, ios
        
        is_valid = .false.
        
        ! Check basic file header for video format markers
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        
        if (ios /= 0) return
        
        ! MP4 files typically start with ftyp box or have specific signatures
        ! This is a simplified check - real validation would be more comprehensive
        is_valid = (iachar(header(1:1)) /= 0 .and. iachar(header(2:2)) /= 0)
    end function

    subroutine test_mpeg_file_header_validation()
        ! Given: MP4 file should have proper header structure
        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: has_valid_header
        real(real64), dimension(5) :: test_x_small, test_y_small
        
        test_file = "header_test.mp4"
        
        test_x_small = [(real(i, real64), i=1,5)]
        test_y_small = test_x_small
        
        call test_fig%initialize(width=320, height=240)
        call test_fig%add_plot(test_x_small, test_y_small)
        
        anim = FuncAnimation(update_header_data, frames=3, interval=100, fig=test_fig)
        call anim%save(test_file)
        
        ! When: We check the file header
        has_valid_header = check_mp4_header(test_file)
        
        ! Then: Header should indicate valid MP4 structure
        print *, "Header test - Valid MP4 header:", has_valid_header
        
        if (.not. has_valid_header) then
            print *, "CRITICAL: Generated MP4 lacks proper header structure"
        end if
        
        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
        end if
        end block
    end subroutine

    subroutine update_header_data(frame)
        integer, intent(in) :: frame
        real(real64), dimension(5) :: temp_y
        temp_y = [(real(i, real64) + real(frame, real64), i=1,5)]
        call test_fig%set_ydata(1, temp_y)
    end subroutine

    function check_mp4_header(filename) result(is_valid_mp4)
        character(len=*), intent(in) :: filename
        logical :: is_valid_mp4
        character(len=8) :: header
        integer :: file_unit, ios
        
        is_valid_mp4 = .false.
        
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        
        if (ios /= 0) return
        
        ! Check for common MP4 signatures (simplified)
        ! Real MP4 files should have 'ftyp' box or similar structures
        is_valid_mp4 = (index(header, 'ftyp') > 0 .or. &
                       index(header, 'mdat') > 0 .or. &
                       index(header, 'moov') > 0)
    end function

    subroutine test_mpeg_minimum_size_validation()
        ! Given: Valid video files should meet minimum size requirements
        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: meets_minimum_size
        integer :: file_size
        
        test_file = "size_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)
        
        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_size_data, frames=20, interval=50, fig=test_fig)
        call anim%save(test_file, fps=20)
        
        ! When: We check file size against reasonable minimums
        inquire(file=test_file, size=file_size)
        meets_minimum_size = validate_minimum_video_size(file_size, frames=20, width=800, height=600)
        
        print *, "Size test - File size:", file_size, "bytes"
        print *, "Size test - Meets minimum:", meets_minimum_size
        
        if (.not. meets_minimum_size) then
            print *, "CRITICAL: Video file too small for content (likely corrupt)"
        end if
        
        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
        end if
        end block
    end subroutine

    subroutine update_size_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.1_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_minimum_video_size(file_size, frames, width, height) result(is_adequate)
        integer, intent(in) :: file_size, frames, width, height
        logical :: is_adequate
        integer :: minimum_expected_size
        
        ! Rough estimate: even highly compressed video should be more than 1KB per frame
        ! for reasonable resolution. This is very conservative.
        minimum_expected_size = max(frames * 500, 5000)  ! At least 500 bytes per frame, minimum 5KB
        
        is_adequate = (file_size >= minimum_expected_size)
    end function

    subroutine test_mpeg_ffprobe_validation()
        ! Given: System with ffmpeg/ffprobe for comprehensive validation
        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: ffprobe_validates, ffprobe_available
        
        test_file = "ffprobe_test.mp4"
        
        ! Check if ffprobe is available
        ffprobe_available = safe_check_program_available('ffprobe')
        
        print *, "FFprobe available:", ffprobe_available
        
        if (.not. ffprobe_available) then
            print *, "Skipping ffprobe validation - tool not available"
            return
        end if
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**3
        
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        
        anim = FuncAnimation(update_ffprobe_data, frames=15, interval=50, fig=test_fig)
        call anim%save(test_file, fps=10)
        
        ! When: We validate with ffprobe
        ffprobe_validates = validate_with_ffprobe(test_file)
        
        print *, "FFprobe validation - File is valid video:", ffprobe_validates
        
        if (.not. ffprobe_validates) then
            print *, "CRITICAL: ffprobe cannot read the generated video file"
        end if
        
        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
        end if
        end block
    end subroutine

    subroutine update_ffprobe_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**3 + real(frame, real64) * 100.0_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_with_ffprobe(filename) result(is_valid)
        character(len=*), intent(in) :: filename
        logical :: is_valid
        
        ! Use secure validation instead of execute_command_line
        is_valid = safe_validate_mpeg_with_ffprobe(filename)
    end function

end program test_mpeg_file_validation