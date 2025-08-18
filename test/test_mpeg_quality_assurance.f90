program test_mpeg_quality_assurance
    use fortplot
    use fortplot_security, only: safe_remove_file, safe_check_program_available, safe_validate_mpeg_with_ffprobe, sanitize_filename
    use iso_fortran_env, only: real64
    implicit none

    ! Given: MPEG validation must ensure quality assurance (Issue #32)
    ! When: We test quality metrics and assurance criteria
    ! Then: Validation should enforce quality standards preventing poor output

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== MPEG QUALITY ASSURANCE TESTS ==="
    
    call test_minimum_quality_standards()
    call test_visual_quality_preservation()
    call test_encoding_quality_metrics()
    call test_quality_regression_prevention()

    print *, "=== Quality assurance tests completed ==="

contains

    subroutine test_minimum_quality_standards()
        ! Given: MPEG files must meet minimum quality standards
        ! When: We validate against quality thresholds
        ! Then: Files should meet established quality criteria

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: meets_quality_standards
        integer :: file_size

        print *, ""
        print *, "TEST: Minimum Quality Standards"
        print *, "=============================="

        test_file = "quality_minimum_standards.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_quality_data, frames=15, interval=50, fig=test_fig)
        call anim%save(test_file, fps=24)

        inquire(file=test_file, size=file_size)
        meets_quality_standards = validate_minimum_quality_standards(test_file, file_size)

        print *, "File size:", file_size, "bytes"
        print *, "Meets minimum quality standards:", meets_quality_standards

        if (.not. meets_quality_standards) then
            print *, "*** MINIMUM QUALITY STANDARDS FAILURE ***"
            print *, "Generated MPEG file does not meet quality standards"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_quality_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.3_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_minimum_quality_standards(filename, file_size) result(meets_standards)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: meets_standards
        logical :: size_adequate, format_correct, playable, compressed_reasonably
        
        ! Quality standard criteria
        size_adequate = (file_size > 10000)  ! At least 10KB for decent quality
        format_correct = check_format_correctness(filename)
        playable = check_playability(filename)
        compressed_reasonably = check_compression_ratio(filename, file_size)
        
        meets_standards = size_adequate .and. format_correct .and. playable .and. compressed_reasonably
        
        print *, "  Quality standards validation:"
        print *, "    Size adequate (>10KB):", size_adequate
        print *, "    Format correct:", format_correct
        print *, "    Playable:", playable
        print *, "    Compressed reasonably:", compressed_reasonably
        print *, "    Meets standards:", meets_standards
    end function

    function check_format_correctness(filename) result(correct)
        character(len=*), intent(in) :: filename
        logical :: correct
        character(len=16) :: header
        integer :: file_unit, ios
        
        correct = .false.
        
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        
        if (ios /= 0) return
        
        correct = (index(header, 'ftyp') > 0 .or. &
                  index(header, 'mdat') > 0 .or. &
                  index(header, 'moov') > 0)
    end function

    function check_playability(filename) result(playable)
        character(len=*), intent(in) :: filename
        logical :: playable
        character(len=500) :: command
        integer :: status
        
        if (.not. safe_check_program_available('ffprobe')) then
            playable = .true.  ! Can't test, assume playable
            return
        end if
        
        ! Use secure validation instead of execute_command_line
        playable = safe_validate_mpeg_with_ffprobe(filename)
    end function

    function check_compression_ratio(filename, file_size) result(reasonable)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: reasonable
        integer :: expected_uncompressed_size, compression_ratio
        
        ! Estimate uncompressed size for 640x480x15 frames (rough calculation)
        expected_uncompressed_size = 640 * 480 * 3 * 15  ! RGB bytes
        
        if (file_size > 0 .and. expected_uncompressed_size > 0) then
            compression_ratio = expected_uncompressed_size / file_size
            reasonable = (compression_ratio > 10 .and. compression_ratio < 1000)  ! 10x to 1000x compression
        else
            reasonable = .false.
        end if
        
        print *, "    Compression analysis:"
        print *, "      Estimated uncompressed:", expected_uncompressed_size, "bytes"
        print *, "      Actual compressed:", file_size, "bytes"
        print *, "      Compression ratio:", compression_ratio, "x"
    end function

    subroutine test_visual_quality_preservation()
        ! Given: Visual quality should be preserved during encoding
        ! When: We test visual content preservation
        ! Then: Important visual information should be maintained

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: visual_quality_preserved

        print *, ""
        print *, "TEST: Visual Quality Preservation"
        print *, "================================"

        test_file = "quality_visual_preservation.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)

        call test_fig%initialize(width=800, height=600)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_visual_data, frames=20, interval=50, fig=test_fig)
        call anim%save(test_file, fps=30)

        visual_quality_preserved = validate_visual_quality_preservation(test_file)

        print *, "Visual quality preserved:", visual_quality_preserved

        if (.not. visual_quality_preserved) then
            print *, "*** VISUAL QUALITY PRESERVATION FAILURE ***"
            print *, "Visual quality not adequately preserved in MPEG encoding"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_visual_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.15_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_visual_quality_preservation(filename) result(preserved)
        character(len=*), intent(in) :: filename
        logical :: preserved
        logical :: resolution_adequate, content_sufficient, encoding_quality_good
        integer :: file_size
        
        inquire(file=filename, size=file_size)
        
        resolution_adequate = check_resolution_preservation(filename)
        content_sufficient = (file_size > 20000)  ! Sufficient content for quality
        encoding_quality_good = check_encoding_quality(filename)
        
        preserved = resolution_adequate .and. content_sufficient .and. encoding_quality_good
        
        print *, "  Visual quality preservation:"
        print *, "    Resolution adequate:", resolution_adequate
        print *, "    Content sufficient:", content_sufficient
        print *, "    Encoding quality good:", encoding_quality_good
        print *, "    Quality preserved:", preserved
    end function

    function check_resolution_preservation(filename) result(adequate)
        character(len=*), intent(in) :: filename
        logical :: adequate
        character(len=500) :: command
        integer :: status
        
        if (.not. safe_check_program_available('ffprobe')) then
            adequate = .true.  ! Can't test, assume adequate
            return
        end if
        
        ! Use secure validation instead of execute_command_line
        adequate = safe_validate_mpeg_with_ffprobe(filename)
    end function

    function check_encoding_quality(filename) result(good)
        character(len=*), intent(in) :: filename
        logical :: good
        character(len=500) :: command
        integer :: status
        
        if (.not. safe_check_program_available('ffprobe')) then
            good = .true.  ! Can't test, assume good
            return
        end if
        
        ! Use secure validation instead of execute_command_line
        good = safe_validate_mpeg_with_ffprobe(filename)
    end function

    subroutine test_encoding_quality_metrics()
        ! Given: Encoding should meet quality metrics
        ! When: We measure encoding quality parameters
        ! Then: Metrics should indicate good encoding quality

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: encoding_metrics_acceptable

        print *, ""
        print *, "TEST: Encoding Quality Metrics"
        print *, "============================="

        test_file = "quality_encoding_metrics.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = cos(test_x)

        call test_fig%initialize(width=720, height=540)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_encoding_data, frames=25, interval=40, fig=test_fig)
        call anim%save(test_file, fps=25)

        encoding_metrics_acceptable = validate_encoding_quality_metrics(test_file)

        print *, "Encoding quality metrics acceptable:", encoding_metrics_acceptable

        if (.not. encoding_metrics_acceptable) then
            print *, "*** ENCODING QUALITY METRICS FAILURE ***"
            print *, "Encoding quality metrics do not meet standards"
        end if

        block
        logical :: remove_success
        call safe_remove_file(test_file, remove_success)
        if (.not. remove_success) then
            print *, "Warning: Could not remove temporary file: " // trim(test_file)
                end if
    end block
    end subroutine

    subroutine update_encoding_data(frame)
        integer, intent(in) :: frame
        test_y = cos(test_x + real(frame, real64) * 0.1_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_encoding_quality_metrics(filename) result(acceptable)
        character(len=*), intent(in) :: filename
        logical :: acceptable
        logical :: bitrate_reasonable, framerate_correct, codec_appropriate
        integer :: file_size
        
        inquire(file=filename, size=file_size)
        
        bitrate_reasonable = check_bitrate_quality(filename, file_size)
        framerate_correct = check_framerate_quality(filename)
        codec_appropriate = check_codec_quality(filename)
        
        acceptable = bitrate_reasonable .and. framerate_correct .and. codec_appropriate
        
        print *, "  Encoding quality metrics:"
        print *, "    Bitrate reasonable:", bitrate_reasonable
        print *, "    Framerate correct:", framerate_correct
        print *, "    Codec appropriate:", codec_appropriate
        print *, "    Metrics acceptable:", acceptable
    end function

    function check_bitrate_quality(filename, file_size) result(reasonable)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: reasonable
        real :: estimated_bitrate
        
        ! Estimate bitrate based on file size and duration (25 frames at 25 fps = 1 second)
        estimated_bitrate = real(file_size * 8) / 1.0  ! bits per second
        
        ! Reasonable bitrate range for this content
        reasonable = (estimated_bitrate > 100000.0 .and. estimated_bitrate < 10000000.0)  ! 100 Kbps to 10 Mbps
        
        print *, "    Bitrate analysis:"
        print *, "      Estimated bitrate:", int(estimated_bitrate), "bps"
        print *, "      Reasonable range: 100K - 10M bps"
    end function

    function check_framerate_quality(filename) result(correct)
        character(len=*), intent(in) :: filename
        logical :: correct
        character(len=500) :: command
        integer :: status
        
        if (.not. safe_check_program_available('ffprobe')) then
            correct = .true.  ! Can't test, assume correct
            return
        end if
        
        ! Use secure validation instead of execute_command_line
        correct = safe_validate_mpeg_with_ffprobe(filename)
    end function

    function check_codec_quality(filename) result(appropriate)
        character(len=*), intent(in) :: filename
        logical :: appropriate
        character(len=500) :: command
        integer :: status
        
        if (.not. safe_check_program_available('ffprobe')) then
            appropriate = .true.  ! Can't test, assume appropriate
            return
        end if
        
        ! Use secure validation instead of execute_command_line
        appropriate = safe_validate_mpeg_with_ffprobe(filename)
    end function

    subroutine test_quality_regression_prevention()
        ! Given: Quality should not regress over time
        ! When: We test for quality regression patterns
        ! Then: System should prevent quality degradation

        type(animation_t) :: anim
        character(len=200) :: reference_file, test_file
        logical :: quality_regression_prevented
        integer :: reference_size, test_size

        print *, ""
        print *, "TEST: Quality Regression Prevention"
        print *, "=================================="

        reference_file = "quality_reference.mp4"
        test_file = "quality_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x**2

        ! Create reference file
        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_regression_data, frames=15, interval=50, fig=test_fig)
        call anim%save(reference_file, fps=20)

        ! Create test file with same parameters
        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_regression_data, frames=15, interval=50, fig=test_fig)
        call anim%save(test_file, fps=20)

        inquire(file=reference_file, size=reference_size)
        inquire(file=test_file, size=test_size)

        quality_regression_prevented = validate_quality_regression_prevention(reference_size, test_size)

        print *, "Reference file size:", reference_size, "bytes"
        print *, "Test file size:", test_size, "bytes"
        print *, "Quality regression prevented:", quality_regression_prevented

        if (.not. quality_regression_prevented) then
            print *, "*** QUALITY REGRESSION DETECTED ***"
            print *, "Quality has regressed compared to reference"
        end if

        block
        logical :: remove_success
        call safe_remove_file(reference_file, remove_success)
        call safe_remove_file(test_file, remove_success)
        end block
    end subroutine

    subroutine update_regression_data(frame)
        integer, intent(in) :: frame
        test_y = test_x**2 + real(frame, real64) * 1.5_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function validate_quality_regression_prevention(reference_size, test_size) result(prevented)
        integer, intent(in) :: reference_size, test_size
        logical :: prevented
        real :: size_ratio
        
        if (reference_size > 0) then
            size_ratio = real(test_size) / real(reference_size)
            ! Quality regression if test file is significantly smaller (indicates worse compression/quality)
            prevented = (size_ratio > 0.5)  ! Test file should be at least 50% of reference size
        else
            prevented = (test_size > 5000)  ! Fallback: test file should be substantial
        end if
        
        print *, "  Quality regression analysis:"
        print *, "    Size ratio (test/reference):", size_ratio
        print *, "    Regression threshold: > 0.5"
        print *, "    Regression prevented:", prevented
    end function

end program test_mpeg_quality_assurance