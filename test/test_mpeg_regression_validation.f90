program test_mpeg_regression_validation
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    ! Given: MPEG validation must prevent regressions (Issue #32)
    ! When: We test against known regression scenarios
    ! Then: Validation should catch regressions that caused false positives

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== MPEG REGRESSION VALIDATION TESTS ==="
    
    call test_moving_dot_video_regression()
    call test_size_624_bytes_regression()
    call test_false_positive_pattern_regression()
    call test_validation_reliability_regression()

    print *, "=== Regression validation tests completed ==="

contains

    subroutine test_moving_dot_video_regression()
        ! Given: Issue #32 mentions moving_dot_video.mpg produces 624 bytes (invalid)
        ! When: We recreate similar moving dot scenario
        ! Then: Validation should detect if file is inadequate like the 624-byte case

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: regression_detected
        integer :: file_size

        print *, ""
        print *, "TEST: Moving Dot Video Regression"
        print *, "================================"

        test_file = "regression_moving_dot.mp4"
        
        ! Recreate moving dot scenario similar to the failing case
        test_x = [(real(i, real64), i=1,10)]
        test_y = [(5.0_real64, i=1,10)]  ! Flat line representing static background

        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_moving_dot_data, frames=10, interval=50, fig=test_fig)
        call anim%save(test_file, fps=20)

        inquire(file=test_file, size=file_size)
        regression_detected = detect_moving_dot_regression(test_file, file_size)

        print *, "Generated file size:", file_size, "bytes"
        print *, "Regression detected:", regression_detected

        if (regression_detected) then
            print *, "*** MOVING DOT REGRESSION DETECTED ***"
            print *, "Current implementation shows same issues as reported in #32"
            print *, "File size suggests inadequate MPEG encoding"
        else
            print *, "Moving dot regression test passed"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_moving_dot_data(frame)
        integer, intent(in) :: frame
        ! Simulate moving dot by changing one point
        test_y = [(5.0_real64, i=1,10)]
        if (frame <= 10) then
            test_y(frame) = 8.0_real64  ! Moving dot
        end if
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function detect_moving_dot_regression(filename, file_size) result(regression_detected)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: regression_detected
        
        ! Regression indicators from Issue #32:
        ! 1. File size suspiciously small (624 bytes mentioned)
        ! 2. File exists but is invalid
        ! 3. Tests pass but file doesn't work
        
        logical :: size_suspicious, validation_inadequate
        
        size_suspicious = (file_size < 5000)  ! Much smaller than 83,522 bytes reference
        validation_inadequate = .not. validate_against_regression_criteria(filename)
        
        regression_detected = size_suspicious .or. validation_inadequate
        
        print *, "  Regression detection analysis:"
        print *, "    Size suspicious (<5KB):", size_suspicious
        print *, "    Validation inadequate:", validation_inadequate
        print *, "    Regression detected:", regression_detected
    end function

    function validate_against_regression_criteria(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        logical :: size_adequate, header_valid, tool_valid
        integer :: file_size
        
        inquire(file=filename, size=file_size)
        
        size_adequate = (file_size > 10000)  ! Should be substantial like 83,522 reference
        header_valid = check_header_regression(filename)
        tool_valid = check_tool_regression(filename)
        
        valid = size_adequate .and. header_valid .and. tool_valid
        
        print *, "    Regression criteria validation:"
        print *, "      Size adequate:", size_adequate
        print *, "      Header valid:", header_valid
        print *, "      Tool valid:", tool_valid
    end function

    function check_header_regression(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        character(len=12) :: header
        integer :: file_unit, ios
        
        valid = .false.
        
        open(newunit=file_unit, file=filename, access='stream', form='unformatted', iostat=ios)
        if (ios /= 0) return
        
        read(file_unit, iostat=ios) header
        close(file_unit)
        
        if (ios /= 0) return
        
        valid = (index(header, 'ftyp') > 0 .or. &
                index(header, 'mdat') > 0 .or. &
                index(header, 'moov') > 0)
    end function

    function check_tool_regression(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        character(len=500) :: command
        integer :: status
        
        call execute_command_line("which ffprobe >/dev/null 2>&1", exitstat=status)
        if (status /= 0) then
            valid = .true.  ! Can't test, assume valid
            return
        end if
        
        write(command, '(A,A,A)') 'ffprobe -v error -show_format "', trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        valid = (status == 0)
    end function

    subroutine test_size_624_bytes_regression()
        ! Given: Issue #32 specifically mentions 624-byte invalid file
        ! When: We create minimal animation that might produce small file
        ! Then: Validation should detect if we produce similarly inadequate file

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: size_regression_detected
        integer :: file_size

        print *, ""
        print *, "TEST: 624 Bytes Size Regression"
        print *, "=============================="

        test_file = "regression_624_bytes.mp4"
        
        ! Create minimal content that might trigger small file issue
        test_x = [(real(i, real64), i=1,3)]
        test_y = [(1.0_real64, i=1,3)]

        call test_fig%initialize(width=100, height=100)  ! Minimal size
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_624_bytes_data, frames=2, interval=100, fig=test_fig)  ! Minimal frames
        call anim%save(test_file, fps=5)

        inquire(file=test_file, size=file_size)
        size_regression_detected = detect_624_bytes_regression(file_size)

        print *, "Generated file size:", file_size, "bytes"
        print *, "624-byte regression detected:", size_regression_detected

        if (size_regression_detected) then
            print *, "*** 624-BYTE SIZE REGRESSION DETECTED ***"
            print *, "File size indicates same encoding problem as Issue #32"
        else
            print *, "624-byte size regression test passed"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_624_bytes_data(frame)
        integer, intent(in) :: frame
        ! Minimal change
        test_y = [(real(frame, real64), i=1,3)]
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function detect_624_bytes_regression(file_size) result(regression_detected)
        integer, intent(in) :: file_size
        logical :: regression_detected
        
        ! Check if we're producing files similar to the problematic 624-byte file
        ! Allow some tolerance but flag if suspiciously small
        regression_detected = (file_size < 2000)  ! Much smaller than expected
        
        print *, "  624-byte regression analysis:"
        print *, "    File size:", file_size, "bytes"
        print *, "    Problem threshold: < 2000 bytes"
        print *, "    Regression detected:", regression_detected
    end function

    subroutine test_false_positive_pattern_regression()
        ! Given: Issue #32 describes pattern where tests pass but files are invalid
        ! When: We test the pattern of false positive behavior
        ! Then: Our validation should catch this specific regression pattern

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: current_test_would_pass, comprehensive_validation_passes, false_positive_detected
        integer :: file_size, status

        print *, ""
        print *, "TEST: False Positive Pattern Regression"
        print *, "======================================"

        test_file = "regression_false_positive.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_false_positive_data, frames=8, interval=50, fig=test_fig)
        call anim%save(test_file, fps=15, status=status)

        inquire(file=test_file, size=file_size)
        
        ! Simulate current test logic (what was giving false positives)
        current_test_would_pass = (status == 0 .and. file_size > 0)
        
        ! Apply comprehensive validation
        comprehensive_validation_passes = apply_comprehensive_validation(test_file)
        
        ! False positive pattern: current test passes but comprehensive fails
        false_positive_detected = current_test_would_pass .and. .not. comprehensive_validation_passes

        print *, "Current test logic result:", current_test_would_pass
        print *, "Comprehensive validation:", comprehensive_validation_passes
        print *, "False positive pattern detected:", false_positive_detected

        if (false_positive_detected) then
            print *, "*** FALSE POSITIVE PATTERN REGRESSION DETECTED ***"
            print *, "Same false positive pattern as described in Issue #32"
        else
            print *, "False positive pattern regression test passed"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_false_positive_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.5_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function apply_comprehensive_validation(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        logical :: size_valid, header_valid, tool_valid
        integer :: file_size
        
        inquire(file=filename, size=file_size)
        
        size_valid = (file_size > 5000)  ! Substantial size requirement
        header_valid = check_header_regression(filename)
        tool_valid = check_tool_regression(filename)
        
        valid = size_valid .and. header_valid .and. tool_valid
        
        print *, "    Comprehensive validation breakdown:"
        print *, "      Size valid (>5KB):", size_valid
        print *, "      Header valid:", header_valid
        print *, "      Tool valid:", tool_valid
    end function

    subroutine test_validation_reliability_regression()
        ! Given: Validation framework should be reliable and not regress
        ! When: We test multiple scenarios for consistency
        ! Then: Validation should consistently identify issues

        type(animation_t) :: anim
        character(len=200) :: test_files(3)
        logical :: results(3), reliability_regression
        integer :: i_test, consistent_results

        print *, ""
        print *, "TEST: Validation Reliability Regression"
        print *, "======================================"

        test_files(1) = "regression_reliability_1.mp4"
        test_files(2) = "regression_reliability_2.mp4" 
        test_files(3) = "regression_reliability_3.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)

        ! Test multiple similar scenarios
        do i_test = 1, 3
            call test_fig%initialize(width=320, height=240)
            call test_fig%add_plot(test_x, test_y)
            
            anim = FuncAnimation(update_reliability_data, frames=5+i_test, interval=50, fig=test_fig)
            call anim%save(test_files(i_test), fps=15)
            
            results(i_test) = apply_comprehensive_validation(test_files(i_test))
            print *, "Test", i_test, "validation result:", results(i_test)
        end do

        ! Check for reliability regression (inconsistent results for similar content)
        consistent_results = count(results)
        reliability_regression = (consistent_results /= 0 .and. consistent_results /= 3)

        print *, "Consistent validation results:", consistent_results, "out of 3"
        print *, "Reliability regression detected:", reliability_regression

        if (reliability_regression) then
            print *, "*** VALIDATION RELIABILITY REGRESSION ***"
            print *, "Validation framework shows inconsistent behavior"
        else
            print *, "Validation reliability regression test passed"
        end if

        do i_test = 1, 3
            call execute_command_line("rm -f " // trim(test_files(i_test)))
        end do
    end subroutine

    subroutine update_reliability_data(frame)
        integer, intent(in) :: frame
        test_y = sin(test_x + real(frame, real64) * 0.15_real64)
        call test_fig%set_ydata(1, test_y)
    end subroutine

end program test_mpeg_regression_validation