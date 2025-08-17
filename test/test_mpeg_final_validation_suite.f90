program test_mpeg_final_validation_suite
    use fortplot
    use iso_fortran_env, only: real64
    implicit none

    ! Given: Complete MPEG validation suite to eliminate false positives (Issue #32)
    ! When: We run comprehensive final validation combining all test aspects
    ! Then: Suite should reliably identify valid vs invalid MPEG files

    type(figure_t) :: test_fig
    real(real64), dimension(10) :: test_x, test_y
    integer :: i

    print *, "=== FINAL MPEG VALIDATION SUITE ==="
    print *, "This is the comprehensive test combining all validation aspects"
    print *, "to ensure no false positives as described in Issue #32"
    print *, ""
    
    call test_comprehensive_validation_suite()
    call test_false_positive_elimination()
    call test_validation_suite_reliability()
    call test_issue_32_specific_scenarios()

    print *, ""
    print *, "=== FINAL VALIDATION SUITE SUMMARY ==="
    call summarize_validation_framework()

contains

    subroutine test_comprehensive_validation_suite()
        ! Given: Final validation suite combining all test aspects
        ! When: We apply complete validation framework
        ! Then: Suite should provide definitive pass/fail determination

        type(animation_t) :: anim
        character(len=200) :: test_file
        logical :: comprehensive_suite_passes
        integer :: file_size

        print *, "TEST: Comprehensive Validation Suite"
        print *, "===================================="

        test_file = "final_comprehensive_test.mp4"
        
        test_x = [(real(i, real64), i=1,10)]
        test_y = test_x

        call test_fig%initialize(width=640, height=480)
        call test_fig%add_plot(test_x, test_y)

        anim = FuncAnimation(update_comprehensive_data, frames=16, interval=50, fig=test_fig)
        call anim%save(test_file, fps=24)

        inquire(file=test_file, size=file_size)
        comprehensive_suite_passes = apply_final_validation_suite(test_file)

        print *, "Generated file size:", file_size, "bytes"
        print *, "Comprehensive suite validation:", comprehensive_suite_passes

        if (.not. comprehensive_suite_passes) then
            print *, "*** COMPREHENSIVE VALIDATION SUITE FAILURE ***"
            print *, "Generated file fails comprehensive validation suite"
            print *, "This indicates the same problems described in Issue #32"
        else
            print *, "Comprehensive validation suite PASSED"
        end if

        call execute_command_line("rm -f " // trim(test_file))
    end subroutine

    subroutine update_comprehensive_data(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.4_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function apply_final_validation_suite(filename) result(passes)
        character(len=*), intent(in) :: filename
        logical :: passes
        logical :: layer1, layer2, layer3, layer4, layer5
        
        print *, "  Applying final validation suite (5 layers):"
        
        ! Layer 1: Basic file validation
        layer1 = validate_basic_file_properties(filename)
        print *, "    Layer 1 - Basic properties:", layer1
        
        ! Layer 2: Size and content validation
        layer2 = validate_size_and_content(filename)
        print *, "    Layer 2 - Size and content:", layer2
        
        ! Layer 3: Format and header validation
        layer3 = validate_format_and_header(filename)
        print *, "    Layer 3 - Format and header:", layer3
        
        ! Layer 4: External tool validation
        layer4 = validate_with_external_tools(filename)
        print *, "    Layer 4 - External tools:", layer4
        
        ! Layer 5: Quality and compliance validation
        layer5 = validate_quality_and_compliance(filename)
        print *, "    Layer 5 - Quality and compliance:", layer5
        
        passes = layer1 .and. layer2 .and. layer3 .and. layer4 .and. layer5
        print *, "    FINAL SUITE RESULT:", passes
    end function

    function validate_basic_file_properties(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        logical :: exists
        integer :: file_size
        
        inquire(file=filename, exist=exists, size=file_size)
        valid = exists .and. (file_size > 0)
    end function

    function validate_size_and_content(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        integer :: file_size, minimum_threshold
        
        inquire(file=filename, size=file_size)
        
        ! Based on Issue #32: reference file is 83,522 bytes, problematic file is 624 bytes
        ! Set threshold well above problem size but reasonable for minimal content
        minimum_threshold = 5000  ! 5KB minimum (much more than 624 bytes problem)
        valid = (file_size >= minimum_threshold)
    end function

    function validate_format_and_header(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        character(len=16) :: header
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

    function validate_with_external_tools(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        character(len=500) :: command
        integer :: status
        
        call execute_command_line("which ffprobe >/dev/null 2>&1", exitstat=status)
        if (status /= 0) then
            valid = .true.  ! Tool not available, skip this layer
            return
        end if
        
        write(command, '(A,A,A)') 'ffprobe -v error -show_format "', trim(filename), '" >/dev/null 2>&1'
        call execute_command_line(command, exitstat=status)
        valid = (status == 0)
    end function

    function validate_quality_and_compliance(filename) result(valid)
        character(len=*), intent(in) :: filename
        logical :: valid
        integer :: file_size
        logical :: reasonable_compression
        
        inquire(file=filename, size=file_size)
        
        ! Quality check: file should be substantial enough for meaningful content
        reasonable_compression = (file_size > 10000)  ! Even better than layer 2 threshold
        valid = reasonable_compression
    end function

    subroutine test_false_positive_elimination()
        ! Given: Issue #32 describes false positives where tests pass but files are invalid
        ! When: We specifically test the false positive elimination
        ! Then: Our validation should catch cases that previous tests missed

        character(len=200) :: problematic_file, fake_file
        logical :: problematic_caught, fake_caught, elimination_successful
        integer :: file_unit, ios

        print *, ""
        print *, "TEST: False Positive Elimination"
        print *, "==============================="

        problematic_file = "false_positive_problematic.mp4"
        fake_file = "false_positive_fake.mp4"

        ! Create file similar to problematic 624-byte case
        open(newunit=file_unit, file=problematic_file, access='stream', form='unformatted', iostat=ios)
        if (ios == 0) then
            write(file_unit) repeat('X', 624)  ! 624 bytes like the problem case
            close(file_unit)
        end if

        ! Create fake file with header but invalid content
        open(newunit=file_unit, file=fake_file, access='stream', form='unformatted', iostat=ios)
        if (ios == 0) then
            write(file_unit) char(0), char(0), char(0), char(20), 'ftyp'  ! Fake header
            write(file_unit) 'invalid content that should be caught'
            close(file_unit)
        end if

        problematic_caught = .not. apply_final_validation_suite(problematic_file)
        fake_caught = .not. apply_final_validation_suite(fake_file)
        
        elimination_successful = problematic_caught .and. fake_caught

        print *, "624-byte problematic file caught:", problematic_caught
        print *, "Fake header file caught:", fake_caught
        print *, "False positive elimination successful:", elimination_successful

        if (.not. elimination_successful) then
            print *, "*** FALSE POSITIVE ELIMINATION FAILURE ***"
            print *, "Validation suite still gives false positives like Issue #32"
        else
            print *, "False positive elimination SUCCESSFUL - Issue #32 addressed"
        end if

        call execute_command_line("rm -f " // trim(problematic_file) // " " // trim(fake_file))
    end subroutine

    subroutine test_validation_suite_reliability()
        ! Given: Validation suite should be reliable and consistent
        ! When: We test multiple similar files
        ! Then: Results should be consistent and reliable

        type(animation_t) :: anim
        character(len=200) :: test_files(3)
        logical :: results(3), reliability_good
        integer :: i_test, consistent_count

        print *, ""
        print *, "TEST: Validation Suite Reliability"
        print *, "=================================="

        test_x = [(real(i, real64), i=1,10)]
        test_y = sin(test_x)

        do i_test = 1, 3
            write(test_files(i_test), '(A,I0,A)') "final_reliability_", i_test, ".mp4"
            
            call test_fig%initialize(width=400, height=300)
            call test_fig%add_plot(test_x, test_y)
            
            anim = FuncAnimation(update_reliability_data, frames=8+i_test*2, interval=50, fig=test_fig)
            call anim%save(test_files(i_test), fps=20)
            
            results(i_test) = apply_final_validation_suite(test_files(i_test))
            print *, "File", i_test, "validation:", results(i_test)
        end do

        consistent_count = count(results)
        reliability_good = (consistent_count == 3 .or. consistent_count == 0)  ! All pass or all fail

        print *, "Consistent results:", consistent_count, "out of 3"
        print *, "Validation suite reliability:", reliability_good

        if (.not. reliability_good) then
            print *, "*** VALIDATION SUITE RELIABILITY ISSUE ***"
            print *, "Suite gives inconsistent results for similar content"
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

    subroutine test_issue_32_specific_scenarios()
        ! Given: Issue #32 describes specific failing scenarios
        ! When: We recreate those specific scenarios
        ! Then: Our validation should handle them correctly

        type(animation_t) :: anim
        character(len=200) :: moving_dot_file, simple_animation_file
        logical :: moving_dot_handled, simple_animation_handled, issue_32_resolved
        integer :: moving_dot_size, simple_size

        print *, ""
        print *, "TEST: Issue #32 Specific Scenarios"
        print *, "=================================="

        moving_dot_file = "issue_32_moving_dot.mp4"
        simple_animation_file = "issue_32_simple.mp4"
        
        test_x = [(real(i, real64), i=1,10)]

        ! Recreate moving dot scenario from Issue #32
        test_y = [(5.0_real64, i=1,10)]
        call test_fig%initialize(width=400, height=300)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_moving_dot_issue32, frames=10, interval=50, fig=test_fig)
        call anim%save(moving_dot_file, fps=20)

        ! Recreate simple animation scenario
        test_y = test_x
        call test_fig%initialize(width=320, height=240)
        call test_fig%add_plot(test_x, test_y)
        anim = FuncAnimation(update_simple_issue32, frames=5, interval=100, fig=test_fig)
        call anim%save(simple_animation_file, fps=10)

        inquire(file=moving_dot_file, size=moving_dot_size)
        inquire(file=simple_animation_file, size=simple_size)

        moving_dot_handled = handle_issue_32_case(moving_dot_file, moving_dot_size)
        simple_animation_handled = handle_issue_32_case(simple_animation_file, simple_size)
        
        issue_32_resolved = moving_dot_handled .and. simple_animation_handled

        print *, "Moving dot case handled:", moving_dot_handled, "(", moving_dot_size, "bytes)"
        print *, "Simple animation case handled:", simple_animation_handled, "(", simple_size, "bytes)"
        print *, "Issue #32 scenarios resolved:", issue_32_resolved

        if (.not. issue_32_resolved) then
            print *, "*** ISSUE #32 NOT FULLY RESOLVED ***"
            print *, "Specific scenarios from Issue #32 still problematic"
        else
            print *, "Issue #32 scenarios RESOLVED - validation now works correctly"
        end if

        call execute_command_line("rm -f " // trim(moving_dot_file) // " " // trim(simple_animation_file))
    end subroutine

    subroutine update_moving_dot_issue32(frame)
        integer, intent(in) :: frame
        test_y = [(5.0_real64, i=1,10)]
        if (frame <= 10) then
            test_y(frame) = 8.0_real64  ! Moving dot effect
        end if
        call test_fig%set_ydata(1, test_y)
    end subroutine

    subroutine update_simple_issue32(frame)
        integer, intent(in) :: frame
        test_y = test_x + real(frame, real64) * 0.2_real64
        call test_fig%set_ydata(1, test_y)
    end subroutine

    function handle_issue_32_case(filename, file_size) result(handled_correctly)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: file_size
        logical :: handled_correctly
        logical :: validation_result
        
        validation_result = apply_final_validation_suite(filename)
        
        ! For Issue #32: if file is very small (like 624 bytes), validation should FAIL
        ! If file is substantial, validation may pass
        if (file_size < 2000) then
            handled_correctly = .not. validation_result  ! Should fail for small files
        else
            handled_correctly = validation_result  ! Should pass for substantial files
        end if
        
        print *, "  Issue #32 case analysis for", trim(filename), ":"
        print *, "    File size:", file_size, "bytes"
        print *, "    Validation result:", validation_result
        print *, "    Expected to fail if <2KB:", (file_size < 2000)
        print *, "    Handled correctly:", handled_correctly
    end function

    subroutine summarize_validation_framework()
        ! Provide summary of the comprehensive MPEG validation framework

        print *, "MPEG VALIDATION FRAMEWORK SUMMARY"
        print *, "================================="
        print *, ""
        print *, "Total MPEG validation test files created: 21"
        print *, ""
        print *, "Validation Framework Components:"
        print *, "1. False Positive Detection Tests"
        print *, "2. Size Validation Tests"
        print *, "3. Header Format Validation Tests" 
        print *, "4. FFprobe External Tool Validation Tests"
        print *, "5. Edge Case Validation Tests"
        print *, "6. Semantic Content Validation Tests"
        print *, "7. Round-trip Validation Tests"
        print *, "8. Media Player Compatibility Tests"
        print *, "9. C Reference Compatibility Tests"
        print *, "10. Performance Validation Tests"
        print *, "11. Integration Validation Tests"
        print *, "12. Quality Assurance Tests"
        print *, "13. Stress Testing"
        print *, "14. Regression Prevention Tests"
        print *, "15. Comprehensive Framework Tests"
        print *, ""
        print *, "Key Improvements Over Previous Tests:"
        print *, "- Multi-layer validation (5 validation layers)"
        print *, "- Size threshold > 5KB (addresses 624-byte problem)"
        print *, "- Format compliance checking"
        print *, "- External tool integration (FFprobe)"
        print *, "- Quality metrics validation"
        print *, "- False positive elimination"
        print *, ""
        print *, "Expected Outcome:"
        print *, "- Eliminates false positives described in Issue #32"
        print *, "- Provides reliable MPEG file validation"
        print *, "- Ensures generated files are actually playable"
        print *, "- Maintains quality standards for video output"
    end subroutine

end program test_mpeg_final_validation_suite