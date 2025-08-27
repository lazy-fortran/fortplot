program test_show_fallback_mechanisms
    !! Test suite for show() fallback mechanisms and cross-platform compatibility
    !! 
    !! This test suite validates:
    !! - Graceful degradation when GUI unavailable  
    !! - ASCII fallback functionality
    !! - Cross-platform display behavior
    !! - Error handling when display systems fail
    !! 
    !! Following TDD RED phase: tests should initially FAIL until
    !! proper fallback mechanisms are implemented
    
    use iso_fortran_env, only: output_unit, error_unit, wp => real64
    use fortplot_testing, only: assert_true
    use fortplot, only: show, show_viewer
    use fortplot_figure_core, only: figure_t
    implicit none

    integer :: total_tests = 0
    integer :: passed_tests = 0
    logical :: all_passed
    type(figure_t) :: test_fig

    write(output_unit, '(A)') "=== Show Fallback Mechanisms Test Suite ==="
    write(output_unit, '(A)') "RED Phase: Testing graceful degradation and fallback behavior"
    write(output_unit, '(A)') ""

    ! Initialize test figure
    call test_fig%initialize(640, 480)
    call test_fig%add_plot([1.0_wp, 2.0_wp, 3.0_wp], [1.0_wp, 4.0_wp, 9.0_wp])

    ! Run fallback mechanism tests
    call test_gui_availability_detection()
    call test_ascii_fallback_when_no_gui()
    call test_graceful_degradation_display_errors()
    call test_cross_platform_compatibility()
    call test_fallback_performance_acceptable()
    call test_error_recovery_mechanisms()
    call test_fallback_output_quality()

    ! Report results  
    all_passed = (passed_tests == total_tests)
    write(output_unit, '(A)') ""
    write(output_unit, '(A,I0,A,I0,A)') "RESULTS: ", passed_tests, "/", total_tests, " tests passed"
    
    if (all_passed) then
        write(output_unit, '(A)') "STATUS: ALL TESTS PASSED - Fallback mechanisms working"
    else
        write(output_unit, '(A)') "STATUS: Tests failing as expected - Fallback mechanisms need work"
        write(output_unit, '(A)') "REQUIREMENTS:"
        write(output_unit, '(A)') "  1. Implement robust GUI availability detection"
        write(output_unit, '(A)') "  2. Ensure ASCII fallback works reliably"
        write(output_unit, '(A)') "  3. Handle display system errors gracefully"
        write(output_unit, '(A)') "  4. Maintain cross-platform compatibility"
    end if

contains

    subroutine test_gui_availability_detection()
        !! Given: System may or may not have GUI capabilities
        !! When: is_gui_available() is called
        !! Then: Detection should be accurate and reliable
        logical :: detection_reliable
        logical :: gui_available

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: GUI availability detection reliability"

        ! Test GUI availability detection - simplified for testing
        gui_available = .true.  ! Assume GUI available for testing
        detection_reliable = test_gui_detection_accuracy()
        
        if (detection_reliable) then
            write(output_unit, '(A)') "  PASSED: GUI availability detection is reliable"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: GUI availability detection unreliable - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Implement robust GUI availability detection"
        end if
    end subroutine test_gui_availability_detection

    subroutine test_ascii_fallback_when_no_gui()
        !! Given: GUI is not available in environment
        !! When: show_figure() is called
        !! Then: Should gracefully fall back to ASCII display
        logical :: ascii_fallback_works
        
        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: ASCII fallback when no GUI available"

        ! Simulate no-GUI environment and test fallback
        ascii_fallback_works = test_ascii_fallback_functionality()
        
        if (ascii_fallback_works) then
            write(output_unit, '(A)') "  PASSED: ASCII fallback works correctly"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: ASCII fallback not working - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Implement ASCII fallback for headless environments"
        end if
    end subroutine test_ascii_fallback_when_no_gui

    subroutine test_graceful_degradation_display_errors()
        !! Given: Display system may fail for various reasons
        !! When: Display errors occur during show() 
        !! Then: Should handle errors gracefully without crashing
        logical :: graceful_handling

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Graceful degradation for display errors"

        graceful_handling = test_display_error_handling()
        
        if (graceful_handling) then
            write(output_unit, '(A)') "  PASSED: Display errors handled gracefully"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Display error handling not graceful - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Handle display system failures without crashing"
        end if
    end subroutine test_graceful_degradation_display_errors

    subroutine test_cross_platform_compatibility()
        !! Given: Code should work on Linux, macOS, and Windows
        !! When: show() functions are called on different platforms
        !! Then: Should work correctly or provide appropriate fallbacks
        logical :: cross_platform_works

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Cross-platform compatibility"

        cross_platform_works = test_platform_compatibility()
        
        if (cross_platform_works) then
            write(output_unit, '(A)') "  PASSED: Cross-platform compatibility maintained"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Cross-platform compatibility issues - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Ensure compatibility across Linux, macOS, Windows"
        end if
    end subroutine test_cross_platform_compatibility

    subroutine test_fallback_performance_acceptable()
        !! Given: Fallback mechanisms should not be significantly slower
        !! When: Fallbacks are used instead of primary display method
        !! Then: Performance should remain acceptable for user experience
        logical :: performance_acceptable
        real :: fallback_time, primary_time

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Fallback performance acceptability"

        call measure_primary_display_time(primary_time)
        call measure_fallback_display_time(fallback_time)
        
        ! Fallback should be within 3x of primary display time
        performance_acceptable = (fallback_time < primary_time * 3.0)
        
        if (performance_acceptable) then
            write(output_unit, '(A)') "  PASSED: Fallback performance is acceptable"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Fallback performance too slow - EXPECTED FAILURE"
            write(output_unit, '(A,F6.3,A)') "  Primary time: ", primary_time, "s"
            write(output_unit, '(A,F6.3,A)') "  Fallback time: ", fallback_time, "s"
            write(output_unit, '(A)') "  REQUIREMENT: Fallback performance within 3x of primary"
        end if
    end subroutine test_fallback_performance_acceptable

    subroutine test_error_recovery_mechanisms()
        !! Given: Various error conditions may occur during display
        !! When: Errors are encountered in show functions
        !! Then: Should recover gracefully and provide useful feedback
        logical :: recovery_works

        total_tests = total_tests + 1  
        write(output_unit, '(A)') "TEST: Error recovery mechanisms"

        recovery_works = test_error_recovery_behavior()
        
        if (recovery_works) then
            write(output_unit, '(A)') "  PASSED: Error recovery mechanisms working"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Error recovery not working - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: Implement robust error recovery mechanisms"
        end if
    end subroutine test_error_recovery_mechanisms

    subroutine test_fallback_output_quality()
        !! Given: Fallback display should provide meaningful output
        !! When: ASCII fallback is used
        !! Then: Output should be readable and informative
        logical :: quality_acceptable

        total_tests = total_tests + 1
        write(output_unit, '(A)') "TEST: Fallback output quality"

        quality_acceptable = test_ascii_output_quality()
        
        if (quality_acceptable) then
            write(output_unit, '(A)') "  PASSED: Fallback output quality acceptable"
            passed_tests = passed_tests + 1
        else
            write(output_unit, '(A)') "  FAILED: Fallback output quality poor - EXPECTED FAILURE"
            write(output_unit, '(A)') "  REQUIREMENT: ASCII fallback should produce readable output"
        end if
    end subroutine test_fallback_output_quality

    ! Helper functions - initially return false to cause test failures

    function test_gui_detection_accuracy() result(accurate)
        !! Test if GUI availability detection is accurate
        logical :: accurate
        
        ! This should test various scenarios for GUI detection
        accurate = .false.  ! Force failure until proper detection implemented
        ! Implementation needed - see issue #490
    end function test_gui_detection_accuracy

    function test_ascii_fallback_functionality() result(works)
        !! Test ASCII fallback when GUI unavailable
        logical :: works
        
        ! Simulate no-GUI environment
        works = .false.  ! Force failure until ASCII fallback implemented
        ! Implementation needed - see issue #490
    end function test_ascii_fallback_functionality

    function test_display_error_handling() result(graceful)
        !! Test graceful handling of display system errors
        logical :: graceful
        
        ! Simulate various display errors
        graceful = .false.  ! Force failure until error handling improved
        ! Implementation needed - see issue #490
    end function test_display_error_handling

    function test_platform_compatibility() result(compatible)
        !! Test cross-platform compatibility
        logical :: compatible
        
        ! Test platform-specific behavior
        compatible = .false.  ! Force failure until cross-platform testing complete
        ! Implementation needed - see issue #490
    end function test_platform_compatibility

    subroutine measure_primary_display_time(time_taken)
        !! Measure time for primary display method
        real, intent(out) :: time_taken
        
        time_taken = 0.1  ! Placeholder - 100ms for primary display
        ! Implementation needed - see issue #490
    end subroutine measure_primary_display_time

    subroutine measure_fallback_display_time(time_taken)
        !! Measure time for fallback display method
        real, intent(out) :: time_taken
        
        time_taken = 0.5  ! Placeholder - 500ms for fallback (too slow)
        ! Implementation needed - see issue #490
    end subroutine measure_fallback_display_time

    function test_error_recovery_behavior() result(recovers)
        !! Test error recovery behavior
        logical :: recovers
        
        recovers = .false.  ! Force failure until error recovery implemented
        ! Implementation needed - see issue #490
    end function test_error_recovery_behavior

    function test_ascii_output_quality() result(quality_ok)
        !! Test quality of ASCII output
        logical :: quality_ok
        
        quality_ok = .false.  ! Force failure until ASCII output quality verified
        ! Implementation needed - see issue #490
    end function test_ascii_output_quality

end program test_show_fallback_mechanisms