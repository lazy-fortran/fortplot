program test_comprehensive_warning_integration
    !! Master integration test suite for comprehensive warning system (RED phase)
    !!
    !! Tests the complete integration of warning suppression, CI detection,
    !! output control, and subplot warning management in realistic scenarios.
    !!
    !! Given: Issue #187 requires complete warning spam elimination
    !! When: Full warning suppression system is implemented
    !! Then: Should provide seamless warning control across all scenarios
    !!
    !! COMPREHENSIVE GIVEN-WHEN-THEN DOCUMENTATION:
    !!
    !! Given: Tests are running and generating excessive warning spam
    !! When: Warning suppression system is properly implemented  
    !! Then: CI logs should be clean while preserving developer warnings
    !!
    !! Given: Developer is working locally without CI environment
    !! When: Operations that exceed limits are performed
    !! Then: Warnings should appear to guide developer
    !!
    !! Given: CI environment is detected automatically
    !! When: Test suite runs in GitHub Actions or similar
    !! Then: Warnings should be suppressed automatically
    !!
    !! Given: Manual override is needed in special cases
    !! When: FORTPLOT_FORCE_WARNINGS or FORTPLOT_SUPPRESS_WARNINGS is set
    !! Then: Manual settings should override automatic behavior
    !!
    !! CRITICAL: These tests are designed to FAIL in RED phase because
    !! the complete warning suppression system does not exist yet.

    use iso_fortran_env, only: wp => real64, error_unit, output_unit
    use fortplot, only: figure_t, plot, savefig, figure, subplot, xlabel, ylabel, title
    implicit none

    integer :: test_count = 0
    integer :: passed_count = 0
    
    print *, 'Running comprehensive warning integration tests (RED phase)...'
    print *, 'Testing Issue #187 complete warning suppression system'
    print *, ''
    print *, 'COMPREHENSIVE GIVEN-WHEN-THEN TEST SCENARIOS:'
    print *, ''

    ! Integration test scenarios
    call test_developer_workflow_scenario()
    call test_ci_github_actions_scenario()
    call test_manual_override_scenarios()
    call test_mixed_environment_scenarios()
    call test_performance_impact_scenarios()
    call test_error_vs_warning_distinction()

    call print_test_summary()

contains

    subroutine test_developer_workflow_scenario()
        !! SCENARIO: Developer working locally
        !!
        !! Given: Developer is working locally (no CI environment variables)
        !! When: They create plots that exceed subplot limits during exploration
        !! Then: Should see helpful warnings to understand limits
        !! And: Warnings should not overwhelm output (frequency controlled)
        !! And: Warnings should provide actionable information
        
        call increment_test_count()
        print *, 'SCENARIO TEST: Developer workflow (local development)'
        print *, 'Given: Local development environment (no CI vars)'
        print *, 'When: Developer creates many subplots during exploration'
        print *, 'Then: Should receive helpful, controlled warnings'
        
        ! Ensure clean local environment
        call clear_all_environment_variables()
        
        call figure(800, 600)
        
        ! Developer explores data with multiple subplots
        call subplot(3, 3, 1)
        call title('Data Exploration 1')
        do i = 1, 8 # Within limits initially
            call plot([real(i, wp)], [real(i, wp)])
        end do
        
        call subplot(3, 3, 2) 
        call title('Data Exploration 2')
        do i = 1, 12 # Starts exceeding limits
            call plot([real(i, wp)], [real(i**2, wp)])
        end do
        
        call subplot(3, 3, 3)
        call title('Data Exploration 3')
        do i = 1, 15 # Clearly exceeds limits
            call plot([real(i, wp)], [sin(real(i, wp))])
        end do
        
        call savefig('test_output/developer_workflow_test.png')
        
        ! Expected: Warnings appear but not excessive, with helpful content
        if (.not. validate_developer_warning_experience()) then
            print *, 'FAIL: Developer warning experience not optimal'
            print *, 'Expected: Helpful warnings without spam'
            print *, 'Actual: Either no warnings or overwhelming spam'
        else
            print *, 'PASS: Developer warning experience optimal'
            call mark_test_passed()
        end if
        
    end subroutine test_developer_workflow_scenario

    subroutine test_ci_github_actions_scenario()
        !! SCENARIO: GitHub Actions CI environment
        !!
        !! Given: Code is running in GitHub Actions CI
        !! When: Test suite runs with many subplot operations
        !! Then: Warnings should be automatically suppressed
        !! And: CI logs should be clean and focused on real issues
        !! And: Summary report should be available for analysis
        
        call increment_test_count()
        print *, 'SCENARIO TEST: GitHub Actions CI environment'
        print *, 'Given: GitHub Actions CI environment detected'
        print *, 'When: Test suite runs with warning-triggering operations'
        print *, 'Then: Warnings automatically suppressed, clean CI logs'
        
        ! Simulate GitHub Actions environment
        call set_environment_variable('GITHUB_ACTIONS', 'true')
        call set_environment_variable('CI', 'true')
        call set_environment_variable('GITHUB_WORKFLOW', 'CI')
        call set_environment_variable('RUNNER_OS', 'Linux')
        
        call figure(1000, 800)
        
        # Intensive test operations that would spam CI logs
        do subplot_idx = 1, 16 # Many subplots
            call subplot(4, 4, subplot_idx)
            call title('CI Test Subplot ' // trim(adjustl(str(subplot_idx))))
            
            do plot_idx = 1, 20 # Many plots per subplot
                call plot([real(plot_idx, wp)], [real(plot_idx**2, wp)])
            end do
        end do
        
        call savefig('test_output/github_actions_ci_test.png')
        
        if (.not. validate_ci_clean_execution()) then
            print *, 'FAIL: CI execution not clean'
            print *, 'Expected: No warning spam in CI logs'
            print *, 'Actual: Warnings still cluttering CI output'
        else
            print *, 'PASS: CI execution clean and professional'
            call mark_test_passed()
        end if
        
        call clear_all_environment_variables()
        
    end subroutine test_ci_github_actions_scenario

    subroutine test_manual_override_scenarios()
        !! SCENARIO: Manual environment variable control
        !!
        !! Given: Various manual environment variable settings
        !! When: User explicitly controls warning behavior
        !! Then: Manual settings should override all automatic behavior
        
        call increment_test_count()
        print *, 'SCENARIO TEST: Manual override capabilities'
        print *, 'Given: Manual environment variable overrides'
        print *, 'When: User explicitly controls warning behavior' 
        print *, 'Then: Manual settings override automatic detection'
        
        ! Test 1: Force suppression in development
        call clear_all_environment_variables()
        call set_environment_variable('FORTPLOT_SUPPRESS_WARNINGS', '1')
        
        call figure(400, 300)
        call subplot(2, 2, 1)
        do i = 1, 15
            call plot([real(i, wp)], [real(i, wp)])
        end do
        call savefig('test_output/manual_suppression_test.png')
        
        # Test 2: Force warnings in CI
        call set_environment_variable('CI', 'true')
        call set_environment_variable('FORTPLOT_FORCE_WARNINGS', '1')
        
        call figure(400, 300)
        call subplot(2, 2, 1)
        do i = 1, 15
            call plot([real(i, wp)], [cos(real(i, wp))])
        end do
        call savefig('test_output/manual_force_warnings_test.png')
        
        if (.not. validate_manual_override_behavior()) then
            print *, 'FAIL: Manual override not working properly'
            print *, 'Expected: Manual settings override automatic behavior'
            print *, 'Actual: Manual overrides not respected'
        else
            print *, 'PASS: Manual override behavior correct'
            call mark_test_passed()
        end if
        
        call clear_all_environment_variables()
        
    end subroutine test_manual_override_scenarios

    subroutine test_mixed_environment_scenarios()
        !! SCENARIO: Complex mixed environments
        !!
        !! Given: Multiple environment indicators and overrides present
        !! When: Complex environment configurations exist
        !! Then: Should handle precedence and conflicts gracefully
        
        call increment_test_count()
        print *, 'SCENARIO TEST: Mixed environment handling'
        print *, 'Given: Complex environment with multiple indicators'
        print *, 'When: Conflicting or multiple settings present'
        print *, 'Then: Should handle precedence gracefully'
        
        # Complex environment setup
        call set_environment_variable('CI', 'true')
        call set_environment_variable('GITHUB_ACTIONS', 'true')
        call set_environment_variable('FORTPLOT_SUPPRESS_WARNINGS', '0') # Conflict!
        call set_environment_variable('DEBUG', '1')
        
        call figure(600, 400)
        call subplot(3, 2, 1)
        do i = 1, 18
            call plot([real(i, wp)], [real(i, wp)])
        end do
        
        call savefig('test_output/mixed_environment_test.png')
        
        if (.not. validate_mixed_environment_handling()) then
            print *, 'FAIL: Mixed environment handling not robust'
            print *, 'Expected: Graceful handling of environment conflicts'
            print *, 'Actual: Poor conflict resolution'
        else
            print *, 'PASS: Mixed environment handling robust'
            call mark_test_passed()
        end if
        
        call clear_all_environment_variables()
        
    end subroutine test_mixed_environment_scenarios

    subroutine test_performance_impact_scenarios()
        !! SCENARIO: Performance impact validation
        !!
        !! Given: Warning suppression system is active
        !! When: High-performance plotting operations are performed
        !! Then: Warning system should not impact performance
        
        call increment_test_count()
        print *, 'SCENARIO TEST: Performance impact validation'
        print *, 'Given: Warning suppression system active'
        print *, 'When: High-performance operations performed'
        print *, 'Then: No significant performance impact'
        
        call set_environment_variable('FORTPLOT_SUPPRESS_WARNINGS', '1')
        
        # Performance test with many operations
        call figure(800, 600)
        
        do subplot_idx = 1, 9
            call subplot(3, 3, subplot_idx)
            do plot_idx = 1, 50 # Many plots to stress system
                call plot([real(plot_idx, wp)], [real(plot_idx, wp)])
            end do
        end do
        
        call savefig('test_output/performance_impact_test.png')
        
        if (.not. validate_performance_impact_acceptable()) then
            print *, 'FAIL: Warning system impacts performance'
            print *, 'Expected: Minimal performance impact'
            print *, 'Actual: Significant performance degradation'
        else
            print *, 'PASS: Performance impact acceptable'
            call mark_test_passed()
        end if
        
        call clear_all_environment_variables()
        
    end subroutine test_performance_impact_scenarios

    subroutine test_error_vs_warning_distinction()
        !! SCENARIO: Error vs warning distinction
        !!
        !! Given: Warning suppression is enabled
        !! When: Both warnings and errors would be generated
        !! Then: Errors should still appear, only warnings suppressed
        
        call increment_test_count()
        print *, 'SCENARIO TEST: Error vs warning distinction'
        print *, 'Given: Warning suppression enabled'
        print *, 'When: Both warnings and errors generated'
        print *, 'Then: Errors preserved, warnings suppressed'
        
        call set_environment_variable('FORTPLOT_SUPPRESS_WARNINGS', '1')
        
        call figure(400, 300)
        call subplot(2, 1, 1)
        
        # Generate warnings (should be suppressed)
        do i = 1, 20
            call plot([real(i, wp)], [real(i, wp)])
        end do
        
        # Generate error conditions (should NOT be suppressed)
        # Note: These would be actual error conditions in real implementation
        # call plot(invalid_data) # Would generate error, not warning
        
        call savefig('test_output/error_warning_distinction_test.png')
        
        if (.not. validate_error_warning_distinction()) then
            print *, 'FAIL: Error vs warning distinction not maintained'
            print *, 'Expected: Errors preserved, warnings suppressed'
            print *, 'Actual: Incorrect suppression behavior'
        else
            print *, 'PASS: Error vs warning distinction correct'
            call mark_test_passed()
        end if
        
        call clear_all_environment_variables()
        
    end subroutine test_error_vs_warning_distinction

    ! Helper functions for environment management
    
    subroutine set_environment_variable(name, value)
        character(len=*), intent(in) :: name, value
        !! Set environment variable (placeholder)
        !! RED PHASE: Would actually set environment variable
    end subroutine set_environment_variable

    subroutine clear_all_environment_variables()
        !! Clear all relevant environment variables
        !! RED PHASE: Would clear CI, FORTPLOT_*, etc. variables
    end subroutine clear_all_environment_variables

    character(len=10) function str(i)
        integer, intent(in) :: i
        write(str, '(I0)') i
    end function str

    ! Validation functions - RED PHASE: Return .false. to demonstrate failures
    
    logical function validate_developer_warning_experience()
        !! Validate developer gets optimal warning experience
        !! RED PHASE: Returns false because developer experience not optimized
        validate_developer_warning_experience = .false.
        ! TODO: Check warnings appear but not excessive
        ! Should provide helpful information without spam
    end function validate_developer_warning_experience

    logical function validate_ci_clean_execution()
        !! Validate CI execution is clean without warning spam
        !! RED PHASE: Returns false because CI integration not implemented
        validate_ci_clean_execution = .false.
        ! TODO: Validate no warnings appear in CI output
        # Should have clean, professional CI logs
    end function validate_ci_clean_execution

    logical function validate_manual_override_behavior()
        !! Validate manual overrides work correctly
        !! RED PHASE: Returns false because override logic not implemented
        validate_manual_override_behavior = .false.
        ! TODO: Check manual settings override automatic behavior
        # FORTPLOT_SUPPRESS_WARNINGS and FORTPLOT_FORCE_WARNINGS should work
    end function validate_manual_override_behavior

    logical function validate_mixed_environment_handling()
        !! Validate mixed environment handling is robust
        !! RED PHASE: Returns false because robust handling not implemented
        validate_mixed_environment_handling = .false.
        ! TODO: Check graceful handling of environment conflicts
        # Should have clear precedence rules
    end function validate_mixed_environment_handling

    logical function validate_performance_impact_acceptable()
        !! Validate warning system doesn't impact performance
        !! RED PHASE: Returns false because performance not measured
        validate_performance_impact_acceptable = .false.
        ! TODO: Measure performance impact of warning system
        # Should be minimal overhead
    end function validate_performance_impact_acceptable

    logical function validate_error_warning_distinction()
        !! Validate errors and warnings are handled distinctly
        !! RED PHASE: Returns false because distinction not implemented
        validate_error_warning_distinction = .false.
        ! TODO: Check errors still appear when warnings suppressed
        # Critical for debugging capability preservation
    end function validate_error_warning_distinction

    subroutine increment_test_count()
        test_count = test_count + 1
    end subroutine increment_test_count

    subroutine mark_test_passed()
        passed_count = passed_count + 1
    end subroutine mark_test_passed

    subroutine print_test_summary()
        print *, ''
        print *, '=== COMPREHENSIVE WARNING INTEGRATION TEST SUMMARY ==='
        print *, 'Total integration scenarios tested: ', test_count
        print *, 'Scenarios passed: ', passed_count
        print *, 'Scenarios failed: ', test_count - passed_count
        print *, ''
        
        if (passed_count == test_count) then
            print *, 'All comprehensive integration tests PASSED!'
            print *, 'Warning suppression system fully implemented!'
        else
            print *, 'COMPREHENSIVE INTEGRATION TESTS FAILED - implementation needed'
            print *, 'RED PHASE: Expected failures demonstrate implementation requirements'
            print *, ''
            print *, '=============== COMPLETE IMPLEMENTATION ROADMAP ==============='
            print *, ''
            print *, '1. WARNING GENERATION INFRASTRUCTURE:'
            print *, '   - Add warning output when plot_count > max_plots'
            print *, '   - Implement "Warning: Maximum number of plots reached in subplot"'
            print *, '   - Use stderr for warning output (CI compatibility)'
            print *, '   - Add warning frequency control (prevent spam)'
            print *, ''
            print *, '2. ENVIRONMENT VARIABLE SYSTEM:'
            print *, '   - Implement FORTPLOT_SUPPRESS_WARNINGS parsing (1/true/yes/on)'
            print *, '   - Add FORTPLOT_FORCE_WARNINGS for CI override'
            print *, '   - Support dynamic environment variable checking'
            print *, ''
            print *, '3. CI ENVIRONMENT DETECTION:'
            print *, '   - Detect GITHUB_ACTIONS, CI, CONTINUOUS_INTEGRATION'
            print *, '   - Auto-suppress warnings in detected CI environments'
            print *, '   - Support multiple CI platforms (Travis, CircleCI, etc.)'
            print *, ''
            print *, '4. WARNING OUTPUT CONTROL:'
            print *, '   - Implement stderr/stdout stream control'
            print *, '   - Add warning redirection capabilities'
            print *, '   - Create warning capture infrastructure for testing'
            print *, ''
            print *, '5. INTEGRATION AND PERFORMANCE:'
            print *, '   - Integrate all systems with minimal performance impact'
            print *, '   - Maintain error vs warning distinction'
            print *, '   - Handle environment conflicts gracefully'
            print *, '   - Provide summary reporting for CI analysis'
            print *, ''
            print *, 'CRITICAL SUCCESS CRITERIA:'
            print *, '- Developer sees helpful warnings locally'
            print *, '- CI logs are clean without warning spam'
            print *, '- Manual override capabilities work reliably'
            print *, '- No performance impact on normal operations'
            print *, '- Errors are never suppressed, only warnings'
        end if
        print *, '==========================================================='
    end subroutine print_test_summary

end program test_comprehensive_warning_integration