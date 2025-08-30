module fortplot_verification_reports
    !! Report generation and CI integration for functionality verification
    !! Split from fortplot_functionality_verification.f90 for file size compliance (Issue #884)
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_verification_core, only: verification_report_t, functionality_verifier_t, &
        create_functionality_verifier, MAX_CATEGORIES
    use fortplot_validation, only: validation_result_t
    implicit none
    
    private
    public :: run_comprehensive_verification, generate_evidence_report, &
              compare_with_baseline_comprehensive, generate_summary_report, export_to_ci_format
    
contains

    !> Generate comprehensive summary  
    subroutine generate_summary_report(report)
        type(verification_report_t), intent(inout) :: report
        character(len=32) :: timestamp
        
        ! Generate timestamp
        write(timestamp, '(a)') "2025-08-29_verification"
        report%report_timestamp = timestamp
        
        ! Generate evidence summary
        write(report%evidence_summary, '(a,i0,a,i0,a,i0,a,f8.4,a)') &
            "VERIFICATION COMPLETE: ", report%passed_tests, "/", report%total_tests, &
            " tests passed, ", report%failed_tests, " failed. Execution time: ", &
            report%total_execution_time, "s"
    end subroutine generate_summary_report
    
    !> Export results in CI-compatible format
    subroutine export_to_ci_format(report, output_file)
        type(verification_report_t), intent(in) :: report
        character(len=*), intent(in) :: output_file
        integer :: file_unit, ios, i
        
        open(newunit=file_unit, file=output_file, iostat=ios)
        if (ios /= 0) return
        
        write(file_unit, '(a)') "# Functionality Verification Report"
        write(file_unit, '(a,a)') "Timestamp: ", trim(report%report_timestamp)
        write(file_unit, '(a,l)') "Overall Status: ", report%overall_passed
        write(file_unit, '(a,i0)') "Total Tests: ", report%total_tests
        write(file_unit, '(a,i0)') "Passed: ", report%passed_tests  
        write(file_unit, '(a,i0)') "Failed: ", report%failed_tests
        write(file_unit, '(a,f8.4)') "Execution Time: ", report%total_execution_time
        write(file_unit, '(a)') ""
        
        write(file_unit, '(a)') "## Category Results"
        do i = 1, MAX_CATEGORIES
            if (report%category_test_count(i) > 0) then
                write(file_unit, '(a,i0,a,l,a,a)') "Category ", i, ": ", &
                    report%category_passed(i), " - ", trim(report%category_messages(i))
            end if
        end do
        
        write(file_unit, '(a)') ""
        write(file_unit, '(a)') "## Technical Evidence"
        write(file_unit, '(a,a)') "Evidence Summary: ", trim(report%evidence_summary)
        
        write(file_unit, '(a)') ""
        write(file_unit, '(a)') "## Performance Metrics"
        write(file_unit, '(a,f8.2,a)') "Performance Regression: ", report%performance_regression_percent, "%"
        write(file_unit, '(a,l)') "Performance Acceptable: ", report%performance_acceptable
        
        write(file_unit, '(a)') ""
        write(file_unit, '(a)') "## Regression Analysis"
        write(file_unit, '(a,i0)') "New Failures: ", report%new_failures_count
        write(file_unit, '(a,i0)') "Fixed Issues: ", report%fixed_issues_count
        write(file_unit, '(a,l)') "Baseline Comparison Success: ", report%baseline_comparison_success
        
        close(file_unit)
    end subroutine export_to_ci_format
    
    !> Main entry point for comprehensive verification
    subroutine run_comprehensive_verification(baseline_dir, output_dir, &
        evidence_dir, report)
        character(len=*), intent(in) :: baseline_dir
        character(len=*), intent(in) :: output_dir
        character(len=*), intent(in) :: evidence_dir  
        type(verification_report_t), intent(out) :: report
        type(functionality_verifier_t) :: verifier
        
        verifier = create_functionality_verifier(baseline_dir, output_dir, &
            evidence_dir)
        call verifier%run_verification_suite(report)
        call generate_summary_report(report)
    end subroutine run_comprehensive_verification
    
    !> Generate technical evidence report
    subroutine generate_evidence_report(report, evidence_file)
        type(verification_report_t), intent(in) :: report
        character(len=*), intent(in) :: evidence_file
        
        call export_to_ci_format(report, evidence_file)
    end subroutine generate_evidence_report
    
    !> Enhanced baseline comparison with comprehensive analysis
    function compare_with_baseline_comprehensive(current_results, &
        baseline_results) result(validation)
        type(verification_report_t), intent(in) :: current_results
        type(verification_report_t), intent(in) :: baseline_results
        type(validation_result_t) :: validation
        
        validation%passed = current_results%overall_passed .and. &
            baseline_results%overall_passed
        validation%metric_value = real(current_results%passed_tests, wp) / &
            real(max(current_results%total_tests, 1), wp)
        
        if (validation%passed) then
            validation%message = "Comprehensive baseline comparison: All functionality preserved"
        else
            write(validation%message, '(a,i0,a,i0,a)') &
                "Baseline regression detected: ", &
                (baseline_results%passed_tests - current_results%passed_tests), &
                " functionality losses, ", &
                (current_results%passed_tests - baseline_results%passed_tests), &
                " improvements"
        end if
    end function compare_with_baseline_comprehensive

end module fortplot_verification_reports