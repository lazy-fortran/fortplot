! fortplot_functionality_verification.f90
! Comprehensive Functionality Preservation Verification System
! Refactored for file size compliance (Issue #884) - delegates to specialized modules
module fortplot_functionality_verification
    use fortplot_verification_core, only: functionality_verifier_t, verification_report_t, baseline_t, &
        create_functionality_verifier
    use fortplot_verification_reports, only: run_comprehensive_verification, generate_evidence_report, &
        compare_with_baseline_comprehensive
    implicit none
    
    private
    public :: functionality_verifier_t, verification_report_t, baseline_t
    public :: create_functionality_verifier, run_comprehensive_verification
    public :: generate_evidence_report, compare_with_baseline_comprehensive

end module fortplot_functionality_verification