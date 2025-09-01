program test_warning_mode_suppression
    use fortplot_validation_context, only: validation_warning_with_context, &
        reset_warning_tracking, get_warning_count, validation_context_t, &
        WARNING_MODE_ALL, WARNING_MODE_ERRORS, WARNING_MODE_SILENT
    implicit none

    type(validation_context_t) :: ctx
    integer :: c0, c1

    ! Baseline: warnings should be tracked in ALL mode
    call reset_warning_tracking()
    ctx%warning_mode = WARNING_MODE_ALL
    ctx%suppress_output = .false.
    ctx%context_name = "mode-test"

    call validation_warning_with_context("Should track in ALL", validation_ctx=ctx)
    c0 = get_warning_count()
    if (c0 /= 1) then
        print *, "ERROR: Expected 1 warning tracked in ALL mode, got ", c0
        stop 1
    end if

    ! Errors-only mode: warnings must NOT be tracked
    call reset_warning_tracking()
    ctx%warning_mode = WARNING_MODE_ERRORS
    ctx%suppress_output = .false.
    ctx%context_name = "mode-test"

    call validation_warning_with_context("Should NOT track in ERRORS", validation_ctx=ctx)
    c1 = get_warning_count()
    if (c1 /= 0) then
        print *, "ERROR: Expected 0 tracked in ERRORS mode, got ", c1
        stop 2
    end if

    ! Silent mode: warnings must NOT be tracked
    call reset_warning_tracking()
    ctx%warning_mode = WARNING_MODE_SILENT
    ctx%suppress_output = .false.
    ctx%context_name = "mode-test"

    call validation_warning_with_context("Should NOT track in SILENT", validation_ctx=ctx)
    c1 = get_warning_count()
    if (c1 /= 0) then
        print *, "ERROR: Expected 0 tracked in SILENT mode, got ", c1
        stop 3
    end if

    print *, "PASS: Warning mode suppression works (ERRORS/SILENT)"
end program test_warning_mode_suppression

