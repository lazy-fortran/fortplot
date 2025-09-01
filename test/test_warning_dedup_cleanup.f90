program test_warning_dedup_cleanup
    use, intrinsic :: iso_fortran_env, only: real64
    use fortplot_validation_context, only: validation_warning, &
        reset_warning_tracking, is_warning_tracking_active
    implicit none

    ! Trigger a warning to allocate tracking storage
    call validation_warning("Sample warning to trigger allocation")

    if (.not. is_warning_tracking_active()) then
        print *, "ERROR: Warning tracking should be allocated after first use"
        stop 1
    end if

    ! Reset and ensure storage is released
    call reset_warning_tracking()

    if (is_warning_tracking_active()) then
        print *, "ERROR: Warning tracking should be deallocated after reset"
        stop 2
    end if

    print *, "PASS: Warning deduplication storage cleanup works"
end program test_warning_dedup_cleanup

