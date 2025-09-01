program test_context_warning_dedup
    use fortplot_validation_context, only: validation_warning_with_context, &
        reset_warning_tracking, get_warning_count
    implicit none

    integer :: c0, c1, c2

    call reset_warning_tracking()
    c0 = get_warning_count()

    call validation_warning_with_context("Duplicate message", "CTX")
    c1 = get_warning_count()
    if (c1 .ne. c0 + 1) then
        print *, "ERROR: First context warning should increase count by 1"
        stop 1
    end if

    ! Same message and context should be deduplicated
    call validation_warning_with_context("Duplicate message", "CTX")
    c2 = get_warning_count()
    if (c2 .ne. c1) then
        print *, "ERROR: Duplicate context warning should not increase count"
        stop 2
    end if

    print *, "PASS: Context-aware warning uses deduplication"
end program test_context_warning_dedup

