program test_context_warning_dedup
    use fortplot_validation_context, only: validation_warning_with_context, &
        reset_warning_tracking, get_warning_count, validation_context_t
    implicit none

    integer :: c0, c1, c2, c3
    type(validation_context_t) :: vctx

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

    ! Now dedup should also work when context comes from validation_context_t
    vctx%context_name = "CTX"
    call validation_warning_with_context("Duplicate message", validation_ctx=vctx)
    c3 = get_warning_count()
    if (c3 .ne. c2) then
        print *, "ERROR: Duplicate via context_name should not increase count"
        stop 3
    end if

    print *, "PASS: Context-aware warning uses deduplication"
end program test_context_warning_dedup
