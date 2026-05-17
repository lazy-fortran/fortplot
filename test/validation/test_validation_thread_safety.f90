! Ensure warning deduplication is atomic and emits once per unique key
program test_validation_thread_safety
    use fortplot_validation_context, only: validation_warning, &
        validation_warning_with_context, validation_context_t, &
        reset_warning_tracking, get_warning_count, WARNING_MODE_ALL
    implicit none

    type(validation_context_t) :: ctx
    integer :: count

    call reset_warning_tracking()

    call validation_warning("Repeated warning", "unit-test")
    call validation_warning("Repeated warning", "unit-test")
    call validation_warning("Repeated warning", "unit-test")

    count = get_warning_count()
    if (count /= 1) then
        print *, "Expected 1 tracked warning, got ", count
        stop 1
    end if

    call reset_warning_tracking()

    ctx%warning_mode = WARNING_MODE_ALL
    ctx%suppress_output = .false.
    ctx%context_name = "ctx-name"

    call validation_warning_with_context("Repeated warning 2", "unit-test-2", ctx)
    call validation_warning_with_context("Repeated warning 2", "unit-test-2", ctx)

    count = get_warning_count()
    if (count /= 1) then
        print *, "Expected 1 tracked warning after context calls, got ", count
        stop 1
    end if

    print *, "Thread-safety dedup test passed"
end program test_validation_thread_safety

