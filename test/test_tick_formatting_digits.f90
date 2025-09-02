program test_tick_formatting_digits
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_axes, only: compute_scale_ticks, MAX_TICKS
    use fortplot_tick_calculation, only: determine_decimal_places_from_step, &
        format_tick_value_consistent
    implicit none

    real(wp) :: ticks(MAX_TICKS)
    integer :: nt, i, decimals
    real(wp) :: step
    character(len=32) :: label
    logical :: ok

    ! Case 1: Linear range [0, 1] should produce ~0.2 step and 1 decimal
    call compute_scale_ticks('linear', 0.0_wp, 1.0_wp, 1.0_wp, ticks, nt)
    if (nt < 2) then
        print *, 'FAIL: Expected at least 2 ticks for range [0,1], got', nt
        stop 1
    end if
    step = abs(ticks(2) - ticks(1))
    do i = 3, nt
        if (abs(ticks(i) - ticks(i-1)) > 1.0e-12_wp) then
            step = min(step, abs(ticks(i) - ticks(i-1)))
        end if
    end do
    decimals = determine_decimal_places_from_step(step)
    if (decimals /= 1) then
        print *, 'FAIL: Expected 1 decimal for [0,1], got', decimals
        stop 1
    end if
    label = format_tick_value_consistent(ticks(2), decimals)
    ok = index(trim(label), '0.200') == 0 .and. index(trim(label), '0.20') == 0
    if (.not. ok) then
        print *, 'FAIL: Label has too many digits: ', trim(label)
        stop 1
    end if

    print *, 'PASS: tick formatting digit selection'
end program test_tick_formatting_digits

