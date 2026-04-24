program test_datetime_step_selection
    !! Verify pick_fixed_step_seconds produces reasonable tick counts
    !! for common date ranges (issue #1744).

    use, intrinsic :: iso_fortran_env, only: int64
    use fortplot_axes, only: pick_fixed_step_seconds
    implicit none

    integer(int64) :: step_s
    integer(int64), parameter :: sp_day = 86400_int64
    logical :: all_passed

    all_passed = .true.

    call check_step(10*sp_day, 2_int64*sp_day, &
                    '10-day range -> 2-day step (was 7-day)', all_passed)
    call check_step(20*sp_day, 3_int64*sp_day, &
                    '20-day range -> 3-day step (was 7-day)', all_passed)
    call check_step(30*sp_day, 5_int64*sp_day, &
                    '30-day range -> 5-day step (was 7-day)', all_passed)
    call check_step(50*sp_day, 7_int64*sp_day, &
                    '50-day range -> 7-day step', all_passed)
    call check_step(59*sp_day, 14_int64*sp_day, &
                    '59-day range -> 14-day step', all_passed)
    call check_step(1*sp_day, 6_int64*3600_int64, &
                    '1-day range -> 6-hour step', all_passed)
    call check_step(2_int64*3600_int64, 15_int64*60_int64, &
                    '2-hour range -> 15-min step', all_passed)
    call check_step(3600_int64, 15_int64*60_int64, &
                    '1-hour range -> 15-min step', all_passed)

    call check_improved(10*sp_day, 7_int64*sp_day, &
                        '10-day range step < 7 days', all_passed)
    call check_improved(20*sp_day, 7_int64*sp_day, &
                        '20-day range step < 7 days', all_passed)
    call check_improved(30*sp_day, 7_int64*sp_day, &
                        '30-day range step < 7 days', all_passed)

    if (all_passed) then
        print *, 'PASS: datetime step selection'
    else
        stop 1
    end if

contains

    subroutine check_step(range_s, expected, msg, passed)
        integer(int64), intent(in) :: range_s, expected
        character(len=*), intent(in) :: msg
        logical, intent(out) :: passed

        step_s = pick_fixed_step_seconds(range_s)
        passed = (step_s == expected)
        if (.not. passed) then
            print *, 'FAIL:', trim(msg)
            print *, '  range_s  =', range_s, 's'
            print *, '  expected =', expected, 's'
            print *, '  got      =', step_s, 's'
        end if
    end subroutine check_step

    subroutine check_improved(range_s, old_step, msg, passed)
        integer(int64), intent(in) :: range_s, old_step
        character(len=*), intent(in) :: msg
        logical, intent(out) :: passed

        step_s = pick_fixed_step_seconds(range_s)
        passed = (step_s < old_step)
        if (.not. passed) then
            print *, 'FAIL:', trim(msg)
            print *, '  range_s  =', range_s, 's'
            print *, '  old step =', old_step, 's'
            print *, '  new step =', step_s, 's'
        end if
    end subroutine check_improved

end program test_datetime_step_selection
