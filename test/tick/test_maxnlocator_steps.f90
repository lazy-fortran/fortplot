program test_maxnlocator_steps
    !! Verify tick step selection uses matplotlib's AutoLocator step set
    !! {1, 2, 2.5, 5, 10}. This is the locator linear axes render with by
    !! default; the algorithm picks the first nice step >= normalized rough step.
    !! Every expected value below matches matplotlib's
    !! MaxNLocator(steps=[1,2,2.5,5,10], nbins=target-1).tick_values for the
    !! same range.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_tick_calculation, only: find_nice_tick_locations
    implicit none

    logical :: all_pass

    all_pass = .true.

    call check(-3.0_wp, 3.0_wp, 6, 2.0_wp, '[-3,3]')
    call check(0.0_wp, 0.07_wp, 6, 0.02_wp, '[0,0.07]')
    call check(0.0_wp, 1.0_wp, 6, 0.2_wp, '[0,1]')
    call check(0.0_wp, 100.0_wp, 6, 20.0_wp, '[0,100]')
    call check(0.0_wp, 13.0_wp, 5, 5.0_wp, '[0,13]')
    call check(0.0_wp, 17.0_wp, 5, 5.0_wp, '[0,17]')
    call check(0.0_wp, 23.0_wp, 5, 10.0_wp, '[0,23]')
    call check(0.0_wp, 31.0_wp, 5, 10.0_wp, '[0,31]')
    call check(0.0_wp, 29.0_wp, 6, 10.0_wp, '[0,29]')
    call check(0.0_wp, 19.0_wp, 6, 5.0_wp, '[0,19]')
    call check(0.0_wp, 11.0_wp, 4, 5.0_wp, '[0,11]')
    call check(0.0_wp, 7.0_wp, 5, 2.0_wp, '[0,7]')
    call check(0.0_wp, 3.0_wp, 5, 1.0_wp, '[0,3]')
    call check(0.0_wp, 5.0_wp, 4, 2.0_wp, '[0,5]')
    call check(0.0_wp, 9.0_wp, 4, 5.0_wp, '[0,9]')
    call check(0.0_wp, 15.0_wp, 4, 5.0_wp, '[0,15]')

    if (all_pass) then
        print *, 'PASS: tick step selection matches matplotlib AutoLocator'
    else
        stop 1
    end if

contains

    subroutine check(vmin, vmax, target_ticks, expected_step, label)
        real(wp), intent(in) :: vmin, vmax, expected_step
        integer, intent(in) :: target_ticks
        character(len=*), intent(in) :: label
        real(wp) :: nice_min, nice_max, nice_step, tick_locs(20)
        integer :: actual_num_ticks
        real(wp) :: tol

        call find_nice_tick_locations(vmin, vmax, target_ticks, &
                                      nice_min, nice_max, nice_step, &
                                      tick_locs, actual_num_ticks)
        tol = max(0.001_wp, 0.01_wp*abs(expected_step))
        if (abs(nice_step - expected_step) > tol) then
            print *, 'FAIL: ', label, ' expected step', expected_step, &
                'got', nice_step
            all_pass = .false.
        end if
    end subroutine check

end program test_maxnlocator_steps
