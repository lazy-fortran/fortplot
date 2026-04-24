program test_maxnlocator_steps
    !! Verify tick step selection uses matplotlib MaxNLocator defaults:
    !! {1, 1.5, 2, 2.5, 3, 4, 5, 6, 8, 10} instead of old {1, 2, 5, 10}
    !! The algorithm picks the first nice step >= normalized rough step.
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_tick_calculation, only: find_nice_tick_locations
    implicit none

    real(wp) :: nice_min, nice_max, nice_step
    real(wp) :: tick_locs(20)
    integer :: actual_num_ticks
    logical :: all_pass

    all_pass = .true.

    ! Range [-3, 3] with 6 target ticks: rough_step=1.5 -> nice step 1.5
    ! Old set {1,2,5,10} would pick step=2 (wrong)
    call find_nice_tick_locations(-3.0_wp, 3.0_wp, 6, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 1.5_wp) > 0.01_wp) then
        print *, 'FAIL: [-3,3] expected step 1.5, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 0.07] with 6 target ticks: rough_step~0.014 -> nice step 0.015
    ! Old set {1,2,5,10} would pick step=0.02 (wrong)
    call find_nice_tick_locations(0.0_wp, 0.07_wp, 6, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 0.015_wp) > 0.001_wp) then
        print *, 'FAIL: [0,0.07] expected step 0.015, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 1] with 6 target ticks: rough_step=0.2 -> nice step 0.2
    call find_nice_tick_locations(0.0_wp, 1.0_wp, 6, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 0.2_wp) > 0.01_wp) then
        print *, 'FAIL: [0,1] expected step 0.2, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 100] with 6 target ticks: rough_step=20 -> nice step 20
    call find_nice_tick_locations(0.0_wp, 100.0_wp, 6, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 20.0_wp) > 1.0_wp) then
        print *, 'FAIL: [0,100] expected step 20, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 13] with 5 target ticks: rough_step=3.25 -> nice step 4
    call find_nice_tick_locations(0.0_wp, 13.0_wp, 5, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 4.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,13] expected step 4, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 17] with 5 target ticks: rough_step=4.25 -> nice step 5
    call find_nice_tick_locations(0.0_wp, 17.0_wp, 5, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 5.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,17] expected step 5, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 23] with 5 target ticks: rough_step=5.75 -> nice step 6
    call find_nice_tick_locations(0.0_wp, 23.0_wp, 5, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 6.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,23] expected step 6, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 31] with 5 target ticks: rough_step=7.75 -> nice step 8
    call find_nice_tick_locations(0.0_wp, 31.0_wp, 5, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 8.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,31] expected step 8, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 29] with 6 target ticks: rough_step=5.8 -> nice step 6
    call find_nice_tick_locations(0.0_wp, 29.0_wp, 6, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 6.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,29] expected step 6, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 19] with 6 target ticks: rough_step=3.8 -> nice step 4
    call find_nice_tick_locations(0.0_wp, 19.0_wp, 6, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 4.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,19] expected step 4, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 11] with 4 target ticks: rough_step=3.67 -> nice step 4
    call find_nice_tick_locations(0.0_wp, 11.0_wp, 4, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 4.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,11] expected step 4, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 7] with 5 target ticks: rough_step=1.75 -> nice step 2.0
    ! Old set {1,2,5,10} would also pick step=2, but now step=2.0 is correct
    ! for ranges where rough_step falls between 1.5 and 2.0
    call find_nice_tick_locations(0.0_wp, 7.0_wp, 5, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 2.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,7] expected step 2.0, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 3] with 5 target ticks: rough_step=0.75 -> step 0.8
    call find_nice_tick_locations(0.0_wp, 3.0_wp, 5, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 0.8_wp) > 0.01_wp) then
        print *, 'FAIL: [0,3] expected step 0.8, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 5] with 4 target ticks: rough_step=1.67 -> nice step 2.0
    call find_nice_tick_locations(0.0_wp, 5.0_wp, 4, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 2.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,5] expected step 2.0, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 9] with 4 target ticks: rough_step=3.0 -> nice step 3.0
    call find_nice_tick_locations(0.0_wp, 9.0_wp, 4, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 3.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,9] expected step 3.0, got', nice_step
        all_pass = .false.
    end if

    ! Range [0, 15] with 4 target ticks: rough_step=5.0 -> nice step 5.0
    call find_nice_tick_locations(0.0_wp, 15.0_wp, 4, &
                                  nice_min, nice_max, nice_step, &
                                  tick_locs, actual_num_ticks)
    if (abs(nice_step - 5.0_wp) > 0.01_wp) then
        print *, 'FAIL: [0,15] expected step 5.0, got', nice_step
        all_pass = .false.
    end if

    if (all_pass) then
        print *, 'PASS: MaxNLocator step selection matches matplotlib defaults'
    else
        stop 1
    end if
end program test_maxnlocator_steps
