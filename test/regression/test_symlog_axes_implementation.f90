program test_symlog_axes_implementation
    !! Test symlog tick generation implementation in fortplot_axes module
    !! Tests edge cases and proper region handling for symmetric logarithmic scale
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_axes, only: compute_scale_ticks, MAX_TICKS
    implicit none
    
    call test_symlog_all_positive()
    call test_symlog_all_negative()  
    call test_symlog_spanning_zero()
    call test_symlog_small_threshold()
    call test_symlog_large_threshold()
    call test_symlog_edge_boundaries()
    call test_symlog_single_region_cases()
    
    print *, "All symlog axes implementation tests passed!"
    
contains

    subroutine test_symlog_all_positive()
        !! Test symlog with all positive values
        real(wp) :: tick_positions(MAX_TICKS)
        integer :: num_ticks
        real(wp), parameter :: threshold = 10.0_wp
        
        call compute_scale_ticks('symlog', 1.0_wp, 1000.0_wp, threshold, tick_positions, num_ticks)
        
        if (num_ticks < 2) then
            print *, "FAIL: symlog all positive should generate multiple ticks"
            print *, "Got", num_ticks, "ticks for range 1 to 1000 with threshold", threshold
            stop 1
        end if
        
        ! Should have ticks in positive log region only
        if (any(tick_positions(1:num_ticks) <= 0.0_wp)) then
            print *, "FAIL: symlog all positive should not have negative ticks"
            stop 1
        end if
        
        ! Should include values > threshold in log region
        if (.not. any(tick_positions(1:num_ticks) > threshold)) then
            print *, "FAIL: symlog all positive should have ticks in log region"
            stop 1
        end if
    end subroutine test_symlog_all_positive

    subroutine test_symlog_all_negative()
        !! Test symlog with all negative values
        real(wp) :: tick_positions(MAX_TICKS)
        integer :: num_ticks, i
        real(wp), parameter :: threshold = 5.0_wp
        
        call compute_scale_ticks('symlog', -500.0_wp, -1.0_wp, threshold, tick_positions, num_ticks)
        
        if (num_ticks < 2) then
            print *, "FAIL: symlog all negative should generate multiple ticks"
            print *, "Got", num_ticks, "ticks for range -500 to -1 with threshold", threshold
            stop 1
        end if
        
        ! Should have only negative ticks
        if (any(tick_positions(1:num_ticks) >= 0.0_wp)) then
            print *, "FAIL: symlog all negative should not have positive ticks"
            stop 1
        end if
        
        ! Should include values < -threshold in negative log region
        if (.not. any(tick_positions(1:num_ticks) < -threshold)) then
            print *, "FAIL: symlog all negative should have ticks in negative log region"
            print *, "No ticks found < -threshold (", -threshold, ")"
            stop 1
        end if
    end subroutine test_symlog_all_negative

    subroutine test_symlog_spanning_zero()
        !! Test symlog spanning zero (most common case)
        real(wp) :: tick_positions(MAX_TICKS)
        integer :: num_ticks
        real(wp), parameter :: threshold = 1.0_wp
        
        call compute_scale_ticks('symlog', -100.0_wp, 100.0_wp, threshold, tick_positions, num_ticks)
        
        if (num_ticks < 3) then
            print *, "FAIL: symlog spanning zero should generate many ticks"
            print *, "Got", num_ticks, "ticks for range -100 to 100 with threshold", threshold
            stop 1
        end if
        
        ! Should have ticks in all three regions
        if (.not. any(tick_positions(1:num_ticks) < -threshold)) then
            print *, "FAIL: symlog spanning zero should have negative log region ticks"
            stop 1
        end if
        
        if (.not. any(abs(tick_positions(1:num_ticks)) <= threshold)) then
            print *, "FAIL: symlog spanning zero should have linear region ticks"
            stop 1
        end if
        
        if (.not. any(tick_positions(1:num_ticks) > threshold)) then
            print *, "FAIL: symlog spanning zero should have positive log region ticks"
            stop 1
        end if
    end subroutine test_symlog_spanning_zero

    subroutine test_symlog_small_threshold()
        !! Test symlog with very small threshold
        real(wp) :: tick_positions(MAX_TICKS)
        integer :: num_ticks
        real(wp), parameter :: threshold = 0.1_wp
        
        call compute_scale_ticks('symlog', -10.0_wp, 10.0_wp, threshold, tick_positions, num_ticks)
        
        if (num_ticks < 2) then
            print *, "FAIL: symlog small threshold should generate ticks"
            print *, "Got", num_ticks, "ticks for range -10 to 10 with threshold", threshold
            stop 1
        end if
        
        ! With small threshold, most data should be in log regions
        if (.not. any(tick_positions(1:num_ticks) < -threshold)) then
            print *, "FAIL: symlog small threshold should have negative log ticks"
            stop 1
        end if
        
        if (.not. any(tick_positions(1:num_ticks) > threshold)) then
            print *, "FAIL: symlog small threshold should have positive log ticks"
            stop 1
        end if
    end subroutine test_symlog_small_threshold

    subroutine test_symlog_large_threshold()
        !! Test symlog with large threshold
        real(wp) :: tick_positions(MAX_TICKS)
        integer :: num_ticks
        real(wp), parameter :: threshold = 50.0_wp
        
        call compute_scale_ticks('symlog', -10.0_wp, 10.0_wp, threshold, tick_positions, num_ticks)
        
        if (num_ticks < 2) then
            print *, "FAIL: symlog large threshold should generate ticks"
            print *, "Got", num_ticks, "ticks for range -10 to 10 with threshold", threshold
            stop 1
        end if
        
        ! With large threshold, all data should be in linear region
        if (any(abs(tick_positions(1:num_ticks)) > threshold)) then
            print *, "FAIL: symlog large threshold should only have linear region ticks"
            print *, "Found tick outside threshold:", maxval(abs(tick_positions(1:num_ticks)))
            stop 1
        end if
    end subroutine test_symlog_large_threshold

    subroutine test_symlog_edge_boundaries()
        !! Test symlog at exact threshold boundaries
        real(wp) :: tick_positions(MAX_TICKS)
        integer :: num_ticks
        real(wp), parameter :: threshold = 10.0_wp
        
        ! Data range exactly at boundaries
        call compute_scale_ticks('symlog', -threshold, threshold, threshold, tick_positions, num_ticks)
        
        if (num_ticks < 1) then
            print *, "FAIL: symlog at boundaries should generate ticks"
            print *, "Got", num_ticks, "ticks for boundary case"
            stop 1
        end if
        
        ! All ticks should be within or at threshold boundaries
        if (any(tick_positions(1:num_ticks) < -threshold - 1.0e-10_wp) .or. &
            any(tick_positions(1:num_ticks) > threshold + 1.0e-10_wp)) then
            print *, "FAIL: symlog boundary ticks should be within threshold"
            stop 1
        end if
    end subroutine test_symlog_edge_boundaries

    subroutine test_symlog_single_region_cases()
        !! Test symlog cases that only touch single regions
        real(wp) :: tick_positions(MAX_TICKS)
        integer :: num_ticks, i
        real(wp), parameter :: threshold = 1.0_wp
        
        ! Test only linear region
        call compute_scale_ticks('symlog', -0.5_wp, 0.5_wp, threshold, tick_positions, num_ticks)
        
        if (num_ticks < 1) then
            print *, "FAIL: symlog linear-only should generate ticks"
            stop 1
        end if
        
        ! All ticks should be in linear region
        if (any(abs(tick_positions(1:num_ticks)) > threshold)) then
            print *, "FAIL: symlog linear-only should only have linear ticks"
            stop 1
        end if
        
        ! Test only positive log region  
        call compute_scale_ticks('symlog', 10.0_wp, 1000.0_wp, threshold, tick_positions, num_ticks)
        
        if (num_ticks < 1) then
            print *, "FAIL: symlog positive-log-only should generate ticks"
            stop 1
        end if
        
        ! All ticks should be positive and > threshold
        if (any(tick_positions(1:num_ticks) <= threshold)) then
            print *, "FAIL: symlog positive-log-only should only have positive log ticks"
            stop 1
        end if
        
        ! Test only negative log region
        call compute_scale_ticks('symlog', -1000.0_wp, -10.0_wp, threshold, tick_positions, num_ticks)
        
        if (num_ticks < 1) then
            print *, "FAIL: symlog negative-log-only should generate ticks"
            stop 1
        end if
        
        ! All ticks should be negative and < -threshold
        if (any(tick_positions(1:num_ticks) >= -threshold)) then
            print *, "FAIL: symlog negative-log-only should only have negative log ticks"
            stop 1
        end if
    end subroutine test_symlog_single_region_cases

end program test_symlog_axes_implementation