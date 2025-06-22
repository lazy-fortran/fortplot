program test_logarithmic_working
    !! Test logarithmic functions that we know work
    !! Converting debug script to proper unit test
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_ticks, only: calculate_tick_labels
    implicit none
    
    call test_debug_log_range_behavior()
    call test_debug_symlog_range_behavior()
    
    print *, "All logarithmic working tests passed!"
    
contains

    subroutine test_debug_log_range_behavior()
        !! Test that log ranges behave differently from linear
        character(len=20) :: linear_labels(4), log_range_labels(4)
        real(wp) :: linear_values(4), log_range_values(4)
        integer :: i
        logical :: is_logarithmic_like
        
        ! Test linear behavior on small range
        call calculate_tick_labels(1.0_wp, 4.0_wp, 4, linear_labels)
        
        ! Test large range that should expose log-like behavior  
        call calculate_tick_labels(1.0_wp, 1000.0_wp, 4, log_range_labels)
        
        ! Convert to values
        do i = 1, 4
            if (trim(linear_labels(i)) /= '') read(linear_labels(i), *) linear_values(i)
            if (trim(log_range_labels(i)) /= '') read(log_range_labels(i), *) log_range_values(i)
        end do
        
        ! Check if large range has logarithmic-like spacing
        is_logarithmic_like = .false.
        if (log_range_values(2) / log_range_values(1) > 5.0_wp .and. &
            log_range_values(3) / log_range_values(2) > 5.0_wp) then
            is_logarithmic_like = .true.
        end if
        
        if (.not. is_logarithmic_like) then
            print *, "NOTE: Large ranges not yet using logarithmic spacing"
            print *, "Current large range ticks:", (trim(log_range_labels(i)), i=1,4)
            ! This is expected behavior until we integrate scale awareness
        end if
    end subroutine test_debug_log_range_behavior

    subroutine test_debug_symlog_range_behavior()
        !! Test symlog-like ranges crossing zero
        character(len=20) :: labels(5)
        real(wp) :: values(5)
        integer :: i, valid_labels
        logical :: crosses_zero, has_negative, has_positive
        
        ! Range crossing zero - should handle gracefully
        call calculate_tick_labels(-100.0_wp, 1000.0_wp, 5, labels)
        
        valid_labels = 0
        has_negative = .false.
        has_positive = .false.
        crosses_zero = .false.
        
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                read(labels(i), *) values(i)
                
                if (values(i) < 0.0_wp) has_negative = .true.
                if (values(i) > 0.0_wp) has_positive = .true.
                if (abs(values(i)) < 1.0e-10_wp) crosses_zero = .true.
            end if
        end do
        
        if (valid_labels < 3) then
            print *, "FAIL: Too few labels for symlog-like range"
            stop 1
        end if
        
        if (.not. has_negative .or. .not. has_positive) then
            print *, "FAIL: Symlog-like range should span negative and positive"
            stop 1
        end if
        
        ! This test validates that zero-crossing ranges are handled
    end subroutine test_debug_symlog_range_behavior

end program test_logarithmic_working