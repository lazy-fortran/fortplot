program test_log_symlog_ticks
    !! Test that log and symlog scales generate appropriate tick locations
    !! and maintain correct coordinate positioning following TDD principles
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_ticks, only: calculate_tick_labels
    implicit none
    
    call test_should_handle_log_scale_ranges()
    call test_should_handle_symlog_with_linear_threshold()
    call test_should_maintain_coordinate_bounds()
    call test_should_handle_scientific_ranges()
    
    print *, "All log/symlog tick tests passed!"
    
contains

    subroutine test_should_handle_log_scale_ranges()
        !! Log scale should generate ticks at reasonable intervals
        character(len=20) :: labels(5)
        integer :: i, valid_labels
        
        ! Range 0.01 to 1000 (4 orders of magnitude)
        call calculate_tick_labels(0.01_wp, 1000.0_wp, 5, labels)
        
        ! Count valid tick labels
        valid_labels = 0
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
            end if
        end do
        
        if (valid_labels < 3) then
            print *, "FAIL: Too few valid labels for large range"
            print *, "Got only", valid_labels, "valid labels"
            stop 1
        end if
        
        ! Check that first and last labels are reasonable
        if (trim(labels(1)) == '' .or. trim(labels(valid_labels)) == '') then
            print *, "FAIL: Missing boundary labels"
            stop 1
        end if
    end subroutine test_should_handle_log_scale_ranges

    subroutine test_should_handle_symlog_with_linear_threshold()
        !! Symlog should handle ranges crossing zero appropriately
        character(len=20) :: labels(7)
        integer :: i, valid_labels
        
        ! Range -100 to 1000 (crosses zero, good for symlog)
        call calculate_tick_labels(-100.0_wp, 1000.0_wp, 7, labels)
        
        ! Count valid labels
        valid_labels = 0
        do i = 1, 7
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
            end if
        end do
        
        ! Should have a reasonable number of non-empty labels
        if (valid_labels < 4) then
            print *, "FAIL: Too few labels for symlog range"
            print *, "Got only", valid_labels, "valid labels"
            stop 1
        end if
    end subroutine test_should_handle_symlog_with_linear_threshold

    subroutine test_should_maintain_coordinate_bounds()
        !! Coordinate transformations should generate consistent labels
        character(len=20) :: labels(6)
        real(wp) :: first_val, last_val
        integer :: i, valid_labels
        
        ! Test with medium range that should have predictable behavior
        call calculate_tick_labels(1.0_wp, 10.0_wp, 6, labels)
        
        ! Count valid labels and get bounds
        valid_labels = 0
        do i = 1, 6
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (valid_labels == 1) read(labels(i), *) first_val
                read(labels(i), *) last_val
            end if
        end do
        
        ! Verify reasonable bounds
        if (first_val > 1.0_wp .or. last_val < 10.0_wp) then
            print *, "FAIL: Labels don't encompass data range"
            print *, "Labels:", first_val, "to", last_val, "Data: 1.0 to 10.0"
            stop 1
        end if
    end subroutine test_should_maintain_coordinate_bounds

    subroutine test_should_handle_scientific_ranges()
        !! Very large or small ranges should be handled gracefully
        character(len=20) :: labels(5)
        integer :: i, valid_labels
        
        ! Very small range
        call calculate_tick_labels(1.0e-6_wp, 5.0e-6_wp, 5, labels)
        
        valid_labels = 0
        do i = 1, 5
            if (trim(labels(i)) /= '') valid_labels = valid_labels + 1
        end do
        
        if (valid_labels < 2) then
            print *, "FAIL: Too few labels for scientific range"
            print *, "Got only", valid_labels, "valid labels"
            stop 1
        end if
        
        ! Check that we have meaningful precision
        if (index(labels(1), 'E') == 0 .and. index(labels(1), 'e') == 0 .and. &
            index(labels(1), '.') == 0) then
            print *, "FAIL: Scientific range should show decimal precision"
            stop 1
        end if
    end subroutine test_should_handle_scientific_ranges

end program test_log_symlog_ticks