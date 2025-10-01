program test_log_symlog_ticks
    !! Test that log and symlog scales generate appropriate tick locations
    !! and maintain correct coordinate positioning following TDD principles
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_ticks, only: calculate_tick_labels
    implicit none
    
    call test_should_handle_log_scale_ranges()
    call test_should_format_log_ticks_as_powers_of_ten()
    call test_should_handle_symlog_with_linear_threshold()
    call test_should_include_symlog_threshold_ticks()
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

    subroutine test_should_format_log_ticks_as_powers_of_ten()
        !! Ensure log scale emits matplotlib-style power-of-ten labels
        use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
        real(wp) :: ticks(MAX_TICKS)
        integer :: num_ticks

        call compute_scale_ticks('log', 1.0_wp, 1000.0_wp, 1.0_wp, ticks, num_ticks)

        if (num_ticks < 3) then
            print *, 'FAIL: expected multiple log ticks, got', num_ticks
            stop 1
        end if

        if (trim(format_tick_label(ticks(1), 'log')) /= '$10^{0}$') then
            print *, 'FAIL: log tick at 1 should be formatted as $10^{0}$'
            stop 1
        end if

        if (trim(format_tick_label(ticks(2), 'log')) /= '$10^{1}$') then
            print *, 'FAIL: log tick at 10 should be formatted as $10^{1}$'
            stop 1
        end if

        if (trim(format_tick_label(ticks(3), 'log')) /= '$10^{2}$') then
            print *, 'FAIL: log tick at 100 should be formatted as $10^{2}$'
            stop 1
        end if
    end subroutine test_should_format_log_ticks_as_powers_of_ten

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

    subroutine test_should_include_symlog_threshold_ticks()
        !! Validate symlog ticks include threshold boundary once with power formatting
        use fortplot_axes, only: compute_scale_ticks, format_tick_label, MAX_TICKS
        real(wp) :: ticks(MAX_TICKS)
        integer :: num_ticks, i
        logical :: found_negative_threshold, found_positive_threshold
        character(len=20) :: label

        call compute_scale_ticks('symlog', -150.0_wp, 120000.0_wp, 10.0_wp, ticks, num_ticks)

        found_negative_threshold = .false.
        found_positive_threshold = .false.

        do i = 1, num_ticks
            if (abs(ticks(i) + 10.0_wp) <= 1.0e-9_wp) then
                label = trim(format_tick_label(ticks(i), 'symlog'))
                if (label /= '$-10^{1}$') then
                    print *, 'FAIL: symlog negative threshold should be $-10^{1}$, got', label
                    stop 1
                end if
                if (found_negative_threshold) then
                    print *, 'FAIL: duplicate negative threshold tick detected'
                    stop 1
                end if
                found_negative_threshold = .true.
            else if (abs(ticks(i) - 10.0_wp) <= 1.0e-9_wp) then
                label = trim(format_tick_label(ticks(i), 'symlog'))
                if (label /= '$10^{1}$') then
                    print *, 'FAIL: symlog positive threshold should be $10^{1}$, got', label
                    stop 1
                end if
                if (found_positive_threshold) then
                    print *, 'FAIL: duplicate positive threshold tick detected'
                    stop 1
                end if
                found_positive_threshold = .true.
            end if
        end do

        if (.not. found_negative_threshold) then
            print *, 'FAIL: missing negative symlog threshold tick'
            stop 1
        end if

        if (.not. found_positive_threshold) then
            print *, 'FAIL: missing positive symlog threshold tick'
            stop 1
        end if

        do i = 2, num_ticks
            if (ticks(i) < ticks(i-1) - 1.0e-9_wp) then
                print *, 'FAIL: symlog ticks must be sorted ascending; violation at index', i
                stop 1
            end if
        end do
    end subroutine test_should_include_symlog_threshold_ticks

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
