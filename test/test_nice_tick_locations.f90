program test_nice_tick_locations
    !! Test that tick locations are placed at nice round numbers like matplotlib
    !! and have appropriate precision following TDD principles
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_margins, only: calculate_tick_labels
    implicit none
    
    call test_should_place_ticks_at_nice_locations()
    call test_should_use_sufficient_precision_for_small_ranges()
    call test_should_handle_scientific_data_ranges()
    call test_should_prefer_round_numbers()
    
    print *, "All nice tick location tests passed!"
    
contains

    subroutine test_should_place_ticks_at_nice_locations()
        !! Ticks should be at nice round numbers, not arbitrary fractions
        character(len=20) :: labels(5)
        
        ! Range 0.123 to 0.789 should generate nice tick locations
        call calculate_tick_labels(0.123_wp, 0.789_wp, 5, labels)
        
        ! Should have nice locations like 0.2, 0.3, 0.4, 0.5, 0.6 or similar
        ! Not 0.123, 0.289, 0.455, 0.621, 0.789
        if (contains_ugly_decimals(labels, 5)) then
            print *, "FAIL: Tick locations are not nice round numbers"
            print *, "Got labels:"
            call print_labels(labels, 5)
            stop 1
        end if
    end subroutine test_should_place_ticks_at_nice_locations

    subroutine test_should_use_sufficient_precision_for_small_ranges()
        !! Small ranges should show enough precision to distinguish values
        character(len=20) :: labels(5)
        integer :: i, valid_labels
        
        ! Range 1.23 to 1.27 - should show at least 2 decimal places
        call calculate_tick_labels(1.23_wp, 1.27_wp, 5, labels)
        
        ! Count valid labels and check precision
        valid_labels = 0
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (count_decimal_places(labels(i)) < 2) then
                    print *, "FAIL: Insufficient precision for small range 1.23-1.27"
                    print *, "Label ", i, ": '", trim(labels(i)), "' has only", &
                             count_decimal_places(labels(i)), "decimal places"
                    stop 1
                end if
            end if
        end do
        
        ! Should have at least 3 valid labels for this range
        if (valid_labels < 3) then
            print *, "FAIL: Too few labels for small range"
            print *, "Got only", valid_labels, "valid labels"
            stop 1
        end if
    end subroutine test_should_use_sufficient_precision_for_small_ranges

    subroutine test_should_handle_scientific_data_ranges()
        !! Ranges like -50 to 150 should use integers, not decimals
        character(len=20) :: labels(5)
        integer :: i
        
        call calculate_tick_labels(-50.0_wp, 150.0_wp, 5, labels)
        
        ! All should be integers (0 decimal places)
        do i = 1, 5
            if (count_decimal_places(labels(i)) /= 0) then
                print *, "FAIL: Large range should use integer formatting"
                print *, "Label ", i, ": '", trim(labels(i)), "' should be integer"
                stop 1
            end if
        end do
        
        ! Should be nice round numbers like -50, 0, 50, 100, 150 or similar
        if (contains_ugly_numbers(labels, 5)) then
            print *, "FAIL: Large range should use nice round numbers"
            call print_labels(labels, 5)
            stop 1
        end if
    end subroutine test_should_handle_scientific_data_ranges

    subroutine test_should_prefer_round_numbers()
        !! Matplotlib prefers 1, 2, 5 multiples for nice spacing
        character(len=20) :: labels(6)
        
        ! Range 0 to 10 with 6 ticks should prefer nice spacing
        call calculate_tick_labels(0.0_wp, 10.0_wp, 6, labels)
        
        ! Should be something like 0, 2, 4, 6, 8, 10 or 0, 1, 2, 3, 4, 5
        ! Not 0, 1.67, 3.33, 5.0, 6.67, 8.33, 10
        if (contains_ugly_decimals(labels, 6)) then
            print *, "FAIL: Should prefer round number spacing"
            call print_labels(labels, 6)
            stop 1
        end if
    end subroutine test_should_prefer_round_numbers

    function contains_ugly_decimals(labels, n) result(has_ugly)
        !! Check if labels contain ugly decimal numbers
        character(len=20), intent(in) :: labels(:)
        integer, intent(in) :: n
        logical :: has_ugly
        integer :: i
        real(wp) :: value
        
        has_ugly = .false.
        do i = 1, n
            read(labels(i), *) value
            ! Check for ugly decimals like 0.123, 0.667, etc.
            if (abs(value - nint(value * 10.0_wp) / 10.0_wp) > 1.0e-10_wp .and. &
                abs(value - nint(value * 4.0_wp) / 4.0_wp) > 1.0e-10_wp .and. &
                abs(value - nint(value * 2.0_wp) / 2.0_wp) > 1.0e-10_wp) then
                has_ugly = .true.
                return
            end if
        end do
    end function contains_ugly_decimals

    function contains_ugly_numbers(labels, n) result(has_ugly)
        !! Check if labels contain ugly round numbers
        character(len=20), intent(in) :: labels(:)
        integer, intent(in) :: n
        logical :: has_ugly
        integer :: i
        real(wp) :: value
        
        has_ugly = .false.
        do i = 1, n
            read(labels(i), *) value
            ! Check for ugly numbers like 67, 133, etc. (prefer multiples of 10, 25, 50)
            if (abs(value) > 10.0_wp .and. &
                mod(nint(abs(value)), 10) /= 0 .and. &
                mod(nint(abs(value)), 25) /= 0 .and. &
                mod(nint(abs(value)), 5) /= 0) then
                has_ugly = .true.
                return
            end if
        end do
    end function contains_ugly_numbers

    function count_decimal_places(str) result(places)
        !! Count number of digits after decimal point
        character(len=*), intent(in) :: str
        integer :: places
        integer :: decimal_pos
        
        decimal_pos = index(trim(str), '.')
        if (decimal_pos == 0) then
            places = 0
        else
            places = len_trim(str) - decimal_pos
        end if
    end function count_decimal_places

    subroutine print_labels(labels, n)
        !! Helper to print label arrays for debugging
        character(len=20), intent(in) :: labels(:)
        integer, intent(in) :: n
        integer :: i
        
        do i = 1, n
            print *, "  ", i, ": '", trim(labels(i)), "'"
        end do
    end subroutine print_labels

end program test_nice_tick_locations