program test_consistent_tick_digits
    !! Test that all tick labels on an axis have consistent number of digits
    !! following matplotlib's behavior for professional appearance
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_ticks, only: calculate_tick_labels
    implicit none
    
    call test_should_have_consistent_decimal_places()
    call test_should_handle_mixed_integer_decimal_range()
    call test_should_use_appropriate_precision_for_range()
    call test_should_handle_small_decimal_ranges()
    call test_should_format_zero_consistently()
    call test_should_handle_range_half_to_two_and_half()
    
    print *, "All consistent tick digit tests passed!"
    
contains

    subroutine test_should_have_consistent_decimal_places()
        !! All ticks should have same number of decimal places
        character(len=20) :: labels(5)
        integer :: i, decimal_places, valid_labels
        
        ! Test range 0.0 to 1.0 - should all have same decimal places
        call calculate_tick_labels(0.0_wp, 1.0_wp, 5, labels)
        
        ! Find first valid label to set reference
        decimal_places = -1
        valid_labels = 0
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (decimal_places == -1) then
                    decimal_places = count_decimal_places(labels(i))
                else if (count_decimal_places(labels(i)) /= decimal_places) then
                    print *, "FAIL: Inconsistent decimal places in range 0-1"
                    print *, "Reference: '", trim(labels(1)), "' (", decimal_places, " places)"
                    print *, "Label ", i, ": '", trim(labels(i)), "' (", count_decimal_places(labels(i)), " places)"
                    stop 1
                end if
            end if
        end do
        
        if (valid_labels < 3) then
            print *, "FAIL: Too few valid labels in range 0-1"
            stop 1
        end if
    end subroutine test_should_have_consistent_decimal_places

    subroutine test_should_handle_mixed_integer_decimal_range()
        !! Range like 0.5 to 2.5 should format consistently
        character(len=20) :: labels(5)
        integer :: i, decimal_places, valid_labels
        
        call calculate_tick_labels(0.5_wp, 2.5_wp, 5, labels)
        
        ! Count valid labels and check consistency
        decimal_places = -1
        valid_labels = 0
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (decimal_places == -1) then
                    decimal_places = count_decimal_places(labels(i))
                else if (count_decimal_places(labels(i)) /= decimal_places) then
                    print *, "FAIL: Inconsistent decimal places in mixed range"
                    stop 1
                end if
            end if
        end do
        
        if (valid_labels < 3) then
            print *, "FAIL: Too few valid labels in mixed range"
            stop 1
        end if
    end subroutine test_should_handle_mixed_integer_decimal_range

    subroutine test_should_use_appropriate_precision_for_range()
        !! Large ranges should use consistent integer formatting
        character(len=20) :: labels(4)
        integer :: i, valid_labels
        
        call calculate_tick_labels(0.0_wp, 1000.0_wp, 4, labels)
        
        ! Count valid labels - all should be integers (0 decimal places)
        valid_labels = 0
        do i = 1, 4
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (count_decimal_places(labels(i)) /= 0) then
                    print *, "FAIL: Large range should use integer formatting"
                    print *, "Label ", i, ": '", trim(labels(i)), "' should be integer"
                    stop 1
                end if
            end if
        end do
        
        if (valid_labels < 3) then
            print *, "FAIL: Too few valid labels for large range"
            stop 1
        end if
    end subroutine test_should_use_appropriate_precision_for_range

    subroutine test_should_handle_small_decimal_ranges()
        !! Very small ranges should have consistent high precision
        character(len=20) :: labels(3)
        integer :: i, decimal_places, valid_labels
        
        call calculate_tick_labels(0.001_wp, 0.003_wp, 3, labels)
        
        ! Count valid labels and check precision
        decimal_places = -1
        valid_labels = 0
        do i = 1, 3
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (decimal_places == -1) then
                    decimal_places = count_decimal_places(labels(i))
                    if (decimal_places < 3) then
                        print *, "FAIL: Small range should have at least 3 decimal places"
                        print *, "Got only", decimal_places, "decimal places"
                        stop 1
                    end if
                else if (count_decimal_places(labels(i)) /= decimal_places) then
                    print *, "FAIL: Inconsistent decimal places in small range"
                    stop 1
                end if
            end if
        end do
        
        if (valid_labels < 2) then
            print *, "FAIL: Too few valid labels for small range"
            stop 1
        end if
    end subroutine test_should_handle_small_decimal_ranges

    subroutine test_should_format_zero_consistently()
        !! Zero should be formatted with same decimal places as other ticks
        character(len=20) :: labels(5)
        integer :: decimal_places
        
        ! Range 0.0 to 1.0 - zero should have same format as others
        call calculate_tick_labels(0.0_wp, 1.0_wp, 5, labels)
        
        ! First label is 0.0, should have same decimal places as others
        decimal_places = count_decimal_places(labels(2))  ! Use second label as reference
        if (count_decimal_places(labels(1)) /= decimal_places) then
            print *, "FAIL: Zero not formatted consistently with other ticks"
            print *, "Zero: '", trim(labels(1)), "' (", count_decimal_places(labels(1)), " places)"
            print *, "Reference: '", trim(labels(2)), "' (", decimal_places, " places)"
            stop 1
        end if
    end subroutine test_should_format_zero_consistently

    subroutine test_should_handle_range_half_to_two_and_half()
        !! Test the specific range 0.5 to 2.5 from debug script
        character(len=20) :: labels(5)
        integer :: i, decimal_places
        
        call calculate_tick_labels(0.5_wp, 2.5_wp, 5, labels)
        
        ! All should have 1 decimal place
        decimal_places = count_decimal_places(labels(1))
        if (decimal_places /= 1) then
            print *, "FAIL: Range 0.5-2.5 should use 1 decimal place"
            print *, "Got", decimal_places, "decimal places"
            stop 1
        end if
        
        ! All should be consistent
        do i = 2, 5
            if (count_decimal_places(labels(i)) /= decimal_places) then
                print *, "FAIL: Inconsistent decimal places in 0.5-2.5 range"
                stop 1
            end if
        end do
        
        ! Check specific values
        if (trim(labels(1)) /= '0.5') then
            print *, "FAIL: Expected '0.5', got '", trim(labels(1)), "'"
            stop 1
        end if
        
        if (trim(labels(5)) /= '2.5') then
            print *, "FAIL: Expected '2.5', got '", trim(labels(5)), "'"
            stop 1
        end if
    end subroutine test_should_handle_range_half_to_two_and_half

    function count_decimal_places(str) result(places)
        !! Count number of digits after decimal point
        character(len=*), intent(in) :: str
        integer :: places
        integer :: decimal_pos
        
        decimal_pos = index(trim(str), '.')
        if (decimal_pos == 0) then
            places = 0  ! No decimal point = 0 decimal places
        else
            places = len_trim(str) - decimal_pos
        end if
    end function count_decimal_places

end program test_consistent_tick_digits