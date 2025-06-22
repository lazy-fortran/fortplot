program test_tick_labels
    !! Test tick label functionality following TDD principles
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_margins, only: calculate_tick_labels, format_tick_value
    implicit none
    
    call test_should_format_small_numbers()
    call test_should_format_large_numbers()
    call test_should_format_zero()
    call test_should_calculate_labels_for_range()
    call test_should_remove_trailing_zeros()
    
    print *, "All tick label tests passed!"
    
contains

    subroutine test_should_format_small_numbers()
        !! Test formatting of small decimal numbers
        character(len=20) :: result
        
        result = format_tick_value(0.123_wp, 1.0_wp)
        if (trim(result) /= '0.12') then
            print *, "FAIL: Expected '0.12', got '", trim(result), "'"
            stop 1
        end if
        
        result = format_tick_value(1.500_wp, 10.0_wp)
        if (trim(result) /= '1.5') then
            print *, "FAIL: Expected '1.5', got '", trim(result), "'"
            stop 1
        end if
    end subroutine test_should_format_small_numbers

    subroutine test_should_format_large_numbers()
        !! Test formatting of large integer numbers
        character(len=20) :: result
        
        result = format_tick_value(1000.0_wp, 5000.0_wp)
        if (trim(result) /= '1000') then
            print *, "FAIL: Expected '1000', got '", trim(result), "'"
            stop 1
        end if
        
        result = format_tick_value(42.7_wp, 100.0_wp)
        if (trim(result) /= '43') then
            print *, "FAIL: Expected '43', got '", trim(result), "'"
            stop 1
        end if
    end subroutine test_should_format_large_numbers

    subroutine test_should_format_zero()
        !! Test formatting of zero values
        character(len=20) :: result
        
        result = format_tick_value(0.0_wp, 1.0_wp)
        if (trim(result) /= '0') then
            print *, "FAIL: Expected '0', got '", trim(result), "'"
            stop 1
        end if
        
        result = format_tick_value(1.0e-15_wp, 1.0_wp)
        if (trim(result) /= '0') then
            print *, "FAIL: Expected '0', got '", trim(result), "'"
            stop 1
        end if
    end subroutine test_should_format_zero

    subroutine test_should_calculate_labels_for_range()
        !! Test calculation of tick labels for a data range
        character(len=20) :: labels(5)
        
        call calculate_tick_labels(0.0_wp, 4.0_wp, 5, labels)
        
        if (trim(labels(1)) /= '0') then
            print *, "FAIL: Expected labels(1)='0', got '", trim(labels(1)), "'"
            stop 1
        end if
        
        if (trim(labels(5)) /= '4') then
            print *, "FAIL: Expected labels(5)='4', got '", trim(labels(5)), "'"
            stop 1
        end if
        
        if (trim(labels(3)) /= '2') then
            print *, "FAIL: Expected labels(3)='2', got '", trim(labels(3)), "'"
            stop 1
        end if
    end subroutine test_should_calculate_labels_for_range

    subroutine test_should_remove_trailing_zeros()
        !! Test that trailing zeros are removed properly
        character(len=20) :: result
        
        result = format_tick_value(2.000_wp, 5.0_wp)
        if (trim(result) /= '2') then
            print *, "FAIL: Expected '2', got '", trim(result), "'"
            stop 1
        end if
        
        result = format_tick_value(3.100_wp, 5.0_wp)
        if (trim(result) /= '3.1') then
            print *, "FAIL: Expected '3.1', got '", trim(result), "'"
            stop 1
        end if
    end subroutine test_should_remove_trailing_zeros

end program test_tick_labels