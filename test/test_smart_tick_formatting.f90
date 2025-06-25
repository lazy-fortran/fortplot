program test_smart_tick_formatting
    !! Test smart tick formatting with length limits
    use fortplot_ticks, only: format_tick_value_smart
    use, intrinsic :: iso_fortran_env, only: wp => real64
    implicit none
    
    real(wp) :: test_values(10)
    character(len=20) :: formatted
    integer :: i, max_chars
    
    ! Test values that might create long labels
    test_values = [0.0_wp, 0.5_wp, 1.23456_wp, 12.3456_wp, 123.456_wp, &
                  1234.56_wp, 12345.6_wp, 0.0001234_wp, 0.00000123_wp, -987.654_wp]
    
    print *, "=== Smart Tick Formatting Test ==="
    max_chars = 8
    print *, "Maximum characters allowed:", max_chars
    print *, ""
    
    do i = 1, size(test_values)
        formatted = format_tick_value_smart(test_values(i), max_chars)
        print '(A, F12.6, A, A, A, I0, A)', "Value ", test_values(i), " -> '", &
              trim(formatted), "' (", len_trim(formatted), " chars)"
    end do
    
    print *, ""
    print *, "All labels should be <= 8 characters for clean appearance"
    
end program test_smart_tick_formatting