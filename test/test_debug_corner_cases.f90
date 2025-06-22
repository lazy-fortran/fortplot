program test_debug_corner_cases
    !! Comprehensive tests for all corner cases discovered during debugging
    !! Following TDD principles and matplotlib compatibility requirements
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_ticks, only: calculate_tick_labels
    implicit none
    
    call test_case_0123_to_0789()
    call test_case_123_to_127()
    call test_case_minus50_to_150()
    call test_case_0_to_10()
    call test_case_05_to_25()
    call test_case_001_to_1000()
    call test_case_scientific_micro_range()
    call test_case_minus100_to_1000()
    call test_case_minus37_to_142()
    call test_case_very_small_range()
    
    print *, "All debug corner case tests passed!"
    
contains

    subroutine test_case_0123_to_0789()
        !! Range 0.123 to 0.789 should give nice round numbers
        character(len=20) :: labels(5)
        real(wp) :: first_val, last_val
        integer :: i, valid_labels
        
        call calculate_tick_labels(0.123_wp, 0.789_wp, 5, labels)
        
        valid_labels = 0
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (valid_labels == 1) read(labels(i), *) first_val
                read(labels(i), *) last_val
            end if
        end do
        
        ! Should encompass the data range
        if (first_val > 0.123_wp .or. last_val < 0.789_wp) then
            print *, "FAIL: Labels don't encompass range 0.123-0.789"
            stop 1
        end if
        
        if (valid_labels < 4) then
            print *, "FAIL: Too few labels for range 0.123-0.789"
            stop 1
        end if
    end subroutine test_case_0123_to_0789

    subroutine test_case_123_to_127()
        !! Small range 1.23 to 1.27 should have appropriate precision
        character(len=20) :: labels(5)
        integer :: i, valid_labels, decimal_places
        
        call calculate_tick_labels(1.23_wp, 1.27_wp, 5, labels)
        
        valid_labels = 0
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                decimal_places = count_decimal_places(labels(i))
                if (decimal_places < 2) then
                    print *, "FAIL: Small range 1.23-1.27 needs at least 2 decimal places"
                    stop 1
                end if
            end if
        end do
        
        if (valid_labels < 3) then
            print *, "FAIL: Too few labels for small range 1.23-1.27"
            stop 1
        end if
    end subroutine test_case_123_to_127

    subroutine test_case_minus50_to_150()
        !! Range -50 to 150 should use nice round integers
        character(len=20) :: labels(5)
        integer :: i, valid_labels
        
        call calculate_tick_labels(-50.0_wp, 150.0_wp, 5, labels)
        
        valid_labels = 0
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (count_decimal_places(labels(i)) /= 0) then
                    print *, "FAIL: Range -50 to 150 should use integers"
                    stop 1
                end if
            end if
        end do
        
        if (valid_labels < 4) then
            print *, "FAIL: Too few labels for range -50 to 150"
            stop 1
        end if
    end subroutine test_case_minus50_to_150

    subroutine test_case_0_to_10()
        !! Range 0 to 10 should have nice spacing
        character(len=20) :: labels(6)
        integer :: i, valid_labels
        
        call calculate_tick_labels(0.0_wp, 10.0_wp, 6, labels)
        
        valid_labels = 0
        do i = 1, 6
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
            end if
        end do
        
        if (valid_labels < 4) then
            print *, "FAIL: Too few labels for range 0 to 10"
            stop 1
        end if
    end subroutine test_case_0_to_10

    subroutine test_case_05_to_25()
        !! Range 0.5 to 2.5 should use 1 decimal place consistently
        character(len=20) :: labels(5)
        integer :: i, valid_labels, decimal_places
        
        call calculate_tick_labels(0.5_wp, 2.5_wp, 5, labels)
        
        valid_labels = 0
        decimal_places = -1
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (decimal_places == -1) then
                    decimal_places = count_decimal_places(labels(i))
                else if (count_decimal_places(labels(i)) /= decimal_places) then
                    print *, "FAIL: Inconsistent decimal places in 0.5-2.5 range"
                    stop 1
                end if
            end if
        end do
        
        if (valid_labels < 3) then
            print *, "FAIL: Too few labels for range 0.5-2.5"
            stop 1
        end if
    end subroutine test_case_05_to_25

    subroutine test_case_001_to_1000()
        !! Large dynamic range 0.01 to 1000 (log-like)
        character(len=20) :: labels(5)
        real(wp) :: first_val, last_val
        integer :: i, valid_labels
        
        call calculate_tick_labels(0.01_wp, 1000.0_wp, 5, labels)
        
        valid_labels = 0
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (valid_labels == 1) read(labels(i), *) first_val
                read(labels(i), *) last_val
            end if
        end do
        
        ! Should encompass the large range
        if (first_val > 0.01_wp .or. last_val < 1000.0_wp) then
            print *, "FAIL: Labels don't encompass large range 0.01-1000"
            stop 1
        end if
        
        if (valid_labels < 3) then
            print *, "FAIL: Too few labels for large range 0.01-1000"
            stop 1
        end if
    end subroutine test_case_001_to_1000

    subroutine test_case_scientific_micro_range()
        !! Very small scientific range 1e-6 to 5e-6
        character(len=20) :: labels(5)
        integer :: i, valid_labels
        
        call calculate_tick_labels(1.0e-6_wp, 5.0e-6_wp, 5, labels)
        
        valid_labels = 0
        do i = 1, 5
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
            end if
        end do
        
        if (valid_labels < 2) then
            print *, "FAIL: Too few labels for scientific micro range"
            stop 1
        end if
    end subroutine test_case_scientific_micro_range

    subroutine test_case_minus100_to_1000()
        !! Range crossing zero -100 to 1000 (symlog-like)
        character(len=20) :: labels(7)
        real(wp) :: first_val, last_val
        integer :: i, valid_labels
        
        call calculate_tick_labels(-100.0_wp, 1000.0_wp, 7, labels)
        
        valid_labels = 0
        do i = 1, 7
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (valid_labels == 1) read(labels(i), *) first_val
                read(labels(i), *) last_val
            end if
        end do
        
        ! Should encompass the range
        if (first_val > -100.0_wp .or. last_val < 1000.0_wp) then
            print *, "FAIL: Labels don't encompass symlog range -100 to 1000"
            stop 1
        end if
        
        if (valid_labels < 4) then
            print *, "FAIL: Too few labels for symlog range -100 to 1000"
            stop 1
        end if
    end subroutine test_case_minus100_to_1000

    subroutine test_case_minus37_to_142()
        !! Irregular range -37 to 142
        character(len=20) :: labels(6)
        real(wp) :: first_val, last_val
        integer :: i, valid_labels
        
        call calculate_tick_labels(-37.0_wp, 142.0_wp, 6, labels)
        
        valid_labels = 0
        do i = 1, 6
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                if (valid_labels == 1) read(labels(i), *) first_val
                read(labels(i), *) last_val
            end if
        end do
        
        ! Should encompass the irregular range  
        if (first_val > -37.0_wp .or. last_val < 142.0_wp) then
            print *, "FAIL: Labels don't encompass irregular range -37 to 142"
            stop 1
        end if
        
        if (valid_labels < 4) then
            print *, "FAIL: Too few labels for irregular range -37 to 142"
            stop 1
        end if
    end subroutine test_case_minus37_to_142

    subroutine test_case_very_small_range()
        !! Very small range 0.001 to 0.003
        character(len=20) :: labels(3)
        integer :: i, valid_labels, decimal_places
        
        call calculate_tick_labels(0.001_wp, 0.003_wp, 3, labels)
        
        valid_labels = 0
        do i = 1, 3
            if (trim(labels(i)) /= '') then
                valid_labels = valid_labels + 1
                decimal_places = count_decimal_places(labels(i))
                if (decimal_places < 3) then
                    print *, "FAIL: Very small range needs at least 3 decimal places"
                    stop 1
                end if
            end if
        end do
        
        if (valid_labels < 2) then
            print *, "FAIL: Too few labels for very small range 0.001-0.003"
            stop 1
        end if
    end subroutine test_case_very_small_range

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

end program test_debug_corner_cases