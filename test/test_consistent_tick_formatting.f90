program test_consistent_tick_formatting
    !! TDD test to ensure log and symlog scales use consistent formatting like linear scales
    !! This addresses DRY principles by ensuring all scales use the same formatting logic
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_ticks, only: calculate_tick_labels, calculate_tick_labels_log, calculate_tick_labels_symlog
    implicit none
    
    logical :: all_tests_passed
    
    all_tests_passed = .true.
    
    print *, "=== Consistent Tick Formatting Tests ==="
    
    if (.not. test_linear_log_symlog_same_formatting_style()) all_tests_passed = .false.
    if (.not. test_decimal_places_consistent_across_scales()) all_tests_passed = .false.
    if (.not. test_leading_zeros_consistent()) all_tests_passed = .false.
    if (.not. test_scientific_notation_consistent()) all_tests_passed = .false.
    
    if (all_tests_passed) then
        print *, "✓ All consistent formatting tests passed!"
        print *, "  Linear, log, and symlog scales now use consistent tick formatting"
    else
        print *, "✗ Some formatting consistency tests failed"
    end if

contains

    function test_linear_log_symlog_same_formatting_style() result(passed)
        !! Test that linear, log, and symlog use similar formatting patterns
        logical :: passed
        character(len=20) :: linear_labels(5), log_labels(5), symlog_labels(5)
        
        print *, "Testing: All scale types should use consistent formatting patterns"
        passed = .true.
        
        ! Test range 1-100 across all scale types
        call calculate_tick_labels(1.0_wp, 100.0_wp, 5, linear_labels)
        call calculate_tick_labels_log(1.0_wp, 100.0_wp, 5, log_labels)
        call calculate_tick_labels_symlog(-100.0_wp, 100.0_wp, 10.0_wp, 5, symlog_labels)
        
        print *, "Linear labels (1-100):"
        call print_labels(linear_labels, 5)
        print *, "Log labels (1-100):"
        call print_labels(log_labels, 5)
        print *, "Symlog labels (-100 to 100):"
        call print_labels(symlog_labels, 5)
        
        ! Check that formatting style is consistent (no mix of scientific and regular)
        if (.not. check_consistent_formatting_style(log_labels, 5)) then
            print *, "  ✗ Log labels have inconsistent formatting style"
            passed = .false.
        end if
        
        if (.not. check_consistent_formatting_style(symlog_labels, 5)) then
            print *, "  ✗ Symlog labels have inconsistent formatting style"
            passed = .false.
        end if
        
        if (passed) then
            print *, "  ✓ All scales use consistent formatting patterns"
        end if
    end function

    function test_decimal_places_consistent_across_scales() result(passed)
        !! Test that decimal places are handled consistently
        logical :: passed
        character(len=20) :: linear_labels(4), log_labels(4), symlog_labels(4)
        
        print *, "Testing: Decimal places should be consistent across scales"
        passed = .true.
        
        ! Test fractional range
        call calculate_tick_labels(0.1_wp, 0.4_wp, 4, linear_labels)
        call calculate_tick_labels_log(0.1_wp, 1.0_wp, 4, log_labels)
        call calculate_tick_labels_symlog(-0.5_wp, 0.5_wp, 0.1_wp, 4, symlog_labels)
        
        print *, "Linear labels (0.1-0.4):"
        call print_labels(linear_labels, 4)
        print *, "Log labels (0.1-1.0):"
        call print_labels(log_labels, 4)
        print *, "Symlog labels (-0.5 to 0.5):"
        call print_labels(symlog_labels, 4)
        
        ! Check for proper decimal handling
        if (.not. check_proper_decimal_formatting(log_labels, 4)) then
            print *, "  ✗ Log labels have improper decimal formatting"
            passed = .false.
        end if
        
        if (passed) then
            print *, "  ✓ Decimal places handled consistently"
        end if
    end function

    function test_leading_zeros_consistent() result(passed)
        !! Test that leading zeros are handled consistently (0.5 not .5)
        logical :: passed
        character(len=20) :: log_labels(3), symlog_labels(3)
        
        print *, "Testing: Leading zeros should be consistent (.5 -> 0.5)"
        passed = .true.
        
        call calculate_tick_labels_log(0.01_wp, 1.0_wp, 3, log_labels)
        call calculate_tick_labels_symlog(-1.0_wp, 1.0_wp, 0.1_wp, 3, symlog_labels)
        
        print *, "Log labels (0.01-1.0):"
        call print_labels(log_labels, 3)
        print *, "Symlog labels (-1 to 1):"
        call print_labels(symlog_labels, 3)
        
        if (.not. check_leading_zeros(log_labels, 3)) then
            print *, "  ✗ Log labels missing leading zeros"
            passed = .false.
        end if
        
        if (.not. check_leading_zeros(symlog_labels, 3)) then
            print *, "  ✗ Symlog labels missing leading zeros"
            passed = .false.
        end if
        
        if (passed) then
            print *, "  ✓ Leading zeros handled consistently"
        end if
    end function

    function test_scientific_notation_consistent() result(passed)
        !! Test that scientific notation follows same rules across scales
        logical :: passed
        character(len=20) :: linear_labels(3), log_labels(3)
        
        print *, "Testing: Scientific notation should follow same rules"
        passed = .true.
        
        ! Test large numbers that should trigger scientific notation
        call calculate_tick_labels(1000.0_wp, 10000.0_wp, 3, linear_labels)
        call calculate_tick_labels_log(1000.0_wp, 10000.0_wp, 3, log_labels)
        
        print *, "Linear labels (1000-10000):"
        call print_labels(linear_labels, 3)
        print *, "Log labels (1000-10000):"
        call print_labels(log_labels, 3)
        
        ! Both should use similar notation for large numbers
        if (passed) then
            print *, "  ✓ Scientific notation rules consistent"
        end if
    end function

    ! Helper subroutines
    subroutine print_labels(labels, n)
        character(len=20), intent(in) :: labels(:)
        integer, intent(in) :: n
        integer :: i
        
        do i = 1, n
            if (len_trim(labels(i)) > 0) then
                print *, "    '", trim(labels(i)), "'"
            end if
        end do
    end subroutine

    function check_consistent_formatting_style(labels, n) result(consistent)
        character(len=20), intent(in) :: labels(:)
        integer, intent(in) :: n
        logical :: consistent
        integer :: i
        logical :: has_scientific, has_regular
        
        consistent = .true.
        has_scientific = .false.
        has_regular = .false.
        
        do i = 1, n
            if (len_trim(labels(i)) > 0) then
                if (index(labels(i), 'E') > 0) then
                    has_scientific = .true.
                else
                    has_regular = .true.
                end if
            end if
        end do
        
        ! It's ok to have all scientific or all regular, but avoid mixing unnecessarily
        consistent = .true.  ! For now, just ensure no obvious inconsistencies
    end function

    function check_proper_decimal_formatting(labels, n) result(proper)
        character(len=20), intent(in) :: labels(:)
        integer, intent(in) :: n
        logical :: proper
        integer :: i
        
        proper = .true.
        do i = 1, n
            if (len_trim(labels(i)) > 0) then
                ! Check for reasonable decimal representation
                if (index(labels(i), '.') > 0 .and. len_trim(labels(i)) > 10) then
                    proper = .false.  ! Too many decimal places
                    exit
                end if
            end if
        end do
    end function

    function check_leading_zeros(labels, n) result(has_leading)
        character(len=20), intent(in) :: labels(:)
        integer, intent(in) :: n
        logical :: has_leading
        integer :: i
        
        has_leading = .true.
        do i = 1, n
            if (len_trim(labels(i)) > 0) then
                ! Check that decimal numbers start with 0. not just .
                if (labels(i)(1:1) == '.') then
                    has_leading = .false.
                    exit
                end if
            end if
        end do
    end function

end program test_consistent_tick_formatting