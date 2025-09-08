module fortplot_tick_formatting
    !! Tick value formatting functions with multiple strategies
    !! 
    !! Provides:
    !! - Range-aware tick formatting
    !! - Smart formatting with character limits
    !! - Logarithmic tick value formatting
    !! - String utility functions for tick labels
    
    use, intrinsic :: iso_fortran_env, only: wp => real64
    use fortplot_constants, only: SCIENTIFIC_THRESHOLD_HIGH
    implicit none
    
    private
    public :: format_tick_value, format_tick_value_smart, format_log_tick_value
    public :: format_power_of_ten_label
    public :: remove_trailing_zeros, ensure_leading_zero

contains

    function format_tick_value(value, range) result(formatted)
        !! Format tick value based on data range like matplotlib
        real(wp), intent(in) :: value, range
        character(len=20) :: formatted
        real(wp) :: abs_value
        
        abs_value = abs(value)
        
        if (abs_value < 1.0e-10_wp) then
            formatted = '0'
        else if (range >= 100.0_wp .or. abs_value >= SCIENTIFIC_THRESHOLD_HIGH) then
            ! Use integer format for large ranges
            write(formatted, '(I0)') nint(value)
        else if (range >= 10.0_wp .or. abs_value >= 10.0_wp) then
            ! 1 decimal place for medium ranges
            write(formatted, '(F0.1)') value
            call ensure_leading_zero(formatted)
            call remove_trailing_zeros(formatted)
        else if (range >= 1.0_wp .or. abs_value >= 1.0_wp) then
            ! 2 decimal places for small ranges
            write(formatted, '(F0.2)') value
            call ensure_leading_zero(formatted)
            call remove_trailing_zeros(formatted)
        else
            ! 3 decimal places for very small ranges
            write(formatted, '(F0.3)') value
            call ensure_leading_zero(formatted)
            call remove_trailing_zeros(formatted)
        end if
    end function format_tick_value

    function format_tick_value_smart(value, max_chars) result(formatted)
        !! Smart tick value formatting with automatic exponential notation for long labels
        !! Limits output to max_chars and uses exponential notation when needed
        real(wp), intent(in) :: value
        integer, intent(in) :: max_chars
        character(len=20) :: formatted
        real(wp) :: abs_value
        
        abs_value = abs(value)
        
        ! Handle zero
        if (abs_value < 1.0e-10_wp) then
            formatted = '0'
            return
        end if
        
        ! Try normal formatting first
        if (abs_value >= SCIENTIFIC_THRESHOLD_HIGH .or. abs_value < 0.001_wp) then
            ! Large or very small numbers - use exponential notation
            write(formatted, '(ES8.1)') value
        else if (abs_value >= 100.0_wp) then
            ! Integer format for hundreds
            write(formatted, '(I0)') nint(value)
        else if (abs_value >= 10.0_wp) then
            ! 1 decimal place
            write(formatted, '(F0.1)') value
            call remove_trailing_zeros(formatted)
        else if (abs_value >= 1.0_wp) then
            ! 2 decimal places
            write(formatted, '(F0.2)') value
            call remove_trailing_zeros(formatted)
        else
            ! Small numbers - up to 3 decimal places
            write(formatted, '(F0.3)') value
            call remove_trailing_zeros(formatted)
        end if
        
        ! If formatted string is too long, use exponential notation
        if (len_trim(formatted) > max_chars) then
            if (max_chars >= 7) then
                write(formatted, '(ES7.0)') value
            else
                write(formatted, '(ES6.0)') value
            end if
        end if
        
        call ensure_leading_zero(formatted)
    end function format_tick_value_smart

    function format_log_tick_value(value) result(formatted)
        !! Format logarithmic tick values using scientific notation like matplotlib
        real(wp), intent(in) :: value
        character(len=20) :: formatted
        real(wp) :: log_val
        integer :: exponent
        logical :: is_power_of_ten
        
        if (abs(value) < 1.0e-10_wp) then
            formatted = '0'
            return
        end if
        
        ! Check if this is a power of 10
        log_val = log10(abs(value))
        exponent = nint(log_val)
        is_power_of_ten = abs(log_val - real(exponent, wp)) < 1.0e-10_wp
        
        if (is_power_of_ten) then
            ! Format common powers of 10 as decimal for readability
            if (value < 0.0_wp) then
                if (exponent >= -3 .and. exponent <= 3) then
                    ! Use decimal format for common negative powers
                    write(formatted, '(F0.0)') value
                else
                    write(formatted, '(A, I0, A)') '-10^{', exponent, '}'
                end if
            else if (exponent == 0) then
                formatted = '1'
            else if (exponent == 1) then
                formatted = '10'
            else if (exponent == 2) then
                formatted = '100'
            else if (exponent == 3) then
                formatted = '1000'
            else if (exponent == -1) then
                formatted = '0.1'
            else if (exponent == -2) then
                formatted = '0.01'
            else if (exponent == -3) then
                formatted = '0.001'
            else
                write(formatted, '(A, I0, A)') '10^{', exponent, '}'
            end if
        else
            ! For non-powers of 10, use regular formatting
            if (abs(value) >= SCIENTIFIC_THRESHOLD_HIGH .or. abs(value) < 0.01_wp) then
                write(formatted, '(ES8.1)') value
            else
                formatted = format_tick_value(value, abs(value))
            end if
        end if
    end function format_log_tick_value

    function format_power_of_ten_label(value) result(formatted)
        !! Build a mathtext-friendly power-of-ten label: 10^{n} or -10^{n}
        real(wp), intent(in) :: value
        character(len=20) :: formatted
        integer :: exponent

        if (abs(value) < 1.0e-10_wp) then
            formatted = '0'
            return
        end if

        exponent = nint(log10(abs(value)))
        if (value < 0.0_wp) then
            write(formatted, '(A, I0, A)') '-10^{', exponent, '}'
        else
            write(formatted, '(A, I0, A)') '10^{', exponent, '}'
        end if
    end function format_power_of_ten_label

    subroutine remove_trailing_zeros(str)
        !! Remove trailing zeros from decimal representation
        character(len=*), intent(inout) :: str
        integer :: i, decimal_pos
        
        decimal_pos = index(str, '.')
        if (decimal_pos == 0) return  ! No decimal point
        
        ! Remove trailing zeros
        do i = len_trim(str), decimal_pos + 1, -1
            if (str(i:i) == '0') then
                str(i:i) = ' '
            else
                exit
            end if
        end do
        
        ! Remove trailing decimal point if all decimals were zeros
        if (len_trim(str) == decimal_pos) then
            str(decimal_pos:decimal_pos) = ' '
        end if
    end subroutine remove_trailing_zeros

    subroutine ensure_leading_zero(str)
        !! Ensure numbers like .5 become 0.5 for readability
        character(len=*), intent(inout) :: str
        character(len=len(str)) :: temp
        
        str = adjustl(str)  ! Remove leading spaces
        if (len_trim(str) > 0) then
            if (str(1:1) == '.') then
                temp = '0' // trim(str)
                str = temp
            else if (str(1:2) == '-.') then
                temp = '-0' // str(2:len_trim(str))
                str = temp
            end if
        end if
    end subroutine ensure_leading_zero

end module fortplot_tick_formatting
