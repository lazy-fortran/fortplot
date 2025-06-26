program test_format_parser
    use iso_fortran_env, only: error_unit
    use fortplot_format_parser, only: parse_format_string
    implicit none
    
    call test_parse_marker_only()
    call test_parse_line_only()
    call test_parse_marker_and_line()
    call test_parse_all_markers()
    call test_parse_dashed_lines()
    call test_parse_empty_string()
    
    write(error_unit, '(A)') 'All format parser tests passed!'
    
contains

    subroutine test_parse_marker_only()
        character(len=20) :: marker, linestyle
        
        ! Test marker only formats
        call parse_format_string('o', marker, linestyle)
        call assert_equal(marker, 'o', 'Expected circle marker')
        call assert_equal(linestyle, '', 'Expected no line style')
        
        call parse_format_string('x', marker, linestyle)
        call assert_equal(marker, 'x', 'Expected x marker')
        call assert_equal(linestyle, '', 'Expected no line style')
    end subroutine

    subroutine test_parse_line_only()
        character(len=20) :: marker, linestyle
        
        ! Test line only formats
        call parse_format_string('-', marker, linestyle)
        call assert_equal(marker, '', 'Expected no marker')
        call assert_equal(linestyle, '-', 'Expected solid line')
        
        call parse_format_string('--', marker, linestyle)
        call assert_equal(marker, '', 'Expected no marker')
        call assert_equal(linestyle, '--', 'Expected dashed line')
    end subroutine

    subroutine test_parse_marker_and_line()
        character(len=20) :: marker, linestyle
        
        ! Test combined formats
        call parse_format_string('o-', marker, linestyle)
        call assert_equal(marker, 'o', 'Expected circle marker')
        call assert_equal(linestyle, '-', 'Expected solid line')
        
        call parse_format_string('-x', marker, linestyle)
        call assert_equal(marker, 'x', 'Expected x marker')
        call assert_equal(linestyle, '-', 'Expected solid line')
        
        call parse_format_string('x-', marker, linestyle)
        call assert_equal(marker, 'x', 'Expected x marker')
        call assert_equal(linestyle, '-', 'Expected solid line')
    end subroutine

    subroutine assert_equal(actual, expected, message)
        character(len=*), intent(in) :: actual, expected, message
        
        if (trim(actual) /= trim(expected)) then
            write(error_unit, '(A)') 'ASSERTION FAILED: ' // trim(message)
            write(error_unit, '(A)') 'Expected: "' // trim(expected) // '"'
            write(error_unit, '(A)') 'Actual: "' // trim(actual) // '"'
            error stop 1
        end if
    end subroutine

    subroutine test_parse_all_markers()
        character(len=20) :: marker, linestyle
        
        ! Test all supported markers
        call parse_format_string('+', marker, linestyle)
        call assert_equal(marker, '+', 'Expected plus marker')
        call assert_equal(linestyle, '', 'Expected no line style')
        
        call parse_format_string('*', marker, linestyle)
        call assert_equal(marker, '*', 'Expected star marker')
        call assert_equal(linestyle, '', 'Expected no line style')
        
        call parse_format_string('s', marker, linestyle)
        call assert_equal(marker, 's', 'Expected square marker')
        call assert_equal(linestyle, '', 'Expected no line style')
        
        call parse_format_string('^', marker, linestyle)
        call assert_equal(marker, '^', 'Expected up triangle marker')
        call assert_equal(linestyle, '', 'Expected no line style')
        
        call parse_format_string('D', marker, linestyle)
        call assert_equal(marker, 'D', 'Expected diamond marker')
        call assert_equal(linestyle, '', 'Expected no line style')
        
        call parse_format_string('x', marker, linestyle)
        call assert_equal(marker, 'x', 'Expected x marker')
        call assert_equal(linestyle, '', 'Expected no line style')
    end subroutine

    subroutine test_parse_dashed_lines()
        character(len=20) :: marker, linestyle
        
        ! Test dashed line
        call parse_format_string('--', marker, linestyle)
        call assert_equal(marker, '', 'Expected no marker for dashed line')
        call assert_equal(linestyle, '--', 'Expected dashed line style')
        
        ! Test dashed line with marker
        call parse_format_string('o--', marker, linestyle)
        call assert_equal(marker, 'o', 'Expected circle marker with dashed line')
        call assert_equal(linestyle, '--', 'Expected dashed line style')
    end subroutine

    subroutine test_parse_empty_string()
        character(len=20) :: marker, linestyle
        
        ! Test empty format string
        call parse_format_string('', marker, linestyle)
        call assert_equal(marker, '', 'Expected no marker for empty string')
        call assert_equal(linestyle, '', 'Expected no line style for empty string')
    end subroutine

end program