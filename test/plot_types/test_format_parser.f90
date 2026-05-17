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
    call test_scatter_plot_behavior()
    call test_parse_color_only()
    call test_parse_color_with_linestyle()
    call test_parse_color_with_marker_and_linestyle()
    call test_no_color_when_absent()

    write(error_unit, '(A)') 'All format parser tests passed!'

contains

    subroutine test_parse_marker_only()
        character(len=20) :: marker, linestyle

        call parse_format_string('o', marker, linestyle)
        call assert_equal(marker, 'o', 'Expected circle marker')
        call assert_equal(linestyle, 'None', 'Expected no line style for markers only')

        call parse_format_string('x', marker, linestyle)
        call assert_equal(marker, 'x', 'Expected x marker')
        call assert_equal(linestyle, 'None', 'Expected no line style for markers only')
    end subroutine

    subroutine test_parse_line_only()
        character(len=20) :: marker, linestyle

        call parse_format_string('-', marker, linestyle)
        call assert_equal(marker, '', 'Expected no marker')
        call assert_equal(linestyle, '-', 'Expected solid line')

        call parse_format_string('--', marker, linestyle)
        call assert_equal(marker, '', 'Expected no marker')
        call assert_equal(linestyle, '--', 'Expected dashed line')
    end subroutine

    subroutine test_parse_marker_and_line()
        character(len=20) :: marker, linestyle

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

        call parse_format_string('+', marker, linestyle)
        call assert_equal(marker, '+', 'Expected plus marker')
        call assert_equal(linestyle, 'None', 'Expected no line style for markers only')

        call parse_format_string('*', marker, linestyle)
        call assert_equal(marker, '*', 'Expected star marker')
        call assert_equal(linestyle, 'None', 'Expected no line style for markers only')

        call parse_format_string('s', marker, linestyle)
        call assert_equal(marker, 's', 'Expected square marker')
        call assert_equal(linestyle, 'None', 'Expected no line style for markers only')

        call parse_format_string('^', marker, linestyle)
        call assert_equal(marker, '^', 'Expected up triangle marker')
        call assert_equal(linestyle, 'None', 'Expected no line style for markers only')

        call parse_format_string('D', marker, linestyle)
        call assert_equal(marker, 'D', 'Expected diamond marker')
        call assert_equal(linestyle, 'None', 'Expected no line style for markers only')

        call parse_format_string('x', marker, linestyle)
        call assert_equal(marker, 'x', 'Expected x marker')
        call assert_equal(linestyle, 'None', 'Expected no line style for markers only')
    end subroutine

    subroutine test_parse_dashed_lines()
        character(len=20) :: marker, linestyle

        call parse_format_string('--', marker, linestyle)
        call assert_equal(marker, '', 'Expected no marker for dashed line')
        call assert_equal(linestyle, '--', 'Expected dashed line style')

        call parse_format_string('o--', marker, linestyle)
        call assert_equal(marker, 'o', 'Expected circle marker with dashed line')
        call assert_equal(linestyle, '--', 'Expected dashed line style')
    end subroutine

    subroutine test_parse_empty_string()
        character(len=20) :: marker, linestyle

        call parse_format_string('', marker, linestyle)
        call assert_equal(marker, '', 'Expected no marker for empty string')
        call assert_equal(linestyle, '', 'Expected no line style for empty string')
    end subroutine

    subroutine test_scatter_plot_behavior()
        character(len=20) :: marker, linestyle

        call parse_format_string('o', marker, linestyle)
        call assert_equal(marker, 'o', 'Expected circle marker')
        call assert_equal(linestyle, 'None', 'Scatter plot should have NO line style')

        call parse_format_string('s', marker, linestyle)
        call assert_equal(marker, 's', 'Expected square marker')
        call assert_equal(linestyle, 'None', 'Scatter plot should have NO line style')

        call parse_format_string('x', marker, linestyle)
        call assert_equal(marker, 'x', 'Expected x marker')
        call assert_equal(linestyle, 'None', 'Scatter plot should have NO line style')

        call parse_format_string('o-', marker, linestyle)
        call assert_equal(marker, 'o', 'Expected circle marker with line')
        call assert_equal(linestyle, '-', 'Expected solid line with marker')

        call parse_format_string('s--', marker, linestyle)
        call assert_equal(marker, 's', 'Expected square marker with dashed line')
        call assert_equal(linestyle, '--', 'Expected dashed line with marker')
    end subroutine

    subroutine test_parse_color_only()
        character(len=20) :: marker, linestyle, color

        ! Single color letter should return color, no marker or linestyle
        call parse_format_string('k', marker, linestyle, color)
        call assert_equal(color, 'k', 'Expected black color')
        call assert_equal(marker, '', 'Expected no marker for color-only')
        call assert_equal(linestyle, '', 'Expected no linestyle for color-only')

        call parse_format_string('r', marker, linestyle, color)
        call assert_equal(color, 'r', 'Expected red color')

        call parse_format_string('b', marker, linestyle, color)
        call assert_equal(color, 'b', 'Expected blue color')

        call parse_format_string('g', marker, linestyle, color)
        call assert_equal(color, 'g', 'Expected green color')

        call parse_format_string('c', marker, linestyle, color)
        call assert_equal(color, 'c', 'Expected cyan color')

        call parse_format_string('m', marker, linestyle, color)
        call assert_equal(color, 'm', 'Expected magenta color')

        call parse_format_string('y', marker, linestyle, color)
        call assert_equal(color, 'y', 'Expected yellow color')

        call parse_format_string('w', marker, linestyle, color)
        call assert_equal(color, 'w', 'Expected white color')
    end subroutine

    subroutine test_parse_color_with_linestyle()
        character(len=20) :: marker, linestyle, color

        ! k-- = black dashed
        call parse_format_string('k--', marker, linestyle, color)
        call assert_equal(color, 'k', 'Expected black from k--')
        call assert_equal(linestyle, '--', 'Expected dashed from k--')
        call assert_equal(marker, '', 'Expected no marker from k--')

        ! r- = red solid
        call parse_format_string('r-', marker, linestyle, color)
        call assert_equal(color, 'r', 'Expected red from r-')
        call assert_equal(linestyle, '-', 'Expected solid from r-')

        ! b: = blue dotted
        call parse_format_string('b:', marker, linestyle, color)
        call assert_equal(color, 'b', 'Expected blue from b:')
        call assert_equal(linestyle, ':', 'Expected dotted from b:')

        ! g-. = green dashdot
        call parse_format_string('g-.', marker, linestyle, color)
        call assert_equal(color, 'g', 'Expected green from g-.')
        call assert_equal(linestyle, '-.', 'Expected dashdot from g-.')

        ! Color after linestyle: --r
        call parse_format_string('--r', marker, linestyle, color)
        call assert_equal(color, 'r', 'Expected red from --r')
        call assert_equal(linestyle, '--', 'Expected dashed from --r')
    end subroutine

    subroutine test_parse_color_with_marker_and_linestyle()
        character(len=20) :: marker, linestyle, color

        ! ro- = red circle solid
        call parse_format_string('ro-', marker, linestyle, color)
        call assert_equal(color, 'r', 'Expected red from ro-')
        call assert_equal(marker, 'o', 'Expected circle from ro-')
        call assert_equal(linestyle, '-', 'Expected solid from ro-')

        ! k--o = black dashed circle
        call parse_format_string('k--o', marker, linestyle, color)
        call assert_equal(color, 'k', 'Expected black from k--o')
        call assert_equal(linestyle, '--', 'Expected dashed from k--o')
        call assert_equal(marker, 'o', 'Expected circle from k--o')

        ! bo = blue circle (marker only, no linestyle)
        call parse_format_string('bo', marker, linestyle, color)
        call assert_equal(color, 'b', 'Expected blue from bo')
        call assert_equal(marker, 'o', 'Expected circle from bo')
        call assert_equal(linestyle, 'None', 'Expected None linestyle from bo')

        ! ^k: = black triangle dotted (any order)
        call parse_format_string('^k:', marker, linestyle, color)
        call assert_equal(color, 'k', 'Expected black from ^k:')
        call assert_equal(marker, '^', 'Expected triangle from ^k:')
        call assert_equal(linestyle, ':', 'Expected dotted from ^k:')
    end subroutine

    subroutine test_no_color_when_absent()
        character(len=20) :: marker, linestyle, color

        ! No color char in format string
        call parse_format_string('o--', marker, linestyle, color)
        call assert_equal(color, '', 'Expected no color from o--')
        call assert_equal(marker, 'o', 'Expected circle from o--')
        call assert_equal(linestyle, '--', 'Expected dashed from o--')

        ! Empty string
        call parse_format_string('', marker, linestyle, color)
        call assert_equal(color, '', 'Expected no color from empty')
    end subroutine

end program
